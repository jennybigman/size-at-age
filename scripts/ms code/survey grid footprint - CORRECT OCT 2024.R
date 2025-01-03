# trim grid to groundfish survey footprint

	# load libraries
	library(sf)
	library(here)
	library(tidyverse)
	library(lubridate)
	library(data.table)
	library(mapdata)
	
	# turn off spherical geometry
	sf_use_s2(FALSE)
	
	## prep shapefile ####
	
	# read in groundfish trawl survey grid shapefile
	EBS_survey_grid <- st_read("./data/Kelly shapefiles/EBS_NBS_2019.shp")	

	plot(EBS_survey_grid)	
	
	# transform to different CRS
	EBS_survey_grid <- EBS_survey_grid %>% st_transform(crs = 4326)
	
	plot(EBS_survey_grid)
	
	
	## trim survey grid to strata included in dataset ####
	
	
	# read in EBS bottom trawl survey data

	# create object names for file names
	dat_prestring <- paste0(here(), ("/data/"))

  file_list <- list.files(path = paste0(here(), ("/data/")))
  
  # pull out specimen data
  spec_dat_file_list <- file_list[str_detect(file_list, "raw_data")]

  dat_list <- list()
  
  for(i in spec_dat_file_list){
  	dat_list[[i]] <- paste0(dat_prestring, i)
  	dat_list
  }

  # read in all files in list
  df_list <- list()
  
  	for(i in dat_list){
  		df_list[[i]] <- readRDS(i)
  		df_list
  	}
  
  # grab the stratum each station is in
  haul_dat <- df_list[[1]]$haul %>%
  	dplyr::select(STRATUM, STATIONID) %>%
  	rename(stationid = STATIONID)
  
	# add strata to secies dfs
	station_filter <- function(sp){
		
		sp_dat <- dat_all %>% filter(short_name == sp)
		
		sp_dat <- left_join(sp_dat, haul_dat)
	
		
	}
	
	sp <- unique(dat_all$short_name)

	sp_dfs <- purrr::map(sp, station_filter)
	
	all_dfs <- sp_dfs %>% bind_rows()
	

	# all strata with samples
	stratums <- unique(all_dfs$STRATUM)	

	# trim survey grid raster
	EBS_survey_grid_trim <- EBS_survey_grid %>%
		filter(STRATUM %in% stratums)
	
	plot(EBS_survey_grid_trim)
	
	## convert polygon to raster for to make grid ####
	
	# combine strate polygons
	survey_polygon <- EBS_survey_grid_trim %>%
		st_union() 

	# create a grid using the raster package
	resolution <- 0.25
	r <- raster::raster(as(survey_polygon, "Spatial"), resolution = resolution)
	rr <- raster::rasterize(as(survey_polygon, "Spatial"), r, getCover = TRUE)
	
	grid <- as.data.frame(raster::rasterToPoints(rr))
	grid$area <- grid$layer * resolution * resolution
	
	grid <- dplyr::filter(grid, area > 0) %>%
	  dplyr::select(-layer) 
	
	# create a variable to test plot 
	var <- rnorm(nrow(grid), 5, 10)
	grid <- grid %>%
		mutate(var = var)
	
	ggplot(grid, aes(x, y, fill = var)) +
	  geom_raster() +
	  scale_colour_viridis_c(direction = -1) +
	  coord_fixed()
	
	# add land 
	reg = map_data("world2Hires")
	reg = subset(reg, region %in% c('USSR', 'USA'))
		
	# convert lat longs
	reg$long = (360 - reg$long)*-1
	
	# set map limits
	lons = c(-181, -160)
	lats = c(52, 65)
	
	ggplot(grid, aes(x, y, fill = var)) +
	  geom_tile() +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	           fill = "darkgrey", color = NA) +
	  scale_colour_viridis_c(direction = -1) +
		coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
		theme_sleek()
	
	
	# is there a grid that has an empty val?
	NAs <- grid[!complete.cases(grid), ]
	
	# save grid
	fwrite(grid, file = here("data/grid.csv"))

	###########################################
	
	# add roms temps 
	
	grid <- grid %>%
		dplyr::select(-var, -area) %>%
		rename(lat = y,
					 lon = x)
	
	grid <- 
		sdmTMB::add_utm_columns(
			grid,
			c("lon", "lat"))
	
	# read in level 2 (by grid cell) ROMS hindcast bottom temps
	#temp <- fread(file = here("./data/ROMS_hind_bottom_temp.csv"))

	temp <- fread(file = here("./data/hindcast_temp_K20P19.csv"))

	# convert lat/longs and summarize by year
	roms_temps <- temp %>%
		ungroup() %>%
		drop_na(temp) |>
		group_by(latitude, longitude, year) %>%
		summarise(temp = mean(temp)) %>%
		mutate(
			long = longitude,
			longitude = case_when(
					 long >= 180 ~ long - 360,
					 long < 180 ~ long)) 
	
	yrs <- unique(roms_temps$year)
	
	temp_yr_plots <- purrr::map(yrs, \(yr){
		
		d <- roms_temps |> filter(year == yr)
		
		p <- 
			ggplot(d, aes(longitude, latitude, color = temp)) +
		  geom_point() +
			geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	          fill = "darkgrey", color = NA) +
			scale_colour_viridis_c(direction = -1) +
			scale_fill_continuous(limits = c(-1.4, 5.2), breaks = seq(-1, 5, by = 1)) +
			coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
			theme_sleek()  +
			ggtitle(yr)
		
		p
		
	})
	
	
	# add UTM cols
	roms_temps <- roms_temps %>%
		sdmTMB::add_utm_columns(
			c("longitude", "latitude")
		)
	
	# function to pull ROMS temps for each grid cell for each year
	
	fill_temps <- function(x){
		
	# summarize to year
	roms_temp_yr <- roms_temps %>%
		filter(year == x)

	grid <- grid %>%
		mutate(year = x)
	
	grid[, c(6, 7)] <- as.data.frame(
		RANN::nn2(roms_temp_yr[, c('Y', 'X')],
              grid[, c('Y', 'X')], k = 1))
	
	# Match nearest lat/long from ROMS
	grid$temp <- pull(roms_temp_yr[c(grid$nn.idx), 'temp'])

	# remove unecessary cols
	grid <- grid %>%
		dplyr::select(-contains("nn"))

	grid
	
	}
	
	yrs <- sort(unique(dat_all$year))
	
	grid_full <- lapply(yrs, fill_temps) %>% bind_rows()

	fwrite(grid_full, paste0(here(), "/data/prediction_grid.csv"))

	# plot temps
	file_path_plots <- paste0(here(), "/plots/")
	
	plot_fun <- function(x){
		
		yr_dat <- grid_full %>% filter(year == x)
		
		p <- 
			ggplot(yr_dat, aes(lon, lat, fill = temp)) +
		  geom_raster() +
			geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	          fill = "darkgrey", color = NA) +
			scale_colour_viridis_c(direction = -1) +
			scale_fill_continuous(breaks = seq(-1, 5, by = 1)) +
			coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
			theme_sleek() +
			ggtitle(x)
		
	p 
		#plot_name <- paste0("temp2_", x, ".png")
		
		#ggsave(p, file = paste0(file_path_plots, plot_name),
		#			 height = 5, width = 10, units = "in")
		
	}
	
	plots <- purrr:::map(yrs, plot_fun)
	
