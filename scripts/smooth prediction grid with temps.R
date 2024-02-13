# prediction grid

	library(tidyverse)
	library(sf)
	library(mapdata)
	#library(raster)


	#read in specimen data to get station lat/lons
	specimen_dat <- read_csv(file = here("./data/dat_all.csv")) %>%
		dplyr::select(-X, -...1, -sex, -start_time, -bottom_depth, -surface_temperature, -species_code) 
	
	# put each species df into a list
	specimen_dat_list <- specimen_dat %>%
		group_by(species_name) %>%
  	group_split() 
	
	# get all unique survey locations 
	station_func <- function(df){
		
		df <- df %>%
		dplyr::select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	}
	
	station_list <- purrr::map(specimen_dat_list, station_func) %>% 
		bind_rows() %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))

	# create grid df
	lats <- seq(min(station_list$latitude), max(station_list$latitude), length.out = 70)
	lons <- seq(min(station_list$longitude), max(station_list$longitude), length.out = 90)
	
	grid_extent <- expand.grid(lons, lats)
	names(grid_extent) <- c('lon', 'lat')
	
	# turn grid df into polygon
	polygon <- station_list %>%
		st_as_sf(coords = c("longitude", "latitude")) %>%
		summarise(geometry = st_union(geometry)) %>%
		st_convex_hull()

	# create a grid using the raster package
	resolution <- 0.25
	r <- raster::raster(as(polygon, "Spatial"), resolution = resolution)
	rr <- raster::rasterize(as(polygon, "Spatial"), r, getCover = TRUE)

	grid <- as.data.frame(raster::rasterToPoints(rr))
	grid$area <- grid$layer * resolution * resolution
	grid <- dplyr::filter(grid, area > 0) |> 
	  dplyr::select(-layer)
	
	grid$var <- rnorm(nrow(grid), 5, 10)
	
	ggplot(grid, aes(x, y, fill = var)) +
	  geom_raster() +
		scale_colour_viridis_c(direction = -1) +
	  coord_fixed()
	
	# match to temp using nearest neighbor
	survey_grid <- grid %>%
		rename(lon = x,
					 lat = y) %>%
		dplyr::select(-area, -var)
	
	survey_grid <- 
		sdmTMB::add_utm_columns(
			survey_grid,
			c("lon", "lat"))
	
	# read in level 2 (by grid cell) ROMS hindcast bottom temps
	temp <- fread(file = here("./data/ROMS_hind_bottom_temp.csv"))

	# convert lat/longs and summarize by year
	roms_temps <- temp %>%
		mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1)) %>%
		group_by(latitude, longitude, year) %>%
		summarise(temp = mean(temp))
	
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

	survey_grid <- survey_grid %>%
		mutate(year = x)
	
	survey_grid[, c(6, 7)] <- as.data.frame(
		RANN::nn2(roms_temp_yr[, c('Y', 'X')],
              survey_grid[, c('Y', 'X')], k = 1))
	
	# Match nearest lat/long from ROMS
	survey_grid$temp <- pull(roms_temp_yr[c(survey_grid$nn.idx), 'temp'])

	# remove unecessary cols
	survey_grid <- survey_grid %>%
		dplyr::select(-contains("nn"))

	survey_grid
	
	}
	
	yrs <- sort(unique(dat_all$year))
	
	survey_grid_full <- lapply(yrs, fill_temps) %>% bind_rows()

	fwrite(survey_grid_full, file = here("./data/survey_grid_temps.csv"))
	
	# plot temps
	reg = map_data("world2Hires")
	reg = subset(reg, region %in% c('USSR', 'USA'))
	
	# convert lat longs
	reg$long = (360 - reg$long)*-1
	
	# set map limits
	lons = c(-181, -160)
	lats = c(52, 65)
	
	file_path_plots <- paste0(here(), "/plots/")
	
	
	plot_fun <- function(x){
		
		yr_dat <- survey_grid_full %>% filter(year == x)
		
		p <-
			ggplot(yr_dat, aes(lon, lat, fill = temp)) +
		  geom_raster() +
			geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	            fill = "darkgrey", color = NA) +
			scale_colour_viridis_c(direction = -1) +
		  coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
			scale_fill_continuous(limits = c(-1.4, 5.2), breaks = seq(-1, 5, by = 1)) +
			ggtitle(x) +
			theme_sleek()
		
		plot_name <- paste0("temp_", x, ".png")
		
		ggsave(p, file = paste0(file_path_plots, plot_name),
					 height = 5, width = 10, units = "in")
		
	}
	
	purrr::map(yrs, plot_fun)
	

		
