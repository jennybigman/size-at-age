# trim grid to groundfish survey footprint

	library(sf)
	library(here)
	library(tidyverse)
	library(lubridate)
	library(data.table)
	
	EBS_survey_grid <- st_read("./data/Kelly shapefiles/EBS_NBS_2019.shp")	

	plot(EBS_survey_grid)	
	
	EBS_survey_grid <- EBS_survey_grid %>% st_transform(crs = 4326)
	
	# need to trim survey grid to strata included in sampling
	
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
	
	survey_polygon <- EBS_survey_grid_trim %>%
		st_union() %>%
		st_simplify()

	plot(survey_polygon)
	
	##### create grid ####
	
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


	# turn grid df into polygon
	polygon <- station_list %>%
		st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%
		summarise(geometry = st_union(geometry)) %>%
		st_convex_hull()
	
	plot(polygon)
	
	# turn into grid
	grid_spacing <- 0.25

	polygon <- 
		sf::st_make_grid(
  	polygon,
  	cellsize = c(grid_spacing, grid_spacing)) %>%
  	sf::st_sf()
	
	plot(polygon)
	
	intersected <- sf::st_intersects(polygon, survey_polygon)
	survey_grid <- polygon[lengths(intersected) > 0, ]

	plot(survey_grid)
	
	overlap <- sf::st_intersection(survey_grid, polygon)
	ggplot(overlap) + geom_sf()
	
	# extract coordinates into a data frame
	# append cell area column if desired
	coord <- survey_grid |>
  	sf::st_centroid() |>
  	sf::st_coordinates() |>
  	as_tibble() 

# plot the grid (colour is cell area)
ggplot(coord, aes(X, Y, colour = area)) +
  geom_tile(width = 2, height = 2, fill = NA) +
  scale_colour_viridis_c(direction = -1) +
  geom_point(size = 0.5) +
  coord_fixed()
	#resolution <- 0.25
	#r <- raster::raster(as(survey_grid, "Spatial"), resolution = resolution)
	#rr <- raster::rasterize(as(survey_grid, "Spatial"), r, getCover = TRUE)
#
	#survey_grid2 <- as.data.frame(raster::rasterToPoints(rr))
#
	#survey_grid2$var <- rnorm(nrow(survey_grid2), 5, 10)
	#
	#ggplot(survey_grid2, aes(x, y, fill = var)) +
	#  geom_raster() +
	#	scale_colour_viridis_c(direction = -1) +
	#  #geom_point(size = 0.5) +
	#  coord_fixed()


		