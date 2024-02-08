	# prediction grid not with raster pkg

	#read in specimen data to get station lat/lons
	specimen_dat <- read_csv(file = here("./data/dat_all.csv")) %>%
		select(-X, -...1, -sex, -start_time, -bottom_depth, -surface_temperature, -species_code) 
	
	# put each species df into a list
	specimen_dat_list <- specimen_dat %>%
		group_by(species_name) %>%
  	group_split() 
	
	# get all unique survey locations 
	station_func <- function(df){
		
		df <- df %>%
		select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	}
	
	station_list <- map(specimen_dat_list, station_func) %>% 
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
	
	plot(polygon)
		
	# choose a grid size in units of our polygon shape file
	grid_spacing <- 0.5

	# create smooth grid over the bounding box of the polygon
	full_grid <- sf::st_make_grid(
	  polygon,
	  cellsize = c(grid_spacing, grid_spacing)
	) %>%
	  sf::st_sf()
	
	ggplot(full_grid) + geom_sf()
	
	
	# subset our grid to cells that intersect our polygon:
	intersected <- sf::st_intersects(full_grid, polygon)
	survey_grid <- full_grid[lengths(intersected) > 0, ]

	# plot it
	ggplot() +
	  geom_sf(data = polygon) +
	  geom_sf(data = survey_grid, fill = NA)
	
	overlap <- sf::st_intersection(survey_grid, polygon)
	
	ggplot(overlap) + geom_sf()
	
	calculated_area <- sf::st_area(overlap)
	
	ggplot(coord, aes(X, Y, colour = area)) +
  	geom_tile(width = 2, height = 2, fill = NA) +
  	scale_colour_viridis_c(direction = -1) +
  	geom_point(size = 0.5) +
  	coord_fixed()
	
	# smooth grid - yay
	ggplot(coord, aes(X, Y, fill = area)) +
  	geom_tile(width = 2, height = 2) +
  	scale_colour_viridis_c(direction = -1) +
  	#geom_point(size = 0.5) +
  	coord_fixed()
	
#############################################################################################
	# with raster package #####
#############################################################################################

######## extra raster code ######

# summarize to distinct locations 
	roms_grid <- temp %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	

	survey_grid[, c(5, 6)] <- as.data.frame(
		RANN::nn2(roms_grid[, c('latitude', 'longitude')],
              survey_grid[, c('lat', 'lon')], k = 1))
	
	temp_yr <- temp %>%
		group_by(latitude, longitude, year) %>%
		summarise(temp = mean(temp))
	
	# select one year just for testing code
	temp_yr_2000 <- temp_yr %>% filter(year == 2000)
	
	# Match nearest lat/long from ROMS
	survey_grid$temp <- pull(temp_yr_2000[c(survey_grid$nn.idx), 'temp'])

	# remove unecessary cols
	survey_grid <- survey_grid %>%
		select(-contains("nn"))
	
	# plot
	ggplot(survey_grid, aes(lon, lat, fill = temp)) +
	  geom_raster() +
	  scale_colour_viridis_c(direction = -1) +
	  #geom_point(size = 0.5) +
	  coord_fixed()

	