# projection grid

	# load ROMS hindcast K20P19 and convert longs to survey format
  ROMS_hind <- fread(file = here("./data/ROMS_hind_bottom_temp.csv")) %>%
  	mutate(longitude = case_when(
				   longitude >= 180 ~ longitude - 360,
			     longitude < 180 ~ longitude * -1)) 
  
  # summarise to unique grid cells
  ROMS_hind_sum <- temp %>%
  	distinct_at(vars("longitude", "latitude")) 
  
  # plot(ROMS_hind_sum$longitude, ROMS_hind_sum$latitude) # quick plot
  
  # convert to sf object
  ROMS_hind_sum_sf <- ROMS_hind_sum %>%
  	st_as_sf(crs = 4326, coords = c("longitude", "latitude"), remove = FALSE) %>%
  	st_transform(crs = 32604)
  
  # sampled locations
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

  # to sf object
  survey_grid_sf <- station_list %>%
  	st_as_sf(crs = 4326, coords = c("longitude", "latitude"), remove = FALSE) %>%
  	st_transform(crs = 32604)

  buff_dist <- 20000
  buff <- st_buffer(survey_grid_sf, buff_dist, dissolve = TRUE) %>%
  	st_union()
  
  conv <- st_convex_hull(st_union(survey_grid_sf))

  grid_in_conv <- ROMS_hind_sum_sf %>%
  	st_intersection(conv) %>%
  	st_transform(crs = 4326) %>%
  	mutate(longitude = st_coordinates(.)[,1],
  				 latitude = st_coordinates(.)[,2]) %>%
  	st_drop_geometry()
  
  plot(grid_in_conv)
  
  # can I add in temps?
  temp1993 <- ROMS_hind %>%
  	filter(year == 1993) %>%