# projection grid

	# load ROMS hindcast K20P19 and convert longs to survey format
  ROMS_hind <- read_csv(file = here("./data/hindcast_temp_K20P19.csv")) %>%
  		mutate(long_not_360 = longitude,
					 longitude = case_when(
					 long_not_360 >= 180 ~ long_not_360 - 360,
					 long_not_360 < 180 ~ long_not_360 * -1)) 
  
  # summarise to unique grid cells
  ROMS_hind_sum <- ROMS_hind %>%
  	distinct_at(vars("longitude", "latitude")) 
  
  # plot(ROMS_hind_sum$longitude, ROMS_hind_sum$latitude) # quick plot
  
  # convert to sf object
  ROMS_hind_sum_sf <- ROMS_hind_sum %>%
  	st_as_sf(crs = 4326, coords = c("longitude", "latitude"), remove = FALSE) %>%
  	st_transform(crs = 32604)
  
  # sampled locations
  survey_grid <- pcod_dat %>%
  	select(longitude, latitude) %>%
  	distinct_at(vars("longitude", "latitude"))
  
  # to sf object
  survey_grid_sf <- survey_grid %>%
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
  	group_by(longitude, latitude) %>%
  	summarise(temp = mean(temp)) %>%
  	na.omit() %>%
  	st_as_sf(crs = 4326, coords = c("longitude", "latitude"), remove = FALSE) 
  
  temp1993_trim <- temp1993 %>% select(latitude, longitude)
  
  test <- geo_inner_join(grid_in_conv, temp1993_trim)

  ggplot(test, aes(longitude, latitude, color = temp)) +
  	geom_point()
  

  test <- nn2(grid_in_conv[, c("longitude", "latitude")],
  						temp1993[, c("longitude", "latitude")],
  						k = 1) %>% as_tibble()

  temp1993_trim$latitud
  
