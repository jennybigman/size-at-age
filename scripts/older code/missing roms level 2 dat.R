# missing ROMS level 2 dat?

	temp_hind_dat <- temp_hind_dat %>%
		mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
		roms_grid <- temp_hind_dat %>%
		distinct_at(vars(latitude, longitude)) 
	
	roms_grid$roms_ID <- 1:nrow(roms_grid)
	
	roms_grid$roms_lat <- roms_grid$latitude
	roms_grid$roms_lon <- roms_grid$longitude

	roms_grid2 <- roms_grid %>%
		select(-latitude, -longitude)
	
	roms_grid_trim <- left_join(survey_grid, roms_grid2)
	
	roms_grid_trim$latlong <- paste0(roms_grid_trim$roms_lat, roms_grid_trim$roms_lon)
	temp_hind_dat$latlong <- paste0(temp_hind_dat$latitude, temp_hind_dat$longitude)
	
	keep_ll <- unique(roms_grid_trim$latlong)
	
	roms_temp_trim <- temp_hind_dat %>%
		filter(latlong %in% keep_ll)
	
	test <- roms_temp_trim[!complete.cases(roms_temp_trim), ]
