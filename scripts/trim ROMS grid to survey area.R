	# create ROMS grid

	# convert longs so same as survey dat
	ROMS_hind <- ROMS_hind %>%
		mutate(long_not_360 = longitude,
					 longitude = case_when(
					 long_not_360 >= 180 ~ long_not_360 - 360,
					 long_not_360 < 180 ~ long_not_360 * -1)) 
	
	# retain only unique locations
	ROMS_grid <- ROMS_hind %>%
		distinct_at(vars("latitude", "longitude"))
	
	# convert to sf obj
	ROMS_grid_sf <- ROMS_grid %>% 
		st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) 

	# survey area pcod
	survey_grid <- pcod2 %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = 4326) %>% # 
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon

	# trimming
	ROMS_grid_trim_sf <- st_filter(ROMS_grid_sf, survey_grid)

	# trim non-sf df
	ROMS_grid_trim_sf$latlong <- paste(ROMS_grid_trim_sf$latitude, ROMS_grid_trim_sf$longitude)
	
	ROMS_grid$latlong <- paste(ROMS_grid$latitude, ROMS_grid$longitude)
	
	latlongs <- ROMS_grid_trim_sf$latlong
	
	ROMS_grid_trim <- ROMS_grid %>% filter(latlong %in% latlongs)
	
	# add utm_cols
	
	ROMS_grid_trim <- ROMS_grid_trim %>%
		add_utm_columns(c("longitude", "latitude"))

	res <- 10 # in km
	ROMS_grid_trim$X_bin <- floor(ROMS_grid_trim$X / res) * res
	ROMS_grid_trim$Y_bin <- floor(ROMS_grid_trim$Y / res) * res
	n_cells <- length(unique(paste0(ROMS_grid_trim$X_bin, ROMS_grid_trim$Y_bin)))
	ROMS_grid_trim$XY_bin <- paste(ROMS_grid_trim$X_bin, ROMS_grid_trim$Y_bin)

	# add in temp - function for each year
	
	#smooth_grid_func <- function(x) {
		
		ROMS_yr_temp <- ROMS_hind %>% 
			filter(year == 1993) %>%
			group_by(latitude, longitude) %>%
			summarise(temp = mean(temp)) %>%
			add_utm_columns(c("longitude", "latitude"))
	
		res <- 10 # in km
		ROMS_yr_temp$X_bin <- floor(ROMS_yr_temp$X / res) * res
		ROMS_yr_temp$Y_bin <- floor(ROMS_yr_temp$Y / res) * res
		
		ROMS_yr_temp$XY_bin <- paste(ROMS_yr_temp$X_bin, ROMS_yr_temp$Y_bin)
		
		temp <- dplyr::group_by(ROMS_yr_temp, XY_bin) %>%
  		dplyr::summarise(temp = mean(temp))
		
		XY_bins <- ROMS_grid_trim$XY_bin
		
		temp_trim <- temp %>% filter(XY_bin %in% XY_bins)
	
		ROMS_grid <- dplyr::left_join(ROMS_grid_trim, temp)
		
		ROMS_grid_test <- na.omit(ROMS_grid)
		
		ROMS_grid_test$X <- as.numeric(unlist(lapply(strsplit(ROMS_grid_test$XY_bin," "), getElement, 1)))
		ROMS_grid_test$Y <- as.numeric(unlist(lapply(strsplit(ROMS_grid_test$XY_bin," "), getElement, 2)))
		ROMS_grid_test <- dplyr::select(ROMS_grid_test, X, Y, temp)
		
		ROMS_grid_test$year <- 1993
	
		#}
	
	yrs <- sort(unique(ROMS_hind$year))
	
	ROMS_grids <- lapply(1993, smooth_grid_func)
	

	test_sf <- ROMS_grid_test %>%
				st_as_sf(coords = c("X", "Y"), remove = FALSE, crs = 4326) 

	ggplot(test_sf) + geom_sf()
	
	
	
	
	
	
	
	
	
		# add in temps
	ROMS_temp_yr <- ROMS_hind %>%
		mutate(long_not_360 = longitude,
					 longitude = case_when(
					 long_not_360 >= 180 ~ long_not_360 - 360,
					 long_not_360 < 180 ~ long_not_360 * -1)) %>%
		group_by(year, latitude, longitude) %>%
		summarise(temp = mean(temp))
	

	
		ROMS_grid_trim2 <- left
	
	
	
	
	# for a given year
	
	ROMS_temp_1993 <- ROMS_grid_trim %>%
		filter(year == 1993)
	
	
	
	
	
	res <- 10 # in km
	ROMS_sum$X_bin <- floor(ROMS_sum$X / res) * res
	ROMS_sum$Y_bin <- floor(ROMS_sum$Y / res) * res
	n_cells <- length(unique(paste0(ROMS_sum$X_bin, ROMS_sum$Y_bin)))
	ROMS_sum$XY_bin <- paste(ROMS_sum$X_bin, ROMS_sum$Y_bin)

	# get mean temp by bin

	temp_yr <- ROMS_hind %>% 
		group_by(year, latitude, longitude) %>%
		summarise(temp = mean(temp)) %>%
		add_utm_columns(c("longitude", "latitude"))

	res <- 10 # in km
	temp_yr$X_bin <- floor(temp_yr$X / res) * res
	temp_yr$Y_bin <- floor(temp_yr$Y / res) * res
	
	temp1993$XY_bin <- paste(temp1993$X_bin, temp1993$Y_bin)
	
	temp <- dplyr::group_by(temp1993, XY_bin) %>%
  	dplyr::summarise(temp = mean(temp))

	pred_grid1993 <- dplyr::left_join(ROMS_sum, temp)
	
	pred_grid1993$X <- as.numeric(unlist(lapply(strsplit(pred_grid1993$XY_bin," "), getElement, 1)))
	pred_grid1993$Y <- as.numeric(unlist(lapply(strsplit(pred_grid1993$XY_bin," "), getElement, 2)))
	pred_grid1993 <- dplyr::select(pred_grid1993, X, Y, temp)

	# convert dataframe to sf object and set CRS 
	pred_grid1993$X_m <- pred_grid1993$X*1000
	pred_grid1993$Y_m <- pred_grid1993$Y*1000
	df_sf1993 <- st_as_sf(pred_grid1993, coords = c("X_m", "Y_m"), crs = get_crs(ROMS_sum, ll_names=c('longitude','latitude')))
	df_geo1993 <- st_transform(df_sf1993, crs = 4326) # transform to geographic coordinates (longitude/latitude)
	coords1993 <- st_coordinates(df_geo1993)

	# Create a new dataframe with the coordinates
	df_coords <- data.frame(
	  lon = coords1993[, 1],
	  lat = coords1993[, 2],
	  temp = pred_grid1993$temp
	)

	df_coords <- na.omit(df_coords)
	# check lat lon conversion
	ggplot(df_coords, aes(lon, lat, color=temp)) +
	   geom_point() + theme_bw() +
		scale_color_viridis_c()
