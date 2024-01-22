	temp1993 <- ROMS_hind %>% 
		filter(year == 1993) %>%
		group_by(latitude, longitude) %>%
		summarise(temp = mean(temp)) %>%
		add_utm_columns(c("longitude", "latitude"))

	res <- 10 # in km
	temp1993$X_bin <- floor(temp1993$X / res) * res
	temp1993$Y_bin <- floor(temp1993$Y / res) * res
	
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
	
	# trim this by survey grid?
	survey_grid <- pcod2 %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = 4326) %>% # 
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon

	df_coords_sf <- df_coords %>%
		rename(longitude = lon,
					 latitude = lat) %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = 4326, remove = FALSE)
	
	# trimming
	ROMS_grid_trim_sf <- st_filter(df_coords_sf, survey_grid)

	ggplot(ROMS_grid_trim_sf, aes(longitude, latitude, color=temp)) +
	   geom_point() + theme_bw() +
		scale_color_viridis_c()
