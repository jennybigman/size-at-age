
	# prediction grid - works but need to resolve dateline issue and trim to survey area

	ROMS_sum <- ROMS_hind %>%
		distinct_at(vars("longitude", "latitude")) %>%
		add_utm_columns(c("longitude", "latitude"))

	res <- 10 # in km
	ROMS_sum$X_bin <- floor(ROMS_sum$X / res) * res
	ROMS_sum$Y_bin <- floor(ROMS_sum$Y / res) * res
	n_cells <- length(unique(paste0(ROMS_sum$X_bin, ROMS_sum$Y_bin)))
	ROMS_sum$XY_bin <- paste(ROMS_sum$X_bin, ROMS_sum$Y_bin)

	# get mean temp by bin

	temp1993 <- ROMS_hind %>% 
		filter(year == 1993) %>%
		group_by(latitude, longitude) %>%
		summarise(temp = mean(temp)) %>%
		add_utm_columns(c("longitude", "latitude"), ll_crs = 4326)

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
	
	
	# limit range limits
	
	df_coords_test <- df_coords %>%
		rename(longitude = lon,
					 latitude = lat) %>%
		filter(between(latitude, 54, 63)) %>%
		filter(between(longitude, -178, -160))
	
	
	ggplot(df_coords_test, aes(longitude, latitude, color=temp)) +
	   geom_point() + theme_bw() +
		scale_color_viridis_c()
	
	
	# add in land
	world <- ne_countries(scale = "medium",
                      returnclass = "sf") 

	US_map <- world %>%
  	filter(name %in% c("United States"))

	# Crop the polygon for plotting and efficiency:
	# st_bbox(map_data) # find the rough coordinates
	AK_coast <- suppressWarnings(suppressMessages(
  	st_crop(US_map,
    c(xmin = -179, ymin = 54, xmax = -155, ymax = 65))))

	utm_zone <- 4326
	AK_coast_proj <- sf::st_transform(AK_coast, crs = utm_zone)
	
	ggplot(AK_coast_proj) + geom_sf() +
		geom_point(data = df_coords_test, aes(longitude, latitude, color = temp))
	


	# trim ROMS grid to survey area #### NOT WORKING
	
	# survey area pcod
	survey_grid <- pcod2 %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = 4326) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	ggplot(survey_grid) + geom_sf()
	
	ggplot() +
		geom_sf(data = survey_grid) +
		geom_sf(data = AK_coast_proj)

	# convert lat/long to same form as survey and trim to unique locations
	ROMS_grid <- ROMS_hind %>%
		distinct_at(vars("longitude", "latitude")) %>%
		mutate(long_not_360 = longitude,
					 longitude = case_when(
						 long_not_360 >= 180 ~ long_not_360 - 360,
						 long_not_360 < 180 ~ long_not_360 * -1))  %>% 
		st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)

	# trimming
	ROMS_grid_trim <- st_filter(ROMS_grid, survey_grid)
	
	# turn into polygon to see plot
	ROMS_grid_trim_poly <- ROMS_grid_trim %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = 4326) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	ggplot(ROMS_grid_trim_poly) + geom_sf()
	
	# create smooth grid 
	
	ROMS_grid_trim <- ROMS_grid_trim %>%
		add_utm_columns(c("longitude", "latitude"))
	
	res <- 10 # in km
	ROMS_grid_trim$X_bin <- floor(ROMS_grid_trim$X / res) * res
	ROMS_grid_trim$Y_bin <- floor(ROMS_grid_trim$Y / res) * res
	
	n_cells <- length(unique(paste0(ROMS_grid_trim$X_bin, ROMS_grid_trim$Y_bin)))
	ROMS_grid_trim$XY_bin <- paste(ROMS_grid_trim$X_bin, ROMS_grid_trim$Y_bin)

	# get mean temp by bin for one year as an example
	temp1993 <- ROMS_hind %>% 
		filter(year == 1993) %>%
		group_by(year, latitude, longitude) %>%
		add_utm_columns(c("longitude", "latitude"))

	res <- 10 # in km
	temp1993$X_bin <- floor(temp1993$X / res) * res
	temp1993$Y_bin <- floor(temp1993$Y / res) * res
	temp1993$XY_bin <- paste(temp1993$X_bin, temp1993$Y_bin)
	
	temp <- dplyr::group_by(temp1993, XY_bin) %>%
  	dplyr::summarise(temp = mean(temp))

	ROMS_grid_trim_smooth <- left_join(ROMS_grid_trim, temp)
	
	ROMS_grid_trim_smooth <- na.omit(ROMS_grid_trim_smooth)
	
	ggplot(ROMS_grid_trim_smooth) +
		geom_raster(aes(X, Y, color = temp)) +
		scale_fill_viridis_c()


	
	
	
	
	
	depth <- dplyr::group_by(ROMS_grid_trim, XY_bin) %>%
	  dplyr::summarise(temp = mean(BottomDepth))
	pred_grid <- data.frame(XY_bin = unique(dfc_south$XY_bin))
	pred_grid <- dplyr::left_join(pred_grid, depth)
	pred_grid$X <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 1)))
	pred_grid$Y <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 2)))
	pred_grid <- dplyr::select(pred_grid, X, Y, BottomDepth)
	
	# convert dataframe to sf object and set CRS
	pred_grid$X_m <- pred_grid$X*1000
	pred_grid$Y_m <- pred_grid$Y*1000
	df_sf <- st_as_sf(pred_grid, coords = c("X_m", "Y_m"), crs = get_crs(dfc_south, ll_names=c('lon','lat')))
	df_geo <- st_transform(df_sf, crs = 4326) # transform to geographic coordinates (longitude/latitude)
	coords <- st_coordinates(df_geo)

