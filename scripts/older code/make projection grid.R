# prediction grid

	# smooth grid that is general area - but how match temps?

	# from https://docs.google.com/document/d/12ZVzOmjbBa8VVTK0tmaNEcN1eP93g6iagDM2c8sSsfo/edit
	grid <- pcod2 %>%
		st_as_sf(coords = c("X", "Y"), 
						 crs = suppressWarnings(sdmTMB::get_crs(pcod2, ll_names = c("longitude", "latitude")))) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	proj_grid <- grid %>%
		st_make_grid(cellsize = rep(10, 2), square = TRUE, what = "centers") %>% # create a grid from the polygon above
		st_sf() %>%
		st_filter(grid) %>%
		st_coordinates() %>%
		as_tibble()
	
	ggplot(proj_grid) +
		geom_raster(aes(X, Y)) +
		scale_fill_viridis_c()


	df_sf <- st_as_sf(proj_grid, coords = c("X", "Y"), 
										crs = get_crs(pcod2, ll_names = c("longitude", "latitude")))
	
	df_geo <- st_transform(df_sf, crs = 4326)
	coords <- st_coordinates(df_geo)
	
	df_coords <- data.frame(
		longitude = coords[,1],
		latitude = coords[,2] * 1000
	)
	

	# use ROMS grid? ####

	ROMS_hind <- read_csv(file = here("./data/hindcast_temp_K20P19.csv")) %>%
  		mutate(long_not_360 = longitude,
					 longitude = case_when(
					 long_not_360 >= 180 ~ long_not_360 - 360,
					 long_not_360 < 180 ~ long_not_360 * -1)) 
  
  # summarise to unique grid cells
  ROMS_hind_sum <- ROMS_hind %>%
  	distinct_at(vars("longitude", "latitude")) 
 
	grid <- ROMS_hind_sum %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = suppressWarnings(sdmTMB::get_crs(ROMS_hind_sum, ll_names = c("longitude", "latitude")))) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	proj_grid <- grid %>%
		st_make_grid(cellsize = rep(10, 2), square = TRUE, what = "centers") %>% # create a grid from the polygon above
		st_sf() %>%
		st_filter(grid) %>%
		st_coordinates() %>%
		as_tibble()
	
	ggplot(proj_grid) +
		geom_raster(aes(X, Y)) +
		scale_fill_viridis_c()


	df_sf <- st_as_sf(proj_grid, coords = c("X", "Y"), 
										crs = get_crs(pcod2, ll_names = c("longitude", "latitude")))
	
	df_geo <- st_transform(df_sf, crs = 4326)
	coords <- st_coordinates(df_geo)
	
	df_coords <- data.frame(
		longitude = coords[,1],
		latitude = coords[,2] * 1000
	)
	
	
	###############
	
	
		grid <- pcod2 %>%
		st_as_sf(coords = c("X", "Y"), 
						 crs = suppressWarnings(sdmTMB::get_crs(pcod2, ll_names = c("longitude", "latitude")))) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	proj_grid <- grid %>%
		st_make_grid(cellsize = rep(25, 2), square = TRUE, what = "centers") %>% # create a grid from the polygon above
		st_sf() %>%
		st_filter(grid) %>%
		st_coordinates() %>%
		as_tibble()
	
	ggplot(proj_grid) +
		geom_raster(aes(X, Y)) +
		scale_fill_viridis_c()


	
	# create survey polygon
	survey_coords <- pcod2 %>%
		distinct_at(vars("longitude", "latitude")) %>%
		select(longitude, latitude)
	
	survey_poly <- survey_coords %>%
		st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
		summarise(geometry = st_combine(geometry)) %>%
		st_cast("POLYGON")
	
	ggplot(survey_poly) + geom_sf()
	
	# summarize ROMS grid to test code
	ROMS_sum <- ROMS_hind %>%
		distinct_at(vars("longitude", "latitude")) %>%
		add_utm_columns(c("longitude", "latitude"))
	
	ROMS_grid_full <- ROMS_sum %>%
		st_as_sf(coords = c("X", "Y"), 
						 crs = suppressWarnings(sdmTMB::get_crs(pcod2, ll_names = c("longitude", "latitude")))) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	ggplot(ROMS_grid_full) + geom_sf()

	ROMS_grid_full_smooth <- ROMS_grid_full %>%
		st_make_grid(cellsize = rep(25, 2), square = TRUE, what = "centers") %>% # create a grid from the polygon above
		st_sf() %>%
		st_filter(grid) %>%
		st_coordinates() %>%
		as_tibble()
	
	# add in temp for a given year
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

	ROMS_grid_full_smooth <- left_join(ROMS_grid_full_smooth, temp,
																		 by = c("X", "Y"))
	
	ggplot(ROMS_grid_full_smooth) +
		geom_raster(aes(X, Y, color = temp)) +
		scale_fill_viridis_c()

	
	ROMS_grid <- 

	ggplot(ROMS_grid) +
		geom_raster(aes(X, Y, fill = val)) +
		scale_fill_viridis_c()

	# try another way
	
	#########################
	
	# create polygon from sampled locations
	survey_grid <- pcod2 %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = 4326) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	ROMS_sum <- ROMS_hind %>%
		distinct_at(vars("longitude", "latitude")) %>%
		st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)

	# filter ROMS grid by survey_polygon
	ROMS_grid_trim <- st_filter(ROMS_sum, survey_grid)
	
	ggplot(ROMS_grid_trim) + geom_sf()
	
	ROMS_grid <- ROMS_hind %>%
		mutate(bin = paste(longitude, latitude))
	
	ROMS_grid_trim <- ROMS_grid_trim %>%
		mutate(bin = paste(longitude, latitude))
	
	bins <- ROMS_grid_trim$bin
	
	ROMS_grid <- ROMS_grid %>%
		filter(bin %in% bins) %>%
		group_by(latitude, longitude, year)

	# can you predict with it?
	
	# pull out one year
	test1993 <- ROMS_grid %>%
		filter(year == 1993)
	
	ggplot(test1993) +
		geom_point(aes(longitude, latitude))
	
	test1993 <- test1993 %>%
		
		select(temp, latitude, longitude)
	
	
	
	
	
	
	
	ROMS_smooth_poly <- ROMS_grid %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = 4326) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	ggplot(ROMS_smooth_poly) + geom_sf()
	
	ROMS_smooth_grid <- ROMS_smooth_poly %>%
		st_make_grid(cellsize = 100, square = TRUE, what = "centers") %>% # create a grid from the polygon above
		st_sf() %>%
		st_filter(ROMS_smooth_poly) %>%
		st_coordinates() %>%
		as_tibble()
	
	proj_grid <- proj_grid %>%
		mutate(val = rnorm(n(), 0, 1))

	ggplot(proj_grid) +
		geom_raster(aes(X, Y, fill = val)) +
		scale_fill_viridis_c()

	
	# prediction grid at specified locations and resolution - grid based on the locations of observations
	# from https://github.com/pbs-assess/sdmTMB-teaching/blob/main/imr-2023/exercises/coastal-survey-ex/coastal-survey-index-south.R#L205

	# prediction grid
	res <- 10 # in km
	
	pcod2$X_bin <- floor(pcod2$X/res) * res
	pcod2$Y_bin <- floor(pcod2$Y/res) * res
	
	n_cells <- length(unique(paste0(pcod2$X_bin, pcod2$Y_bin)))
	pcod2$XY_bin <- paste(pcod2$X_bin, pcod2$Y_bin)

	# get mean temp by bin by year
	temp <- dplyr::group_by(pcod2, XY_bin, year) %>%
  	dplyr::summarise(mean_temp = mean(presurvey_btemp))
	
	pred_grid <- data.frame(XY_bin = unique(pcod2$XY_bin))
	pred_grid <- dplyr::left_join(pred_grid, temp)
	pred_grid$X <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 1)))
	pred_grid$Y <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 2)))
	pred_grid <- dplyr::select(pred_grid, X, Y, mean_temp, year)

	# convert dataframe to sf object and set CRS
	pred_grid$X_m <- pred_grid$X * 1000
	pred_grid$Y_m <- pred_grid$Y * 1000
	

	df_sf <- st_as_sf(pred_grid, coords = c("X_m", "Y_m"), 
										crs = get_crs(pcod2, ll_names = c("longitude", "latitude")))
	
	df_geo <- st_transform(df_sf, crs = 4326)
	coords <- st_coordinates(df_geo)
	
	# create a new dataframe with the coordinates
	df_coords <- data.frame(
		longitude = coords[,1],
		latitude = coords[,2],
		presurvey_btemp = pred_grid$mean_temp,
		year = pred_grid$year
	)
	
	# check lat/long conversion
	ggplot(df_coords, aes(longitude, latitude)) +
		geom_point() + theme_bw() +
		facet_wrap(~ year)
	
	## predict
	#pred_grid$presurvey_btemp <- pred_grid$mean_temp
#
	#preds <- predict(mod_st, newdata = pred_grid, return_tmb_object = TRUE)
	#
	#pred_grid$est <- preds$data$est
#
	## plot fit of mod from other file
	#ggplot(pred_grid) +
	#	geom_raster(aes(X, Y, fill = est)) +
	#	scale_fill_viridis_c() +
	#	facet_wrap(~ year)

		
	# try something else
	### other way

 grid2 <- pcod2 %>%
 	select(year, presurvey_btemp, longitude, latitude, X, Y, age_f) %>%
 	distinct_all()

	bs <- proj_grid %>%
	  st_as_sf(crs=4326,coords=c("X","Y")) %>%
	  st_transform(crs=32604)
	
	bs1 <- bs %>%
	  mutate(longitude=st_coordinates(.)[,1],
	         latitude=st_coordinates(.)[,2]) %>%
	  st_drop_geometry() %>%
	  dplyr::select(c(latitude, longitude, presurvey_btemp, age_f))

	# what about predicting with observations? 
	
	preds_dat <- predict(mod_st)
	
	ggplot(preds_dat, aes(X, Y, color = est)) +
		geom_point() + theme_sleek() +
		facet_wrap(~ year)

	
	