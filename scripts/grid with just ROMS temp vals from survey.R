# prediction grid
res <- 10 # in km
pcod2$X_bin <- floor(pcod2$X / res) * res
pcod2$Y_bin <- floor(pcod2$Y / res) * res
n_cells <- length(unique(paste0(pcod2$X_bin, pcod2$Y_bin)))
pcod2$XY_bin <- paste(pcod2$X_bin, pcod2$Y_bin)

# get mean depth by bin
temp <- dplyr::group_by(pcod2, XY_bin) %>%
  dplyr::summarise(temp = mean(presurvey_btemp))

pred_grid <- data.frame(XY_bin = unique(pcod2$XY_bin))
pred_grid <- dplyr::left_join(pred_grid, temp)
pred_grid$X <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 1)))
pred_grid$Y <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 2)))
pred_grid <- dplyr::select(pred_grid, X, Y, temp)

# convert dataframe to sf object and set CRS 
pred_grid$X_m <- pred_grid$X*1000
pred_grid$Y_m <- pred_grid$Y*1000
df_sf <- st_as_sf(pred_grid, coords = c("X_m", "Y_m"), crs = get_crs(pcod2, ll_names=c('longitude','latitude')))
df_geo <- st_transform(df_sf, crs = 4326) # transform to geographic coordinates (longitude/latitude)
coords <- st_coordinates(df_geo)

# Create a new dataframe with the coordinates
df_coords <- data.frame(
  lon = coords[, 1],
  lat = coords[, 2],
  temp = pred_grid$temp
)
# check lat lon conversion
ggplot(df_coords, aes(lon, lat, color=temp)) +
   geom_point() + theme_bw() 

# need all covariates (bottom depth, year, X, Y)
predgrid <- crossing(pred_grid, year=unique(dfc_south$year))
predgrid$fyear <- factor(predgrid$year)
predgrid_geo <- crossing(df_coords, fyear=unique(dfc_south$fyear))
preds <- predict(fit5, newdata = predgrid, return_tmb_object = TRUE, type = "response")
predgrid$est <- preds$data$est
predgrid_geo$est <- predgrid$est


ggplot(df_coords, aes(lon, lat, color=temp)) +
   geom_point() + theme_bw() 


######### below works and has land but no temps -- how to add in temps? 

	grid <- pcod2 %>%
		st_as_sf(coords = c("longitude", "latitude"), 
						 crs = suppressWarnings(sdmTMB::get_crs(pcod2, ll_names = c("longitude", "latitude")))) %>% # convert to sf frame with geometry col using X & Y
		summarise(geometry = st_combine(geometry)) %>% # combine into next highest category, multipoint
		st_convex_hull() # make polygon
	
	proj_grid <- grid %>%
		st_make_grid(n = 100, square = TRUE, what = "centers") %>% # create a grid from the polygon above
		st_sf() %>%
		st_filter(grid) %>%
		st_coordinates() %>%
		as_tibble()
	
	n <- distinct_at(proj_grid, vars(X, Y))
	
	proj_grid <- proj_grid %>%
		mutate(val = rnorm(n(), 0, 1))

	ggplot(proj_grid) +
		geom_raster(aes(X, Y, fill = val)) +
		scale_fill_viridis_c() + 
	geom_sf(data = AK_coast_proj)

	# try to add in temp
	res <- 10 # in km
	pcod2$X_bin <- floor(pcod2$X / res) * res
	pcod2$Y_bin <- floor(pcod2$Y / res) * res
	n_cells <- length(unique(paste0(pcod2$X_bin, pcod2$Y_bin)))
	pcod2$XY_bin <- paste(pcod2$X_bin, pcod2$Y_bin)
	
	# get mean temp by bin
	temp <- dplyr::group_by(pcod2, XY_bin) %>%
	  dplyr::summarise(mean_temp = mean(presurvey_btemp))
	
	pred_grid <- data.frame(XY_bin = unique(pcod2$XY_bin))

	pred_grid <- dplyr::left_join(pred_grid, temp)
	
	pred_grid$X <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 1)))
	pred_grid$Y <- as.numeric(unlist(lapply(strsplit(pred_grid$XY_bin," "), getElement, 2)))
	pred_grid <- dplyr::select(pred_grid, X, Y, mean_temp)

	ggplot(pred_grid) +
		geom_raster(aes(X, Y, fill = mean_temp)) +
		scale_fill_viridis_c()

	# convert X/Y to lat/long
	proj_grid$X_m <- proj_grid$X * 1000
	proj_grid$Y_m <- proj_grid$Y * 1000
	
	proj_grid_sf <- st_as_sf(proj_grid, coords = c("X_m", "Y_m"), crs = get_crs(pcod2, ll_names=c('longitude','latitude')))

	proj_grid_geo <- st_transform(proj_grid_sf, crs = 4326) # transform to geographic coordinates (longitude/latitude)
	coords <- st_coordinates(proj_grid_geo)
	
	df_coords <- data.frame(
  	lon = coords[, 1],
  	lat = coords[, 2],
  	val = proj_grid$val
	)
	
	# check lat lon conversion
	ggplot(df_coords, aes(lon, lat, color = val)) +
   geom_point() + theme_bw() 
	
	# add land
	
