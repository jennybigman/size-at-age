
	# download and transform level 2 hindcast output

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
  library(lubridate)
	library(data.table)
	library(tidync)
	library(tidyverse)
	library(ggsidekick)
	library(sf)
	
	sf_use_s2(FALSE)


	#### hindcast temps ####
	
	hcsim <- "B10K-K20P19_CORECFS"  # hindcast
	vname1 <- "temp_bottom5m"    # Variable name in filename
	vname2 <- "temp"             # Variable name in file
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
	
	
	temp_hindcast_download_func <- function(yr){

	# set up filepath
  fname <- file.path(tdsbase, 
                     hcsim, 
                     "Level2",
                     paste0(yr, "-", yr + 4), 
                     paste0(hcsim, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
  
  # read data
  ncin <- nc_open(fname)
  
  # get lats, longs, ocean_time
  lats <- ncvar_get(ncin,"lat_rho")
  lons <- ncvar_get(ncin,"lon_rho")
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get temp
  temp_array <- ncvar_get(ncin, vname2)
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	temp_df <- reshape2::melt(temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")

	# translate to lat/lon and time
	temp_df$DateTime <- as.POSIXct (temp_df$Time, origin = "1900-01-01", tz = "GMT")
	temp_df$longitude <- lons[cbind(temp_df$Xi, temp_df$Eta)]
	temp_df$latitude <- lats[cbind (temp_df$Xi, temp_df$Eta)]
	
	return(temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	temp_hind_dat_list <- lapply(yr, temp_hindcast_download_func)
	
	temp_hind_dat <- bind_rows(temp_hind_dat_list)
	
	temp_hind_dat$month <- lubridate::month(temp_hind_dat$DateTime) # month of year
	temp_hind_dat$week <-  lubridate::week  (temp_hind_dat$DateTime) # week of year
	temp_hind_dat$year <-  lubridate::year  (temp_hind_dat$DateTime)
	
	# plot
	
	temp_yr_sum <- temp_hind_dat %>%
		group_by(year) %>%
		summarise(mean_val = mean(temp, na.rm = TRUE)) %>%
		mutate(var = "temp",
					 mean_val_std = as.vector(scale(mean_val)))

	yrs <- 1987:2023
	
	temp_yr_sum <- temp_yr_sum %>%
		filter(year %in% yrs)

	breaks = c(-2, 0, 2, 4)
	labels = c(-2, 0, 2, 4)
	lims = c(-2.5, 4.25)

	ggplot(temp_yr_sum) +
		geom_line(aes(x = year, y = mean_val_std)) +
			scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) +
		theme_sleek()
	
#	write_csv(temp_hind_dat, file = here("./data/hindcast_temp_K20P19.csv"))
	
	
	
	
	
	#### hindcast oxygen ####

	hcsim <- "B10K-K20P19_CORECFS"  # hindcast
	vname1 <- "oxygen_bottom5m"    # Variable name in filename
	vname2 <- "oxygen"             # Variable name in file
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
	
	
	oxy_hindcast_download_func <- function(yr){

	# set up filepath
  fname <- file.path(tdsbase, 
                     hcsim, 
                     "Level2",
                     paste0(yr, "-", yr + 4), 
                     paste0(hcsim, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
  
  # read data
  ncin <- nc_open(fname)
  
  # get lats, longs, ocean_time
  lats <- ncvar_get(ncin,"lat_rho")
  lons <- ncvar_get(ncin,"lon_rho")
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get oxygen
  oxy_array <- ncvar_get(ncin, "oxygen")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(oxy_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															"Time" = ocean_time)

	# turn array into dataframe 
	oxy_df <- reshape2::melt(oxy_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "oxygen")

	# translate to lat/lon and time
	oxy_df$DateTime <- as.POSIXct (oxy_df$Time, origin = "1900-01-01", tz = "GMT")
	oxy_df$longitude <- lons[cbind(oxy_df$Xi, oxy_df$Eta)]
	oxy_df$latitude <- lats[cbind (oxy_df$Xi, oxy_df$Eta)]
	
	return(oxy_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	oxy_hind_dat_list <- lapply(yr, oxy_hindcast_download_func)
	
	oxy_hind_dat <- bind_rows(oxy_hind_dat_list)
	
	oxy_hind_dat$month <- lubridate::month (oxy_hind_dat$DateTime) # month of year
	oxy_hind_dat$week <-  lubridate::week  (oxy_hind_dat$DateTime) # week of year
	oxy_hind_dat$year <-  lubridate::year  (oxy_hind_dat$DateTime)
	
	#write_csv(oxy_hind_dat, file = here("./data/hindcast_oxy_K20P19.csv"))
	
	###################################################################################
	# check by plotting
	###################################################################################

	
	temp_hind_dat <- read_csv(file = here("./data/hindcast_temp_K20P19.csv"))
	oxy_hind_dat <- read_csv(file = here("./data/hindcast_oxy_K20P19.csv"))
	

	temp_yr_sum <- temp_hind_dat %>%
		dplyr::group_by(year) %>%
		dplyr::summarise(mean_val = mean(temp, na.rm = TRUE)) %>%
		mutate(var = "temp",
					 mean_val_std = as.vector(scale(mean_val)))

	yrs <- 1987:2023
	
	temp_yr_sum <- temp_yr_sum %>%
		filter(year %in% yrs)
	
	# oxygen
	oxy_yr_sum <- oxy_hind_dat %>%
		group_by(year) %>%
		summarise(mean_val = mean(oxygen, na.rm = TRUE)) %>%
		mutate(var = "oxygen",
					 mean_val_std = as.vector(scale(mean_val)))

	yrs <- 1987:2023
	
	oxy_yr_sum <- oxy_yr_sum %>%
		filter(year %in% yrs)
	
	envr_vars <- bind_rows(temp_yr_sum, oxy_yr_sum)
	
		
	breaks = c(-2, 0, 2, 4)
	labels = c(-2, 0, 2, 4)
	lims = c(-2.5, 4.25)

	
	p1 <- 
		ggplot(envr_vars) +
		geom_line(aes(x = year, y = mean_val_std, group = var, color = var)) +
			scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) +
		theme_sleek()
	
		ggsave(here("./plots/envr_time_series_ROMS.png"), p1,
				 height = 4, width = 7.5)

	
	## over survey grid ####
		
	EBS_survey_grid <- st_read("./data/Kelly shapefiles/EBS_NBS_2019.shp")	

	plot(EBS_survey_grid)	
	
	EBS_survey_grid <- st_transform(EBS_survey_grid, 4326)
	
	plot(EBS_survey_grid)

	# trim temp & oxygen datasets to SEBS grid ####
	
	# unify polygon
	BS_poly <- st_union(EBS_survey_grid$geometry)

	plot(BS_poly)	
	
	temp_sf <- temp_hind_dat %>%
		mutate(lon = case_when(
			longitude >= 180 ~ longitude - 360,
			longitude < 180 ~ longitude))  %>% 
		rename(lat = latitude) %>%
  	st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
	oxy_sf <- oxy_hind_dat %>%
		mutate(lon = case_when(
			longitude >= 180 ~ longitude - 360,
			longitude < 180 ~ longitude))  %>% 
		rename(lat = latitude) %>%
  	st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
	
	temp_survey_grid <- temp_sf %>%
  	st_intersection(BS_poly) 
	
	temp_survey_grid <- temp_survey_grid %>%
  	st_transform(crs = 4326) 
	
	temp_survey_grid <- temp_survey_grid %>%
  	mutate(longitude = st_coordinates(.)[,1],
  				 latitude = st_coordinates(.)[,2]) %>%
  	st_drop_geometry()
  
	# do the same for this 
	oxy_survey_grid <- oxy_sf %>%
  	st_intersection(BS_poly) 
	
	oxy_survey_grid <- oxy_survey_grid %>%
  	st_transform(crs = 4326) %>%
  	mutate(longitude = st_coordinates(.)[,1],
  				 latitude = st_coordinates(.)[,2]) %>%
  	st_drop_geometry()

	# write_csv(temp_survey_grid, file = here("./data/temp_survey_grid_hind.csv"))
	# write_csv(oxy_survey_grid, file = here("./data/oxy_survey_grid_hind.csv"))
  
	# summarize by year
	
	# how many NAs
	
	# temp
	temp_cc <- temp_survey_grid %>% filter(is.na(temp))
	unique(temp_cc$year)
	
	NA_sum <- temp_cc %>% group_by(year, month) %>%
		summarise(count = n())
	
	temp_cc_sum <- temp_cc %>%
		group_by(lat, lon) %>%
		summarise(mean_temp = mean(temp))
	
	# map these by year
	ggplot(temp_cc_sum) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  							 fill = "darkgrey", color = NA) +
		coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) + 
		geom_point(aes(x = lon, y = lat))
	
	# oxygen
	oxy_cc <- oxy_survey_grid %>% filter(is.na(oxygen))
	unique(oxy_cc$year)
	
	NA_sum <- oxy_cc %>% group_by(year, month) %>%
		summarise(count = n())
	
	oxy_cc_sum <- oxy_cc %>%
		group_by(lat, lon) %>%
		summarise(oxygen = mean(oxygen))
	
	# map these by year
	ggplot(oxy_cc_sum) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  							 fill = "darkgrey", color = NA) +
		coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) + 
		geom_point(aes(x = lon, y = lat))

	
	# can probably drop them - on land?
		
	temp_grid_sum <- temp_survey_grid %>%
		drop_na(temp) %>%
 		group_by(year) %>%
		summarise(mean_val = mean(temp)) %>%
		mutate(var = "temp",
					 mean_val_std = as.vector(scale(mean_val)))

	oxy_grid_sum <- oxy_survey_grid %>%
		drop_na() %>%
		group_by(year) %>%
		summarise(mean_val = mean(oxygen)) %>%
		mutate(var = "oxygen",
					 mean_val_std = as.vector(scale(mean_val)))

	envr_var_grid <- bind_rows(temp_grid_sum, oxy_grid_sum) %>%
		filter(year %in% yrs)
	
	p2 <- 
		ggplot(envr_var_grid) +
		geom_line(aes(x = year, y = mean_val_std, 
									group = var, color = var)) +
		scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) +
		theme_sleek()
	
	ggsave(here("./plots/envr_time_series_ROMS_surveygrid.png"), p2,
				 				 height = 4, width = 7.5)


	# calculating presurvey temp ####
	presurvey_mo <- 4:6 # (April to June)
  
  presurvey_temp <- temp_survey_grid %>%
  	drop_na(temp) %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(year) %>%
  	summarise(pre_val = mean(temp)) %>%
  	filter(year %in% yrs) %>%
  	mutate(var = "pre_temp",
  				 pre_val_std = as.vector(scale(pre_val))) 

	 presurvey_oxy <- oxy_survey_grid %>%
  	drop_na(oxygen) %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(year) %>%
  	summarise(pre_val = mean(oxygen)) %>%
	 	filter(year %in% yrs) %>%
	 	mutate(var = "pre_oxygen",
  				 pre_val_std = as.vector(scale(pre_val))) 
	 
	 pre_survey_vars <- bind_rows(presurvey_temp, presurvey_oxy)
	 
	p3 <- 
		ggplot(pre_survey_vars) +
		geom_line(aes(x = year, y = pre_val_std, 
									group = var, color = var)) +
		scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) + 
		theme_sleek()
	
	ggsave(here("./plots/envr_time_series_ROMS_surveygrid_presurvey.png"), p3,
				 				 height = 4, width = 7.5)

	 # calculating year prior temp ####
	 
	  yr_prior_temp_func <- function(x){
   	
   	# temp same year as collected, Jan - June
   	current <- temp_survey_grid %>%
   		drop_na(temp) %>%
   		filter(year == x & month <= 6)
 
   	# temp previous year July - Dec
		previous <- temp_survey_grid %>%
			drop_na() %>%
  		filter(year == x - 1 & month > 6)
 
		# combine and take a mean temp July - June
  	var_yr <- bind_rows(current, previous) %>%
  		summarise(yr_val = mean(temp)) %>%
  		mutate(var = "yr_temp",
  					 year = x)
   
   }
   
  yr_prior_temp <- lapply(1970:2023, yr_prior_temp_func) %>% bind_rows()
  
  yr_prior_temp_trim <- yr_prior_temp %>% 
  	filter(year %in% yrs) %>%
  	 	mutate(yr_val_std = as.vector(scale(yr_val))) 
	
  # calculating year prior temp ####
	 
	 yr_prior_oxy_func <- function(x){
   	
   	# temp same year as collected, Jan - June
   	current <- oxy_survey_grid %>%
   		drop_na() %>%
   		filter(year == x & month <= 6)
 
   	# temp previous year July - Dec
		previous <- oxy_survey_grid %>%
			drop_na() %>%
  		filter(year == x - 1 & month > 6)
 
		# combine and take a mean temp July - June
  	var_yr <- bind_rows(current, previous) %>%
  		summarise(yr_val = mean(oxygen)) %>%
  		mutate(var = "yr_oxy",
  					 year = x)
   
   }
   
  yr_prior_oxy <- lapply(1970:2023, yr_prior_oxy_func) %>% bind_rows()
  
  yr_prior_oxy_trim <- yr_prior_oxy %>% 
  	filter(year %in% yrs) %>%
  	 	mutate(yr_val_std = as.vector(scale(yr_val))) 
	
  yr_prior_vars <- bind_rows(yr_prior_temp_trim, yr_prior_oxy_trim)
  
	p4 <- 
		ggplot(yr_prior_vars) +
		geom_line(aes(x = year, y = yr_val_std, 
									group = var, color = var)) +
		scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) +
		theme_sleek()
	
	ggsave(here("./plots/envr_time_series_ROMS_surveygrid_yrprior.png"), p4,
				 				 height = 4, width = 7.5)
