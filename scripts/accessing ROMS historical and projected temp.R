	# Variable to read 
	
	library(ncdf4)
	library(lubridate)
	library(abind)
	library(tidyverse)
	
	yrblock <- seq(1980, 2014, by = 5) # Calibration block

	vname1 <- "temp_bottom5m" # Variable name in filename
	vname2 <- "temp"          # Variable name in file
	
	# Simulation names
	fcsim <- "B10K-K20P19_CMIP6_cesm_historical" # forecast, historical period
	fcssp_low <- "B10K-K20P19_CMIP6_cesm_ssp126"
	fcssp_hi <- "B10K-K20P19_CMIP6_cesm_ssp585"     # forecast, SSP 5-8.5
	
	# PMEL server 
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server

	#--------------------
	# Read forecast data
	# (historical period)
	#--------------------

	for (yr in yrblock) {

	fname <- file.path(tdsbase, 
	                   fcsim, 
	                   "Level2",
	                   paste0(yr, "-", yr + 4), 
	                   paste0(fcsim, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
	 
	# Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == yrblock[1]){
    tfc <- ttmp
    vfc <- vtmp
  } else {
    tfc <- c(tfc, ttmp)
    vfc <- abind(vfc, vtmp)
  }
}

	# convert to df
	temp <- reshape2::melt(vfc, varnames = c("Xi", "Eta", "Time"), 
						value.name = "temp")
	
	# add and format time column
	temp$DateTime <- tfc
	
	temp$month <- month(temp$DateTime) # month of year
	temp$week <-   week(temp$DateTime) # week of year
	temp$year <-   year(temp$DateTime)
	
	# get lat/longs and add to temp df
	
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
	 
	nc <- nc_open(paste(url_base, opendap_area, sep=""))
	 
	# create objects for known lats and longs and xi and eta axes
	lats <- ncvar_get(nc,"lat_rho")
	lons <- ncvar_get(nc,"lon_rho")
	
	# add and format lat/long columns
	temp$longitude <- lons[cbind(temp$Xi, temp$Eta)]
	temp$latitude <-  lats[cbind(temp$Xi, temp$Eta)]

	# get domain df  
	xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
	eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  # download domain array
  domain_array<- ncvar_get(nc, "domain_feast")
  
   # name the dimensions
  dim(domain_array)
  
  dimnames(domain_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

  # turn into dataframe from array
  domain_df <- reshape2::melt(domain_array, 
                  varnames=c("Xi", "Eta"), 
                  value.name="domain")
  
  # add lat/long cols
  domain_df$longitude <- lons[cbind(domain_df$Xi,domain_df$Eta)]
  domain_df$latitude <- lats[cbind(domain_df$Xi,domain_df$Eta)]

	# remove domains not needed 
	temp <- left_join(temp, domain_df)
	
	temp <- temp %>% filter(domain > 0)

	# examine output
	temp_sum <- temp %>%
		group_by(latitude, longitude, month, year) %>%
		summarise(temp = mean(temp))
	
	# are there NAs? 
	temp_sum_NAs <- temp_sum[!complete.cases(temp_sum), ]
	
	# where are they
	temp_NAs_loc <- temp_sum_NAs %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
				 longitude >= 180 ~ longitude - 360,
			   longitude < 180 ~ longitude * -1))
	
	missing_roms <- 
		ggplot(AK_coast_proj) +
		geom_sf() +
		geom_point(data = temp_NAs_loc,
							 aes(x = longitude, y = latitude)) 
	
	ggsave(here("./plots/map_missing_roms_output.jpg"),
				 missing_roms)
	
	# what years?
	temp_NAs_time <- temp_sum_NAs %>%
		group_by(month, year) %>%
		summarise(number = n())
	

	fwrite(temp, file = here("./data/ROMS_hist_cesm_bottom_temp.csv"))
	
	
	#--------------------
	# Read forecast data
	# (SSP)
	#--------------------

	#### LOW EMISSION ####
	for (yr in seq(2015, 2095, by=5)) { 
  
  	fname <- file.path(tdsbase, 
	                   fcssp_low, 
	                   "Level2",
	                   paste0(yr, "-", yr + 4), 
	                   paste0(fcssp_low, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
	 
  
  # Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == 2015){
    tssp <- ttmp
    vssp <- vtmp
  } else {
    tssp <- c(tssp, ttmp)
    vssp <- abind(vssp, vtmp)
  }
}


	# convert to df
	temp <- reshape2::melt(vssp, varnames = c("Xi", "Eta", "Time"), 
						value.name = "temp")
	
	# add and format time column
	temp$DateTime <- tssp
	
	temp$month <- month(temp$DateTime) # month of year
	temp$week <-   week(temp$DateTime) # week of year
	temp$year <-   year(temp$DateTime)
	
	# get lat/longs and add to temp df
	
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
	 
	nc <- nc_open(paste(url_base, opendap_area, sep=""))
	 
	# create objects for known lats and longs and xi and eta axes
	lats <- ncvar_get(nc,"lat_rho")
	lons <- ncvar_get(nc,"lon_rho")
	
	# add and format lat/long columns
	temp$longitude <- lons[cbind(temp$Xi, temp$Eta)]
	temp$latitude <-  lats[cbind(temp$Xi, temp$Eta)]

	# get domain df  
	xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
	eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  # download domain array
  domain_array<- ncvar_get(nc, "domain_feast")
  
   # name the dimensions
  dim(domain_array)
  
  dimnames(domain_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

  # turn into dataframe from array
  domain_df <- reshape2::melt(domain_array, 
                  varnames=c("Xi", "Eta"), 
                  value.name="domain")
  
  # add lat/long cols
  domain_df$longitude <- lons[cbind(domain_df$Xi,domain_df$Eta)]
  domain_df$latitude <- lats[cbind(domain_df$Xi,domain_df$Eta)]

	# remove domains not needed 
	temp <- left_join(temp, domain_df)
	
	temp <- temp %>% filter(domain > 0)

	# examine output
	temp_sum <- temp %>%
		group_by(latitude, longitude, month, year) %>%
		summarise(temp = mean(temp))
	
	# are there NAs? 
	temp_sum_NAs <- temp_sum[!complete.cases(temp_sum), ]
	
	# where are they
	temp_NAs_loc <- temp_sum_NAs %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
				 longitude >= 180 ~ longitude - 360,
			   longitude < 180 ~ longitude * -1))
	
	missing_roms <- 
		ggplot(AK_coast_proj) +
		geom_sf() +
		geom_point(data = temp_NAs_loc,
							 aes(x = longitude, y = latitude)) 
	
	ggsave(here("./plots/map_missing_roms_output.jpg"),
				 missing_roms)
	
	# what years?
	temp_NAs_time <- temp_sum_NAs %>%
		group_by(month, year) %>%
		summarise(number = n())
	

	fwrite(temp, file = here("./data/ROMS_proj_cesm_low_bottom_temp.csv"))
	
	
	#### HIGH EMISSION ####
	for (yr in seq(2015, 2095, by=5)) { 
  
  	fname <- file.path(tdsbase, 
	                   fcssp_hi, 
	                   "Level2",
	                   paste0(yr, "-", yr + 4), 
	                   paste0(fcssp_hi, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
	 
  
  # Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == 2015){
    tssp <- ttmp
    vssp <- vtmp
  } else {
    tssp <- c(tssp, ttmp)
    vssp <- abind(vssp, vtmp)
  }
}


	# convert to df
	temp <- reshape2::melt(vssp, varnames = c("Xi", "Eta", "Time"), 
						value.name = "temp")
	
	# add and format time column
	temp$DateTime <- tssp
	
	temp$month <- month(temp$DateTime) # month of year
	temp$week <-   week(temp$DateTime) # week of year
	temp$year <-   year(temp$DateTime)
	
	# get lat/longs and add to temp df
	
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
	 
	nc <- nc_open(paste(url_base, opendap_area, sep=""))
	 
	# create objects for known lats and longs and xi and eta axes
	lats <- ncvar_get(nc,"lat_rho")
	lons <- ncvar_get(nc,"lon_rho")
	
	# add and format lat/long columns
	temp$longitude <- lons[cbind(temp$Xi, temp$Eta)]
	temp$latitude <-  lats[cbind(temp$Xi, temp$Eta)]

	# get domain df  
	xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
	eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  # download domain array
  domain_array<- ncvar_get(nc, "domain_feast")
  
   # name the dimensions
  dim(domain_array)
  
  dimnames(domain_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

  # turn into dataframe from array
  domain_df <- reshape2::melt(domain_array, 
                  varnames=c("Xi", "Eta"), 
                  value.name="domain")
  
  # add lat/long cols
  domain_df$longitude <- lons[cbind(domain_df$Xi,domain_df$Eta)]
  domain_df$latitude <- lats[cbind(domain_df$Xi,domain_df$Eta)]

	# remove domains not needed 
	temp <- left_join(temp, domain_df)
	
	temp <- temp %>% filter(domain > 0)

	# examine output
	temp_sum <- temp %>%
		group_by(latitude, longitude, month, year) %>%
		summarise(temp = mean(temp))
	
	# are there NAs? 
	temp_sum_NAs <- temp_sum[!complete.cases(temp_sum), ]
	
	# where are they
	temp_NAs_loc <- temp_sum_NAs %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
				 longitude >= 180 ~ longitude - 360,
			   longitude < 180 ~ longitude * -1))
	
	missing_roms <- 
		ggplot(AK_coast_proj) +
		geom_sf() +
		geom_point(data = temp_NAs_loc,
							 aes(x = longitude, y = latitude)) 
	
	ggsave(here("./plots/map_missing_roms_output.jpg"),
				 missing_roms)
	
	# what years?
	temp_NAs_time <- temp_sum_NAs %>%
		group_by(month, year) %>%
		summarise(number = n())
	

	fwrite(temp, file = here("./data/ROMS_proj_cesm_high_bottom_temp.csv"))
	
	