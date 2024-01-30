# download and concatenate level 2 ROMS output

#-------------------
# Setup
#-------------------

	library(ncdf4)
	library(lubridate)
	library(abind)
	library(tidyverse)
	
	yrblock <- seq(1970, 2020, by=5) # Calibration block
	
	# Variable to read 
	
	vname1 <- "temp_bottom5m" # Variable name in filename
	vname2 <- "temp"          # Variable name in file
	
	vname3 <- "oxygen_bottom5m"
	vname4 <- "oxygen"
		
	# Simulation names
	
	hcsim <- "B10K-K20P19_CORECFS"                  # hindcast
	#fcsim <- "B10K-K20P19_CMIP6_cesm_historical" # forecast, historical period
	#fcssp <- "B10K-K20P19_CMIP6_cesm_ssp585"     # forecast, SSP 5-8.5
	
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
	
	#--------------------
	# Read hindcast data
	#--------------------
	
	for (yr in yrblock) {
	
	  fname <- file.path(tdsbase, 
	                     hcsim, 
	                     "Level2",
	                     paste0(yr, "-", yr+4), 
	                     paste0(hcsim, "_", yr, "-", yr+4, "_average_", vname1, ".nc"))
	  
	# Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == yrblock[1]){
    thc <- ttmp
    vhc <- vtmp
  } else {
    thc <- c(thc, ttmp)
    vhc <- abind(vhc, vtmp)
  }
}
 
	# convert to df
	temp <- reshape2::melt(vhc, varnames = c("Xi", "Eta", "Time"), 
						value.name = "temp")
	
	# add and format time column
	temp$DateTime <- thc
	
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
	

	fwrite(temp, file = here("./data/ROMS_hind_bottom_temp.csv"))
	
	temp_yr <- temp %>%
		group_by(year) %>%
		summarise(count = n())
	
	
	
	
	
	#### oxygen ####
	
	#--------------------
	# Read hindcast data
	#--------------------
	
	for (yr in yrblock) {
	
	  fname <- file.path(tdsbase, 
	                     hcsim, 
	                     "Level2",
	                     paste0(yr, "-", yr+4), 
	                     paste0(hcsim, "_", yr, "-", yr+4, "_average_", vname3, ".nc"))
	  
	# Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  toxy <- ncvar_get(ncin,"ocean_time")
  toxy <- lubridate::ymd("1900-01-01") + lubridate::seconds(toxy)
  voxy <- ncvar_get(ncin, vname4)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == yrblock[1]){
    thc <- toxy
    vhc <- voxy
  } else {
    thc <- c(thc, toxy)
    vhc <- abind(vhc, voxy)
  }
}
 
	# convert to df
	oxygen <- reshape2::melt(vhc, varnames = c("Xi", "Eta", "Time"), 
						value.name = "oxygen")
	
	# add and format time column
	oxygen$DateTime <- thc
	
	oxygen$month <- month(oxygen$DateTime) # month of year
	oxygen$week <-   week(oxygen$DateTime) # week of year
	oxygen$year <-   year(oxygen$DateTime)
	
	# get lat/longs and add to temp df
	
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
	 
	nc <- nc_open(paste(url_base, opendap_area, sep=""))
	 
	# create objects for known lats and longs and xi and eta axes
	lats <- ncvar_get(nc,"lat_rho")
	lons <- ncvar_get(nc,"lon_rho")
	
	# add and format lat/long columns
	oxygen$longitude <- lons[cbind(oxygen$Xi, oxygen$Eta)]
	oxygen$latitude <-  lats[cbind(oxygen$Xi, oxygen$Eta)]

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
	oxygen <- left_join(oxygen, domain_df)
	
	oxygen <- oxygen %>% filter(domain > 0)

	# examine output
	oxygen_sum <- oxygen %>%
		group_by(latitude, longitude, month, year) %>%
		summarise(oxygen = mean(oxygen))
	
	# are there NAs? 
	oxygen_sum_NAs <- oxygen_sum[!complete.cases(oxygen_sum), ]
	
	# where are they
	oxygen_NAs_loc <- oxygen_sum_NAs %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
				 longitude >= 180 ~ longitude - 360,
			   longitude < 180 ~ longitude * -1))
	
	missing_roms <- 
		ggplot(AK_coast_proj) +
		geom_sf() +
		geom_point(data = oxygen_NAs_loc,
							 aes(x = longitude, y = latitude)) 
	
	ggsave(here("./plots/map_missing_roms_output.jpg"),
				 missing_roms)
	
	# what years?
	oxygen_sum_NAs <- oxygen_sum_NAs %>%
		group_by(month, year) %>%
		summarise(number = n())
	

	fwrite(oxygen, file = here("./data/ROMS_hind_bottom_oxygen.csv"))

	#### forecast ####	













