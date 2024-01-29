# download level 2 hindcast

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)
	library(tidyverse)


	#### hindcast temps ####
	
	# bottom temp ####
	hcsim <- "B10K-K20P19_CORECFS"  # hindcast
	vname1 <- "temp_bottom5m"    # Variable name in filename
	vname2 <- "temp"             # Variable name in file
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
	
	
	bot_temp_hindcast_download_func <- function(yr){

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
  bot_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(bot_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	bot_temp_df <- reshape2::melt(bot_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")

	# translate to lat/lon and time
	bot_temp_df$DateTime <- as.POSIXct(bot_temp_df$Time, origin = "1900-01-01", tz = "GMT")
	bot_temp_df$longitude <- lons[cbind(bot_temp_df$Xi, bot_temp_df$Eta)]
	bot_temp_df$latitude <- lats[cbind(bot_temp_df$Xi, bot_temp_df$Eta)]
	
	return(bot_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	bot_temp_hind_dat_list <- lapply(yr, bot_temp_hindcast_download_func)
	
	bot_temp_hind_dat <- bind_rows(bot_temp_hind_dat_list)
	
	bot_temp_hind_dat$month <- month(bot_temp_hind_dat$DateTime) # month of year
	bot_temp_hind_dat$week <- week(bot_temp_hind_dat$DateTime) # week of year
	bot_temp_hind_dat$year <- year(bot_temp_hind_dat$DateTime)

#	# integrated temp ####
#	
#	hcsim <- "B10K-K20P19_CORECFS"  # hindcast
#	vname1 <- "temp_depthavg"    # Variable name in filename
#	vname2 <- "temp"             # Variable name in file
#	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
#	
#	
#	int_temp_hindcast_download_func <- function(yr){
#
#	# set up filepath
#  fname <- file.path(tdsbase, 
#                     hcsim, 
#                     "Level2",
#                     paste0(yr, "-", yr + 4), 
#                     paste0(hcsim, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
#  
#  # read data
#  ncin <- nc_open(fname)
#  
#  # get lats, longs, ocean_time
#  lats <- ncvar_get(ncin,"lat_rho")
#  lons <- ncvar_get(ncin,"lon_rho")
#  ocean_time <- ncvar_get(ncin, "ocean_time")
#  
#  # get temp
#  temp_array <- ncvar_get(ncin, "temp")
#  
#  # assign dim names and ocean_time
#  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
#  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates
#
#  dimnames(temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
#															 "Time" = ocean_time)
#
#	# turn array into dataframe 
#	int_temp_df <- reshape2::melt(temp_array, 
#														varnames = c("Xi", "Eta", "Time"), 
#														value.name = "temp")
#
#	# translate to lat/lon and time
#	int_temp_df$DateTime <- as.POSIXct(int_temp_df$Time, origin = "1900-01-01", tz = "GMT")
#	int_temp_df$longitude <- lons[cbind(int_temp_df$Xi, int_temp_df$Eta)]
#	int_temp_df$latitude <- lats[cbind(int_temp_df$Xi, int_temp_df$Eta)]
#	
#	return(int_temp_df)
#	
#	# close the connection with the remote netCDF file
#	nc_close(ncin)
#	
#	}
#	
#	yr <- seq(1970, 2020, by=5) 
#	
#	int_temp_hind_dat_list <- lapply(yr, int_temp_hindcast_download_func)
#	
#	int_temp_hind_dat <- bind_rows(int_temp_hind_dat_list)
#	
#	int_temp_hind_dat$month <- month(int_temp_hind_dat$DateTime) # month of year
#	int_temp_hind_dat$week <- week(int_temp_hind_dat$DateTime) # week of year
#	int_temp_hind_dat$year <- year(int_temp_hind_dat$DateTime)
#	
#	
#
	#write_csv(int_temp_hind_dat, file = here("./data/hindcast_int_temp_K20P19.csv"))
	
	# add bottom and integrated 
#	bot_temp_hind_dat <- bot_temp_hind_dat %>%
#		rename(bot_temp = temp)
#	
#	int_temp_hind_dat <- int_temp_hind_dat %>%
#		rename(int_temp = temp)
#	
#	roms_hind_temps <- full_join(bot_temp_hind_dat, int_temp_hind_dat)
	
	fwrite(bot_temp_hind_dat, file = here("./data/roms_hind_temps.csv"))
	