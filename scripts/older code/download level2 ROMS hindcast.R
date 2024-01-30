# ROMS grid - trim to survey area

	
	# download and transform level 2 hindcast output

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)


	#### hindcast temps ####
	
	hcsim <- "B10K-K20P19_CORECFS"  # hindcast
	vname1 <- "temp_bottom5m"    # Variable name in filename
	vname2 <- "temp"             # Variable name in file
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
	
	
	hindcast_download_func <- function(yr){

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
  temp_array <- ncvar_get(ncin, "temp")
  
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
	temp_df$DateTime <- as.POSIXct(temp_df$Time, origin = "1900-01-01", tz = "GMT")
	temp_df$longitude <- lons[cbind(temp_df$Xi, temp_df$Eta)]
	temp_df$latitude <- lats[cbind(temp_df$Xi, temp_df$Eta)]
	
	return(temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	temp_hind_dat_list <- lapply(yr, hindcast_download_func)
	
	temp_hind_dat <- bind_rows(temp_hind_dat_list)
	
	temp_hind_dat$month <- month(temp_hind_dat$DateTime) # month of year
	temp_hind_dat$week <- week(temp_hind_dat$DateTime) # week of year
	temp_hind_dat$year <- year(temp_hind_dat$DateTime)
	
	

	write_csv(temp_hind_dat, file = here("./data/hindcast_temp_K20P19.csv"))
	
	