	
	# download bottom temp data from Bering 10k ROMS, trim to space of bottom trawl survey, and take avg

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)
	library(tidyverse)


	# set up download from server 
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap  <- "dodsC/Level2/B10K-K20_CORECFS_bottom5m.nc"
	
	# test_path <- paste0(url_base, opendap)
	
	# tidy_temps <- tidync(test_path) %>% hyper_tibble()
	
	nc <- nc_open(paste(url_base, opendap, sep = ""))

	# create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

	# create object for time axis
	t_axis   <- ncvar_get(nc,"ocean_time")
	time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 

	# download temp array -- for some reason, only works without last two time steps
	temp_array <- ncvar_get(nc, "temp", start = c(1,1,1), count = c(182,258,2662)) 

  # name the dimensions
	dim(temp_array)

	dimnames(temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = t_axis[1:2662])

	# turn array into dataframe 
	temp_df <- reshape2::melt(temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")

	# translate to lat/lon and time
	temp_df$DateTime <- as.POSIXct(temp_df$Time, origin = "1900-01-01", tz = "GMT")
	temp_df$longitude <- lons[cbind(temp_df$Xi, temp_df$Eta)]
	temp_df$latitude <- lats[cbind(temp_df$Xi, temp_df$Eta)]
	
	#### area, depth, domain ####
	
	# set up download from server for meta data file
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap_area,sep=""))

	#### area ####
  
  # download area array
  area_array<- ncvar_get(nc, "area_feast")
  
  # name the dimensions
  dim(area_array)
  
  dimnames(area_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

  # turn into dataframe from array
  area_df <- reshape2::melt(area_array, 
                  varnames=c("Xi", "Eta"), 
                  value.name="area_km2")

  # add lat/long cols
  area_df$longitude <- lons[cbind(area_df$Xi,area_df$Eta)]
  area_df$latitude <- lats[cbind(area_df$Xi,area_df$Eta)]

 #### depth ####
  
  # download array
  final_bathy_array <- ncvar_get(nc, "h") # long name is 'final bathymetry at RHO-points'
  
  # turn into dataframe from array
  
  depth_df <- reshape2::melt(final_bathy_array, 
                  	varnames=c("Xi", "Eta"), 
                  	value.name="depth")

  # add lat/long cols
  depth_df$longitude <- lons[cbind(depth_df$Xi,depth_df$Eta)]
  depth_df$latitude <- lats[cbind(depth_df$Xi,depth_df$Eta)]
	
  #### domain ####
  
  # download area array
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

  # close the connection
  nc_close(nc)

  # merge
	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta"))

	area_depth_domain_df <- merge(area_depth_df, domain_df,
													by = c("latitude", "longitude", "Xi", "Eta"))

	ROMS_dat_hind <- merge(temp_df, area_depth_domain_df, 
												 by = c("latitude", "longitude", "Xi", "Eta"),
												 all = TRUE)
	
	ROMS_dat_hind <- na.omit(ROMS_dat_hind)
	
	# trim grid to only survey replicated strata/domains
	
	ROMS_dat_hind_trim <- ROMS_dat_hind %>%
		filter(domain > 0) %>%
		filter(., between(depth, 0, 250))
	
	ROMS_dat_hind_trim$date <- as.Date(ROMS_dat_hind_trim$DateTime) # date in Date format
	ROMS_dat_hind_trim$month <- month(ROMS_dat_hind_trim$date) # month of year
	ROMS_dat_hind_trim$week <- week(ROMS_dat_hind_trim$date) # week of year
	ROMS_dat_hind_trim$year <- year(ROMS_dat_hind_trim$date)

	# yearly avg
	ROMS_bot_temp_yr <- ROMS_dat_hind_trim %>%
		group_by(year) %>%
		summarise(mean_temp = mean(temp))
	
	fwrite(ROMS_bot_temp_yr, "./data/ROMS_bot_temp_yr.csv")
	
	# avg for summer
	months_keep <- c("4", "5", "6")
	
	ROMS_sum_temp_avg <- ROMS_dat_hind_trim %>%
		filter(month %in% months_keep) %>%
		group_by(year) %>%
		summarize(mean_sum_temp = mean(temp))

	fwrite(ROMS_sum_temp_avg, "./data/ROMS_sum_temp_avg.csv")
