# function to download and concatenate Bering10k ROMS historical & projected time series

	library(ncdf4)
	library(lubridate)
	library(abind)
	library(tidyverse)
	
	# PMEL server 
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC/" # top-level folder, PMEL server
	
	
	#### historical ####
	
	B10K_hist_func <- function(sim, yr, var_long){
	
		
		fname <- paste0(
			tdsbase, 
		  sim, 
		  "/Level2/",
		  paste0(yr, "-", yr + 4, "/"), 
		  paste0(sim, "_", yr, "-", yr + 4, "_", 
		  "average_", var_long, ".nc"))
	
  	ncin <- nc_open(fname)
  	
		# get lats, longs, ocean_time
		lats <- ncvar_get(ncin,"lat_rho")
		lons <- ncvar_get(ncin,"lon_rho")
		ocean_time <- ncvar_get(ncin, "ocean_time")
  
  	# get temp
		var_short <- str_remove(var_long, "_bottom5m")
  	var_array <- ncvar_get(ncin, var_short)
  
  	# assign dim names and ocean_time
	 	xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  	eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  	dimnames(var_array) <- list("Xi" = xi_axis,
  															"Eta" = eta_axis,
															  "Time" = ocean_time)

		# turn array into dataframe 
		var_df <- reshape2::melt(var_array, 
														 varnames = c("Xi", "Eta", "Time"), 
														 value.name = var_short)

	# translate to lat/lon and time
	var_df$DateTime <- as.POSIXct(var_df$Time, origin = "1900-01-01", tz = "GMT")
	
	var_df$month <- month(var_df$DateTime) # month of year
	var_df$week <-   week(var_df$DateTime) # week of year
	var_df$year <-   year(var_df$DateTime)

	var_df$longitude <- lons[cbind(var_df$Xi, var_df$Eta)]
	var_df$latitude <-  lats[cbind(var_df$Xi, var_df$Eta)]
	
  # download domain array so can remove domain 0
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
	
	nc <- nc_open(paste(url_base, opendap_area, sep=""))

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
	var_df <- left_join(var_df, domain_df)
	
	var_df <- var_df %>% 
		filter(domain > 0) %>%
		mutate(var = var_short,
					 start_yr = yr)
	
	} 

  
	var_longs <- c('temp_bottom5m', "oxygen_bottom5m")
	
	yrs <- seq(1980, 2014, by = 5) 

	hist_sims <- c("B10K-K20P19_CMIP6_cesm_historical", 
							 "B10K-K20P19_CMIP6_gfdl_historical", 
							 "B10K-K20P19_CMIP6_miroc_historical")

	fdf <- crossing(
		hist_sims = hist_sims,
		yrs = yrs,
		var_longs = var_longs
	)
	
	args <- list(sim = fdf$hist_sims, 
							 yr = fdf$yrs, 
							 var_long = fdf$var_longs)

	var_ROMS_output_hist <- pmap(args, B10K_hist_func)
	
	ROMS_hist <- var_ROMS_output_hist %>% bind_rows()
	
	saveRDS(ROMS_hist, file = "./data/ROMS_hist_proj.rds")

	
	#### projections ####
	
	B10K_proj_func <- function(sim, yr, var_long){
	
		
		fname <- paste0(
			tdsbase, 
		  sim, 
		  "/Level2/",
		  paste0(yr, "-", yr + 4, "/"), 
		  paste0(sim, "_", yr, "-", yr + 4, "_", 
		  "average_", var_long, ".nc"))
	
  	ncin <- nc_open(fname)
  	
		# get lats, longs, ocean_time
		lats <- ncvar_get(ncin,"lat_rho")
		lons <- ncvar_get(ncin,"lon_rho")
		ocean_time <- ncvar_get(ncin, "ocean_time")
  
  	# get temp
		var_short <- str_remove(var_long, "_bottom5m")
  	var_array <- ncvar_get(ncin, var_short)
  
  	# assign dim names and ocean_time
	 	xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  	eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  	dimnames(var_array) <- list("Xi" = xi_axis,
  															"Eta" = eta_axis,
															  "Time" = ocean_time)

		# turn array into dataframe 
		var_df <- reshape2::melt(var_array, 
														 varnames = c("Xi", "Eta", "Time"), 
														 value.name = var_short)

	# translate to lat/lon and time
	var_df$DateTime <- as.POSIXct(var_df$Time, origin = "1900-01-01", tz = "GMT")
	
	var_df$month <- month(var_df$DateTime) # month of year
	var_df$week <-   week(var_df$DateTime) # week of year
	var_df$year <-   year(var_df$DateTime)

	var_df$longitude <- lons[cbind(var_df$Xi, var_df$Eta)]
	var_df$latitude <-  lats[cbind(var_df$Xi, var_df$Eta)]
	
  # download domain array so can remove domain 0
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
	
	nc <- nc_open(paste(url_base, opendap_area, sep=""))

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
	var_df <- left_join(var_df, domain_df)
	
	var_df <- var_df %>% 
		filter(domain > 0) %>%
		mutate(var = var_short,
					 start_yr = yr)
	
	} 

  
	var_longs <- c('temp_bottom5m', "oxygen_bottom5m")
	
	yrs <- seq(2015, 2095, by = 5) # Calibration block


	proj_sims <- c("B10K-K20P19_CMIP6_cesm_ssp126",
								 "B10K-K20P19_CMIP6_cesm_ssp585",
								 "B10K-K20P19_CMIP6_gfdl_ssp126",
								 "B10K-K20P19_CMIP6_gfdl_ssp585",
								 "B10K-K20P19_CMIP6_miroc_ssp126",
								 "B10K-K20P19_CMIP6_miroc_ssp585")

	fdf <- crossing(
		proj_sims = proj_sims,
		yrs = yrs, 
		var_longs = var_longs
	)
	
	
	args <- list(sim = fdf$proj_sims, 
							 yr = fdf$yrs, 
							 var_long = fdf$var_longs)

	var_ROMS_output_proj <- pmap(args, B10K_proj_func)
	
	ROMS_proj <- var_ROMS_output_proj %>% bind_rows()
	
	saveRDS(ROMS_proj, file = "./data/ROMS_proj.rds")

	
