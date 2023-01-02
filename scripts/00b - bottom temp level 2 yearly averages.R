	
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

	# future temps
	
	# read in bias corrected temps
	cesm_dfs_trim_domain <- fread("../../Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/data/cesm_dfs_trim_domain.csv")
  gfdl_dfs_trim_domain <- fread("../../Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/data/gfdl_dfs_trim_domain.csv")
  miroc_dfs_trim_domain <- fread("../../Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/data/miroc_dfs_trim_domain.csv")

  proj_list <- list(cesm_dfs_trim_domain, gfdl_dfs_trim_domain, miroc_dfs_trim_domain)
  
  # summarize by yr mean temp
  
  yr_mean_func <- function(df){
  	
  	df_yr <- df %>%
  		group_by(scenario, simulation, year) %>%
  		summarise(mean_temp = mean(bc_temp))
 
  	df_yr
  }
  
  yr_mean_temp_list <- lapply(proj_list, yr_mean_func)
  
  yr_mean_proj_temps <- bind_rows(yr_mean_temp_list)
  
  # summer mean temps
  
  mo_keep <- 4:6 
  
	sum_trim_function <- function(df){
		
		df_yr <- df %>%
			filter(month %in% mo_keep) %>%
  		group_by(scenario, simulation, year) %>%
  		summarise(sum_mean_temp = mean(bc_temp))
 
  	df_yr
	}

  sum_mean_temp_list <- lapply(proj_list, sum_trim_function)

  sum_mean_proj_temps <- bind_rows(sum_mean_temp_list)
  
  # combine together and save
  ROMS_proj_temps_saa <- left_join(yr_mean_proj_temps, sum_mean_proj_temps)
  
  ROMS_proj_temps_saa <- ROMS_proj_temps_saa %>%
  	filter(year > 2019)
  
  fwrite(ROMS_proj_temps_saa, here("./data/ROMS_proj_temps_saa.csv"))
  
	#### temps from ACLIM ####
	
	load("../../ACLIM2//Data/out/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hist_mn.Rdata")
	load("../../ACLIM2/Data/out/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hind_mn.Rdata")
  load("../../ACLIM2//Data/out/K20P19_CMIP6/allEBS_means/ACLIM_weekly_fut_mn.Rdata")
  
  temp_hist <- ACLIM_weekly_hist %>%
    filter(var == "temp_bottom5m")
  	
  temp_hind <- ACLIM_weekly_hind %>%
    filter(var == "temp_bottom5m")
  
  temp_proj <- ACLIM_weekly_fut %>%
    filter(var == "temp_bottom5m")
  
  sum_mo <- 4:6
  
  ACLIM_temp_hind_sum_mean <- temp_hind %>%
  	filter(mo %in% sum_mo) %>%
  	group_by(year) %>%
  	summarise(mean_temp_sum_ACLIM = mean(mn_val))

  ACLIM_temp_hind_yr_mean <- temp_hind %>%
  	group_by(year) %>%
  	summarise(mean_temp_yr_ACLIM = mean(mn_val))
  
  ACLIM_hind_temps <- left_join(ACLIM_temp_hind_yr_mean, ACLIM_temp_hind_sum_mean)
  
  ACLIM_temp_proj_sum_mean <- temp_proj %>%
  	filter(mo %in% sum_mo) %>%
  	group_by(year, GCM, scen) %>%
  	summarise(mean_temp = mean(val_biascorrected)) %>%
  		rename(scenario = scen,
  				 simulation = GCM)

  ACLIM_temp_proj_yr_mean <- temp_proj %>%
  	group_by(year, GCM, scen) %>%
  	summarise(mean_temp = mean(val_biascorrected)) %>%
  	rename(scenario = scen,
  				 simulation = GCM)

  # compare level 2 hindcast temps to level 3
  
  # hindcast temp
  
  ggplot() +
  	geom_line(data = ROMS_hind_temps,
  						aes(x = year, y = mean_sum_temp),
  						color = "red") +
  	geom_line(data = ACLIM_temp_hind_sum_mean,
  						aes(x = year, y = mean_temp),
  						color = "blue")
  
   ggplot() +
  	geom_line(data = ROMS_hind_temps,
  						aes(x = year, y = yr_mean_temp),
  						color = "red") +
  	geom_line(data = ACLIM_temp_hind_yr_mean,
  						aes(x = year, y = mean_temp),
  						color = "blue")

   # future temp
   
   # yearly mean
   ggplot() +
  	geom_line(data = ROMS_proj_temps_saa,
  						aes(x = year, y = mean_temp),
  						color = "red") +
  	geom_line(data = ACLIM_temp_proj_yr_mean,
  						aes(x = year, y = mean_temp),
  						color = "blue") +
    facet_grid(simulation ~ scenario)
 
   # summer mean
   ggplot() +
  	geom_line(data = ROMS_proj_temps_saa,
  						aes(x = year, y = sum_mean_temp),
  						color = "red") +
  	geom_line(data = ACLIM_temp_proj_sum_mean,
  						aes(x = year, y = mean_temp),
  						color = "blue") +
    facet_grid(simulation ~ scenario)