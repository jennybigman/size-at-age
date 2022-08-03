# ROMS temp 

	# download bottom temp data 

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)
  library(tidyverse)

	# set up download from server 
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap  <- "dodsC/B10K-K20_CORECFS/Level3/ACLIMsurveyrep_B10K-K20_CORECFS.nc"
	

	# use tidync package to download data and transform into tidy format
	file_path <- paste0(url_base, opendap)
	
	tidy_dat <- tidync(file_path) %>% hyper_tibble()
	
  ROMS_L3_temp_hinddat <- tidy_dat %>%
    select(year, station, temp_bottom5m, temp_integrated, temp_surface5m) %>%
  	filter
	
  # close the connection with the remote netCDF file
	nc_close(nc)
	
	unique(ROMS_L3_temp_hinddat$station)

	# save temp df
	fwrite(ROMS_L3_temp_hinddat, ".data//ROMS_L3_temp_hinddat.csv")

	##############
	
	ROMS_survey_key <- read.csv(file = here("./data/ROMS_rho_points_Intersect.csv"))
	
	ROMS_survey_key$station <- ROMS_survey_key$STATION_GR

	test_dat <- merge(ROMS_L3_temp_hinddat, ROMS_survey_key, by = "station")	
	