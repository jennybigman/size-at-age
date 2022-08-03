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
	opendap  <- "dodsC/B10K-K20_CORECFS/Level1/B10K-K20_CORECFS_2020-2024_station_constants.nc"
	
	# use tidync package to download data and transform into tidy format
	file_path <- paste0(url_base, opendap)
	
	tidy_dat_region <- tidync(file_path) %>% hyper_tibble()
	
  