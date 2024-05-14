# 00 - read in specimen data 

	library(here)
	library(tidyverse)
	library(lubridate)
	library(data.table)
	library(conflicted)
	library(here)
	library(tidyverse)
	library(lubridate)
	library(mgcv)
	library(MuMIn)
	library(visreg)
	library(data.table)
	library(gamm4)
	library(brms)
	library(mgcViz)
	library(patchwork)
	library(grid)
	library(gratia)
	library(sf)
	library(AICcmodavg)
	library(sdmTMB)
	#library(sdmTMBextra)
	library(future)
	library(DHARMa)
	#library(tidymv)
	library(ggsidekick)
	library(see)
	library(rnaturalearth)
	library(ggh4x)

		
	conflicts_prefer(dplyr::filter)
  conflicts_prefer(dplyr::select)
  
	# read in data from GAP
	GAP_dat <- readRDS("~/Library/CloudStorage/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/data/X_index_data_goa_ebs_nbs/ak_bts_goa_ebs_nbs_all_levels.RDS")

	# grab the specimen data (age, weight, length)
	spec_dat <- GAP_dat$specimen
	
	# grab the cruise meta-data (lat/long, dates, etc)
  haul_dat <- GAP_dat$haul
  
  # combine data based on haul join
  specimen_dat <- left_join(spec_dat, haul_dat)
  
  # load in survey stratum
  EBS_survey_grid <- st_read("./data/Kelly shapefiles/EBS_NBS_2019.shp")	
  
  plot(EBS_survey_grid)

  survey_stratum <- unique(EBS_survey_grid$STRATUM)
 
  # filter out samples not in BS
  specimen_dat <- specimen_dat %>% filter(STRATUM %in% survey_stratum)
  
  specimen_dat <- specimen_dat %>% filter(REGION == "BS")
  
  unique_strata <- unique(specimen_dat$STRATUM)
 
	# change col names to lowercase
	names(specimen_dat) <- tolower(names(specimen_dat))
	
	#drop unneeded cols
	names_keep <- c("species_code", "length", "sex", "weight", 
									"age", "start_time", "start_latitude", 
									"start_longitude", "stationid", "stratum")
	
	specimen_dat <- specimen_dat %>%
		dplyr::select(all_of(names_keep))
		
	# combine species info with df
	species_dat <- GAP_dat$species %>%
  		dplyr::select(SPECIES_CODE, SPECIES_NAME, COMMON_NAME) %>%
			distinct_all()
		
	names(species_dat) <- tolower(names(species_dat))
		
	# filter out species to include
	sp <- c("Gadus chalcogrammus", "Gadus macrocephalus", "Atheresthes stomias", "Limanda aspera")
	
	species_dat_trim <- species_dat %>% dplyr::filter(species_name %in% sp) %>% distinct_all()
		
	codes <- unique(species_dat_trim$species_code)
		
	specimen_dat <- specimen_dat %>% dplyr::filter(species_code %in% codes)
		
	specimen_dat <- left_join(specimen_dat, species_dat_trim)
		
 	# convert dates and rename cols
	specimen_dat <- specimen_dat %>%
		mutate(date =  as_date(start_time),
					 month = lubridate::month(date),
					 year =  lubridate::year(date),
					 jday =  lubridate::yday(date),
					 cohort = year - age)
		
	specimen_dat <- specimen_dat %>%
			rename(latitude = start_latitude,
						 longitude = start_longitude)
	
	# drop any observations with missing age or survey data
 	specimen_dat <- specimen_dat %>%
 		drop_na(stationid, year, age, weight)

	write.csv(specimen_dat, file = here("./data/specimen_dat_all_GAP.csv"))
	