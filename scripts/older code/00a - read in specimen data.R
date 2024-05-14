# 00 - read in specimen data 

	library(here)
	library(tidyverse)
	library(lubridate)
	library(data.table)
	library(conflicted)
		
	conflicts_prefer(dplyr::filter)
  conflicts_prefer(dplyr::select)
  
	# read in data from GAP
	GAP_dat <- readRDS("~/Library/CloudStorage/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/data/X_index_data_goa_ebs_nbs/ak_bts_goa_ebs_nbs_all_levels.RDS")

	# grab the specimen data (age, weight, length)
	spec_dat <- GAP_dat$specimen
	
	# grab the cruise meta-data (lat/long, dates, etc)
  haul_dat <- GAP_dat$haul
  
  # only EBS
  haul_dat_BS <- haul_dat %>% filter(REGION == "BS")
  
  # maps 
  #ggplot(haul_dat_EBS, aes(START_LONGITUDE, START_LATITUDE)) +
	#  geom_point() +
	#	geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  #	           fill = "darkgrey", color = NA) +
	#  #scale_colour_viridis_c(direction = -1) +
	#	coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
	#	theme_sleek()
  
  # join dfs
	specimen_dat <- left_join(spec_dat, haul_dat_BS, by = "HAULJOIN")
  
	# change col names to lowercase
	names(specimen_dat) <- tolower(names(specimen_dat))
	
	#drop unneeded cols
	names_keep <- c("species_code", "length", "sex", "weight", 
									"age", "start_time", "start_latitude", 
									"start_longitude", "stationid")
	
	specimen_dat <- specimen_dat %>%
		dplyr::select(all_of(names_keep))
		
	# combine species info with df
	species_dat <- GAP_dat$species %>%
  		dplyr::select(SPECIES_CODE, SPECIES_NAME, COMMON_NAME) %>%
			distinct_all()
		
	names(species_dat) <- tolower(names(species_dat))
		
	# filter out species to include
	sp <- c("Gadus chalcogrammus", "Gadus macrocephalus", "Atheresthes stomias", "Limanda aspera")
	
	species_dat_trim <- species_dat %>% filter(species_name %in% sp) %>% distinct_all()
		
	codes <- unique(species_dat_trim$species_code)
		
	specimen_dat <- specimen_dat %>% filter(species_code %in% codes)
		
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

#	write.csv(specimen_dat, file = here("./data/specimen_dat_Apr2024.csv"))
	