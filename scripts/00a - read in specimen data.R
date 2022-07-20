# read in EBS bottom trawl survey data

	library(here)
	library(tidyverse)

	# create object names for file names
	dat_prestring <- paste0(here(), ("/data/"))

  file_list <- list.files(path = paste0(here(), ("/data/")))
  
  # pull out specimen data
  spec_dat_file_list <- file_list[str_detect(file_list, "raw_data")]

  dat_list <- list()
  
  for(i in spec_dat_file_list){
  	dat_list[[i]] <- paste0(dat_prestring, i)
  	dat_list
  }

  # read in all files in list
  df_list <- list()
  
  	for(i in dat_list){
  		df_list[[i]] <- readRDS(i)
  		df_list
  	}
  
  # change names of list
  list_names_fun <- function(x){
  	name <- paste0("df_", x)
  	name
  }
  
  list_names <- sapply(c("pcod", "pollock", "yfsole"), list_names_fun)
  
  names(df_list) <- list_names
  
  list2env(setNames(df_list, list_names), .GlobalEnv)
  
  # Pcod dat
  pcod_age_weight_dat <- df_pcod$specimen
  pcod_haul <- df_pcod$haul
  
	#names_haul <- names(haul)
	#names_pcod_dat <- names(pcod_age_weight_dat)
	#names_match <- intersect(names_haul, names_pcod_dat)
	
	pcod_specimen_dat <- full_join(pcod_age_weight_dat, pcod_haul, 
																 by = c("CRUISEJOIN",
																 			 "HAULJOIN"))
	
	names(pcod_specimen_dat) <- tolower(names(pcod_specimen_dat))
	
	# Pollock dat 
	pollock_age_weight_dat <- df_pollock$specimen
  pollock_haul <- df_pollock$haul
  
	pollock_specimen_dat <- full_join(pollock_age_weight_dat, pollock_haul, 
																 by = c("CRUISEJOIN",
																 			 "HAULJOIN")) 
	
	names(pollock_specimen_dat) <- tolower(names(pollock_specimen_dat))

	# Yellowfin sole dat 
	yfinsole_age_weight_dat <- df_yfsole$specimen
	yfinsole_haul <- df_yfsole$haul
  
	yfinsole_specimen_dat <- full_join(yfinsole_age_weight_dat, yfinsole_haul, 
																 by = c("CRUISEJOIN",
																 			 "HAULJOIN")) 
	
	names(yfinsole_specimen_dat) <- tolower(names(yfinsole_specimen_dat))

 unique(yfinsole_specimen_dat$stationid)
 