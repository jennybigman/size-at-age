# read in EBS bottom trawl survey data

	library(here)
	library(tidyverse)

	# create object names for file names
	ebs_prestring <- paste0(here(), ("/data/Bottom Trawl Survey Data EBS/"))

  ebs_file_list <- list.files(path = paste0(here(), ("/data/Bottom Trawl Survey Data EBS/")))

  ebs_dat_list <- list()
  
  for(i in ebs_file_list){
  	ebs_dat_list[[i]] <- paste0(ebs_prestring, i)
  	ebs_dat_list
  }

  # read in all files in list
  ebs_df_list <- list()
  
  	for(i in ebs_dat_list){
  		ebs_df_list[[i]] <- read.csv(i)
  		ebs_df_list
  	}
  
  # change names of list
  list_names_fun <- function(x){
  	name <- paste0("df", x)
  	name
  }
  
  list_names <- sapply(1:9, list_names_fun)
  
  names(ebs_df_list) <- list_names
  
  # convert columns needed to numeric/integer to combine into one df
  
	ebs_df_list_fixed <- map(ebs_df_list, ~ .x %>% 
                  mutate(LATITUDE = as.numeric(LATITUDE),
                  			 LONGITUDE = as.numeric(LONGITUDE),
                  			 STRATUM = as.integer(STRATUM),
                  			 YEAR = as.integer(YEAR),
                  			 WTCPUE = as.numeric(WTCPUE),
                  			 NUMCPUE = as.numeric(NUMCPUE),
                  			 SID = as.integer(SID),
                  			 BOT_DEPTH = as.integer(BOT_DEPTH),
                  			 BOT_TEMP = as.numeric(BOT_TEMP),
                  			 SURF_TEMP =  as.numeric(SURF_TEMP),
                  			 VESSEL = as.integer(VESSEL),
                  			 CRUISE = as.integer(CRUISE),
                  			 HAUL = as.integer(HAUL)))
  
  ebs_dfs <- bind_rows(ebs_df_list_fixed)

	# filter out Pcod & Pollock and include data for 1990 onwards because of problems with scales
  years_keep <- 1990:2019
  
  ebs_dfs_trim <- ebs_dfs %>%
  	filter(SCIENTIFIC == "Gadus chalcogrammus" |
  				 SCIENTIFIC == "Gadus macrocephalus") %>%
  	filter(YEAR %in% years_keep)
	