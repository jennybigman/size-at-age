# read in EBS bottom trawl survey data
	
	library(erer)
	library(here)
	library(tidyverse)
	library(lubridate)
	

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
 
 # wrangle
 wrangling_func <- function(x){
		
 		#pull out relevant dfs from list a d join
		spec_dat <- x$specimen
  	haul_dat <- x$haul
  
		specimen_dat <- full_join(spec_dat, haul_dat, 
																 by = "HAULJOIN")
		# change col names to lowercase
		names(specimen_dat) <- tolower(names(specimen_dat))
	
		# drop unneeded cols
		names_keep <- c("length", "sex", "weight", "age", "start_time", 
										"start_latitude", "start_longitude", "stationid",
										"bottom_depth", "surface_temperature", "haul.x")
	
		specimen_dat <- specimen_dat %>%
			select(all_of(names_keep))
		
		specimen_dat$species <- 
									
		# convert dates
		specimen_dat <- specimen_dat %>%
			mutate(date = as_date(start_time),
						 month = month(date),
						 year = year(date),
						 jday = yday(date),
						 cohort = year - age)
		
	# turn cohort and age into factors for the model and log variables
	specimen_dat <- specimen_dat %>%
		filter(weight > 0) %>%
		rename(latitude = start_latitude,
					 longitude = start_longitude)
	
 }
 
 df_list_wrangled <- lapply(df_list, wrangling_func) 
 
 # add species as column, bind rows, and save file
 	df_list_wrangled_names <- dplyr::bind_rows(df_list_wrangled, .id = "species")
 	
	write.csv(df_list_wrangled_names, file = here("./data/df_list_wrangled_names.csv"))
	
	
	#### does pcod dat have NAs
	
