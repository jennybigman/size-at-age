# read in EBS bottom trawl survey data
	
	#library(erer)
	library(here)
	library(tidyverse)
	library(lubridate)
	library(data.table)
	

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
  
  haul_dat <- df_list[[1]]$haul %>%
  	dplyr::select(STRATUM, STATIONID) %>%
  	rename(stationid = STATIONID)
  
  # wrangle
	wrangling_func <- function(x){
 
		spec_dat <- x$specimen
		
  	haul_dat <- x$haul
  	
		specimen_dat <- full_join(spec_dat, haul_dat,
															by = "HAULJOIN")
  
		# change col names to lowercase
		names(specimen_dat) <- tolower(names(specimen_dat))
	
		#drop unneeded cols
		names_keep <- c("length", "sex", "weight", "age", "start_time", 
										"start_latitude", "start_longitude", "stationid",
										"bottom_depth", "surface_temperature", "haul.x")
	
		specimen_dat <- specimen_dat %>%
			select(all_of(names_keep))
		
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
	
		# combine species info with df
		species_dat <- x$species %>%
  			select(SPECIES_CODE, SPECIES_NAME, COMMON_NAME) 
		
		species_dat <- species_dat %>% slice(rep(row_number(), nrow(specimen_dat)))
	
		names(species_dat) <- tolower(names(species_dat))
	
		specimen_dat <- bind_cols(specimen_dat, species_dat)
	
 }
 
 df_list_wrangled <- lapply(df_list, wrangling_func) 
 
 	
 # add species as column, bind rows, and save file
 	df_list_wrangled_names <- dplyr::bind_rows(df_list_wrangled) %>%
 		drop_na(weight, age, stationid, year)

	write.csv(df_list_wrangled_names, file = here("./data/df_list_wrangled_names.csv"))
	
	# histograms of weight by age
	pollock_dat <- df_list_wrangled_names %>%
		filter(common_name == "walleye pollock") %>%
		group_by(age) %>%
		filter(n() > 100) %>%
		drop_na(age)
	
	pcod_dat <- df_list_wrangled_names %>%
		filter(common_name == "Pacific cod") %>%
		group_by(age) %>%
		filter(n() > 100) %>%
		drop_na(age)
	
	yfinsole_dat <- df_list_wrangled_names %>%
		filter(common_name == "yellowfin sole") %>%
		group_by(age) %>%
		filter(n() > 100) %>%
		drop_na(age)
	
	# put into list for plotting
	dat_all_pre_list <- list(pollock_dat, pcod_dat, yfinsole_dat)
	
	# file_path
	file_path <- "/plots/"
	
	hist_func <- function(df){
	
		plot <-
			ggplot(data = df, aes(log10(weight))) +
			geom_histogram() +
			facet_wrap(~ age, scales = "free") +
			ggtitle(unique(df$common_name))
		
		ggsave(paste0(here(), file_path, unique(df$common_name), "hist_pre_out_rm.png"),
					 plot, height = 5, width = 10, units = "in")
		
	}
	
	lapply(dat_all_pre_list, hist_func)
	
	# need to remove outliers
	# bind rows to see sum by species
	dat_all_pre <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat)

	write.csv(dat_all_pre, file = here("./data/dat_all_pre.csv"))
