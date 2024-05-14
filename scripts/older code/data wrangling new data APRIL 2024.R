# read in AFSC EBS bottom trawl survey data 

	library(here)
	library(tidyverse)
	library(lubridate)
	library(data.table)
	
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
		select(all_of(names_keep))
		
	# combine species info with df
	species_dat <- GAP_dat$species %>%
  		select(SPECIES_CODE, SPECIES_NAME, COMMON_NAME) %>%
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
		mutate(date = as_date(start_time),
					 month = month(date),
					 year = year(date),
					 jday = yday(date),
					 cohort = year - age)
		
	specimen_dat <- specimen_dat %>%
			rename(latitude = start_latitude,
						 longitude = start_longitude)
	
	# drop any observations with missing age or survey data
 	specimen_dat <- specimen_dat %>%
 		drop_na(stationid, year, age, weight)

	write.csv(specimen_dat, file = here("./data/specimen_dat_Apr2024.csv"))
	
	# histograms of weight by age
	
	wrangle_fun <- function(sp){
		
		sp_dat <- specimen_dat %>%
			filter(common_name == sp) %>%
			filter(weight > 0) %>%
			filter(length > 0) %>%
			mutate(log_wt = log10(weight),
						 age_f = as.factor(age))

		sp_dat <- sp_dat %>%
			group_by(age) %>%
			filter(n() > 100) 
		
		sp_dat
			
	}
	
	sp <- unique(specimen_dat$common_name)
	
	specimen_dat <- purrr::map(sp, wrangle_fun) %>% bind_rows()
	
#	specimen_dat_NA <- drop_na(specimen_dat)
	

	# put into list for plotting
	dat_all_pre_list <- specimen_dat %>%
		group_by(common_name) %>%
		group_split()
	
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
	
	#write.csv(dat_all_pre_Apr2024, file = here("./data/dat_all_pre_Apr2024.csv"))


	# remove outliers using a Bonferroni correction ####
	library(car)

	#dat_all_pre_Apr2024 <- read.csv(file = here("./data/dat_all_pre_Apr2024.csv"))
	
	# df combined, only including ages > 100 samples, remove observations with no age

	# how many samples by species
	sample_sizes_pre <- specimen_dat %>%
		group_by(common_name) %>%
		summarise(count = n())
	
	# bonferroni correction
	bonf_func <- function(x){	

		df <- specimen_dat %>% filter(common_name == x)
		
		df <- df %>%
			mutate(age_f = as.factor(age))
		
		mod <- lm(log10(weight) ~ log10(length)*age_f, data = df)
	
		test_results <- outlierTest(mod, cutoff = 0.7, n.max = 50000) # keep number bigger than n obs
		
		row_rm <- as.numeric(names(test_results$rstudent))
		
		df_trim <- df[-c(row_rm), ]


	}
	
	species <- unique(specimen_dat$common_name)

	specimen_dat <- purrr::map(species, bonf_func) %>% bind_rows()
	
	# how many samples by species
	sample_sizes_post <- specimen_dat %>%
		group_by(common_name) %>%
		summarise(count = n())
	
	# sample size comparison
	sample_sizes_pre <- sample_sizes_pre %>%
		rename(count_before = count)
	
	sample_sizes_post <- sample_sizes_post %>%
		rename(count_after = count)
	
	sample_size_comp <- full_join(sample_sizes_pre, sample_sizes_post) %>%
		rowwise() %>%
		mutate(diff = count_before - count_after)
	
	# split into list for plotting
	dat_split <- specimen_dat %>% 
		group_by(common_name) %>%
		group_split()
	
	# histograms of weight by age
	file_path <- "/plots/"
	
	hist_func <- function(df){
	
		plot <-
			ggplot(data = df, aes(log10(weight))) +
			geom_histogram() +
			facet_wrap(~ age, scales = "free") +
			ggtitle(unique(df$common_name))
		
			
		ggsave(paste0(here(), file_path, unique(df$common_name), "hist_post_out_rm.png"),
					 plot, height = 5, width = 10, units = "in")

	}
	
	lapply(dat_split, hist_func)
	
	
	
	write.csv(specimen_dat, file = here("./data/dat_all_Apr2024.csv"))

	#####################################################################
	

	# data wrangling species dataframes

	#dat_all_Apr2024 <- read_csv(file = here("./data/dat_all_Apr2024.csv")) %>%
	#	dplyr::select(-...1, -sex, -start_time, -bottom_depth, -surface_temperature, -species_code) 
	
	# cohort as factors and log weight
	specimen_dat <- specimen_dat %>%
		mutate(cohort_f = as.factor(cohort)) 
	
	# how many NAs per species do I have
	spec_dat_0 <- specimen_dat %>%
		dplyr::select(species_name, age) %>%
		group_by(species_name) %>%
		summarise(NAs = sum(is.na(age))) 
	
	# add short name
	specimen_dat <- specimen_dat %>%
		mutate(
			short_name = case_when(
				common_name == "walleye pollock" ~ "pollock",
				common_name == "Pacific cod" ~ "pcod",
				common_name == "yellowfin sole" ~ "yfin",
				common_name == "arrowtooth flounder" ~ "atooth")
			)
	
	# put each species df into a list
	specimen_dat_list <- specimen_dat %>%
		group_by(species_name) %>%
  	group_split() 

	# add columns
	
	weight_metrics_func <- function(x){ 

		# calculate mean and sd weight by age
		dat <- x %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt)) %>%
		rowwise() %>%
		mutate(log_wt_std = (log_wt - mean_wt_age)/sd_wt_age)
	
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, weight_metrics_func)
	
	# prep df for sdmTMB()
	
	# add XY cols for making mesh
	xy_func <- function(df){
	
		df <- df %>% 
			drop_na(longitude, latitude)
	
		df <- df %>% sdmTMB::add_utm_columns(., c("longitude", "latitude"))

	}
	
	specimen_dat_list <- lapply(specimen_dat_list, xy_func)
	
	#### ROMS output ####

	# read in level 2 (by grid cell) ROMS hindcast bottom temps
	temp <- fread(file = here("./data/ROMS_hind_bottom_temp.csv"))
	
	# check 
	#temp_sum <- temp %>%
	#	distinct_at(vars(longitude, latitude), .keep_all = TRUE) %>%
	#	mutate(longitude = case_when(
	#		 longitude >= 180 ~ longitude - 360,
	#	   longitude < 180 ~ longitude * -1))
	#
	#ggplot(AK_coast_proj) +
	#	geom_sf() +
	#	geom_point(data = temp_sum,
	#						 aes(x = longitude, y = latitude)) 
	#
	
	
	oxygen <- fread(file = here("./data/ROMS_hind_bottom_oxygen.csv"))
	# check
	#oxy_sum <- oxygen %>%
	#	distinct_at(vars(longitude, latitude), .keep_all = TRUE) %>%
	#	mutate(longitude = case_when(
	#		 longitude >= 180 ~ longitude - 360,
	#	   longitude < 180 ~ longitude * -1))
	#
	#ggplot(AK_coast_proj) +
	#	geom_sf() +
	#	geom_point(data = oxy_sum,
	#						 aes(x = longitude, y = latitude)) 
	#
	
	
	# need to get all of the grid stations from all 4 species
	
	station_func <- function(df){
		
		df <- df %>%
		dplyr::select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	}
	
	station_list <- purrr::map(specimen_dat_list, station_func) %>% 
		bind_rows() %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))

	# find temp value closest to survey haul locations
	roms_grid <- temp %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	# plot
	ggplot(AK_coast_proj) +
		geom_sf() +
		geom_point(data = roms_grid,
							 aes(x = longitude, y = latitude)) 

	
	roms_grid$roms_ID <- 1:nrow(roms_grid)
	
	# match lat/longs of survey grid to nearest neighbor from ROMS grid	
	
	# use nn2() to calculate min distance to nearest ROMS lat/long
	station_list[, c(4, 5)] <- as.data.frame(RANN::nn2(roms_grid[, c('latitude', 'longitude')],
                                                  station_list[, c('latitude', 'longitude')],
                                                  k = 1))
	
	# Match nearest lat/long from ROMS
	station_list$roms_ID <- pull(roms_grid[c(station_list$nn.idx), 'roms_ID'])
	
	# any NAs in matching?
	which(is.na(station_list$roms_ID), )
	
	#roms_grid2 <- roms_grid %>%
	#	rename(roms_lat = latitude,
	#				 roms_long = longitude)
	#
	#test <- left_join(survey_grid, roms_grid2)
	
	# drop cols from nn2
	station_list <- station_list %>%
		dplyr::select(-nn.idx, -nn.dists)
	
	# create a df for each month and year to match to ROMS temps
	survey_df_func <- function(x, y){
		
		df <- station_list %>%
			mutate(year = x,
						 month = y)
	}
	
	df_func <- crossing(
		x = 1970:2021, # survey data goes to 2021 for some species
		y = 1:12
	)
	
	survey_grid_full <- purrr::map2(df_func$x, df_func$y, survey_df_func) %>%
		bind_rows

	# summarise roms temps by month and match to survey grid based on lat long
	
	# remove unecessary cols and convert lat/long
	temp <- temp %>%
			dplyr::select(-Xi, -Eta, -DateTime, -Time) %>%
			mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	oxygen <- oxygen %>%
		dplyr::select(-Xi, -Eta, -DateTime, -Time) %>%
		mutate(longitude = case_when(
				 longitude >= 180 ~ longitude - 360,
			   longitude < 180 ~ longitude * -1))
	
	# add in col denoting grid ID based on lat/long
	temp_grid <- left_join(temp, roms_grid)
	
	envr_grid <- left_join(temp_grid, oxygen, by = c("month", "week", "year", "longitude", "latitude", "domain"))
	
	\NA_check <- na.omit(envr_grid)
	
	# filter out grid cells/points that aren't in survey data
	ID_keep <- sort(unique(station_list$roms_ID))

	envr_grid <- envr_grid %>%
		rename(roms_lat = latitude,
					 roms_long = longitude) %>%
		dplyr::select(-week, - domain) %>%
		filter(roms_ID %in% ID_keep)
	
	# summarise temps by month and year for each grid cell/point
	envr_grid_sum <- envr_grid %>%
		group_by(roms_ID, month, year, roms_lat, roms_long) %>%
		summarise(bot_temp = mean(temp),
							bot_oxy = mean(oxygen)) 
	
	missing_roms <- envr_grid_sum[!complete.cases(envr_grid_sum), ]
	unique(missing_roms$roms_ID)
	
	# where is this?
	miss_roms <- missing_roms %>% distinct_at(vars(roms_long, roms_lat), .keep_all = TRUE)
	
	ggplot(AK_coast_proj) +
		geom_sf() +
		geom_point(data = miss_roms,
							 aes(x = roms_long, y = roms_lat)) 

	# monthly roms temps that match survey stations
	survey_roms_grid <- left_join(survey_grid_full, envr_grid_sum, 
																by = c('roms_ID', "year", "month"))

	# any NAs?
	NAs <- survey_roms_grid[!complete.cases(survey_roms_grid), ]
	sort(unique(NAs$roms_ID))
	
	# remove rows of that grid cell
	survey_roms_grid <- survey_roms_grid %>% drop_na(bot_temp)

	# add roms IDs to species data
	survey_grid_trim <- survey_roms_grid %>% 
		dplyr::select(stationid, roms_ID) %>%
		distinct_all()
	

	sp_roms <- function(df){
		
		df <- left_join(df, survey_grid_trim, by = "stationid") 
		
	
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, sp_roms)
	
	##### calculate temperature 3 months prior to survey ####
	
  presurvey_mo <- 4:6 # (April to June)
  
  nn_presurvey_vars <- survey_roms_grid %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(roms_ID, year) %>%
  	summarise(presurvey_btemp = mean(bot_temp),
  						presurvey_boxy = mean(bot_oxy)) 
  
  # any NAs?
  nn_presurvey_vars[!complete.cases(nn_presurvey_vars), ]
  
  # add to dataframe list
	dat_join_func <- function(df){
		
		#df <- df %>% 
		#	dplyr::select(-month)
 #
 		df <- left_join(df, nn_presurvey_vars, by = c("year", "roms_ID"))
	}
 
	specimen_dat_list2 <- lapply(specimen_dat_list, dat_join_func)
	
	test <- specimen_dat_list %>% bind_rows()
	
	missing <- test[!complete.cases(test), ]

	#### calculate yearly temp -- avg of temp July - Dec of year before survey & Jan - June year of survey) ####
  
  yr_prior_func <- function(x){
   	
   	# temp same year as collected, Jan - June
   	current <- survey_roms_grid %>%
   		filter(year == x & month <= 6)
 
   	# temp previous year July - Dec
		previous <- survey_roms_grid %>%
  		filter(year == x - 1 & month > 6)
 
		# combine and take a mean temp July - June
  	var_yr <- bind_rows(current, previous) %>%
  		group_by(roms_ID) %>%
  		summarise(yrprior_btemp = mean(bot_temp),
  							yrprior_boxy = mean(bot_oxy)) %>%
  		mutate(year = x)
   
   }
   
  nn_yr_prior_vars <- lapply(1970:2021, yr_prior_func) %>% bind_rows()
  
  # any NAs?
  nn_yr_prior_vars[!complete.cases(nn_yr_prior_vars), ]

  # add to dataframe list
	dat_join_func <- function(df){
 
 			df <- left_join(df, nn_yr_prior_vars, by = c("year", "roms_ID"))
	}
 
	specimen_dat_list <- lapply(specimen_dat_list, dat_join_func)

		
	test2 <- specimen_dat_list %>% bind_rows() %>%
		filter(roms_ID != 2536)
	
	missing <- test2[!complete.cases(test2), ]

	# any NAs?
	nn_specimen_dat <- specimen_dat_list %>% 
		bind_rows() 
	
	# any NAs?
	NAs <- nn_specimen_dat[!complete.cases(nn_specimen_dat), ]

	## scale cols
	
	std_func <- function(df){
		
 		df <- df %>%
			mutate_at(vars(contains(c("temp", "oxy"))), ~ scale(.) %>% as.vector) %>%
 			mutate(weight_std = as.vector(scale(weight)),
 						 log_wt_std = as.vector(scale(log_wt)),
 						 log_wt_c = as.vector(scale(log_wt)), scale = FALSE)
		
		df <- df %>% 
			mutate(jday_std = as.vector(scale(jday)))

	}

	specimen_dat_list <- lapply(specimen_dat_list, std_func)
	
	# add a col for year as a factor
  
  col_wrangle_func <- function(df){
  	
  	df <- df %>% mutate(year_f = as.factor(year))
  	df$year_f <- droplevels(df$year_f)
  	df
  	
  }
 
	specimen_dat_list <- lapply(specimen_dat_list, col_wrangle_func)
  

	# separate for species-specific wrangling tasks

	#pollock_dat <- specimen_dat_list[[1]]
	## any NAs
	#pol_NAs <- pollock_dat[!complete.cases(pollock_dat), ] 
#
	#pcod_dat <- specimen_dat_list[[2]] 
	## any NAs
	#pcod_NAs <- pcod_dat[!complete.cases(pcod_dat), ] 
#
	#yfinsole_dat <- specimen_dat_list[[3]]
	## any NAs
	#yfs_NAs <- yfinsole_dat[!complete.cases(yfinsole_dat), ] 


	## drop levels
	#pcod_dat$age_f <- droplevels(pcod_dat$age_f)
	#pollock_dat$age_f <- droplevels(pollock_dat$age_f)
	#yfinsole_dat$age_f <- droplevels(yfinsole_dat$age_f)

	# combine species dfs for running models
	#dat_all <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat)
	
	dat_all <- bind_rows(specimen_dat_list) 
	
	fwrite(dat_all, file = here("./data/sp_dat_all.csv"))
	
	# any NAs
	NA_dat_all <- dat_all[!complete.cases(dat_all), ] 

##############
	# where are these 6516 NAs coming from
	
	length(unique(nn_presurvey_vars$roms_ID))
	
	
	specimen_dat <- read_csv(file = here("./data/dat_all_Apr2024.csv"))

	glimpse(specimen_dat)
	
	stations <- unique(specimen_dat$stationid)

	glimpse(station_list)	

	overlap <- setdiff(station_list$roms_ID, survey_grid_trim$roms_ID)	
	