	# data wrangling species dataframes

	specimen_dat <- read.csv(file = here("./data/df_list_wrangled_names.csv")) %>%
		select(-X, -sex, -start_time, -bottom_depth, -surface_temperature, -species_code) %>%
		drop_na(weight, age, stationid, year)
	
	
	# age and cohort as factors and log weight
	specimen_dat <- specimen_dat %>%
		mutate(age_f = as.factor(age),
					 cohort_f = as.factor(cohort),
					 log_wt = log10(weight)) 
		
	# how many NAs per species do I have
	spec_dat_0 <- specimen_dat %>%
		dplyr::select(species_name, age) %>%
		group_by(species_name) %>%
		summarise(NAs = sum(is.na(age))) 
	
	# change haul col name
	specimen_dat$haul <- specimen_dat$haul.x

	# add short name
	specimen_dat <- specimen_dat %>%
		mutate(
			short_name = case_when(
				common_name == "walleye pollock" ~ "pollock",
				common_name == "Pacific cod" ~ "pcod",
				common_name == "yellowfin sole" ~ "yfin")
			)
	
	# put each species df into a list
	specimen_dat_list <- list()
 
	specimen_dat_list <- specimen_dat %>%
  	group_split(species_name) 

	# add columns
	
	#weight_metrics_func <- function(x){ 
#
	#	# calculate mean and sd weight by age
	#	dat <- x %>%
	#	group_by(age_f) %>%
	#	mutate(mean_wt_age = mean(log_wt),
	#				 sd_wt_age = sd(log_wt)) %>%
	#	rowwise() %>%
	#	mutate(log_wt_std = (log_wt - mean_wt_age)/sd_wt_age)
	#
	#}
	#
	#specimen_dat_list <- lapply(specimen_dat_list, weight_metrics_func)
	
	# remove 2021 because no output in hindcast and will throw error in sdmTMB()
	rem_2021 <- function(x){
		x <- x %>% filter(year != 2021)
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, rem_2021)
	
	# prep df for sdmTMB()
	
	# add XY cols for making mesh
	xy_func <- function(df){
	
		df <- df %>% 
			drop_na(longitude, latitude)
	
		df <- df %>% sdmTMB::add_utm_columns(., c("longitude", "latitude"))

	}
	
	specimen_dat_list <- lapply(specimen_dat_list, xy_func)
	
	#### ROMS output ####

	# read in level 2 (by grid cell) ROMS hindcast temps
	temp <- fread(file = here("./data/ROMS_hind_bottom_temp.csv"))
	
	# need to get all of the grid stations from all 3 species
	station_func <- function(df){
		
		df <- df %>%
		select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	}
	
	station_list <- map(specimen_dat_list, station_func) %>% 
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
	#ggplot(AK_coast_proj) +
	#	geom_sf() +
	#	geom_point(data = roms_grid,
	#						 aes(x = longitude, y = latitude)) 

	
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
		select(-nn.idx, -nn.dists)
	
	# create a df for each month and year to match to ROMS temps
	survey_df_func <- function(x, y){
		
		df <- station_list %>%
			mutate(year = x,
						 month = y)
	}
	
	df_func <- crossing(
		x = 1970:2023,
		y = 1:12
	)
	
	survey_grid_full <- map2(df_func$x, df_func$y, survey_df_func) %>%
		bind_rows

	
	# summarise roms temps by month and match to survey grid based on lat long
	
	# remove unecessary cols and convert lat/long
	temp <- temp %>%
			select(-Xi, -Eta, -DateTime, -Time) %>%
			mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	# add in col denoting grid ID based on lat/long
	temp_grid <- left_join(temp, roms_grid)
	
	# filter out grid cells/points that aren't in survey data
	ID_keep <- sort(unique(station_list$roms_ID))

	temp_grid <- temp_grid %>%
		rename(roms_lat = latitude,
					 roms_long = longitude) %>%
		select(-week, - domain) %>%
		filter(roms_ID %in% ID_keep)
	
	# summarise temps by month and year for each grid cell/point
	temp_grid_sum <- temp_grid %>%
		group_by(roms_ID, month, year, roms_lat, roms_long) %>%
		summarise(bot_temp = mean(temp)) # change this to both integrated and bottom
	
	# monthly roms temps that match survey stations
	survey_roms_grid <- left_join(survey_grid_full, temp_grid_sum, 
																by = c('roms_ID', "year", "month"))

	# any NAs?
	NAs <- survey_roms_grid[!complete.cases(survey_roms_grid), ]
	
	# where and when are the NAs
	NAs_time <- NAs %>%
		group_by(month, year) %>%
		summarise(count = n())
	
	# these NAs are all in the last 4 months of 2023 so don't worry about them\
	
	# add roms IDs to species data
	survey_grid_trim <- station_list %>% select(stationid, roms_ID)
	
	sp_roms <- function(df){
		
		df <- left_join(df, survey_grid_trim, by = "stationid") 
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, sp_roms)
	
	##### calculate temperature 3 months prior to survey ####
	
  presurvey_mo <- 4:6 # (April to June)
  
  nn_presurvey_temps <- survey_roms_grid %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(roms_ID, year) %>%
  	summarise(presurvey_btemp = mean(bot_temp)) # change to both bot and integrated temp
  
  # any NAs?
  nn_presurvey_temps[!complete.cases(nn_presurvey_temps), ]
  
  # add to dataframe list
	dat_join_func <- function(df){
 
 			df <- left_join(df, nn_presurvey_temps, by = c("year", "roms_ID"))
	}
 
	specimen_dat_list <- lapply(specimen_dat_list, dat_join_func)

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
  		summarise(yrprior_btemp = mean(bot_temp)) %>%
  		mutate(year = x)
   
   }
   
  nn_yr_prior_temp <- lapply(1971:2020, yr_prior_func) %>% bind_rows()
  
  # any NAs?
  nn_yr_prior_temp[!complete.cases(nn_yr_prior_temp), ]

  # add to dataframe list
	dat_join_func <- function(df){
 
 			df <- left_join(df, nn_yr_prior_temp, by = c("year", "roms_ID"))
	}
 
	specimen_dat_list <- lapply(specimen_dat_list, dat_join_func)

	
	# any NAs with spatially-avg temp from ROMS?
	nn_specimen_dat <- specimen_dat_list %>% 
		bind_rows() 
	
	# any NAs?
  spec_NAs <- nn_specimen_dat %>%
  	filter(if_any(c(presurvey_btemp, yrprior_btemp), is.na))
  
  #sort(unique(spec_NAs$stationid))
	
	## scale cols
	
	std_func <- function(df){
		
 		df <- df %>%
			mutate_at(vars(contains("temp")), ~ scale(.) %>% as.vector) %>%
 			mutate(weight_std = as.vector(scale(weight)),
 						 log_wt_std = as.vector(scale(log_wt)))
		
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

	pollock_dat <- specimen_dat_list[[1]] #%>% na.omit()
	# any NAs
	pol_NAs <- pollock_dat[!complete.cases(pollock_dat), ] 

	pcod_dat <- specimen_dat_list[[2]] #%>% na.omit()
	# any NAs
	pcod_NAs <- pcod_dat[!complete.cases(pcod_dat), ] 

	yfinsole_dat <- specimen_dat_list[[3]] #%>% na.omit()
	# any NAs
	yfs_NAs <- yfinsole_dat[!complete.cases(yfinsole_dat), ] 

	
	# how many samples per age class?
	pol_sum <- pollock_dat %>% # ages 1-20 have >= 100
		group_by(age) %>%
		summarise(n = n())
	
	pcod_sum <- pcod_dat %>% # ages 1-10 have >= 100
		group_by(age) %>%
		summarise(n = n())
	
	yf_sum <- yfinsole_dat %>% # ages 3-28
		group_by(age) %>%
		summarise(n = n())
	
	# trim data set by largest age with >= 100 samples
	pollock_dat <- pollock_dat  %>% filter(between(age, 1, 20))
	
	pcod_dat <- pcod_dat  %>% filter(between(age, 1, 10))

	yfinsole_dat <- yfinsole_dat  %>% filter(between(age, 3, 28))

	# drop levels
	pcod_dat$age_f <- droplevels(pcod_dat$age_f)
	pollock_dat$age_f <- droplevels(pollock_dat$age_f)
	yfinsole_dat$age_f <- droplevels(yfinsole_dat$age_f)

	# combine species dfs for running models
	dat_all <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat)
	
	fwrite(dat_all, file = here("./data/sp_dat_all.csv"))
	
	
	# any NAs
	NA_dat_all <- dat_all[!complete.cases(dat_all), ] 
