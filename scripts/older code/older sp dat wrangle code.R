	# data wrangling species dataframes

	specimen_dat <- read.csv(file = here("./data/df_list_wrangled_names.csv"))
	
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
	
	# have to remove some NAs 
	specimen_dat <- specimen_dat %>% 
		drop_na(c(age, date, month, year, jday, cohort))
	
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
	
	# remove 2021 because no output in hindcast and will throw error in sdmTMB()
	rem_2021 <- function(x){
		x <- x %>% filter(year != 2021)
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, rem_2021)
	
	# prep df for sdmTMB()
	
	# add XY cols for making mesh
	xy_func <- function(df){
	
		df <- df %>% 
			dplyr::select(-X) %>%
			drop_na(longitude, latitude)
	
		df <- df %>% sdmTMB::add_utm_columns(., c("longitude", "latitude"))

	}
	
	specimen_dat_list <- lapply(specimen_dat_list, xy_func)
	
	#### ROMS output ####

	# read in CMIP6 ROMS output - averaged by basin annually
  #load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hind_mn.Rdata")
  #load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/allEBS_means/ACLIM_weekly_fut_mn.Rdata")
  
  # for PC
  #load("/data/ACLIM_weekly_hind_mn.Rdata")
  #load("/data/ACLIM_weekly_fut_mn.Rdata")
  
  # filter out bottom temp & oxygen and stick to SEBS
  
  #vars <- c("temp_bottom5m", "oxygen_bottom5m")
  #
  #hind_var <- ACLIM_weekly_hind %>%
  #  filter(var %in% vars) %>%
  #	filter(year < 2021) %>%
  #	filter(basin == "SEBS")
  #
  #proj_var <- ACLIM_weekly_fut %>%
  #  filter(var %in% vars) %>%
  #	filter(year >= 2021) %>%
  #	filter(basin == "SEBS")

  # matching temp from sp dfs to hindcast based on station ID

	#### 1. Spring/Summer temps preceding survey ####
	
  #presurvey <- 4:6 # (April to June)
  #
  #presurvey_hind_var <- hind_var %>%
  #	filter(mo %in% presurvey) %>%
  #	group_by(year, var) %>%
  #	summarise(presurvey_mean_val = mean(mn_val))
  #
  #presurvey_hind_var_short <- 
  #	spread(presurvey_hind_var, key = var, value = presurvey_mean_val) %>%
  #	rename(presurvey_btemp = "temp_bottom5m",
  #				 presurvey_boxy = "oxygen_bottom5m")
  	
  # plot
  #presurvey_long <- presurvey_hind_var_short %>%
  #	pivot_longer(
  #		cols = starts_with("presurvey"),
  #		names_to = "var",
  #		values_to = "val")
  #
	#ggplot(data = presurvey_long) +
	#	geom_line(aes(x = year, y = val)) +
	#	facet_wrap( ~ var, scales = "free")
	
 
  ## add to dataframe list
	#dat_join_func <- function(x){
 #
 	#		specimen_dat <- left_join(x, presurvey_hind_var_short, by = "year")
	#}
 #
	#specimen_dat <- lapply(specimen_dat_list, dat_join_func)
#
#
  ##### 2. Yearly temp -- avg of temp July - Dec of year before survey & Jan - June year of survey) ####
  #
  #yr_prior_func <- function(x){
  # 	
  # 	# temp same year as collected, Jan - June
  # 	current <- hind_var %>%
  # 		filter(year == x & mo <= 6)
 #
  # 	# temp previous year July - Dec
	#	previous <- hind_var %>%
  #		filter(year == x - 1 & mo > 6)
 #
	#	# combine and take a mean temp July - June
  #	var_yr <- bind_rows(current, previous) %>%
  #		group_by(var) %>%
  #		summarise(mean_yr = mean(mn_val)) %>%
  #		mutate(year = x)
  # 
  # }
  # 
  #yr_prior <- lapply(1971:2020, yr_prior_func) %>% bind_rows()
  #
  #yr_prior_short <- 
  #	spread(yr_prior, key = var, value = mean_yr) %>%
	#	rename(yrprior_btemp = "temp_bottom5m",
  #				 yrprior_boxy = "oxygen_bottom5m")
  #
  # plot
  #yrprior_long <- yr_prior_short %>%
  #	pivot_longer(
  #		cols = starts_with("yrprior"),
  #		names_to = "var",
  #		values_to = "val")
  #
	#ggplot(data = yrprior_long) +
	#	geom_line(aes(x = year, y = val)) +
	#	facet_wrap( ~ var, scales = "free")
	
  
  #dat_join_func <- function(x){
 #
 	#		specimen_dat <- left_join(x, yr_prior_short, by = "year")
	#}
 #
	#specimen_dat <- lapply(specimen_dat, dat_join_func)

  #### add in spatially-explicit temperature
	
	# read in level 2 (by grid cell) ROMS hindcast temps
	roms_hind_temps <- fread(file = here("./data/roms_hind_temps.csv"))
	
	# pollock has the highest number of unique stations so use this df to get 
	# avg lat long of stations over the hindcast period
	
	pollock_dat <- specimen_dat_list[[1]]
	
	survey_grid <- pollock_dat %>%
		select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	
	# find temp value closest to survey haul locations
	roms_grid <- roms_hind_temps %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	roms_grid$roms_ID <- 1:nrow(roms_grid)
	
	# match lat/longs of survey grid to nearest neighbor from ROMS grid	
	
	# use nn2() to calculate min distance to nearest ROMS lat/long
	survey_grid[, c(4, 5)] <- as.data.frame(RANN::nn2(roms_grid[, c('latitude', 'longitude')],
                                                  survey_grid[, c('latitude', 'longitude')],
                                                  k = 1))
	
	# Match nearest lat/long from ROMS
	survey_grid$roms_ID <- pull(roms_grid[c(survey_grid$nn.idx), 'roms_ID'])
	
	# any NAs in matching?
	which(is.na(survey_grid$roms_ID), )
	
	# drop cols from nn2
	survey_grid <- survey_grid %>%
		select(-nn.idx, -nn.dists)
	
	# create a df for each month and year to match to ROMS temps
	survey_df_func <- function(x, y){
		
		df <- survey_grid %>%
			mutate(year = x,
						 month = y)
	}
	
	df_func <- crossing(
		x = 1970:2020,
		y = 1:12
	)
	
	survey_grid_full <- map2(df_func$x, df_func$y, survey_df_func) %>%
		bind_rows

	
	# summarise roms temps by month and match to survey grid based on lat long
	
	# remove unecessary cols and convert lat/long
	roms_hind_temps <- roms_hind_temps %>%
			select(-Xi, -Eta, -DateTime, -Time) %>%
			mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	# add in col denoting grid ID based on lat/long
	roms_hind_temps <- left_join(roms_hind_temps, roms_grid)
	
	# filter out grid cells/points that aren't in survey data
	ID_keep <- sort(unique(survey_grid$roms_ID))

	roms_temps <- roms_hind_temps %>%
 		filter(roms_ID %in% ID_keep) %>%
		rename(roms_lat = latitude,
					 roms_long = longitude)
	
	# summarise temps by month and year for each grid cell/point
	roms_temp_sum <- roms_temps %>%
		group_by(roms_ID, month, year, roms_lat, roms_long) %>%
		summarise(bot_temp = mean(bot_temp)) # change this to both integrated and bottom
	
	# monthly roms temps that match survey stations
	survey_roms_grid <- left_join(survey_grid_full, roms_temp_sum, by = c('roms_ID', "year", "month"))

	# any NAs?
	NAs <- survey_roms_grid[!complete.cases(survey_roms_grid), ]
	
	grid_problems <- test %>%
		group_by(roms_ID) %>%
		summarise(n())
	
	# add roms IDs to species data
	survey_grid_trim <- survey_grid %>% select(stationid, roms_ID)
	
	sp_roms <- function(df){
		
		df <- left_join(df, survey_grid_trim, by = "stationid") 
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, sp_roms)
	
	##### calculate temperature 3 months prior to survey ####
  presurvey_mo <- 4:6 # (April to June)
  
  presurvey_temps <- survey_roms_grid %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(roms_ID, year) %>%
  	summarise(presurvey_btemp = mean(bot_temp)) # change to both bot and integrated temp
  
  # add to dataframe list
	dat_join_func <- function(df){
 
 			df <- left_join(df, presurvey_temps, by = c("year", "roms_ID"))
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
   
  yr_prior_temp <- lapply(1971:2020, yr_prior_func) %>% bind_rows()
  
  # add to dataframe list
	dat_join_func <- function(df){
 
 			df <- left_join(df, yr_prior_temp, by = c("year", "roms_ID"))
	}
 
	specimen_dat_list <- lapply(specimen_dat_list, dat_join_func)

	
	## scale cols
	
	std_func <- function(df){
		
 		df <- df %>%
			mutate_at(vars(contains("temp")), ~ scale(.) %>% as.vector)
		
		df <- df %>% 
			mutate(jday_std = (jday - (mean(jday)))/sd(jday))

	}

	specimen_dat_list <- lapply(specimen_dat_list, std_func)
	
	# change species names, add a col for year as a factor, order age classes
  
  col_wrangle_func <- function(df){
  	
  	df <- df %>% mutate(year_f = as.factor(year))
  	df$year_f <- droplevels(df$year_f)
  	df
  	
  }
 
	specimen_dat_list <- lapply(specimen_dat_list, col_wrangle_func)
  
	# separate for species-specific wrangling tasks

	pollock_dat <- specimen_dat_list[[1]] %>% na.omit()
	pcod_dat <- specimen_dat_list[[2]] %>% na.omit()
	yfinsole_dat <- specimen_dat_list[[3]] %>% na.omit()

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
	