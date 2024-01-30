# spatially explicit temp 

	# matching temp from sp dfs to hindcast based on station ID

	# ACLIM survey-replicated hindcast
  load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/BC_ACLIMsurveyrep/ACLIMsurveyrep_OperationalHindcast_BC_hind.Rdata")
 
	# filter bottom temp & oxy, keep only some cols, and trim the whitespace around station_id
  bot_vars <- c("temp_bottom5m", "oxygen_bottom5m") # no oxygen right now?

  bot_hind_var <- hind %>%
    filter(var %in% bot_vars) %>%
  	filter(year < 2021) %>%
  	select(year, station_id, mn_val, val_raw, var) %>%
  	mutate(stationid = str_trim(station_id))
 
  # filter bottom temp & oxy, keep only some cols, and trim the whitespace around station_id
  int_vars <- c("temp_integrated", "oxygen_integrated")
	
  int_hind_var <- hind %>%
    filter(var %in% int_vars) %>%
  	filter(year < 2021) %>%
  	select(year, station_id, mn_val, val_raw, var) %>%
  	mutate(stationid = str_trim(station_id))

  ## calculate year prior temp and oxygen
  #yr_prior_func <- function(df, x){
  # 	
  # 	# temp same year as collected, Jan - June
  # 	current <- df %>%
  # 		filter(year == x & mo <= 6)
 #
  # 	# temp previous year July - Dec
	#	previous <- df %>%
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
  #	hind_yrs <- sort(unique(bot_hind_var$year))
  #	
  #	df_list <- list(bot_hind_var, int_hind_var)
  #	
  #	dfs_func <- expand_grid(
  #		dfs = df_list,
  #		x = hind_yrs
  #	)
  #	
	#	dfs <- map2(dfs_func$dfs, dfs_func$x, yr_prior_func)
		
  #yr_prior_short <- 
  #	spread(yr_prior, key = var, value = mean_yr) %>%
	#	rename(yrprior_btemp = "temp_bottom5m",
  #				 yrprior_boxy = "oxygen_bottom5m")
  
  # match ROMS temps to station ids in species dfs
  
  ## pollock - integrated temp ##
  pol_survey_stations <- sort(unique(pollock_dat$stationid))

	pol_roms_match <- bot_hind_var %>% 
		filter(stationid %in% pol_survey_stations) %>%
		select(-station_id)
	
	roms_station_pollock <- sort(unique(pol_roms_match$stationid))
	
	pol_missing_stations <- setdiff(pol_survey_stations, roms_station_pollock)
		
	pollock_dat <- left_join(pollock_dat, pol_roms_match, by = c("stationid", "year"))

  
  ## pcod - bottom temp ##
	pcod_survey_stations <- sort(unique(pcod_dat$stationid))

	pcod_roms_match <- bot_hind_var %>% 
		filter(stationid %in% pcod_survey_stations) %>%
		select(-station_id)
	
	roms_station_pcod <- sort(unique(pcod_roms_match$stationid))
	
	pcod_missing_stations <- setdiff(pcod_survey_stations, roms_station_pcod)
	
	station_count <- pcod_dat %>%
		group_by(stationid) %>%
		summarise(n())
	
	pcod_dat <- left_join(pcod_dat, pcod_roms_match, by = c("stationid", "year"))
		
	## yfin - bottom temp ##
	yfin_survey_stations <- sort(unique(yfinsole_dat$stationid))

	yfin_roms_match <- bot_hind_var %>% 
		filter(stationid %in% yfin_survey_stations) %>%
		select(-station_id)
	
	roms_station_yfin <- sort(unique(yfin_roms_match$stationid))
	
	yfin_missing_stations <- setdiff(yfin_survey_stations, roms_station_yfin)
	
	yfinsole_dat <- left_join(yfinsole_dat, yfin_roms_match, by = c("stationid", "year"))

	dat_all <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat)
	
	########## by nearest neighbor ##############
	
	# pollock has the highest number of unique stations so use this df to get avg lat long of stations over the hindcast period
	survey_grid <- pollock_dat %>%
		select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	
	# read in ROMS hindcast temps
	roms_hind_temps <- fread(file = here("./data/roms_hind_temps.csv"))

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
	roms_hind_temps <- roms_hind_temps %>%
			select(-Xi, -Eta, -DateTime, -Time) %>%
			mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
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

	#### calculate temp metrics ####
	
	#### 1. Spring/Summer temps preceding survey ####
	
  presurvey_mo <- 4:6 # (April to June)
  
  presurvey_temps <- survey_roms_grid %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(roms_ID, year) %>%
  	summarise(presurvey_btemp = mean(bot_temp))
  
  
  # add to dataframe list
	dat_join_func <- function(x){
 
 			specimen_dat <- left_join(x, presurvey_temps, by = c("year", "roms_ID"))
	}
 
	specimen_dat <- lapply(specimen_dat_list, dat_join_func)


  #### 2. Yearly temp -- avg of temp July - Dec of year before survey & Jan - June year of survey) ####
  
  yr_prior_func <- function(x){
   	
   	# temp same year as collected, Jan - June
   	current <- hind_var %>%
   		filter(year == x & mo <= 6)
 
   	# temp previous year July - Dec
		previous <- hind_var %>%
  		filter(year == x - 1 & mo > 6)
 
		# combine and take a mean temp July - June
  	var_yr <- bind_rows(current, previous) %>%
  		group_by(var) %>%
  		summarise(mean_yr = mean(mn_val)) %>%
  		mutate(year = x)
   
   }
   
  yr_prior <- lapply(1971:2020, yr_prior_func) %>% bind_rows()
  
  yr_prior_short <- 
  	spread(yr_prior, key = var, value = mean_yr) %>%
		rename(yrprior_btemp = "temp_bottom5m",
  				 yrprior_boxy = "oxygen_bottom5m")
  
