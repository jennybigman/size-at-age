#00d - data wrangling specimen data and nearest neighbor matching

	library(rnaturalearth)
	library(sf)
	library(data.table)
	
	sf_use_s2(FALSE)
	
	`%!in%` = Negate(`%in%`)

	# polygons of AK for plotting
	world <- ne_countries(scale = "medium",
                      returnclass = "sf") 

	US_map <- world %>%
  	filter(name %in% c("United States"))

	# Crop the polygon for plotting and efficiency:
	# st_bbox(map_data) # find the rough coordinates
	AK_coast <- suppressWarnings(suppressMessages(
  	st_crop(US_map,
    c(xmin = -179, ymin = 54, xmax = -155, ymax = 65))))

	utm_zone <- 4326
	AK_coast_proj <- sf::st_transform(AK_coast, crs = utm_zone)
	
	#### data wrangling ####
	dat_all <- read.csv(file = here("./data/dat_all.csv")) %>%
		dplyr::select(-X) 
	
	# age and cohort as factors and log weight
	dat_all <- dat_all %>%
		mutate(cohort = year - age, 
					 cohort_f = as.factor(cohort))
	
	# how many NAs per species do I have
	spec_dat_0 <- dat_all %>%
		dplyr::select(common_name, age) %>%
		group_by(common_name) %>%
		summarise(NAs = sum(is.na(age))) 
	
	# add short name
	dat_all <- dat_all %>%
		mutate(
			short_name = case_when(
				common_name == "walleye pollock" ~ "pollock",
				common_name == "Pacific cod" ~ "pcod",
				common_name == "yellowfin sole" ~ "yfin",
				common_name == "arrowtooth flounder" ~ "atooth")
			)
	
	# put each species df into a list
	specimen_dat_list <- dat_all %>%
		group_by(short_name) %>%
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

	# read in level 2 (by grid cell) ROMS hindcast bottom temps & oxygen

	temp <- fread(file = here("./data/hindcast_temp_K20P19.csv")) %>%
	#temp <- temp_hind_dat %>%
		drop_na(temp) %>%
		dplyr::select(temp, month, week, year, longitude, latitude) %>%
		group_by(month, year, longitude, latitude) %>%
		summarise(temp = mean(temp))
	
	oxygen <- fread(file = here("./data/hindcast_oxy_K20P19.csv")) %>%
	#oxygen <- oxy_hind_dat %>%
		drop_na(oxygen) %>%
		dplyr::select(oxygen, month, week, year, longitude, latitude) %>%
		group_by(month, year, longitude, latitude) %>%
		summarise(oxygen = mean(oxygen))
	
	envr_vars <- full_join(temp, oxygen)
	
	# get grid from ROMS df
	roms_grid <- envr_vars %>%
		ungroup() %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	# assign an ID to each unique lat/long combo (grid cell)
	roms_grid$roms_ID <- 1:nrow(roms_grid)
	
	# remove unecessary cols and convert lat/long
	envr_vars <- envr_vars %>%
			mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	# add in col denoting grid ID based on lat/long
	envr_vars <- left_join(envr_vars, roms_grid)
	
	# NAs
	NAs <- envr_vars[!complete.cases(envr_vars), ]
	empty_cells <- unique(NAs$roms_ID)

	# match roms grid cells to survey cells
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
							longitude = mean(longitude)) %>%
		drop_na()
	
	# match lat/longs of survey grid to nearest neighbor from ROMS grid	
	
	# use nn2() to calculate min distance to nearest ROMS lat/long
	station_list[, c(4,5)] <- as.data.frame(RANN::nn2(roms_grid[, c('latitude', 'longitude')],
                                                    station_list[, c('latitude', 'longitude')],
                                                    k = 1))

	# Match nearest lat/long from ROMS
	station_list$roms_ID <- pull(roms_grid[c(station_list$nn.idx), 'roms_ID'])

	# any NAs in matching?
	which(is.na(station_list$roms_ID), )
	
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
		x = 1970:2023, # survey data goes to 2021 for some species
		y = 1:12
	)
	
	survey_grid_full <- purrr::map2(df_func$x, df_func$y, survey_df_func) %>%
		bind_rows
	
	# filter out grid cells/points that aren't in survey data
	ID_keep <- sort(unique(station_list$roms_ID))

	envr_vars <- envr_vars %>%
		rename(roms_lat = latitude,
					 roms_long = longitude) %>%
		filter(roms_ID %in% ID_keep)
	
	NAs <- envr_vars[!complete.cases(envr_vars), ]
	empty_cells <- unique(NAs$roms_ID)
	

	# monthly roms temps that match survey stations
	survey_roms_grid <- left_join(survey_grid_full, envr_vars, 
																by = c('roms_ID', "year", "month"))

	# any NAs?
	NAs <- survey_roms_grid[!complete.cases(survey_roms_grid), ]
	unique(NAs$year)
	unique(NAs$month)
	# no data from Sept - Dec 2023
	
	survey_roms_grid <- survey_roms_grid %>%
		drop_na()

	NAs <- survey_roms_grid[!complete.cases(survey_roms_grid), ]

	# add roms IDs to species data
	survey_grid_trim <- station_list %>% 
		dplyr::select(stationid, roms_ID)
	
	sp_roms <- function(df){
		
		df <- left_join(df, survey_grid_trim, by = "stationid") 
		
	
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, sp_roms)
	
	##### calculate temperature 3 months prior to survey ####
	
  presurvey_mo <- 4:6 # (April to June)
  
  presurvey_vars <- survey_roms_grid %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(roms_ID, year) %>%
  	summarise(presurvey_btemp = mean(temp),
  						presurvey_boxy = mean(oxygen)) 
  
  # any NAs?
  NAs <- presurvey_vars[!complete.cases(presurvey_vars), ]
  
	presurvey_vars <- left_join(presurvey_vars, survey_grid_trim, by = "roms_ID") %>% dplyr::select(-stationid)
	
	NAs <- presurvey_vars[!complete.cases(presurvey_vars), ]
  
	# add to dataframe list
	dat_join_func <- function(df){
		
		df <- df %>% 
			dplyr::select(-month)
 
 		df <- left_join(df, presurvey_vars, by = c("year", "roms_ID"))
	}
 
	specimen_dat_list <- lapply(specimen_dat_list, dat_join_func)
	
	#test <- specimen_dat_list %>% bind_rows()
	
	#missing <- test[!complete.cases(test), ]
	
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
  		summarise(yrprior_btemp = mean(temp),
  							yrprior_boxy = mean(oxygen)) %>%
  		mutate(year = x)
   
   }
   
  yrprior_vars <- lapply(1970:2023, yr_prior_func) %>% bind_rows()
  
  # any NAs?
  yrprior_vars[!complete.cases(yrprior_vars), ]

  # add to dataframe list
	dat_join_func <- function(df){
 
 			df <- left_join(df, yrprior_vars, by = c("year", "roms_ID"))
	}
 
	specimen_dat_list <- lapply(specimen_dat_list, dat_join_func)

		
	#test2 <- specimen_dat_list %>% bind_rows()
	
	#missing <- test2[!complete.cases(test2), ]

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
  
	
	dat_all <- bind_rows(specimen_dat_list) 
	
	fwrite(dat_all, file = here("./data/sp_dat_all_May_2024.csv"))
	
	# any NAs
	NA_dat_all <- dat_all[!complete.cases(dat_all), ] 

	
	sum <- dat_all %>% 
		group_by(short_name, age_f) %>%
		summarise(count = n())
	
	
	
	
	
	
