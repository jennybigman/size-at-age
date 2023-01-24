# load each time

 # for screen
 setwd("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age")

	library(here)
	library(tidyverse)
	library(lubridate)
	library(mgcv)
	library(MuMIn)
	library(visreg)
	library(data.table)
	library(gamm4)
	library(tidymv)
	library(brms)
	library(mgcViz)
	library(patchwork)
	library(grid)
	library(gratia)
	library(sf)
	library(AICcmodavg)

	#### temp data ####
	
	# ROMS temps from Pcod paper ####
	
	# yearly-averaged hindcast temp output
	ROMS_bot_temp_yr <- fread(here("./data/ROMS_bot_temp_yr.csv")) %>%
		rename(yr_mean_temp = mean_temp)
	
	# summer averaged temp output
	ROMS_sum_temp_avg <- fread(here("./data/ROMS_sum_temp_avg.csv"))

	# join temp data
	ROMS_hind_temps <- merge(ROMS_bot_temp_yr, ROMS_sum_temp_avg,
											by = c("year"))
	
	# ACLIM bc temps ####
	
	load("../../ACLIM2/Data/out/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hind_mn.Rdata")

  temp_hind <- ACLIM_weekly_hind %>%
    filter(var == "temp_bottom5m")

  sum_mo <- 4:6
  
  ACLIM_temp_hind_sum_mean <- temp_hind %>%
  	filter(mo %in% sum_mo) %>%
  	group_by(year) %>%
  	summarise(mean_temp_sum_ACLIM = mean(mn_val))

  ACLIM_temp_hind_yr_mean <- temp_hind %>%
  	group_by(year) %>%
  	summarise(mean_temp_yr_ACLIM = mean(mn_val))
  
  ACLIM_hind_temps <- left_join(ACLIM_temp_hind_yr_mean, ACLIM_temp_hind_sum_mean)
	
  # keep basin separate 
  SEBS_ACLIM_temp_hind_sum_mean <- temp_hind %>%
  	filter(mo %in% sum_mo) %>%
  	filter(basin == "SEBS") %>%
  	group_by(year) %>%
  	summarise(SEBS_mean_sum_temp = mean(mn_val))

  SEBS_ACLIM_temp_hind_yr_mean <- temp_hind %>%
  	filter(basin == "SEBS") %>%
  	group_by(year) %>%
  	summarise(SEBS_mean_yr_temp = mean(mn_val))
  
  SEBS_ACLIM_hind_temps <- left_join(SEBS_ACLIM_temp_hind_sum_mean, SEBS_ACLIM_temp_hind_yr_mean)

	# specimen data
	specimen_dat <- read.csv(file = here("./data/df_list_wrangled_names.csv"))

	# how many NAs per species do I have
	spec_dat_0 <- specimen_dat %>%
		dplyr::select(species, age) %>%
		group_by(species) %>%
		summarise(NAs = sum(is.na(age)))
	
	# join specimen data and ROMS temp
	
	# put each species df into a list
	specimen_dat$haul <- specimen_dat$haul.x
	specimen_dat$cohort_f <- as.factor(specimen_dat$cohort_f)
	

	specimen_dat_list <- list()
 
	specimen_dat_list <- specimen_dat %>%
  	group_split(species) 
	
	
	# merge with temp
	temp_join_func <- function(x){
 
 		specimen_dat <- left_join(x, ROMS_hind_temps, by = "year")
 		specimen_dat <- left_join(specimen_dat, ACLIM_hind_temps, by = "year")
 		specimen_dat <- left_join(specimen_dat, SEBS_ACLIM_hind_temps, by = "year")
 		
	}
 
	spec_temp_dat <- lapply(specimen_dat_list, temp_join_func)


	# individual data sets ####
	
	# pollock #
	pollock_dat <- spec_temp_dat[[2]] %>%
		rename(julian_day = jday)

	pollock_dat$age_f <- as_factor(pollock_dat$age)
	
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	pollock_age_sum <- pollock_dat %>%
		group_by(age_f) %>%
		summarize(n())
	
	pollock_dat <- pollock_dat  %>% filter(between(age, 1, 10))
	# stock assessment goes to 14, but after age class 10, not many individuals per age class

	# set up random effect coding
	
	pol_yr <- sort(unique(pollock_dat$year))
	ID <- letters[1:length(pol_yr)]
	
	pol_yr_id <- data.frame(pol_yr, ID) %>%
		rename(year = pol_yr)
	
	pollock_dat <- merge(pollock_dat, pol_yr_id, by = "year")
	
	pollock_dat <- pollock_dat %>%
		mutate(haul_id = paste(ID, haul, sep = "_"))
	
	pollock_dat <- pollock_dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))

	glimpse(pollock_dat)
	

 # pcod #

	pcod_dat <- spec_temp_dat[[1]] %>%
		rename(julian_day = jday)
	
	pcod_dat$age_f <- as_factor(pcod_dat$age)
	
	pcod_dat <- pcod_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	pcod_dat <- pcod_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	pcod_age_sum <- pcod_dat %>%
		group_by(age_f) %>%
		summarize(n())
	
	pcod_dat <- pcod_dat  %>% filter(between(age, 1, 12))
	# stock assessment goes to 12+
	
	pcod_age_sum <- pcod_dat %>%
		group_by(age_f) %>%
		summarize(n())

	# set up random effect coding
	
	pcod_year <- sort(unique(pcod_dat$year))
	ID <- letters[1:length(pcod_year)]
	
	pcod_years_id <- data.frame(pcod_year, ID) %>%
		rename(year = pcod_year)
	
	pcod_dat <- merge(pcod_dat, pcod_years_id, by = "year")
	
	pcod_dat <- pcod_dat %>%
		mutate(haul_id = paste(ID, haul, sep = "_"))
	
	pcod_dat <- pcod_dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))

	glimpse(pcod_dat)


	# yellowfin sole #
		
	# data wrangling
	yfinsole_dat <- spec_temp_dat[[3]] %>%
		rename(julian_day = jday)
	
	yfinsole_dat$age_f <- as_factor(yfinsole_dat$age)
	
	yfinsole_dat <- yfinsole_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	yfinsole_dat <- yfinsole_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	yfinsole_age_sum <- yfinsole_dat %>%
		group_by(age_f) %>%
		summarize(n())
	
	yfinsole_dat <- yfinsole_dat  %>% filter(between(age, 1, 17))
	# stock assessment goes to 17+

	# set up random effect coding
	
	yfin_year <- sort(unique(yfinsole_dat$year))
	ID <- letters[1:length(yfin_year)]
	
	yfin_years_id <- data.frame(yfin_year, ID) %>%
		rename(year = yfin_year)
	
	yfinsole_dat <- merge(yfinsole_dat, yfin_years_id, by = "year")
	
	yfinsole_dat <- yfinsole_dat %>%
		mutate(haul_id = paste(ID, haul, sep = "_"))
	
	yfinsole_dat <- yfinsole_dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))

	glimpse(yfinsole_dat)

  
  # temp during age 1 ####
  
	# create a function to add in avg temp during year 1 ####
			
	hind_temps <- SEBS_ACLIM_temp_hind_yr_mean %>%
		dplyr::select(year, SEBS_mean_yr_temp)

	# function to add temp during age1
	cohort_temp_func <- function(df){
		
		cohorts <- df %>%
			dplyr::select(cohort_f, cohort) %>%
			rename(year = cohort) %>%
			distinct()
		
		cohort_temps <- left_join(cohorts, hind_temps) %>%
			rename(temp_firstyr = SEBS_mean_yr_temp) %>%
			dplyr::select(-year)
	
		dat <- left_join(df, cohort_temps)

	}	
	
	dat_list <- lapply(list(pollock_dat, pcod_dat, yfinsole_dat), cohort_temp_func)
	
	pollock_dat <- dat_list[[1]]
	pcod_dat <- dat_list[[2]]
	yfinsole_dat <- dat_list[[3]]
	
	# temp during first year of life (age0 -1) ####
	
		temp_age0_func <- function(df){
		
		df <- df %>%
		mutate(year_age0 = cohort - 1,
					 year_age0_f = as.factor(year_age0))
		
	yr_age0_dat <- df %>%
			dplyr::select(year_age0_f, year_age0) %>%
			rename(year = year_age0) %>%
			distinct()
		
	yr_age0_temps <- left_join(yr_age0_dat, hind_temps) %>%
			rename(temp_age0 = SEBS_mean_yr_temp) %>%
			dplyr::select(-year)
	
	dat <- left_join(df, yr_age0_temps)
	

	}	
	
	dat_list <- lapply(list(pollock_dat, pcod_dat, yfinsole_dat), temp_age0_func)
	
	pollock_dat <- dat_list[[1]]
	pcod_dat <- dat_list[[2]]
	yfinsole_dat <- dat_list[[3]]
	
	# temp avg to maturity ####
	
	# pollock ####

 	# unique age and cohorts
  pollock_dat_cohorts <- pollock_dat %>%
 		dplyr::select(age, cohort) %>%
  	distinct() %>%
  	group_by(age)

	pollock_dat_list <- group_split(pollock_dat_cohorts)

	# age 1
	pollock_age1_dat <- pollock_dat_list[[1]] %>% 
		mutate(year0 = cohort,
					 year1 = cohort + 1)
	
	pollock_age1_dat <- 
		left_join(pollock_age1_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) 

	pollock_age1_dat <- pollock_age1_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)


	# age 2
	pollock_age2_dat <- pollock_dat_list[[2]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2)
	
	pollock_age2_dat <- 
		left_join(pollock_age2_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) 

	
	pollock_age2_dat <- pollock_age2_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 3
	pollock_age3_dat <- pollock_dat_list[[3]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3)
																 
	pollock_age3_dat <- 
		left_join(pollock_age3_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	pollock_age3_dat <- pollock_age3_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 4
	pollock_age4_dat <- pollock_dat_list[c(4:10)] %>%
		bind_rows()
	
	pollock_age4_dat <- pollock_age4_dat %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4)
		
	pollock_age4_dat <- 
		left_join(pollock_age4_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) 
	
	pollock_age4_dat <- pollock_age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS")))) %>%
		select(age, cohort, temp_mat)
	
	pollock_temp_mat <- bind_rows(pollock_age1_dat, pollock_age2_dat, pollock_age3_dat, pollock_age4_dat)

	pollock_dat <- left_join(pollock_dat, pollock_temp_mat, by = c("age", "cohort"))
	
	# pcod ####
	
 	# unique age and cohorts
  pcod_dat_cohorts <- pcod_dat %>%
 		dplyr::select(age, cohort) %>%
  	distinct() %>%
  	group_by(age)

	pcod_dat_list <- group_split(pcod_dat_cohorts)

	# age 1
	pcod_age1_dat <- pcod_dat_list[[1]] %>% 
		mutate(year0 = cohort,
					 year1 = cohort + 1)
	
	pcod_age1_dat <- 
		left_join(pcod_age1_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) 

	pcod_age1_dat <- pcod_age1_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)


	# age 2
	pcod_age2_dat <- pcod_dat_list[[2]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2)
	
	pcod_age2_dat <- 
		left_join(pcod_age2_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) 

	
	pcod_age2_dat <- pcod_age2_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 3
	pcod_age3_dat <- pcod_dat_list[[3]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3)
																 
	pcod_age3_dat <- 
		left_join(pcod_age3_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	pcod_age3_dat <- pcod_age3_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 4
	pcod_age4_dat <- pcod_dat_list[[4]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4)
																 
	pcod_age4_dat <- 
		left_join(pcod_age4_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	pcod_age4_dat <- pcod_age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 5
	pcod_age5_dat <- pcod_dat_list[c(5:10)] %>%
		bind_rows()
	
	pcod_age5_dat <- pcod_age5_dat %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4,
					 year5 = cohort + 5)
		
	pcod_age5_dat <- 
		left_join(pcod_age5_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) %>%
		left_join(., hind_temps, by=c('year5'='year')) 

	
	pcod_age5_dat <- pcod_age5_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS")))) %>%
		select(age, cohort, temp_mat)
	
	pcod_temp_mat <- bind_rows(pcod_age1_dat, pcod_age2_dat, pcod_age3_dat, pcod_age4_dat)

	pcod_dat <- left_join(pcod_dat, pcod_temp_mat, by = c("age", "cohort"))
	
	
	
	
	
	
	
	
	# beepr function shortcut
	
	beep <- function(x = "fanfare"){
		beepr::beep(x)
	}
	