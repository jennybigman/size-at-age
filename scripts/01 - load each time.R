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


	# beepr function shortcut
	
	beep <- function(x = "fanfare"){
		beepr::beep(x)
	}