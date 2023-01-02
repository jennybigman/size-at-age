# load each time

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
 		specimen_dat <- left_join(x, ACLIM_hind_temps, by = "year")
	}
 
	spec_temp_dat <- lapply(specimen_dat_list, temp_join_func)

	

	# beepr function shortcut
	
	beep <- function(x = "fanfare"){
		beepr::beep(x)
	}