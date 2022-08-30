# load each time

	library(here)
	library(tidyverse)
	library(lubridate)
	library(mgcv)
	library(MuMIn)
	library(visreg)
	library(data.table)
	library(gamm4)

	# yearly-averaged temp data
	ROMS_bot_temp_yr <- fread(here("./data/ROMS_bot_temp_yr.csv")) %>%
		rename(yr_mean_temp = mean_temp)
	
	# summer averaged temp data
	ROMS_sum_temp_avg <- fread(here("./data/ROMS_sum_temp_avg.csv"))

	# join temp data
	ROMS_temps <- merge(ROMS_bot_temp_yr, ROMS_sum_temp_avg,
											by = c("year"))
	
	
	# specimen data
	specimen_dat <- read.csv(file = here("./data/df_list_wrangled_names.csv"))

	# join specimen data and ROMS temp
	
	# put each species df into a list
	specimen_dat$haul <- specimen_dat$haul.x
	specimen_dat$age_f <- as.factor(specimen_dat$age)
	specimen_dat$cohort_f <- as.factor(specimen_dat$cohort_f)
	

	specimen_dat_list <- list()
 
	specimen_dat_list <- specimen_dat %>%
  	group_split(species) 
	
	
	# merge with temp
	join_func <- function(x){
 
 		specimen_dat <- merge(x, ROMS_temps, by = "year")
	}
 
	spec_temp_dat <- lapply(specimen_dat_list, join_func)
