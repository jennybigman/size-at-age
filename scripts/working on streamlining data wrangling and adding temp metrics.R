	# libraries
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
	
	#### read in specimen age & weight data ####
	
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

	## code random effect haul and year ##
	
	ranef_setup_func <- function(x){
		
		year_sort <- sort(unique(x$year))
		ID <- letters[1:length(year_sort)]
		
		year_ID <- data.frame(year_sort, ID) %>%
			rename(year = year_sort)
		
		dat <- merge(x, year_ID, by = "year")
	
		dat <- dat %>%
			mutate(haul_id = paste(ID, haul, sep = "_"))
	
		dat <- dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))
		
		
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, ranef_setup_func)
	
	# add columns
	
	weight_metrics_func <- function(x){ 

		# calculate mean and sd weight by age
		dat <- x %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt)) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, weight_metrics_func)
	
	#### temp output ####
	
	# load ACLIM temps
	
	# bias corrected using delta method
  load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6-001/allEBS_means/ACLIM_weekly_hind_mn.Rdata")
  load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6-001/allEBS_means/ACLIM_weekly_fut_mn.Rdata")

  # for PC
  #load("/data/ACLIM_weekly_hind_mn.Rdata")
  #load("/data/ACLIM_weekly_fut_mn.Rdata")

  # filter out bottom temp & oxygen and stick to SEBS
  
  vars <- c("temp_bottom5m", "oxygen_bottom5m")
  
  hind_var <- ACLIM_weekly_hind %>%
    filter(var %in% vars) %>%
  	filter(basin == "SEBS")
  
  proj_var <- ACLIM_weekly_fut %>%
    filter(var %in% vars) %>%
  	filter(year >= 2021) %>%
  	filter(basin == "SEBS")
  
	#### 1. Spring/Summer temps preceding survey ####
	
  presurvey <- 4:6 # (April to June)
  
  presurvey_hind_var <- hind_var %>%
  	filter(mo %in% presurvey) %>%
  	group_by(year, var) %>%
  	summarise(presurvey_mean_val = mean(mn_val))
  
  presurvey_hind_var_short <- 
  	spread(presurvey_hind_var, key = var, value = presurvey_mean_val) %>%
  	rename(presurvey_btemp = "temp_bottom5m",
  				 presurvey_boxy = "oxygen_bottom5m")
  	
  					
  # add to dataframe list
	dat_join_func <- function(x){
 
 			specimen_dat <- left_join(x, presurvey_hind_var_short, by = "year")
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
  	  
  
  dat_join_func <- function(x){
 
 			specimen_dat <- left_join(x, yr_prior_short, by = "year")
	}
 
	specimen_dat <- lapply(specimen_dat, dat_join_func)

  #### 3. temp during first year of life ####
  
 age0_func <- function(x){
		
		df <- x %>%
		mutate(year_age0 = cohort,
					 year_age0_f = as.factor(year_age0))
		
		yr_age0_dat <- df %>%
			dplyr::select(year_age0_f, year_age0) %>%
			rename(year = year_age0) %>%
			distinct()
		
		yr_age0_vars <- left_join(yr_age0_dat, yr_prior_short) %>%
			rename(age0_btemp = yrprior_btemp,
						 age0_boxy = yrprior_boxy) %>%
			dplyr::select(-year)
	
		dat <- left_join(df, yr_age0_vars)
	

	}	
	
	specimen_dat <- lapply(specimen_dat, age0_func)
	
	# change jday to julian_day to match model parameterization
	
	rename_func <- function(x){
		df <- x %>% rename(julian_day = jday)
	}
	
	specimen_dat <- lapply(specimen_dat, rename_func)
	
	names(specimen_dat) <- c("pcod_dat", "pollock_dat", "yfinsole_dat")
	
	# separate for species-specific wrangling tasks
	pcod_dat <- specimen_dat[[1]]
	pollock_dat <- specimen_dat[[2]]
	yfinsole_dat <- specimen_dat[[3]]
	
	pcod_age_sum <- pcod_dat %>%
		group_by(age_f) %>%
		summarize(n())
		
		pcod_age_sum %>%
			head()
	
	pollock_age_sum <- pollock_dat %>%
		group_by(age_f) %>%
		summarize(n()) 
	
		pollock_age_sum	%>%
			head()
		
	yfinsole_age_sum <- yfinsole_dat %>%
		group_by(age_f) %>%
		summarize(n()) 
	
		yfinsole_age_sum %>%
			head()
	

	# trim data set by largest age with >= 100 samples
	pcod_dat <- pcod_dat  %>% filter(between(age, 1, 28))

	pollock_dat <- pollock_dat  %>% filter(between(age, 1, 10))

	yfinsole_dat <- yfinsole_dat  %>% filter(between(age, 1, 20))


	
	
  
 