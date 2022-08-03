	library(here)
	library(tidyverse)
	library(lme4)
	library(readr)
	library(lubridate)
	library(tidymv)
	library(mgcv)
	library(gamm4)
	library(cAIC4)
	library(visreg)
	library(mgcViz)
	library(lme4)
	library(MuMIn)

	#### data wrangling ####
	
	# read data 
	pollock_dat <- read_csv("https://raw.githubusercontent.com/mikelitzow/bold-new-pollock/master/data/survey%20data/Litzow_pollock_02032021.csv")

	# SST dat
	clim_dat <- read_csv(file = here("./data/climate data.csv"))
	
	# change all column names to lower
	names(pollock_dat) <- tolower(names(pollock_dat))
	
	# format dates
	pollock_dat$date <- mdy(pollock_dat$tow_date)
	pollock_dat$month <- month(pollock_dat$date) # month of year
	pollock_dat$week <- week(pollock_dat$date) # week of year
	pollock_dat$year <- year(pollock_dat$date)
	pollock_dat$julian_day <- yday(pollock_dat$date)
	
	# calculate cohort
	pollock_dat <- pollock_dat %>%
		mutate(cohort = year - age)
	
	# turn cohort and age into factors for the model and log variables
	pollock_dat <- pollock_dat %>%
		filter(weight > 0) %>%
		mutate(age_f = as.factor(age),
					 cohort_f = as.factor(cohort),
					 log_wt = log10(weight))
	
	# scale weights for each age
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	pollock_dat <- pollock_dat %>%
		filter(year %in% yrs_keep) 
	
	# SST
	clim_dat <- clim_dat %>%
		filter(year %in% yrs_keep) %>%
		select(year, south.sst.amj)
	
	# join SST and data together
	pollock_dat <- merge(pollock_dat, clim_dat, by = "year")
	
	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	#### models ####
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	base_mod <- gamm4(log_wt_scaled ~ s(age) + t2(latitude, longitude) + s(julian_day),
										random = ~ (1|cohort) + (1|year/haul),
										data = pollock_dat)
	
	# with bam()
	base_mod <- bam(log_wt_scaled ~ s(age) + t2(latitude, longitude) + s(julian_day),
									random = ~ (1|cohort) + (1|year/haul),
									data = pollock_dat)
	
	summary(base_mod)
	summary(base_mod$mer)	
  gam.check(base_mod$gam)	
  AICc(base_mod)
  
  # in brms
  base_mod_brms <- brm(bf(log_wt_scaled ~ s(age) + t2(latitude, longitude) + s(julian_day) +
												 (1|cohort) + (1|year/haul)),
												 data = pollock_dat, family = gaussian(), cores = 4, 
  											 iter = 5000, warmup = 1000, thin = 10)

  
  # 2. weight ~ age + julian day + temp + random effects of cohort +  haul nested within year
	temp_mod <- gamm4(log_wt ~ s(age) + s(south.sst.amj) + t2(latitude, longitude) + s(julian_day),
										random = ~ (1|cohort) + (1|year/haul),
										data = pollock_dat)
	
	summary(temp_mod$gam)
	summary(temp_mod$mer)	
  gam.check(temp_mod$gam)	
  AICc(temp_mod$gam)
  
  temp_mod_brms <- brm(bf(log_wt_scaled ~ s(age) + s(south.sst.amj) +
  												t2(latitude, longitude) + s(julian_day) +
													(1|cohort) + (1|year/haul)),
													data = pollock_dat, family = gaussian(), cores = 4, 
 													iter = 5000, warmup = 1000, thin = 10)
  
  # 2. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod <- gamm4(log_wt ~  s(age) + s(south.sst.amj) + s(south.sst.amj, by = age), # +
										#t2(latitude, longitude) + s(julian_day),
										#random = ~ (1|cohort) + (1|year/haul),
										data = pollock_dat)
	
	summary(temp_age_int_mod$gam)
	summary(temp_age_int_mod$mer)	
  gam.check(temp_age_int_mod$gam)	
  AICc(temp_age_int_mod)
  
 temp_age_int_mod_brms <- brm(bf(log_wt_scaled ~ s(age) + s(south.sst.amj) + 
 																 s(age, south.sst.amj, bs = "fs") + 
  												t2(latitude, longitude) + s(julian_day) +
													(1|cohort) + (1|year/haul)),
													data = pollock_dat, family = gaussian(), cores = 4, 
 													iter = 5000, warmup = 1000, thin = 10)
  