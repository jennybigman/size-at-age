# gams for temp and size at age for pollock, yf sole, & pcod

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

	#### PCOD ####
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	pcod_dat <- spec_temp_dat[[1]]
	
	base_mod <- bam(log_wt ~ s(sage) + t2(latitude, longitude) + s(jday),
									random = ~ (1|cohort) + (1|year/haul),
									data = pcod_dat)
	
	summary(base_mod)
  AICc(base_mod)

  # 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_age_mod <- bam(log_wt ~  s(age) + s(mean_temp) + 
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = pcod_dat)
	summary(temp_age_mod)
  AICc(temp_age_mod)
 
  # 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod <- bam(log_wt ~  s(age) + s(mean_temp) + s(mean_temp, by = age)  +
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = pcod_dat)
	
	summary(temp_age_int_mod)
  AICc(temp_age_int_mod)
 
  visreg(temp_age_int_mod, "mean_temp", "age", gg=TRUE, ylab="log_wt")
  
  #### POLLOCK ####
  
  # 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	pollock_dat <- spec_temp_dat[[2]]
	
	base_mod <- bam(log_wt ~ s(age) + t2(latitude, longitude) + s(jday),
									random = ~ (1|cohort) + (1|year/haul),
									data = pollock_dat)
	
	summary(base_mod)
  AICc(base_mod)

  # 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_age_mod <- bam(log_wt ~  s(age) + s(mean_temp) + 
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = pollock_dat)
	summary(temp_age_mod)
  AICc(temp_age_mod)
 
  # 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod <- bam(log_wt ~  s(age) + s(mean_temp) + s(mean_temp, by = age)  +
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = pollock_dat)
	
	summary(temp_age_int_mod)
  AICc(temp_age_int_mod)
 
  #### YELLOWFIN SOLE ####
  
  # 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	yellowfin_sole_dat <- spec_temp_dat[[3]]
	
	base_mod <- bam(log_wt ~ s(age) + t2(latitude, longitude) + s(jday),
									random = ~ (1|cohort) + (1|year/haul),
									data = yellowfin_sole_dat)
	
	summary(base_mod)
  AICc(base_mod)

  # 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_age_mod <- bam(log_wt ~  s(age) + s(mean_temp) + 
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = yellowfin_sole_dat)
	summary(temp_age_mod)
  AICc(temp_age_mod)
 
  # 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod <- bam(log_wt ~  s(age) + s(mean_temp) + s(mean_temp, by = age)  +
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = yellowfin_sole_dat)
	
	summary(temp_age_int_mod)
  AICc(temp_age_int_mod)
 
  