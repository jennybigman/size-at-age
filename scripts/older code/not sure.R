# gams using age as a covariate

	#### PCOD ####
	
	# data wrangling
	pcod_dat <- spec_temp_dat[[2]] %>%
		rename(julian_day = jday)
	
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
	
	pcod_dat <- pcod_dat  %>% filter(between(age, 1, 10))

	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	pcod_dat <- pcod_dat %>%
		filter(year %in% yrs_keep) 

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. log weight  ~  age + julian day + random effects of cohort +  haul nested within year
	base_mod_pcod_age <- gamm4(log_wt ~ age_f + t2(latitude, longitude) + s(julian_day),
											 random = ~ (1|cohort) + (1|year/haul),
											 data = pcod_dat)
	
  AICc(base_mod_pcod_age$mer)
 
	
  # 2. log weight  ~ age + temp + julian day + random effects of cohort +  haul nested within year
	temp_mod_pcod_age <- gamm4(log_wt ~  age_f + s(mean_sum_temp) + t2(latitude, longitude) + s(julian_day),
										random = ~ (1|cohort) + (1|year/haul),
										data = pcod_dat)
	
	AICc(temp_mod_pcod_age$mer)
	

  # 3. log weight ~  + age*temp + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_pcod_age <- gamm4(log_wt ~  s(mean_sum_temp, by = age_f) + t2(latitude, longitude) +
														s(julian_day),
													  random = ~ (1|cohort) + (1|year/haul),
														data = pcod_dat)

  AICc(temp_age_int_mod_pcod_age$mer)
 
  #save(base_mod_pcod, file = here("./output/base_mod_pcod.rds"))
  #save(temp_mod_pcod, file = here("./output/temp_mod_pcod.rds"))
  #save(temp_age_int_mod_pcod, file = here("./output/temp_age_int_mod_pcod.rds"))

	#### pollock ####

	# data wrangling 
	pollock_dat_new <- spec_temp_dat[[1]] %>%
		rename(julian_day = jday)
	
	pollock_dat_new <- pollock_dat_new %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	pollock_dat_new <- pollock_dat_new %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	pollock_age_sum <- pollock_dat_new %>%
		group_by(age_f) %>%
		summarize(n())
	
	pollock_dat_new <- pollock_dat_new  %>% filter(between(age, 1, 10))

	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	pollock_dat_new <- pollock_dat_new %>%
		filter(year %in% yrs_keep) 

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. log, scaled weight at age ~  julian day + random effects of cohort +  haul nested within year
	base_mod_pol <- gamm4(log_wt_scaled ~ t2(latitude, longitude) + s(julian_day),
										random = ~ (1|cohort) + (1|year/haul),
										data = pollock_dat_new)
	
  AICc(base_mod_pol$mer)
 
	
  # 2. log, scaled weight at age ~  temp + julian day + random effects of cohort +  haul nested within year
	temp_mod_pol <- gamm4(log_wt_scaled ~  s(mean_sum_temp) + t2(latitude, longitude) + s(julian_day),
										random = ~ (1|cohort) + (1|year/haul),
										data = pollock_dat_new)
	
	AICc(temp_mod_pol$mer)

	

  # 3. log, scaled weight at age ~  + age*temp + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_pol <- gamm4(log_wt_scaled ~  s(mean_sum_temp, by = age) + t2(latitude, longitude) +
														s(julian_day),
													  random = ~ (1|cohort) + (1|year/haul),
														data = pollock_dat_new)
	
		
  AICc(temp_age_int_mod_pol$mer)
 
 # save(base_mod_pol, file = here("./output/base_mod_pol.rds"))
 # save(temp_mod_pol, file = here("./output/temp_mod_pol.rds"))
 # save(temp_age_int_mod_pol, file = here("./output/temp_age_int_mod_pol.rds"))#
	
  ### yfin sole ####
  
  # data wrangling 
	yfinsole_dat <- spec_temp_dat[[3]] %>%
		rename(julian_day = jday)
	
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
	
	yfinsole_dat <- yfinsole_dat  %>% filter(between(age, 1, 20)) 

	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	yfinsole_dat <- yfinsole_dat %>%
		filter(year %in% yrs_keep) 

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. log, scaled weight at age ~  julian day + random effects of cohort +  haul nested within year
	base_mod_yfin <- gamm4(log_wt_scaled ~ t2(latitude, longitude) + s(julian_day),
										random = ~ (1|cohort) + (1|year/haul),
										data = yfinsole_dat)
	
	
  AICc(base_mod_yfin$mer)
 
	
  # 2. log, scaled weight at age ~  temp + julian day + random effects of cohort +  haul nested within year
	temp_mod_yfin <- gamm4(log_wt_scaled ~  s(mean_sum_temp) + t2(latitude, longitude) + s(julian_day),
										random = ~ (1|cohort) + (1|year/haul),
										data = yfinsole_dat)
	
	AICc(temp_mod_yfin$mer)


  # 3. log, scaled weight at age ~  + age*temp + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_yfin <- gamm4(log_wt_scaled ~  s(mean_sum_temp, by = age) + t2(latitude, longitude) +
														s(julian_day),
													  random = ~ (1|cohort) + (1|year/haul),
														data = yfinsole_dat)
	AICc(temp_age_int_mod_yfin$mer)
