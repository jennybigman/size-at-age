# linear models of size at age

	#### PCOD ####
	
	# data wrangling
	pcod_dat <- spec_temp_dat[[2]] %>%
		rename(julian_day = jday)
	
	pcod_dat <- pcod_dat  %>% filter(between(age_f, 1, 10))

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
	
	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	pcod_dat <- pcod_dat %>%
		filter(year %in% yrs_keep) 

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. weight ~ age + temp
	
	#get_prior(log_wt ~ age_f + mean_sum_temp + 
	#													(1|year/haul) + (1|julian_day),
	#													data = pcod_dat, family = gaussian())
	
	pcod_temp_age_glm <- brm(log_wt ~ age_f + mean_sum_temp + 
													(1|year/haul) + (1|julian_day),
													data = pcod_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	pcod_temp_int_age_glm <- brm(log_wt ~ age_f * mean_sum_temp + 
													(1|year/haul) + (1|julian_day),
													data = pcod_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, cores = 4, chains = 4)
	
  # save models
	save(pcod_temp_age_glm, file = "./output/model output/pcod_temp_age_glm.rda")
	save(pcod_temp_int_age_glm, file = "./output/model output/pcod_temp_int_age_glm.rda")
	
	# compare models
	loo(pcod_temp_age_glm) #looic = -16694.7 #elpd_loo = 8347.4
	loo(pcod_temp_int_age_glm) #looic = -17565.6 #elpd_loo = 8782.8
	
	# plot predictions
	newdata <- data.frame(
		julian_day = pcod_dat$julian_day,
		haul = pcod_dat$haul,
		year = pcod_dat$year,
		mean_sum_temp = pcod_dat$mean_sum_temp,
  	age_f = pcod_dat$age_f)
	
	pcod_fits <- predict(pcod_temp_int_age_glm, newdata = newdata, reformula = NA) %>% 
		as_tibble()

	pcod_fits <- merge(pcod_fits, newdata)
	
	pcod_fits %>%
		ggplot(aes(mean_sum_temp, Estimate)) +
		#geom_smooth_ci() +
		facet_wrap(~ age_f)
	

	