# gamms with brms

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
	
	pcod_temp_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp) + t2(latitude, longitude) +
													(1|year/haul) + (1|julian_day),
													data = pcod_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	pcod_temp_int_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + t2(latitude, longitude) +
													(1|year/haul) + (1|julian_day),
													data = pcod_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, cores = 4, chains = 4)
	
	# compare to spatial smoother by year or age 
	# temp occupied -- more refined temp metric based on where fish actually are (e.g., nearshore -- mean temp < 100 m depth )
	
  # save models
	save(pcod_temp_age_gam, file = "./output/model output/pcod_temp_age_gam.rda")
	save(pcod_temp_int_age_gam, file = "./output/model output/pcod_temp_int_age_gam.rda")
	
	# load models 
	#load(file = "./output/model output/pcod_temp_age_gam.rda")
	#load(file = "./output/model output/pcod_temp_int_age_gam.rda")

	
	# compare models
	loo(pcod_temp_age_gam) #looic = -17132.5 #elpd_loo = 8566.3
	loo(pcod_temp_int_age_gam) #looic =  -18680.3 #elpd_loo = 9340.2
	
	# plot predictions
	newdata <- data.frame(
		julian_day = pcod_dat$julian_day,
		haul = pcod_dat$haul,
		year = pcod_dat$year,
		latitude = pcod_dat$latitude,
		longitude = pcod_dat$longitude,
		mean_sum_temp = pcod_dat$mean_sum_temp,
  	age_f = pcod_dat$age_f)
	
	pcod_fits <- predict(pcod_temp_int_age_gam, newdata = newdata, reformula = NA) %>% 
		as_tibble()

	pcod_fits <- bind_cols(pcod_fits, newdata)
	
	pcod_fits %>%
		ggplot() +
		geom_smooth(aes(mean_sum_temp, Estimate)) +
		facet_wrap(~ age_f)
	
	#### POLLOCK ####
	
	# data wrangling
	pollock_dat <- spec_temp_dat[[1]] %>%
		rename(julian_day = jday)
	
	pollock_dat <- pollock_dat  %>% filter(between(age_f, 1, 10))

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
	
	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	pollock_dat <- pollock_dat %>%
		filter(year %in% yrs_keep) 

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. weight ~ age + temp
	
	#get_prior(log_wt ~ age_f + mean_sum_temp + 
	#													(1|year/haul) + (1|julian_day),
	#													data = pcod_dat, family = gaussian())
	
	poll_temp_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp) + t2(latitude, longitude) +
													(1|year/haul) + (1|julian_day),
													data = pollock_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	poll_temp_int_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + t2(latitude, longitude) +
													(1|year/haul) + (1|julian_day),
													data = pollock_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, cores = 4, chains = 4)

  # save models
	save(poll_temp_age_gam, file = "./output/model output/poll_temp_age_gam.rda")
	save(poll_temp_int_age_gam, file = "./output/model output/poll_temp_int_age_gam.rda")
	
	# load models 
	load(file = "./output/model output/poll_temp_age_glm.rda")
	load(file = "./output/model output/poll_temp_int_age_glm.rda")

	
	# compare models
	loo(poll_temp_age_glm) # looic = -7730.7. elpd loo =  3865.4
	loo(poll_temp_int_age_glm) # looic = -7896.0, elpd loo =  3948.0 

	# plot predictions
	newdata <- data.frame(
		julian_day = pollock_dat$julian_day,
		haul = pollock_dat$haul,
		year = pollock_dat$year,
		mean_sum_temp = pollock_dat$mean_sum_temp,
  	age_f = pollock_dat$age_f)
	
	poll_fits <- predict(poll_temp_int_age_glm, newdata = newdata, reformula = NA) %>% 
		as_tibble()

	poll_fits <- bind_cols(poll_fits, newdata)
	
	poll_fits %>%
		ggplot() +
		geom_smooth(aes(mean_sum_temp, Estimate)) +
		facet_wrap(~ age_f)
	
	#### YELLOWFIN SOLE ####
	
	# data wrangling
	yfinsole_dat <- spec_temp_dat[[3]] %>%
		rename(julian_day = jday)
	
	yfinsole_dat <- yfinsole_dat  %>% filter(between(age_f, 1, 10))

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
	
	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	yfinsole_dat <- yfinsole_dat %>%
		filter(year %in% yrs_keep) 

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. weight ~ age + temp
	
	#get_prior(log_wt ~ age_f + mean_sum_temp + 
	#													(1|year/haul) + (1|julian_day),
	#													data = pcod_dat, family = gaussian())
	
	yfin_temp_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp) + t2(latitude, longitude) +
													(1|year/haul) + (1|julian_day),
													data = yfinsole_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	yfin_temp_int_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + t2(latitude, longitude) +
													(1|year/haul) + (1|julian_day),
													data = yfinsole_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, cores = 4, chains = 4)

  # save models
	save(yfin_temp_age_gam, file = "./output/model output/yfin_temp_age_gam.rda")
	save(yfin_temp_int_age_gam, file = "./output/model output/yfin_temp_int_age_gam.rda")
	
	# load models 
	load(file = "./output/model output/yfin_temp_age_glm.rda")
	load(file = "./output/model output/yfin_temp_int_age_glm.rda")

	
	# compare models
	loo(yfin_temp_age_glm) # looic =   ; elpdloo = 
	loo(yfin_temp_int_age_glm) # looic =   ; elpdloo = 

	# plot predictions
	newdata <- data.frame(
		julian_day = yfinsole_dat$julian_day,
		haul = yfinsole_dat$haul,
		year = yfinsole_dat$year,
		mean_sum_temp = yfinsole_dat$mean_sum_temp,
  	age_f = yfinsole_dat$age_f)
	
	yfin_fits <- predict(yfin_temp_int_age_glm, newdata = newdata, reformula = NA) %>% 
		as_tibble()

	yfin_fits <- bind_cols(yfin_fits, newdata)
	
	yfin_fits %>%
		ggplot() +
		geom_smooth(aes(mean_sum_temp, Estimate)) +
		facet_wrap(~ age_f)
	

	