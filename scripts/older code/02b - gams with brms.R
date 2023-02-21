# gamms with brms

	#### POLLOCK ####
	
	# data wrangling
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

	## trim to 1999 forward & EBS only
	#yrs_keep <- 1999:2019
	#
	#pollock_dat <- pollock_dat %>%
	#	filter(year %in% yrs_keep) 

	# 1. weight ~ age + temp
	poll_temp_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp) + t2(latitude, longitude) + s(julian_day) +
													(1|year/haul) + (1|cohort),
													data = pollock_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	poll_temp_int_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + t2(latitude, longitude) +
															s(julian_day) + (1|year/haul) + (1|cohort),
															data = pollock_dat,
															family = gaussian(),
															save_all_pars = TRUE,
                  						warmup = 1000, iter = 5000, cores = 4, chains = 4)
	

  # save models
	saveRDS(poll_temp_age_gam, file = here("./output/model output/poll_temp_age_gam.rds"))	
	saveRDS(poll_temp_int_age_gam, file = here("./output/model output/poll_temp_int_age_gam.rds"))

	# load models 
	poll_temp_age_gam_brms <- readRDS(file = "./output/model output/poll_temp_age_gam.rds")
	poll_temp_int_age_gam_brms <- readRDS(file = "./output/model output/older model output/poll_temp_int_age_gam.rds")
	
	# compare models
	loo(poll_temp_age_gam_brms, re_loo = TRUE)
	loo(poll_temp_int_age_gam_brms, re_loo = TRUE) 

	## plot ##
	poll_temp_age_int_mod_ms <- conditional_smooths(poll_temp_int_age_gam_brms)

	poll_temp_age_int_df <- poll_temp_age_int_mod_ms[[1]] %>%
		mutate(age = as.numeric(age_f),
					 age_f = fct_reorder(age_f, age)) %>%
		rename(estimate = estimate__,
					 lwr = lower__,
					 upr = upper__)
	
	poll_temp_age_int_brms_plot <- 
		ggplot(poll_temp_age_int_df) +
		geom_ribbon(aes(ymin = lwr, ymax = upr, x = mean_sum_temp), fill = "lightgrey") +
		geom_line(aes(mean_sum_temp, estimate), color = "black") +
		facet_wrap(~age_f, ncol = 5) +
		scale_x_continuous(
			name = "mean summer temperature (April - June) (˚C)",
			breaks = c(0, 1, 2),
			labels = c(0, 1, 2)
		) +
		scale_y_continuous(
			name = "partial effect on weight-at-age",
			breaks = c(-0.2, 0, 0.2),
			labels = c(-0.2, 0, 0.2)
		) +
		theme(
			strip.text = element_text(color = "black"),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA),
			axis.text=element_text(colour = "black"),
  		axis.title= element_text(color = "black"),
  		axis.line = element_line(color = "black"),
  		axis.ticks = element_line(colour = "black"),
  		panel.background = element_rect(fill = "white"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "white", color = "white"))
	
	
	ggsave(file = here("./output/plots/poll_temp_age_int_brms_plot.png"),
				 poll_temp_age_int_brms_plot,
				 width = 10, height = 5, units = "in")

	ggplot_build(poll_temp_age_int_brms_plot)$layout$panel_scales_y[[1]]$range$range
	
	#### PCOD ####
	
	# data wrangling
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
	
	pcod_dat <- pcod_dat  %>% filter(between(age, 1, 28))

	pcod_age_sum <- pcod_dat %>%
		group_by(age_f) %>%
		summarize(n())

	## trim to 1999 forward & EBS only
	#yrs_keep <- 1999:2019
#
	#pcod_dat2 <- pcod_dat %>%
	#	filter(year %in% yrs_keep) 
#
	#pcod_age_sum2 <- pcod_dat2 %>%
	#	group_by(age_f) %>%
	#	summarize(n())

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. weight ~ age + temp
	
	#get_prior(log_wt ~ age_f + mean_sum_temp + 
	#													(1|year/haul) + (1|julian_day),
	#													data = pcod_dat, family = gaussian())
	
	# 1. weight ~ age + temp
	pcod_temp_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp) + t2(latitude, longitude) + s(julian_day) +
													(1|year/haul) + (1|cohort),
													data = pcod_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	pcod_temp_int_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + t2(latitude, longitude) +
															s(julian_day) + (1|year/haul) + (1|cohort),
															data = pcod_dat,
															family = gaussian(),
															save_all_pars = TRUE,
                  						warmup = 1000, iter = 5000, cores = 4, chains = 4)

  # save models
	saveRDS(pcod_temp_age_gam, file = here("./output/model output/pcod_temp_age_gam.rds"))	
	saveRDS(pcod_temp_int_age_gam, file = here("./output/model output/pcod_temp_int_age_gam.rds"))

	# load models 
	pcod_temp_age_gam <- readRDS(file = "./output/model output/pcod_temp_age_gam.rds")
	pcod_temp_int_age_gam <- readRDS(file = "./output/model output/older model output/pcod_temp_int_age_gam.rds")
	
	# compare models
	loo(pcod_temp_age_gam) #looic = -17132.5 #elpd_loo = 8566.3
	loo(pcod_temp_int_age_gam) #looic =  -18680.3 #elpd_loo = 9340.2
	
	### plots ###
	
	pcod_temp_mod_ms <- conditional_smooths(pcod_temp_age_gam)
	pcod_temp_mod_brms_plot <- plot(pcod_temp_mod_ms)

	pcod_temp_age_mod_ms <- conditional_smooths(pcod_temp_int_age_gam)
	pcod_temp_age_mod_brms_plot <- plot(pcod_temp_age_mod_ms)

	pcod_temp_plot <- pcod_temp_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("ROMS mean summer temp (A-J)") +
		ggtitle("brms")
	
	# can also do 
	pcod_temp_age_int_df <- pcod_temp_age_mod_ms[[1]] %>%
		mutate(age = as.numeric(age_f),
					 age_f = fct_reorder(age_f, age)) %>%
		rename(estimate = estimate__,
					 lwr = lower__,
					 upr = upper__) %>%
		filter(age_f == 1:10)
	
	pcod_temp_age_int_brms_plot <- 
		ggplot(pcod_temp_age_int_df) +
		geom_ribbon(aes(ymin = lwr, ymax = upr, x = mean_sum_temp), fill = "lightgrey") +
		geom_line(aes(mean_sum_temp, estimate), color = "black") +
		facet_wrap(~age_f, ncol = 5) +
		theme(
			strip.text = element_text(color = "black"),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA),
			axis.text=element_text(colour = "black"),
  		axis.title= element_text(color = "black"),
  		axis.line = element_line(color = "black"),
  		axis.ticks = element_line(colour = "black"),
  		panel.background = element_rect(fill = "white"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "white", color = "white"))
	
	
	
############ old
	# plot predictions
	newdata <- data.frame(
		julian_day = pcod_dat$julian_day,
		haul = pcod_dat$haul,
		year = pcod_dat$year,
		latitude = pcod_dat$latitude,
		longitude = pcod_dat$longitude,
		mean_sum_temp = pcod_dat$mean_sum_temp,
  	age_f = pcod_dat$age_f)
	
	pcod_fits <- predict(pcod_temp_int_age_gam, newdata = newdata, 
											 reformula = NA, allow_new_levels = TRUE) %>% 
							 as_tibble()

	pcod_fits <- bind_cols(pcod_fits, newdata)
	
	pcod_fits %>%
		ggplot() +
		geom_smooth(aes(mean_sum_temp, Estimate)) +
		facet_wrap(~ age_f)
	
	
	#### YELLOWFIN SOLE ####
	
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
	
	yfinsole_dat <- yfinsole_dat  %>% filter(between(age, 1, 20))

	
	## trim to 1999 forward & EBS only
	#yrs_keep <- 1999:2019
	#
	#yfinsole_dat <- yfinsole_dat %>%
	#	filter(year %in% yrs_keep) 

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))

	# 1. weight ~ age + temp
	
	#get_prior(log_wt ~ age_f + mean_sum_temp + 
	#													(1|year/haul) + (1|julian_day),
	#													data = pcod_dat, family = gaussian())
	
	# 1. weight ~ age + temp
	yfin_temp_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp) + t2(latitude, longitude) + s(julian_day) +
													(1|year/haul) + (1|cohort),
													data = yfinsole_dat,
													family = gaussian(),
													save_all_pars = TRUE,
                  				warmup = 1000, iter = 5000, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	yfin_temp_int_age_gam <- brm(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + t2(latitude, longitude) +
															s(julian_day) + (1|year/haul) + (1|cohort),
															data = yfinsole_dat,
															family = gaussian(),
															save_all_pars = TRUE,
                  						warmup = 1000, iter = 5000, cores = 4, chains = 4)

  # save models
	saveRDS(yfin_temp_age_gam, file = here("./output/model output/yfin_temp_age_gam.rds"))	
	saveRDS(yfin_temp_int_age_gam, file = here("./output/model output/yfin_temp_int_age_gam.rds"))

	# load models 
	yfin_temp_age_gam <- readRDS(file = "./output/model output/yfin_temp_age_gam.rds")
	yfin_temp_int_age_gam <- readRDS(file = "./output/model output/older model output/yfin_temp_int_age_gam.rds")

	
	# compare models
	loo(yfin_temp_age_glm) # looic =   ; elpdloo = 
	loo(yfin_temp_int_age_glm) # looic =   ; elpdloo = 

	# plot predictions
	yfin_temp_age_mod_ms <- conditional_smooths(yfin_temp_int_age_gam)

	yfin_temp_age_int_df <- yfin_temp_age_mod_ms[[1]] %>%
		mutate(age = as.numeric(age_f),
					 age_f = fct_reorder(age_f, age)) %>%
		rename(estimate = estimate__,
					 lwr = lower__,
					 upr = upper__) %>%
		filter(age_f == 1:17)
	
	ggplot(yfin_temp_age_int_df) +
		geom_ribbon(aes(ymin = lwr, ymax = upr, x = mean_sum_temp), fill = "grey") +
		geom_line(aes(mean_sum_temp, estimate), color = "white") +
		facet_wrap(~age_f) +
		theme(
			strip.text = element_text(color = "white"),
			strip.background = element_blank(),
			panel.border = element_rect(color = "white", fill = NA),
			axis.text=element_text(colour = "white"),
  		axis.title= element_text(color = "white"),
  		axis.line = element_line(color = "white"),
  		axis.ticks = element_line(colour = "white"),
  		panel.background = element_rect(fill = "black"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "black", color = "black"))

	
	
	
	####################### OLD 
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
	

	