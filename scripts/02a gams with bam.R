# models with cohort effect and using bam

	#### data wrangling ####
	
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
	# stock assessment goes to 14, but after age class 10, not many indivudals per age class

	# set up random effect coding
	
	pol_yr <- unique(pollock_dat$year)
	ID <- letters[1:23]
	
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
	
	pcod_year <- unique(pcod_dat$year)
	ID <- letters[1:24]
	
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
	
	yfin_year <- unique(yfinsole_dat$year)
	ID <- letters[1:23]
	
	yfin_years_id <- data.frame(yfin_year, ID) %>%
		rename(year = yfin_year)
	
	yfinsole_dat <- merge(yfinsole_dat, yfin_years_id, by = "year")
	
	yfinsole_dat <- yfinsole_dat %>%
		mutate(haul_id = paste(ID, haul, sep = "_"))
	
	yfinsole_dat <- yfinsole_dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))

	glimpse(yfinsole_dat)


  #### models with hindast temps from my Pcod paper ####
  
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp_age_bam <- bam(log_wt ~ age_f + s(mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat)
	
	appraise(pol_temp_age_bam)
														

  # 2. weight ~ age * temp
	pol_temp_int_age_bam <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat)
														
		
	AICc(pol_temp_age_bam)
	AICc(pol_temp_int_age_bam)

	# save
  saveRDS(pol_temp_age_bam, 
  				file = here("./output/model output/pollock/pol_temp_age_bam.rds"))
  saveRDS(pol_temp_int_age_bam, 
  				file = here("./output/model output/pollock/pol_temp_int_age_bam.rds"))
 
  # load
  pol_temp_age_bam <- readRDS(here("./output/model output/pollock/pol_temp_age_bam.rds"))
  pol_temp_int_age_bam <- readRDS(here("./output/model output/pollock/pol_temp_int_age_bam.rds"))

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp_age_bam <- bam(log_wt ~ age_f + s(mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat)
														

  # 2. weight ~ age * temp
	pcod_temp_int_age_bam <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat)
														
		
	AICc(pcod_temp_age_bam)
	AICc(pcod_temp_int_age_bam)

	# save
  saveRDS(pcod_temp_age_bam, 
  				file = here("./output/model output/pollock/pcod_temp_age_bam.rds"))
  saveRDS(pcod_temp_int_age_bam, 
  				file = here("./output/model output/pollock/pcod_temp_int_age_bam.rds"))
 
  # load
  pcod_temp_age_bam <- readRDS(here("./output/model output/pollock/pcod_temp_age_bam.rds"))
  pcod_temp_int_age_bam <- readRDS(here("./output/model output/pollock/pcod_temp_int_age_bam.rds"))
	
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam <- bam(log_wt ~ age_f + s(mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat)
														

  # 2. weight ~ age * temp
	yfin_temp_int_age_bam <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat)
														
		
	AICc(yfin_temp_age_bam)
	AICc(yfin_temp_int_age_bam)

	# save
  saveRDS(yfin_temp_age_bam, 
  				file = here("./output/model output/pollock/yfin_temp_age_bam.rds"))
  saveRDS(yfin_temp_int_age_bam, 
  				file = here("./output/model output/pollock/yfin_temp_int_age_bam.rds"))
 
  # load
  yfin_temp_age_bam <- readRDS(here("./output/model output/pollock/yfin_temp_age_bam.rds"))
  yfin_temp_int_age_bam <- readRDS(here("./output/model output/pollock/yfin_temp_int_age_bam.rds"))

	
	#### plots ####
	
	# theme_set
	theme_set(
		theme_bw() +
		theme(
			panel.grid = element_blank(),
			axis.title = element_text(size = 14),
			axis.text = element_text(size = 10)))
	
	# pollock
	pol_hind_plot <- 
		visreg(pol_temp_int_age_bam,
					 "mean_sum_temp", by = "age_f",
					 gg = TRUE, partial = FALSE, 
					 rug= FALSE) +
		facet_wrap(~ age_f, ncol = 5) +
		ylab("partial effect\nlog weight-at-age") +
		xlab("mean summer temperature (April - June, ˚C)") +
		scale_x_continuous(
			breaks = c(-0.5, 0.5, 1.5),
			labels = c(-0.5, 0.5, 1.5)) + 
		theme_update(
			strip.background = element_blank(),
			strip.text = element_text(size = 10, face = "bold"))
	
	ggsave(file = here("./output/plots/pollock_hind_plot.png"),
				 pol_hind_plot,
				 width = 7, height = 5, units = "in")
	
	# pcod
	pcod_hind_plot <- 
		visreg(pcod_temp_int_age_bam,
					 "mean_sum_temp", by = "age_f",
					 gg = TRUE, partial = FALSE, 
					 rug= FALSE) +
		facet_wrap(~ age_f, ncol = 6) +
		ylab("partial effect\nlog weight-at-age") +
		xlab("mean summer temperature (April - June, ˚C)") +
		scale_x_continuous(
			breaks = c(-0.5, 0.5, 1.5),
			labels = c(-0.5, 0.5, 1.5)) + 
		theme_update(
			strip.background = element_blank(),
			strip.text = element_text(size = 10, face = "bold"))
	
	ggsave(file = here("./output/plots/pcod_hind_plot.png"),
				 pcod_hind_plot,
				 width = 7, height = 5, units = "in")
	
	# yfin sole
	yfin_hind_plot <- 
		visreg(yfin_temp_int_age_bam,
					 "mean_sum_temp", by = "age_f",
					 gg = TRUE, partial = FALSE, 
					 rug= FALSE) +
		facet_wrap(~ age_f, ncol = 5) +
		ylab("partial effect\nlog weight-at-age") +
		xlab("mean summer temperature (April - June, ˚C)") +
		scale_x_continuous(
			breaks = c(-0.5, 0.5, 1.5),
			labels = c(-0.5, 0.5, 1.5)) + 
		theme_update(
			strip.background = element_blank(),
			strip.text = element_text(size = 10, face = "bold"))
	
	ggsave(file = here("./output/plots/yfin_hind_plot.png"),
				 yfin_hind_plot,
				 width = 7, height = 5, units = "in")
	
	
	#### with ACLIM temps #####
	
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp_age_bam_ACLIM <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat)
	

  saveRDS(pol_temp_age_bam_ACLIM, 
  				file = here("./output/model output/ACLIM temps/pol_temp_age_bam_ACLIM.rds"))
  
  # 2. weight ~ age * temp
	pol_temp_int_age_bam_ACLIM <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat)
														
		
	saveRDS(pol_temp_int_age_bam_ACLIM, 
  				file = here("./output/model output/ACLIM temps/pol_temp_int_age_bam_ACLIM.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp_age_bam_ACLIM <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat)
	
	saveRDS(pcod_temp_age_bam_ACLIM, 
  				file = here("./output/model output/ACLIM temps/pcod_temp_age_bam_ACLIM.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_temp_int_age_bam_ACLIM <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat)
														
		
	saveRDS(pcod_temp_int_age_bam_ACLIM, 
  				file = here("./output/model output/ACLIM temps/pcod_temp_int_age_bam_ACLIM.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam_ACLIM <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat)
														
	saveRDS(yfin_temp_age_bam_ACLIM, 
  				file = here("./output/model output/ACLIM temps/yfin_temp_age_bam_ACLIM.rds"))
 

  # 2. weight ~ age * temp
	yfin_temp_int_age_bam_ACLIM <- bam(log_wt ~ age_f + 
																		 	s(mean_temp_sum_ACLIM, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat)
		
	saveRDS(yfin_temp_int_age_bam_ACLIM, 
  				file = here("./output/model output/ACLIM temps/yfin_temp_int_age_bam_ACLIM.rds"))
  
