# models with cohort effect and using bam


  #### models with summer (April - June avgs) hindcast temps bias-corrected at monthly, domain level ####
  
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
													data = pollock_dat, nthreads = 6)
														
		
	AICc(pol_temp_age_bam)
	AICc(pol_temp_int_age_bam)

	# save
  saveRDS(pol_temp_age_bam, 
  				file = here("./output/model output/ROMS temps from Pcod paper/pol_temp_age_bam.rds"))
  saveRDS(pol_temp_int_age_bam, 
  				file = here("./output/model output/ROMS temps from Pcod paper/pol_temp_int_age_bam.rds"))
 
  # load
  pol_temp_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pol_temp_age_bam.rds"))
  pol_temp_int_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pol_temp_int_age_bam.rds"))

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
													data = pcod_dat,
													nthreads = 6)
														
		
	AICc(pcod_temp_age_bam)
	AICc(pcod_temp_int_age_bam)

	# save
  saveRDS(pcod_temp_age_bam, 
  				file = here("./output/model output/ROMS temps from Pcod paper/pcod_temp_age_bam.rds"))
  saveRDS(pcod_temp_int_age_bam, 
  				file = here("./output/model output/ROMS temps from Pcod paper/pcod_temp_int_age_bam.rds"))
 
  # load
  pcod_temp_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pcod_temp_age_bam.rds"))
  pcod_temp_int_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pcod_temp_int_age_bam.rds"))
	
 
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
  				file = here("./output/model output/ROMS temps from Pcod paper/yfin_temp_age_bam.rds"))
  saveRDS(yfin_temp_int_age_bam, 
  				file = here("./output/model output/ROMS temps from Pcod paper/yfin_temp_int_age_bam.rds"))
 
  # load
  yfin_temp_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/yfin_temp_age_bam.rds"))
  yfin_temp_int_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/yfin_temp_int_age_bam.rds"))

	
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
	
	
	#### with ACLIM summer (April - June) temps #####
	
	## both NEBS and SEBS averaged ####
	
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
  
	
	## just SEBS temps ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat)
	

  saveRDS(pol_temp_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pol_temp_age_bam_ACLIM_SEBS.rds"))
  
  # 2. weight ~ age * temp
	pol_temp_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, nthreads = 6)
														
		
	saveRDS(pol_temp_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pol_temp_int_age_bam_ACLIM_SEBS.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat)
	
	saveRDS(pcod_temp_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pcod_temp_age_bam_ACLIM_SEBS.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_temp_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat)
														
		
	saveRDS(pcod_temp_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pcod_temp_int_age_bam_ACLIM_SEBS.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat)
														
	saveRDS(yfin_temp_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/yfin_temp_age_bam_ACLIM_SEBS.rds"))
 

  # 2. weight ~ age * temp
	yfin_temp_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + 
																		 	s(SEBS_mean_sum_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat, nthreads = 8)
		
	saveRDS(yfin_temp_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/yfin_temp_int_age_bam_ACLIM_SEBS.rds"))


	#### yearly temps ACLIM ####
	
		# pollock #	
  
	#1. weight ~ age + temp
	pol_yrtemp_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat)
	

  saveRDS(pol_yrtemp_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pol_yrtemp_age_bam_ACLIM_SEBS.rds"))
  
  # 2. weight ~ age * temp #### NEXT
	pol_yrtemp_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, nthreads = 8)
														
		
	saveRDS(pol_yrtemp_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pol_yrtemp_int_age_bam_ACLIM_SEBS.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	pcod_yrtemp_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat)
	
	saveRDS(pcod_yrtemp_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pcod_yrtemp_age_bam_ACLIM_SEBS.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_yrtemp_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, nthreads = 8)
														
		
	saveRDS(pcod_yrtemp_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pcod_yrtemp_int_age_bam_ACLIM_SEBS.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_yrtemp_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat)
														
	saveRDS(yfin_yrtemp_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/yfin_yrtemp_age_bam_ACLIM_SEBS.rds"))
 

  # 2. weight ~ age * temp
	yfin_yrtemp_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + 
																		 	s(SEBS_mean_yr_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat, nthreads = 8)
		
	saveRDS(yfin_yrtemp_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/yfin_yrtemp_int_age_bam_ACLIM_SEBS.rds"))

	
	#### temp during first year of life ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp1_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(temp_firstyr) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, nthreads = 8)

  saveRDS(pol_temp1_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pol_temp1_age_bam_ACLIM_SEBS.rds"))
  
  # 2. weight ~ age * temp
	pol_temp1_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(temp_firstyr, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, nthreads = 8)
														
		
	saveRDS(pol_temp1_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pol_temp1_int_age_bam_ACLIM_SEBS.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp1_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(temp_firstyr) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, nthreads = 8)
	
	saveRDS(pcod_temp1_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pcod_temp1_age_bam_ACLIM_SEBS.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_temp1_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(temp_firstyr, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, nthreads = 8)
														
		
	saveRDS(pcod_temp1_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/pcod_temp1_int_age_bam_ACLIM_SEBS.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp1_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(temp_firstyr) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat, nthreads = 8)
														
	saveRDS(yfin_temp1_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/yfin_temp1_age_bam_ACLIM_SEBS.rds"))
 

  # 2. weight ~ age * temp
	yfin_temp1_int_age_bam_ACLIM_SEBS <- bam(log_wt ~ age_f + s(temp_firstyr, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat, nthreads = 8)
		
	saveRDS(yfin_temp1_int_age_bam_ACLIM_SEBS, 
  				file = here("./output/model output/ACLIM temps/yfin_temp1_int_age_bam_ACLIM_SEBS.rds"))
