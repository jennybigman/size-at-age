# gams with smoothed age and temp effects 

	#### presurvey temp ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_pretemp_age_bam <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + 
														 	s(julian_day) + te(latitude, longitude) + 
											  			s(ID_f, bs = "re") + # haul in year random effect
								 			  			s(haul_id_f, bs = "re") + # haul in year random effect
								 			  			s(cohort_f, bs = "re"),
											  			data = pollock_dat, nthreads = 8)
	
  #saveRDS(pol_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_pretemp_age_bam.rds"))
  
  saveRDS(pol_pretemp_age_bam, 
  				file = here("./output/model output PC/pol_pretemp_age_bam.rds"))
 
  # 2. weight ~ age * temp
	pol_pretemp_int_age_bam <- bam(log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + 
																 	s(jday) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pollock_dat, nthreads = 8)
	saveRDS(pol_pretemp_int_age_bam, 
	 				file = here("./output/model output/output Feb 2023/pol_pretemp_int_age_bam.rds"))
 
	# pcod #
	
	#1. weight ~ age + temp
	pcod_pretemp_age_bam <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + 
																s(julian_day) + te(latitude, longitude) + 
																s(ID_f, bs = "re") + # haul in year random effect
								 								s(haul_id_f, bs = "re") + # haul in year random effect
								 								s(cohort_f, bs = "re"),
																data = pcod_dat, nthreads = 8)
	
	#saveRDS(pcod_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pcod_pretemp_age_bam.rds"))
 
	saveRDS(pcod_pretemp_age_bam, 
	 				file = here("./output/model output PC/pcod_pretemp_age_bam.rds"))
 
  # 2. weight ~ age * temp
	
	keepl <- c("3", "4", "5", "6", "7", "8", "9", "10")
	
	pcod_dat_trim <- pcod_dat %>%
		filter(age_f %in% keepl)
	
	pcod_dat_trim$age_f <- droplevels(pcod_dat_trim$age_f)

	pcod_pretemp_int_age_bam <- bam(log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + 
															 	   s(jday) + te(latitude, longitude) + 
																   s(ID_f, bs = "re") + # haul in year random effect
								 								   s(haul_id_f, bs = "re") + # haul in year random effect
								 								   s(cohort_f, bs = "re"),
																   data = pcod_dat_trim, nthreads = 8)
														
  saveRDS(pcod_pretemp_int_age_bam, 
  				file = here("./output/model output/output Feb 2023/pcod_pretemp_int_age_bam.rds"))

  pcod_pretemp_int_age_bam <- readRDS(file = here("./output/model output/output Feb 2023/pcod_pretemp_int_age_bam.rds"))
 	
  ### with temp0
  pcod_temp0_int_age_bam <- bam(log_wt_std ~ age_f + s(age0_btemp, by = age_f) + 
															 	   s(jday) + te(latitude, longitude) + 
																   s(ID_f, bs = "re") + # haul in year random effect
								 								   s(haul_id_f, bs = "re") + # haul in year random effect
								 								   s(cohort_f, bs = "re"),
																   data = pcod_dat_trim, nthreads = 8)
														
  saveRDS(pcod_temp0_int_age_bam, 
  				file = here("./output/model output/output Feb 2023/pcod_temp0_int_age_bam.rds"))

  # yfin sole #
	
	# 1. weight ~ age + temp
	yfin_pretemp_age_bam <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + 
																s(julian_day) + te(latitude, longitude) + 
																s(ID_f, bs = "re") + # haul in year random effect
								 								s(haul_id_f, bs = "re") + # haul in year random effect
								 								s(cohort_f, bs = "re"),
																data = yfinsole_dat, nthreads = 8)
														
	#saveRDS(yfin_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_pretemp_age_bam.rds"))
 
  saveRDS(yfin_pretemp_age_bam, 
  				file = here("./output/model output PC/yfin_pretemp_age_bam.rds"))

  # 2. weight ~ age * temp
  
  keepl <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
	
	yfin_dat_trim <- yfinsole_dat %>%
		filter(age_f %in% keepl)
	
	yfin_dat_trim$age_f <- droplevels(yfin_dat_trim$age_f)

	yfin_pretemp_int_age_bam <- bam(log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + 
																		s(jday) + te(latitude, longitude) + 
																		s(ID_f, bs = "re") + # haul in year random effect
								 										s(haul_id_f, bs = "re") + # haul in year random effect
								 										s(cohort_f, bs = "re"),
																		data = yfin_dat_trim, nthreads = 8)
		
	#saveRDS(yfin_pretemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_pretemp_int_age_bam.rds"))

  saveRDS(yfin_pretemp_int_age_bam, 
  				file = here("./output/model output/output Feb 2023/yfin_pretemp_int_age_bam.rds"))

  
  #### year prior temps  ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_yrtemp_age_bam <- bam(log_wt_std ~ age_f + s(presurvey_btemp) + 
															s(jday) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = pollock_dat)
	
	#saveRDS(pol_yrtemp_age_bam, 
	#				file = here("./output/model output/ACLIM temps/pol_yrtemp_age_bam_ACLIM_SEBS.rds"))
  
	saveRDS(pol_yrtemp_age_bam, file = here("./output/model output PC/pol_pretemp_age_bam.rds"))

  # 2. weight ~ age * temp #### NEXT
	pol_yrtemp_int_age_bam <- bam(log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + 
																	s(jday) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pollock_dat, nthreads = 8)
														
		
	#saveRDS(pol_yrtemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_yrtemp_int_age_bam.rds"))
  
  saveRDS(pol_yrtemp_int_age_bam, 
  				file = here("./output/model output PC/ppol_yrtemp_int_age_bam.rds"))

  # pcod #
	
	#1. weight ~ age + temp
	pcod_yrtemp_age_bam <- bam(log_wt ~ age_f + s(mean_yr_temp) +	
														 	s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = pcod_dat)
	
	#saveRDS(pcod_yrtemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pcod_yrtemp_age_bam.rds"))
 
  saveRDS(pcod_yrtemp_age_bam, 
  				file = here("./output/model output PC/pcod_yrtemp_age_bam.rds"))

  # 2. weight ~ age * temp
	pcod_yrtemp_int_age_bam <- bam(log_wt ~ age_f + s(mean_yr_temp, by = age_f) + 
																 	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pcod_dat, nthreads = 8)
														
	#saveRDS(pcod_yrtemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pcod_yrtemp_int_age_bam.rds"))
  
  saveRDS(pcod_yrtemp_int_age_bam, 
  				file = here("./output/model output PC/pcod_yrtemp_int_age_bam.rds"))

  # yfin sole #
	
	# 1. weight ~ age + temp
	yfin_yrtemp_age_bam <- bam(log_wt ~ age_f + s(mean_yr_temp) + 
														 	s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = yfinsole_dat)
														
	#saveRDS(yfin_yrtemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_yrtemp_age_bam.rds"))
 
  saveRDS(yfin_yrtemp_age_bam, 
  				file = here("./output/model output PC/yfin_yrtemp_age_bam.rds"))

  # 2. weight ~ age * temp
	yfin_yrtemp_int_age_bam <- bam(log_wt ~ age_f + s(mean_yr_temp, by = age_f) + 
																 	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = yfinsole_dat, nthreads = 8)
		
	#saveRDS(yfin_yrtemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_yrtemp_int_age_bam.rds"))

  saveRDS(yfin_yrtemp_int_age_bam, 
  				file = here("./output/model output PC/yfin_yrtemp_int_age_bam.rds"))

  
  #### temp during first year of life ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp0_age_bam <- bam(log_wt ~ age_f + s(temp_age0) + 
													 	s(julian_day) + te(latitude, longitude) + 
														s(ID_f, bs = "re") + # haul in year random effect
								 						s(haul_id_f, bs = "re") + # haul in year random effect
								 						s(cohort_f, bs = "re"),
														data = pollock_dat, nthreads = 8)

  #saveRDS(pol_temp0_age_bam, file = here("./output/model output/ACLIM temps/pol_temp0_age_bam.rds"))
  
  saveRDS(pol_temp0_age_bam, file = here("./output/model output PC/pol_temp0_age_bam.rds"))

  # 2. weight ~ age * temp
	pol_temp0_int_age_bam <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
															 	s(julian_day) + te(latitude, longitude) + 
																s(ID_f, bs = "re") + # haul in year random effect
								 								s(haul_id_f, bs = "re") + # haul in year random effect
								 								s(cohort_f, bs = "re"),
																data = pollock_dat, nthreads = 8)
														
	#saveRDS(pol_temp0_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_temp0_int_age_bam.rds"))
  
  saveRDS(pol_temp0_int_age_bam, 
  				file = here("./output/model output PC/pol_temp0_int_age_bam.rds"))

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp0_age_bam <- bam(log_wt ~ age_f + s(temp_age0) + 
															s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = pcod_dat, nthreads = 8)
	
	#saveRDS(pcod_temp0_age_bam, file = here("./output/model output/ACLIM temps/pcod_temp0_age_bam.rds"))
 
	saveRDS(pcod_temp0_age_bam, file = here("./output/model output PC/pcod_temp0_age_bam.rds"))
												
  # 2. weight ~ age * temp
	pcod_temp0_int_age_bam <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
																	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pcod_dat, nthreads = 8)
														
		
	#saveRDS(pcod_temp0_int_age_bam, 
	#				file = here("./output/model output/ACLIM temps/pcod_temp0_int_age_bam.rds"))
  
	saveRDS(pol_pretemp_age_bam, file = here("./output/model output PC/pcod_temp0_int_age_bam.rds"))

	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp0_age_bam <- bam(log_wt ~ age_f + s(temp_age0) + 
															s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = yfinsole_dat, nthreads = 8)
														
	#saveRDS(yfin_temp0_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_temp0_age_bam.rds"))
 
  saveRDS(yfin_temp0_age_bam, file = here("./output/model output PC/yfin_temp0_age_bam.rds"))

  # 2. weight ~ age * temp
	yfin_temp0_int_age_bam <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
																	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = yfinsole_dat, nthreads = 8)
		
	#saveRDS(yfin_temp0_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_temp0_int_age_bam.rds"))

	saveRDS(yfin_temp0_int_age_bam, file = here("./output/model output PC/yfin_temp0_int_age_bam.rds"))
