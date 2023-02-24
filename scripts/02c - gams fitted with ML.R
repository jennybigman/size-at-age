# gams with ML 

	#### SEBS presurvey temps ####

	#1. weight ~ age + temp
	pol_temp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")

  saveRDS(pol_temp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp_age_bam_ACLIM_SEBS_ML.rds"))
  
  # 2. weight ~ age * temp
	pol_temp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")
		
	saveRDS(pol_temp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp_int_age_bam_ACLIM_SEBS_ML.rds"))
  

	# pcod # 
	
	1. weight ~ age + temp
	pcod_temp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML")
	
	saveRDS(pcod_temp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_temp_age_bam_ACLIM_SEBS_ML.rds"))
 
	 # 2. weight ~ age * temp
	pcod_temp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML", nthreads = 8)
		
	saveRDS(pcod_temp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_temp_int_age_bam_ACLIM_SEBS_ML.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "ML")
														
	saveRDS(yfin_temp_age_bam_ACLIM_SEBS_ML, 
  #				file = here("./output/model output/ACLIM temps/ML output/yfin_temp_age_bam_ACLIM_SEBS_ML.rds"))
 

  # 2. weight ~ age * temp 
	yfin_temp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + 
																		 	s(presurvey_mean_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													discrete=TRUE, nthreads=4,
													method = "ML")
		
	saveRDS(yfin_temp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_temp_int_age_bam_ACLIM_SEBS_ML.rds"))

	#### temp year prior ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_yrtemp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")
	
  saveRDS(pol_yrtemp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
  
  # 2. weight ~ age * temp
	pol_yrtemp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(mean_yr_temp, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")
		
	saveRDS(pol_yrtemp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	pcod_yrtemp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML")

		saveRDS(pcod_yrtemp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(mean_yr_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML")
		
	saveRDS(pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  
 
	# yfin sole # 
	
	# 1. weight ~ age + temp 
	yfin_yrtemp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													nthreads=8,
													method = "ML")
														
	saveRDS(yfin_yrtemp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
 

  # 2. weight ~ age * temp
	yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + 
																		 	s(mean_yr_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													nthreads=8, 
													method = "ML")

	saveRDS(yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))

	#### temp during age1 ####
	
	# pollock #	
  
	1. weight ~ age + temp
	pol_temp1_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_firstyr) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													nthreads = 8, 
													method = "ML")

  saveRDS(pol_temp1_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp1_age_bam_ACLIM_SEBS_ML.rds"))
  
  # 2. weight ~ age * temp 
	pol_temp1_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_firstyr, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, 
													nthreads=8, 
													method = "ML")
														
		
	saveRDS(pol_temp1_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp1_int_age_bam_ACLIM_SEBS_ML.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	#pcod_temp1_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_firstyr) + s(julian_day) + 
	#												te(latitude, longitude) + 
	#												s(ID_f, bs = "re") + # haul in year random effect
	#							 					s(haul_id_f, bs = "re") + # haul in year random effect
	#							 					s(cohort_f, bs = "re"),
	#												data = pcod_dat,			
	#												nthreads=8, 
	#												method = "ML")
	#
	#saveRDS(pcod_temp1_age_bam_ACLIM_SEBS_ML, 
  #				file = here("./output/model output/ACLIM temps/ML output/pcod_temp1_age_bam_ACLIM_SEBS_ML.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_temp1_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_firstyr, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, 			
													nthreads=8, 
													method = "ML")
														
		
	saveRDS(pcod_temp1_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_temp1_int_age_bam_ACLIM_SEBS_ML.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	#yfin_temp1_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_firstyr) + s(julian_day) + 
	#												te(latitude, longitude) + 
	#												s(ID_f, bs = "re") + # haul in year random effect
	#							 					s(haul_id_f, bs = "re") + # haul in year random effect
	#							 					s(cohort_f, bs = "re"),
	#												data = yfinsole_dat,
	#												nthreads=8, 
	#												method = "ML")
	#													
	#saveRDS(yfin_temp1_age_bam_ACLIM_SEBS_ML, 
  #				file = here("./output/model output/ACLIM temps/ML output/yfin_temp1_age_bam_ACLIM_SEBS_ML.rds"))
 

  # 2. weight ~ age * temp
	yfin_temp1_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_firstyr, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat, 			
													nthreads=8, 
													method = "ML")
		
	saveRDS(yfin_temp1_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_temp1_int_age_bam_ACLIM_SEBS_ML.rds"))

	#### temp during age 0 ####
	
	# pollock
	pol_temp0_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, 
													nthreads = 8, 
													method = "ML")
														
		
	saveRDS(pol_temp0_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp0_int_age_bam_ACLIM_SEBS_ML.rds"))
 
	#pcod
	
	pcod_temp0_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, 
													nthreads = 8, 
													method = "ML")
														
		
	saveRDS(pcod_temp0_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_temp0_int_age_bam_ACLIM_SEBS_ML.rds"))
 
	 
	# yfin 
	
	yfin_temp0_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat, 
													nthreads = 8, 
													method = "ML")
														
		
	saveRDS(yfin_temp0_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_temp0_int_age_bam_ACLIM_SEBS_ML.rds"))
 
	 
	#### temp to maturity ####
	
		#### temp during age 0 ####
	
	# pollock
	pol_temp_mat_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_mat, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, 
													nthreads = 8, 
													method = "ML")
														
		
	saveRDS(pol_temp_mat_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp_mat_int_age_bam_ACLIM_SEBS_ML.rds"))
 
	#pcod
	
	pcod_temp_mat_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_mat, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, 
													nthreads = 8, 
													method = "ML")
														
		
	saveRDS(pcod_temp_mat_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_temp_mat_int_age_bam_ACLIM_SEBS_ML.rds"))
 
	 
	# yfin 
	
	yfin_temp_mat_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(temp_mat, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat, 
													nthreads = 8, 
													method = "ML")
														
		
	saveRDS(yfin_temp_mat_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_temp_mat_int_age_bam_ACLIM_SEBS_ML.rds"))
 
	 