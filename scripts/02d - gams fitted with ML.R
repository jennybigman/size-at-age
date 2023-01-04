# gams with ML 

	#### SEBS summer temps ####

	#1. weight ~ age + temp
	pol_temp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")

  saveRDS(pol_temp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp_age_bam_ACLIM_SEBS_ML.rds"))
  
  # 2. weight ~ age * temp
	pol_temp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")
		
	saveRDS(pol_temp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_temp_int_age_bam_ACLIM_SEBS_ML.rds"))
  

	# pcod # ##### START HERE #####
	
	#1. weight ~ age + temp
	pcod_temp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML")
	
	saveRDS(pcod_temp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_temp_age_bam_ACLIM_SEBS_ML.rds"))
 
	 # 2. weight ~ age * temp
	pcod_temp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML")
		
	saveRDS(pcod_temp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/GCV output/pcod_temp_int_age_bam_ACLIM_SEBS_ML.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "ML")
														
	saveRDS(yfin_temp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_temp_age_bam_ACLIM_SEBS_ML.rds"))
 

  # 2. weight ~ age * temp
	yfin_temp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + 
																		 	s(SEBS_mean_sum_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "ML")
		
	saveRDS(yfin_temp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_temp_int_age_bam_ACLIM_SEBS_ML.rds"))

	#### ACLIM temps SEBS only year means ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_yrtemp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")
	
  saveRDS(pol_yrtemp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pol_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
  
  # 2. weight ~ age * temp
	pol_yrtemp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp, by = age_f) + 
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
	pcod_yrtemp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML")

		saveRDS(pcod_yrtemp_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/pcod_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp, by = age_f) + s(julian_day) + 
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
	yfin_yrtemp_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "ML")
														
	saveRDS(yfin_yrtemp_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
 

  # 2. weight ~ age * temp
	yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML <- bam(log_wt ~ age_f + 
																		 	s(SEBS_mean_yr_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "ML")
		
	saveRDS(yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML, 
  				file = here("./output/model output/ACLIM temps/ML output/yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
