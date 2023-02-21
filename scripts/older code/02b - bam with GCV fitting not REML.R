# models with GCV fitting and not REML

 #### models with ROMS temps from Pcod paper ####
  
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML")
	
 saveRDS(pol_temp_age_bam_gcv, 
  				file = here("./output/model output/ROMS temps from Pcod paper/GCV output/pol_temp_age_bam_gcv.rds"))
 														

  # 2. weight ~ age * temp
	pol_temp_int_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")
														
		
 saveRDS(pol_temp_int_age_bam_gcv, 
  				file = here("./output/model output/ROMS temps from Pcod paper/GCV output/pol_temp_int_age_bam_gcv.rds"))
 
  # load
  pol_temp_age_bam_gcv <- readRDS(here("./output/model output/ROMS temps from Pcod paper/GCV output/pol_temp_age_bam_gcv.rds"))
  pol_temp_int_age_bam_gcv <- readRDS(here("./output/model output/ROMS temps from Pcod paper/GCV output/pol_temp_int_age_bam_gcv.rds"))

  AIC(pol_temp_age_bam_gcv)
  AIC(pol_temp_int_age_bam_gcv)
  
	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
														
  saveRDS(pcod_temp_age_bam_gcv, 
  				file = here("./output/model output/ROMS temps from Pcod paper/GCV output/pcod_temp_age_bam_gcv.rds"))
  

  # 2. weight ~ age * temp
	pcod_temp_int_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
														
  saveRDS(pcod_temp_int_age_bam_gcv, 
  				file = here("./output/model output/ROMS temps from Pcod paper/GCV output/pcod_temp_int_age_bam_gcv.rds"))
 
  # load
 # pcod_temp_age_bam_gcv <- readRDS(here("./output/model output/ROMS temps from Pcod paper/GCV output/pcod_temp_age_bam_gcv.rds"))
 # pcod_temp_int_age_bam_gcv <- readRDS(here("./output/model output/ROMS temps from Pcod paper/GCV output/pcod_temp_int_age_bam_gcv.rds"))
	
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
														
	saveRDS(yfin_temp_age_bam_gcv, 
  				file = here("./output/model output/ROMS temps from Pcod paper/GCV output/yfin_temp_age_bam_gcv.rds"))
	
  # 2. weight ~ age * temp
	yfin_temp_int_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
														
  saveRDS(yfin_temp_int_age_bam_gcv, 
  				file = here("./output/model output/ROMS temps from Pcod paper/GCV output/yfin_temp_int_age_bam_gcv.rds"))
 
  # load
  yfin_temp_age_bam_gcv <- readRDS(here("./output/model output/ROMS temps from Pcod paper/GCV output/yfin_temp_age_bam_gcv.rds"))
  yfin_temp_int_age_bam_gcv <- readRDS(here("./output/model output/ROMS temps from Pcod paper/GCV output/yfin_temp_int_age_bam_gcv.rds"))

  #### with ACLIM temps ####
  
  # summer temps ####
  
  # SEBS and NEBS avg ####
  
  # pollock #	
  
	#1. weight ~ age + temp
	pol_temp_age_bam_ACLIM_gcv <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")

  saveRDS(pol_temp_age_bam_ACLIM_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pol_temp_age_bam_ACLIM_gcv.rds"))
  
  # 2. weight ~ age * temp
	pol_temp_int_age_bam_ACLIM_gcv <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")

	saveRDS(pol_temp_int_age_bam_ACLIM_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pol_temp_int_age_bam_ACLIM_gcv.rds"))
  

	# pcod #

	#1. weight ~ age + temp
	pcod_temp_age_bam_ACLIM_gcv <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
	
	saveRDS(pcod_temp_age_bam_ACLIM_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pcod_temp_age_bam_ACLIM_gcv.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_temp_int_age_bam_ACLIM_gcv <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
														
		
	saveRDS(pcod_temp_int_age_bam_ACLIM_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pcod_temp_int_age_bam_ACLIM_gcv.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam_ACLIM_gcv <- bam(log_wt ~ age_f + s(mean_temp_sum_ACLIM) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
														
	saveRDS(yfin_temp_age_bam_ACLIM_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/yfin_temp_age_bam_ACLIM_gcv.rds"))
 

  # 2. weight ~ age * temp
	yfin_temp_int_age_bam_ACLIM_gcv <- bam(log_wt ~ age_f + 
																		 	s(mean_temp_sum_ACLIM, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
		
	saveRDS(yfin_temp_int_age_bam_ACLIM_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/yfin_temp_int_age_bam_ACLIM_gcv.rds"))
  
	#### with ACLIM summer temps for just SEBS 
	
	#### NEED TO RUN ##############################
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")

  saveRDS(pol_temp_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pol_temp_age_bam_ACLIM_SEBS_gcv.rds"))
  
  # 2. weight ~ age * temp
	pol_temp_int_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")
		
	saveRDS(pol_temp_int_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pol_temp_int_age_bam_ACLIM_SEBS_gcv.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
	
	saveRDS(pcod_temp_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pcod_temp_age_bam_ACLIM_SEBS_gcv.rds"))
 
	 # 2. weight ~ age * temp
	pcod_temp_int_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
		
	saveRDS(pcod_temp_int_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pcod_temp_int_age_bam_ACLIM_SEBS_gcv.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
														
	saveRDS(yfin_temp_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/yfin_temp_age_bam_ACLIM_SEBS_gcv.rds"))
 

  # 2. weight ~ age * temp
	yfin_temp_int_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + 
																		 	s(SEBS_mean_sum_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
		
	saveRDS(yfin_temp_int_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/yfin_temp_int_age_bam_ACLIM_SEBS_gcv.rds"))


	#### ACLIM temps SEBS only year means ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_yrtemp_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")
	
  saveRDS(pol_yrtemp_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pol_yrtemp_age_bam_ACLIM_SEBS_gcv.rds"))
  
  # 2. weight ~ age * temp
	pol_yrtemp_int_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp, by = age_f) + 
													s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")
		
	saveRDS(pol_yrtemp_int_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pol_yrtemp_int_age_bam_ACLIM_SEBS_gcv.rds"))
  

	# pcod #
	
	#1. weight ~ age + temp
	pcod_yrtemp_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")

		saveRDS(pcod_yrtemp_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pcod_yrtemp_age_bam_ACLIM_SEBS_gcv.rds"))
 
														

  # 2. weight ~ age * temp
	pcod_yrtemp_int_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
		
	saveRDS(pcod_yrtemp_int_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/pcod_yrtemp_int_age_bam_ACLIM_SEBS_gcv.rds"))
  
 
	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_yrtemp_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + s(SEBS_mean_yr_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
														
	saveRDS(yfin_yrtemp_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/yfin_yrtemp_age_bam_ACLIM_SEBS_gcv.rds"))
 

  # 2. weight ~ age * temp
	yfin_yrtemp_int_age_bam_ACLIM_SEBS_gcv <- bam(log_wt ~ age_f + 
																		 	s(SEBS_mean_yr_temp, by = age_f) + 
																		 	s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
		
	saveRDS(yfin_yrtemp_int_age_bam_ACLIM_SEBS_gcv, 
  				file = here("./output/model output/ACLIM temps/GCV output/yfin_yrtemp_int_age_bam_ACLIM_SEBS_gcv.rds"))
