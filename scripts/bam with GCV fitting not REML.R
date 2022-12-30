# models with GCV fitting and not REML

 #### models ####
  
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")
	
 saveRDS(pol_temp_age_bam_gcv, 
  				file = here("./output/model output/pollock/pol_temp_age_bam_gcv.rds"))
 														

  # 2. weight ~ age * temp
	pol_temp_int_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "GCV.Cp")
														
		
 saveRDS(pol_temp_int_age_bam_gcv, 
  				file = here("./output/model output/pollock/pol_temp_int_age_bam_gcv.rds"))
 
  # load
  pol_temp_age_bam_gcv <- readRDS(here("./output/model output/pollock/pol_temp_age_bam_gcv.rds"))
  pol_temp_int_age_bam_gcv <- readRDS(here("./output/model output/pollock/pol_temp_int_age_bam_gcv.rds"))

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
  				file = here("./output/model output/pollock/pcod_temp_age_bam_gcv.rds"))
  

  # 2. weight ~ age * temp
	pcod_temp_int_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "GCV.Cp")
														
  saveRDS(pcod_temp_int_age_bam_gcv, 
  				file = here("./output/model output/pollock/pcod_temp_int_age_bam_gcv.rds"))
 
  # load
 # pcod_temp_age_bam_gcv <- readRDS(here("./output/model output/pollock/pcod_temp_age_bam_gcv.rds"))
 # pcod_temp_int_age_bam_gcv <- readRDS(here("./output/model output/pollock/pcod_temp_int_age_bam_gcv.rds"))
	
 
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
  				file = here("./output/model output/pollock/yfin_temp_age_bam_gcv.rds"))
	
  # 2. weight ~ age * temp
	yfin_temp_int_age_bam_gcv <- bam(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + s(julian_day) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfinsole_dat,
													method = "GCV.Cp")
														
  saveRDS(yfin_temp_int_age_bam_gcv, 
  				file = here("./output/model output/pollock/yfin_temp_int_age_bam_gcv.rds"))
 
  # load
  yfin_temp_age_bam_gcv <- readRDS(here("./output/model output/pollock/yfin_temp_age_bam_gcv.rds"))
  yfin_temp_int_age_bam_gcv <- readRDS(here("./output/model output/pollock/yfin_temp_int_age_bam_gcv.rds"))
