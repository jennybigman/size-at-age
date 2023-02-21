# gams with linear age and temp effects 


  #### 1. With temp 4 months preceding temp ####
  
	# pollock #	
  
	#1. weight ~ age + temp
	pol_presurvey_templin_age_bam <- bam(log_wt ~ age_f + presurvey_mean_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(pol_presurvey_templin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/pol_presurvey_templin_age_bam.rds"))


  # 2. weight ~ age * temp
	pol_presurvey_templin_int_age_bam <- bam(log_wt ~ age_f * presurvey_mean_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(pol_presurvey_templin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/pol_presurvey_templin_int_age_bam.rds"))
 
  # load
  pol_presurvey_templin_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/pol_presurvey_templin_age_bam.rds"))
  pol_presurvey_templin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/pol_presurvey_templin_int_age_bam.rds"))

	# pcod #
	
	#1. weight ~ age + temp
	pcod_presurvey_templin_age_bam <- bam(log_wt ~ age_f + presurvey_mean_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(pcod_presurvey_templin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/pcod_presurvey_templin_age_bam.rds"))


  # 2. weight ~ age * temp
	pcod_presurvey_templin_int_age_bam <- bam(log_wt ~ age_f * presurvey_mean_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(pcod_presurvey_templin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/pcod_presurvey_templin_int_age_bam.rds"))
 
  # load
  pcod_presurvey_templin_age_bam <- readRDS(here("./output/model output/ACLIM temps/pcod_presurvey_templin_age_bam.rds"))
  pcod_presurvey_templin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/pcod_presurvey_templin_int_age_bam.rds"))
	
 
	# yfin sole #
	
	#1. weight ~ age + temp
	yfin_presurvey_templin_age_bam <- bam(log_wt ~ age_f + presurvey_mean_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfin_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(yfin_presurvey_templin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_presurvey_templin_age_bam.rds"))


  # 2. weight ~ age * temp
	yfin_presurvey_templin_int_age_bam <- bam(log_wt ~ age_f * presurvey_mean_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfin_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(yfin_presurvey_templin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_presurvey_templin_int_age_bam.rds"))
 
  # load
  yfin_presurvey_templin_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_presurvey_templin_age_bam.rds"))
  yfin_presurvey_templin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_presurvey_templin_int_age_bam.rds"))
 
	#### 2. yearly temps ####
	
  # pollock #	
  
	#1. weight ~ age + temp
	pol_mean_yr_templin_age_bam <- bam(log_wt ~ age_f + mean_yr_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(pol_mean_yr_templin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/pol_mean_yr_templin_age_bam.rds"))


  # 2. weight ~ age * temp
	pol_mean_yr_templin_int_age_bam <- bam(log_wt ~ age_f * mean_yr_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(pol_mean_yr_templin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/pol_mean_yr_templin_int_age_bam.rds"))
 
  # load
  pol_mean_yr_templin_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/pol_mean_yr_templin_age_bam.rds"))
  pol_mean_yr_templin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/pol_mean_yr_templin_int_age_bam.rds"))

	# pcod #
	
	#1. weight ~ age + temp
	pcod_mean_yr_templin_age_bam <- bam(log_wt ~ age_f + mean_yr_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(pcod_mean_yr_templin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/pcod_mean_yr_templin_age_bam.rds"))


  # 2. weight ~ age * temp
	pcod_mean_yr_templin_int_age_bam <- bam(log_wt ~ age_f * mean_yr_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(pcod_mean_yr_templin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/pcod_mean_yr_templin_int_age_bam.rds"))
 
  # load
  pcod_mean_yr_templin_age_bam <- readRDS(here("./output/model output/ACLIM temps/pcod_mean_yr_templin_age_bam.rds"))
  pcod_mean_yr_templin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/pcod_mean_yr_templin_int_age_bam.rds"))
	
 
	# yfin sole #
	
	#1. weight ~ age + temp
	yfin_mean_yr_templin_age_bam <- bam(log_wt ~ age_f + mean_yr_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfin_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(yfin_mean_yr_templin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_mean_yr_templin_age_bam.rds"))


  # 2. weight ~ age * temp
	yfin_mean_yr_templin_int_age_bam <- bam(log_wt ~ age_f * mean_yr_temp + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfin_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(yfin_mean_yr_templin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_mean_yr_templin_int_age_bam.rds"))
 
  # load
  yfin_mean_yr_templin_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_mean_yr_templin_age_bam.rds"))
  yfin_mean_yr_templin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_mean_yr_templin_int_age_bam.rds"))
 
	
	#### 3. temp during first year of life ####
  
  # pollock #	
  
	#1. weight ~ age + temp
	pol_temp0lin_age_bam <- bam(log_wt ~ age_f + temp_age0 + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(pol_temp0lin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/pol_temp0lin_age_bam.rds"))


  # 2. weight ~ age * temp
	pol_temp0lin_int_age_bam <- bam(log_wt ~ age_f * temp_age0 + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pollock_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(pol_temp0lin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/pol_temp0lin_int_age_bam.rds"))
 
  # load
  pol_temp0lin_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/pol_temp0lin_age_bam.rds"))
  pol_temp0lin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/pol_temp0lin_int_age_bam.rds"))

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp0lin_age_bam <- bam(log_wt ~ age_f + temp_age0 + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(pcod_temp0lin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/pcod_temp0lin_age_bam.rds"))


  # 2. weight ~ age * temp
	pcod_temp0lin_int_age_bam <- bam(log_wt ~ age_f * temp_age0 + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = pcod_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(pcod_temp0lin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/pcod_mean_yr_temp0in_int_age_bam.rds"))
 
  # load
  pcod_temp0lin_age_bam <- readRDS(here("./output/model output/ACLIM temps/pcod_temp0lin_age_bam.rds"))
  pcod_temp0lin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/pcod_temp0lin_int_age_bam.rds"))
	
 
	# yfin sole #
	
	#1. weight ~ age + temp
	yfin_temp0lin_age_bam <- bam(log_wt ~ age_f + temp_age0 + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfin_dat,
													method = "ML",
													nthreads = 8)
	
	 saveRDS(yfin_temp0lin_age_bam, 
  			file = here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_temp0lin_age_bam.rds"))


  # 2. weight ~ age * temp
	yfin_temp0lin_int_age_bam <- bam(log_wt ~ age_f * temp_age0 + 
													s(jday) + 
													te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
													data = yfin_dat, 
													method = "ML",
													nthreads = 8)

  saveRDS(yfin_temp0lin_int_age_bam, 
  				file = here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_temp0lin_int_age_bam.rds"))
 
  # load
  yfin_temp0lin_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_temp0lin_age_bam.rds"))
  yfin_temp0lin_int_age_bam <- readRDS(here("./output/model output/ACLIM temps/linear effect of age and temp/yfin_mean_yr_templin_int_age_bam.rds"))
 
	
