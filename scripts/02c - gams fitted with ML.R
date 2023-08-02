# gams with smoothed age and temp/oxygen effects 

	##############################################################################
	# temperature ####
	##############################################################################
	
	## presurvey temp ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_pretemp_age_bam_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + 
														 	s(julian_day) + te(latitude, longitude) + 
											  			s(ID_f, bs = "re") + # haul in year random effect
								 			  			s(haul_id_f, bs = "re") + # haul in year random effect
								 			  			s(cohort_f, bs = "re"),
											  			data = pollock_dat, 
															nthreads = 8,
															method = "ML")
	
  #saveRDS(pol_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_pretemp_age_bam.rds"))
  
  saveRDS(pol_pretemp_age_bam_ML, 
  				file = here("./output/model output PC/pol_pretemp_age_bam_ML.rds"))
 
  # 2. weight ~ age * temp
	pol_pretemp_int_age_bam_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp, by = age_f) + 
																 	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pollock_dat, 
																	nthreads = 8,
																	method = "ML")
														
	#saveRDS(pol_pretemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_pretemp_int_age_bam.rds"))
  
	saveRDS(pol_pretemp_int_age_bam_ML, 
	 				file = here("./output/model output PC/pol_pretemp_int_age_bam_ML.rds"))
 
	# pcod #
	
	#1. weight ~ age + temp
	pcod_pretemp_age_bam_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + 
																s(julian_day) + te(latitude, longitude) + 
																s(ID_f, bs = "re") + # haul in year random effect
								 								s(haul_id_f, bs = "re") + # haul in year random effect
								 								s(cohort_f, bs = "re"),
																data = pcod_dat, 
																nthreads = 8,
																method = "ML")
	
	#saveRDS(pcod_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pcod_pretemp_age_bam.rds"))
 
	saveRDS(pcod_pretemp_age_bam_ML, 
	 				file = here("./output/model output PC/pcod_pretemp_age_bam_ML.rds"))
 
  # 2. weight ~ age * temp
	pcod_pretemp_int_age_bam_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp, by = age_f) + 
															 	   s(julian_day) + te(latitude, longitude) + 
																   s(ID_f, bs = "re") + # haul in year random effect
								 								   s(haul_id_f, bs = "re") + # haul in year random effect
								 								   s(cohort_f, bs = "re"),
																   data = pcod_dat, 
																	 nthreads = 8,
																	 method = "ML")
														
	#saveRDS(pcod_pretemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pcod_pretemp_int_age_bam.rds"))
  
  saveRDS(pcod_pretemp_int_age_bam_ML, 
  				file = here("./output/model output PC/pcod_pretemp_int_age_bam_ML.rds"))

 	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_pretemp_age_bam_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + 
																s(julian_day) + te(latitude, longitude) + 
																s(ID_f, bs = "re") + # haul in year random effect
								 								s(haul_id_f, bs = "re") + # haul in year random effect
								 								s(cohort_f, bs = "re"),
																data = yfinsole_dat, 
																nthreads = 8,
																method = "ML")
														
	#saveRDS(yfin_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_pretemp_age_bam.rds"))
 
  saveRDS(yfin_pretemp_age_bam_ML, 
  				file = here("./output/model output PC/yfin_pretemp_age_bam_ML.rds"))

  # 2. weight ~ age * temp
	yfin_pretemp_int_age_bam_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp, by = age_f) + 
																		s(julian_day) + te(latitude, longitude) + 
																		s(ID_f, bs = "re") + # haul in year random effect
								 										s(haul_id_f, bs = "re") + # haul in year random effect
								 										s(cohort_f, bs = "re"),
																		data = yfinsole_dat, 
																		nthreads = 8,
																		method = "ML")
		
	#saveRDS(yfin_pretemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_pretemp_int_age_bam.rds"))

  saveRDS(yfin_pretemp_int_age_bam_ML, 
  				file = here("./output/model output PC/yfin_pretemp_int_age_bam_ML.rds"))

  
  #### year prior temps  ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_yrtemp_age_bam_ML <- bam(log_wt ~ age_f + s(mean_yr_temp) + 
															s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = pollock_dat, 
															nthreads = 8,
															method = "ML")
	
	#saveRDS(pol_yrtemp_age_bam, 
	#				file = here("./output/model output/ACLIM temps/pol_yrtemp_age_bam_ACLIM_SEBS.rds"))
  
	saveRDS(pol_yrtemp_age_bam_ML, 
					file = here("./output/model output PC/pol_pretemp_age_bam_ML.rds"))

  # 2. weight ~ age * temp #### NEXT
	pol_yrtemp_int_age_bam_ML <- bam(log_wt ~ age_f + s(mean_yr_temp, by = age_f) + 
																	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pollock_dat, 
																	nthreads = 8,
																	method = "ML")
														
		
	#saveRDS(pol_yrtemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_yrtemp_int_age_bam.rds"))
  
  saveRDS(pol_yrtemp_int_age_bam_ML, 
  				file = here("./output/model output PC/ppol_yrtemp_int_age_bam_ML.rds"))

  # pcod #
	
	#1. weight ~ age + temp
	pcod_yrtemp_age_bam_ML <- bam(log_wt ~ age_f + s(mean_yr_temp) +	
														 	s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = pcod_dat,
															nthreads = 8,
															method = "ML")
	
	#saveRDS(pcod_yrtemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pcod_yrtemp_age_bam.rds"))
 
  saveRDS(pcod_yrtemp_age_bam_ML, 
  				file = here("./output/model output PC/pcod_yrtemp_age_bam_ML.rds"))

  # 2. weight ~ age * temp
	pcod_yrtemp_int_age_bam_ML <- bam(log_wt ~ age_f + s(mean_yr_temp, by = age_f) + 
																 	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pcod_dat, 
																	nthreads = 8,
																	method = "ML")
														
	#saveRDS(pcod_yrtemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pcod_yrtemp_int_age_bam.rds"))
  
  saveRDS(pcod_yrtemp_int_age_bam_ML, 
  				file = here("./output/model output PC/pcod_yrtemp_int_age_bam_ML.rds"))

  # yfin sole #
	
	# 1. weight ~ age + temp
	yfin_yrtemp_age_bam_ML <- bam(log_wt ~ age_f + s(mean_yr_temp) + 
														 	s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = yfinsole_dat,
															nthreads = 8,
															method = "ML")
														
	#saveRDS(yfin_yrtemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_yrtemp_age_bam.rds"))
 
  saveRDS(yfin_yrtemp_age_bam_ML, 
  				file = here("./output/model output PC/yfin_yrtemp_age_bam_ML.rds"))

  # 2. weight ~ age * temp
	yfin_yrtemp_int_age_bam_ML <- bam(log_wt ~ age_f + s(mean_yr_temp, by = age_f) + 
																 	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = yfinsole_dat, 
																	nthreads = 8,
																	method = "ML")
		
	#saveRDS(yfin_yrtemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_yrtemp_int_age_bam.rds"))

  saveRDS(yfin_yrtemp_int_age_bam_ML, 
  				file = here("./output/model output PC/yfin_yrtemp_int_age_bam_ML.rds"))

  
  #### temp during first year of life ####
	
	# pollock #	
  
	#1. weight ~ age + temp
	pol_temp0_age_bam_ML <- bam(log_wt ~ age_f + s(temp_age0) + 
													 	s(julian_day) + te(latitude, longitude) + 
														s(ID_f, bs = "re") + # haul in year random effect
								 						s(haul_id_f, bs = "re") + # haul in year random effect
								 						s(cohort_f, bs = "re"),
														data = pollock_dat, 
														nthreads = 8,
														method = "ML")

  #saveRDS(pol_temp0_age_bam, file = here("./output/model output/ACLIM temps/pol_temp0_age_bam.rds"))
  
  saveRDS(pol_temp0_age_bam_ML, 
  				file = here("./output/model output PC/pol_temp0_age_bam_ML.rds"))

  # 2. weight ~ age * temp
	pol_temp0_int_age_bam_ML <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
															 	s(julian_day) + te(latitude, longitude) + 
																s(ID_f, bs = "re") + # haul in year random effect
								 								s(haul_id_f, bs = "re") + # haul in year random effect
								 								s(cohort_f, bs = "re"),
																data = pollock_dat, 
																nthreads = 8,
																method = "ML")
														
	#saveRDS(pol_temp0_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_temp0_int_age_bam.rds"))
  
  saveRDS(pol_temp0_int_age_bam_ML, 
  				file = here("./output/model output PC/pol_temp0_int_age_bam_ML.rds"))

	# pcod #
	
	#1. weight ~ age + temp
	pcod_temp0_age_bam_ML <- bam(log_wt ~ age_f + s(temp_age0) + 
															s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = pcod_dat, 
															nthreads = 8,
															method = "ML")
	
	#saveRDS(pcod_temp0_age_bam, file = here("./output/model output/ACLIM temps/pcod_temp0_age_bam.rds"))
 
	saveRDS(pcod_temp0_age_bam_ML, 
					file = here("./output/model output PC/pcod_temp0_age_bam_ML.rds"))
												
  # 2. weight ~ age * temp
	pcod_temp0_int_age_bam_ML <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
																	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pcod_dat, 
																	nthreads = 8,
																	method = "ML")
														
		
	#saveRDS(pcod_temp0_int_age_bam, 
	#				file = here("./output/model output/ACLIM temps/pcod_temp0_int_age_bam.rds"))
  
	saveRDS(pol_pretemp_age_bam_ML, 
					file = here("./output/model output PC/pcod_temp0_int_age_bam_ML.rds"))

	# yfin sole #
	
	# 1. weight ~ age + temp
	yfin_temp0_age_bam_ML <- bam(log_wt ~ age_f + s(temp_age0) + 
															s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = yfinsole_dat, 
															nthreads = 8,
															method = "ML")
														
	#saveRDS(yfin_temp0_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_temp0_age_bam.rds"))
 
  saveRDS(yfin_temp0_age_bam_ML,
  				file = here("./output/model output PC/yfin_temp0_age_bam_ML.rds"))

  # 2. weight ~ age * temp
	yfin_temp0_int_age_bam_ML <- bam(log_wt ~ age_f + s(temp_age0, by = age_f) + 
																	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = yfinsole_dat, 
																	nthreads = 8,
																	method = "ML")
		
	#saveRDS(yfin_temp0_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/yfin_temp0_int_age_bam.rds"))

	saveRDS(yfin_temp0_int_age_bam_ML,
					file = here("./output/model output PC/yfin_temp0_int_age_bam_ML.rds"))

	
	##############################################################################
	# Oxygen ####
	##############################################################################

	## presurvey oxygen (oxygen April to June of same year as survey) ####

	#1. weight ~ age + oxygen
	
	presurvey_boxy_func <- function(df){
	
	bam(log_wt ~ age_f + s(presurvey_boxy) + 
														 	s(julian_day) + te(latitude, longitude) + 
											  			s(ID_f, bs = "re") + # haul in year random effect
								 			  			s(haul_id_f, bs = "re") + # haul in year random effect
								 			  			s(cohort_f, bs = "re"),
											  			data = df, 
															nthreads = 8,
															method = "ML")
	}
	
	presurvey_boxy_fit <- lapply(specimen_dat, presurvey_boxy_func)
	
	#### EDIT ####
  #saveRDS(pol_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_pretemp_age_bam.rds"))
  #### EDIT ####
 
  # 2. weight ~ age * oxygen
	
  presurvey_boxy_int_func <- function(df){

  	bam(log_wt ~ age_f + s(presurvey_boxy, by = age_f) + 
																 	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = df, 
																	nthreads = 8,
																	method = "ML")
  }
	
	presurvey_boxy_int_fit <- lapply(specimen_dat, presurvey_boxy_int_func)

  #### EDIT ####													
	#saveRDS(pol_pretemp_int_age_bam_ML, 
		# 				file = here("./output/model output PC/pol_pretemp_int_age_bam_ML.rds"))
	#### EDIT ####
	
	#### year prior oxygen average ####
	
	#1. weight ~ age + oxygen
	
	yrprior_boxy_func <- function(df){

		bam(log_wt ~ age_f + s(yrprior_boxy) + 
															s(julian_day) + te(latitude, longitude) + 
															s(ID_f, bs = "re") + # haul in year random effect
								 							s(haul_id_f, bs = "re") + # haul in year random effect
								 							s(cohort_f, bs = "re"),
															data = df, 
															nthreads = 8,
															method = "ML")
	}
	
	yrprior_boxy_fit <- lapply(specimen_dat, yrprior_boxy_func)

	  ##### EDIT ####													
		# saveRDS(pol_yrtemp_age_bam_ML, 
		#		file = here("./output/model output PC/pol_pretemp_age_bam_ML.rds"))
	  ##### EDIT ####													

	# 2. weight ~ age * oxygen 
	
	yrprior_boxy_int_func <- function(df){

			bam(log_wt ~ age_f + s(yrprior_boxy, by = age_f) + 
																	s(julian_day) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = df, 
																	nthreads = 8,
																	method = "ML")
		}
	
		yrprior_boxy_int_fit <- lapply(specimen_dat, yrprior_boxy_int_func)
	
	#### EDIT ####													
	#saveRDS(pol_yrtemp_int_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_yrtemp_int_age_bam.rds"))
  #### EDIT ####													

  #### oxygen during first year of life ####
	
	#1. weight ~ age + oxygen
	
	firstyr_boxy_func <- function(df){
	
			bam(log_wt ~ age_f + s(age0_boxy) + 
													 	s(julian_day) + te(latitude, longitude) + 
														s(ID_f, bs = "re") + # haul in year random effect
								 						s(haul_id_f, bs = "re") + # haul in year random effect
								 						s(cohort_f, bs = "re"),
														data = pollock_dat, 
														nthreads = 8,
														method = "ML")
	}
		
	firstyr_boxy_fit <- lapply(specimen_dat, firstyr_boxy_func)
	
	#### EDIT ####													
  #saveRDS(pol_temp0_age_bam, file = here("./output/model output/ACLIM temps/pol_temp0_age_bam.rds"))
 	#### EDIT ####													


  # 2. weight ~ age * oxygen
	
	firstyr_boxy_int_func <- function(df){
		
			bam(log_wt ~ age_f + s(age0_boxy, by = age_f) + 
															 	s(julian_day) + te(latitude, longitude) + 
																s(ID_f, bs = "re") + # haul in year random effect
								 								s(haul_id_f, bs = "re") + # haul in year random effect
								 								s(cohort_f, bs = "re"),
																data = pollock_dat, 
																nthreads = 8,
																method = "ML")	
	}
		
	firstyr_boxy_int_fit <- lapply(specimen_dat, firstyr_boxy_int_func)

	#### EDIT ####													
  #saveRDS(pol_temp0_age_bam, file = here("./output/model output/ACLIM temps/pol_temp0_age_bam.rds"))
 	#### EDIT ####													
