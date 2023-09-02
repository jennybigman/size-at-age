# older models

 
	pol_pretemp_int_age_bam_ML_std <- bam(log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + 
																 	s(jday) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pollock_dat, 
																	nthreads = 8,
																	method = "ML")
  
	saveRDS(pol_pretemp_int_age_bam_ML_std, 
	 				file = here("./output/model output/output Feb 2023/pol_pretemp_int_age_bam_ML_std.rds"))
 
	pcod_pretemp_int_age_bam_ML <- bam(log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + 
																 	s(jday) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = pcod_dat_trim, 
																	nthreads = 8,
																	method = "ML")
  
	saveRDS(pcod_pretemp_int_age_bam_ML, 
	 				file = here("./output/model output/output Feb 2023/pcod_pretemp_int_age_bam_ML_std.rds"))
 
	yfin_pretemp_int_age_bam_ML <- bam(log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + 
																 	s(jday) + te(latitude, longitude) + 
																	s(ID_f, bs = "re") + # haul in year random effect
								 									s(haul_id_f, bs = "re") + # haul in year random effect
								 									s(cohort_f, bs = "re"),
																	data = yfinsole_dat_trim, 
																	nthreads = 8,
																	method = "ML")
  
	saveRDS(yfin_pretemp_int_age_bam_ML, 
	 				file = here("./output/model output/output Feb 2023/yfin_pretemp_int_age_bam_ML_std.rds"))
 