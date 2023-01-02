# predicting with gam


	
	new_data <- ROMS_proj_temps_saa$mean_temp

	preds <- predict_gam(pol_temp_int_age_bam)
											 newdata = ROMS_proj_temps_saa$sum_mean_temp)
	
											 exclude = "s(julian)", "te(latitude, longitude")
	
	
		te(latitude, longitude) + 
													s(ID_f, bs = "re") + # haul in year random effect
								 					s(haul_id_f, bs = "re") + # haul in year random effect
								 					s(cohort_f, bs = "re"),
	