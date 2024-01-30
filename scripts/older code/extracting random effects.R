	presurvey_btemp_no_int_mod_yr_RE_pollock <- readRDS( 
  				file = here("./output/model output/sdmTMB output/with year as RE/presurvey_btemp_no_int_mod_yr_RE_pollock.rds"))
 
	tidy(presurvey_btemp_no_int_mod_yr_RE_pollock)
	
	tidy(presurvey_btemp_no_int_mod_yr_RE_pollock, effects = "ran_pars", conf.int = TRUE)
	
	new_grid <- pollock_dat %>%
		select(age_f_ord, presurvey_btemp, X, Y)
	
	new_grid_yrs <- replicate_df(new_grid,
															 "year",
															 pollock_dat$year_f)
	

	preds <- predict(presurvey_btemp_no_int_mod_yr_RE_pollock,
									 newdata = new_grid_yrs)
	
	test <- tidy(presurvey_btemp_no_int_mod_yr_RE_pollock, 
							 effects = "ran_vals", conf.int = TRUE)
