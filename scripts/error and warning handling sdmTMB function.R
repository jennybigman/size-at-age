	
	sdmTMB_cv_yr_RE_func <- function(sp, y){
		
		# filter df by species
		new_dat <- dat_all %>% filter(species == sp)
  
		# order age class
  	levels_to_ord <- sort(unique(new_dat$age_f))
  	new_dat$age_f_ord <- ordered(new_dat$age_f, levels = c(levels_to_ord))	
  	
  	# make year a factor and drop unused levels
		new_dat$year_f <- as.factor(new_dat$year)
		new_dat$year_f <- droplevels(new_dat$year_f)

		# make mesh
		mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
	
		# set up prior
		pc <- pc_matern(range_gt = 300, sigma_lt = 0.4)

		# run models		
		print(paste('running no int model for', sp, "with", y))
		
		# set up formulas
		form1 <- paste0("log_wt_std ~ 0 + age_f_ord + s(" , y, ") + (1|year_f)")

 		# model without interaction 
		mod_cv <- 
			try(
				sdmTMB_cv(
					formula = as.formula(form1),
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "off",
					k_folds = max(new_dat$fold),
        	fold_ids = new_dat$fold,
					control = sdmTMBcontrol(nlminb_loops = 3)))

		
		 if (class(mod_cv) == "try-error"){
		 	
    		print(paste("error!"))
		 
		 } else if (sanity(mod_cv$models[[1]])[[9]] == "TRUE") {
		 	 	
		 		write_rds(mod_cv, 
					file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", # change to file path on local machine
										 y, "_no_int_mod_yr_RE_", sp, ".rds"))
		 		
		 		print(paste("no int model for", sp, "with", y, "complete"))

		 } else {
	
				print('running extra optimization')
				
				mod_cv_eo <- run_extra_optimization(mod_cv$models[[1]], nlminb_loops = 0, newton_loops = 1)
		
						if (class(mod_cv_eo) == "try-error"){
		 				
    					print(paste("error!"))
					
						} else if (sanity(mod_cv_eo$models[[1]])[[9]] == "TRUE") {
		 				 	
		 					write_rds(mod_cv, 
								file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", # change to file path on local machine
										 y, "_no_int_mod_yr_RE_", sp, ".rds"))
		 		
		 		print(paste("no int model for", sp, "with", y, "complete"))

					  } else {
			
    		print('extra optim did not help')
		 
		 } }
	}
	
	
	
	sp <- c("pcod", "pollock")
	
	vars <- c('presurvey_btemp', "yrprior_btemp")
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)
		
	map2(df_func$sp, df_func$y, sdmTMB_cv_yr_RE_func)

	
	
	
	
	
	
	
	
	
	
	