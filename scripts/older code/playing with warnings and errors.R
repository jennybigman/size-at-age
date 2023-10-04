	
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
		form1 <- paste0("log_wt ~ 0 + age_f_ord + s(" , y, ") + (1|year_f)")

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
					file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", 
										 y, "_no_int_mod_yr_RE_", sp, ".rds"))
		 		
		 		print(paste("no int model for", sp, "with", y, "complete"))

		 } else if (sanity(mod_cv$models[[1]]))[[9]] == "FALSE") {
	
				print('running extra optimization')
				
				#mod_cv_eo <- run_extra_optimization(mod_cv, nlminb_loops = 0, newton_loops = 1)
				
				mod_cv_eo <- 
					try(
						sdmTMB_cv(
							formula = as.formula(form1),
							data = new_dat,
							mesh = mesh,
							spatial = "on",
							spatiotemporal = "off",
							k_folds = max(new_dat$fold),
      	  		fold_ids = new_dat$fold,
							control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1)))
		 
				write_rds(mod_cv_eo, 
					file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", 
									 y, "_no_int_mod_yr_RE_", sp, ".rds"))
		
    		print(paste("no int model for", sp, "with", y, "complete"))
		 
		 } 
	}
	
	
	
	sp <- c("pcod", "pollock")
	
	vars <- c('presurvey_btemp', "yrprior_btemp")
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)
		
	map2(df_func$sp, df_func$y, sdmTMB_cv_yr_RE_func)

	
	
	
	
	
	
	
	
	
	
	
	
	
	# file path for mods with year as RE
	file_path_yr_RE <- "/output/model output/sdmTMB output/with year as RE/"
	
	s <- read_rds(file = 
		paste0(here(), file_path_yr_RE, 'sanity for_presurvey_btemp_no_int_mod_yr_RE_pcod.rds'))


		
	# try one mod
	presurvey_btemp_no_int_mod_yr_RE_pcod <- read_rds(
		file = here('./output/model output/sdmTMB output/with year as RE/presurvey_btemp_no_int_mod_yr_RE_pcod.rds'))
	
	yrprior_btemp_int_mod_yr_RE_pcod <- read_rds(
		file = here('./output/model output/sdmTMB output/with year as RE/yrprior_btemp_int_mod_yr_RE_pcod.rds'))

	s <- unlist(sanity(yrprior_btemp_int_mod_yr_RE_pcod$models[[1]]))

	ind <- which(s == FALSE)
	
	not_probs <- c(5, 6) # gradients_ok & se_magnitude_ok
	
	if (test %!in% not_probs){
		print("yay")
	} else {
		print("boo")
	}
	
		
	log <- c(TRUE, TRUE, FALSE)
		
	names <- c("test1", "test2", "test3")

	names(log) <- names
	
	test <- which(log == FALSE)
	
	issues <- c(4, 5)
	
	if (test %!in% issues) {
		print("yay")
	} else {
		print("ugh")
	}
	
	# try with real model 
	
	test_mod <- read_rds(
		file = here('./output/model output/sdmTMB output/older models/age0_btemp_int_yfin_nj.rds'))

	s <- unlist(sanity(test_mod))
	
	test <- which(s == FALSE) %>% as.numeric()
	
#	ifelse(test %in% not_probs, print('boo'), print('yay'))
	
	if (any(test %!in% not_probs)) {
		print('boo')
	} else {
		print('yay')
	}
				 

  