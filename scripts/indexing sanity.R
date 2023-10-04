#	s <- unlist(sanity(test_mod))
#	
#	test <- which(s == FALSE) %>% as.numeric()
#	
##	ifelse(test %in% not_probs, print('boo'), print('yay'))
#	
#	if (any(test %!in% not_probs)) {
#		print('boo')
#	} else {
#		print('yay')
#	}

	
	###### starts here ########
	
		sdmTMB_yr_RE_func <- function(sp, y){
		
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

		#### run models	 ####	
		print(paste('running no int model for', sp, "with", y))
		
		# set up formulas
		form1 <- paste0("log_wt_std ~ 0 + age_f_ord + s(" , y, ") + (1|year_f)")
		form2 <- paste0("log_wt_std ~ 0 + age_f_ord + s(" , y, ", by = age_f_ord) + (1|year_f)")
		 
		# model with interaction
		print(paste('running int model for', sp, "with", y))

		mod_int <- 
			try(
				sdmTMB(	
 					formula = as.formula(form2),
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "off",
					control = sdmTMBcontrol(nlminb_loops = 3)))
		
		s <- unlist(sanity(mod_int))
		
		s
		
		ind <- which(s == FALSE) %>% as.numeric()
	
		
			 if (class(mod_int) == "try-error"){
		 	
    		print(paste("error!"))
		 
		 } else if (sanity(mod_int)[[9]] == "TRUE") {
		 	 	
		 		write_rds(mod_int, 
					file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", # change to file path on local machine
										 y, "_int_mod_yr_RE_", sp, ".rds"))
		 		
		 		print(paste("int model for", sp, "with", y, "complete"))

		 } else if (any(ind %!in% not_probs)) {
					
		 		print('running extra optimization')
				
				mod_int_eo <- try(run_extra_optimization(mod_int))
		
						if (class(mod_int_eo) == "try-error"){
		 				
    					print(paste("error!"))
					
						} else if (sanity(mod_int_eo)[[9]] == "TRUE") {
		 				 	
		 					write_rds(mod_int_eo, 
								file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", # change to file path on local machine
										 y, "_int_mod_yr_RE_", sp, ".rds"))
		 		
		 					print(paste("int model for", sp, "with", y, "complete"))

					  } else {
			
    					print('boo')
					  } 
				}
		}
	
	sp <- "pcod"
	
	vars <- c("presurvey_btemp", "yrprior_btemp")
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_yr_RE_func)
	
		
		 	
	
			
		
		
		