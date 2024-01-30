# sdmTMB function - no CV for now

	dat_list <- list(pollock_dat, pcod_dat_trim, yfinsole_dat)
	
	dat_all <- dat_list %>% bind_rows()
	
	# remove any rows with no age0 environmental data (the year in which some individuals were age 0 was prior to 1970, first year of hindcast)
	#dat_all <- dat_all %>%
	#	drop_na(age0_boxy)
	
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/no year RE and k/"
	
	#### fit models without interaction ####
	
	# function to fit multiple models with no spatiotemporal random field and no RE of year
	
	sdmTMB_no_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(species == sp)
  			
					# order age class
  				levels_to_ord <- sort(unique(new_dat$age_f))
  				new_dat$age_f_ord <- ordered(new_dat$age_f, levels = c(levels_to_ord))	
  				
  				# drop unused factor levels of year (error when do this outside function)
  				new_dat$year_f <- droplevels(new_dat$year_f)
  				
					# make mesh
					mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
				
					# for mod name
					mod_name <- "_no_int_mod_"
					
					# set up which sanity checks are not really issues (but will check residual plots)
					#not_probs <- c(5, 6) # gradients_ok & se_magnitude_ok
	
		# run models	
		
					print(paste('running no int model for', sp, "with", y))
		
					# set up formulas
					form1 <- paste0("log_wt_std ~ s(" , y, ",k = 3)")
			
 					# model without interaction 
					mod <- 
						try(
							sdmTMB(
								formula = as.formula(form1),
								data = new_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3)))
			
					s <- sanity(mod, gradient_thresh = 0.05)
	
		# deal with warnings and issues
					
					# if error, tell me
						if (class(mod) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (s$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod, 
									file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
		 						
		 						print(paste("no int model for", sp, "with", y, "complete"))
		 	
					  } else if 
					  			 (sanity(mod)$hessian_ok ==      "TRUE" &
						  			sanity(mod)$eigen_values_ok == "TRUE" &
					  				sanity(mod)$nlminb_ok ==       "TRUE" &
					  				sanity(mod)$range_ok ==        "TRUE" &
					  				sanity(mod)$se_na_ok ==        "TRUE" &
					  				sanity(mod)$sigmas_ok ==       "TRUE") {
	
							write_rds(mod, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
							print(paste("no int model for", sp, "with", y, "complete"))

		 				
		 				} else if 
							(sanity(mod)$hessian_ok      != "TRUE" |
						 	 sanity(mod)$eigen_values_ok != "TRUE" |
					  	 sanity(mod)$nlminb_ok       != "TRUE" |
					  	 sanity(mod)$range_ok        != "TRUE" |
					  	 sanity(mod)$se_na_ok        != "TRUE" |
					  	 sanity(mod)$sigmas_ok       != "TRUE" ) {
	
									print('running extra optimization')
								
									mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 1)) 
							
											# if model with extra optimization (eo) threw an error, rerun with no newton loops
 											if (class(mod_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 0)) 
 											
											} else if (sanity(mod_eo)$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
													print(paste("no int model for", sp, "with", y, "complete"))
		 											
													write_rds(mod_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
					
											} else if
												 (sanity(mod_eo)$hessian_ok ==      "TRUE" &
						  						sanity(mod_eo)$eigen_values_ok == "TRUE" &
					  							sanity(mod_eo)$nlminb_ok ==       "TRUE" &
					  							sanity(mod_eo)$range_ok ==        "TRUE" &
					  							sanity(mod_eo)$se_na_ok ==        "TRUE" &
					  							sanity(mod_eo)$sigmas_ok ==       "TRUE") {
	
											print(paste("no int model for", sp, "with", y, "complete"))

											write_rds(mod_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
													

											 } else {
												 	print('boo - extra optimization did not solve the issue(s)')
											 }
		 								
							# if original model object before optimization (mod_int) looks good aside from a few non-issues, save model			
									 	
					  	 } else if 
					  			 (sanity(mod)$hessian_ok ==      "TRUE" &
						  			sanity(mod)$eigen_values_ok == "TRUE" &
					  				sanity(mod)$nlminb_ok ==       "TRUE" &
					  				sanity(mod)$range_ok ==        "TRUE" &
					  				sanity(mod)$se_na_ok ==        "TRUE" &
					  				sanity(mod)$sigmas_ok ==       "TRUE") {
	
							write_rds(mod, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
									print(paste("no int model for", sp, "with", y, "complete"))

		 									
					  	 } else {
					  	 	
									print(paste("boo - no int model for", sp, "with", y, "has issues"))

					  	 } 
	}

	
	
	# run function
	
	sp <- unique(dat_all$species)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_no_int_func)

	
	###################################################################################
	# Models with a factor-smooth interaction ####
	###################################################################################
	
	sdmTMB_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(species == sp)
  			
					# order age class
  				levels_to_ord <- sort(unique(new_dat$age_f))
  				new_dat$age_f_ord <- ordered(new_dat$age_f, levels = c(levels_to_ord))	
  				
  				# drop factor levels of year
  				new_dat$year_f <- droplevels(new_dat$year_f)
  				
  				# make mesh
					mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
				
					# set up prior
					pc <- pc_matern(range_gt = 300, sigma_lt = 0.4)
					
					# mod name
					mod_name <- "_int_mod_"
					
					# set up which sanity checks are not really issues (but will check residual plots)
					not_probs <- c(5, 6) # gradients_ok & se_magnitude_ok
					
		# run models	
		
					print(paste('running int model for', sp, "with", y))
		
					# set up formulas
					form2 <- paste0("log_wt_std ~ s(" , y, ", by = age_f_ord, k = 3)")

 					# model with interaction 
					mod_int <- 
						try(
							sdmTMB(
								formula = as.formula(form2),
								data = new_dat,
								mesh = mesh,
								priors = sdmTMBpriors(matern_s = pc),
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3)))

						# deal with warnings and issues
					
					# if error, tell me
						if (class(mod_int) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (sanity(mod_int)$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod_int, 
									file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
		 						
		 						print(paste("int model for", sp, "with", y, "complete"))
		 	
					  } else if 
					  			 (sanity(mod_int)$hessian_ok ==      "TRUE" &
						  			sanity(mod_int)$eigen_values_ok == "TRUE" &
					  				sanity(mod_int)$nlminb_ok ==       "TRUE" &
					  				sanity(mod_int)$range_ok ==        "TRUE" &
					  				sanity(mod_int)$se_na_ok ==        "TRUE" &
					  				sanity(mod_int)$sigmas_ok ==       "TRUE") {
	
							write_rds(mod_int, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
							print(paste("int model for", sp, "with", y, "complete"))

		 				
		 				} else if 
							(sanity(mod_int)$hessian_ok      != "TRUE" |
						 	 sanity(mod_int)$eigen_values_ok != "TRUE" |
					  	 sanity(mod_int)$nlminb_ok       != "TRUE" |
					  	 sanity(mod_int)$range_ok        != "TRUE" |
					  	 sanity(mod_int)$se_na_ok        != "TRUE" |
					  	 sanity(mod_int)$sigmas_ok       != "TRUE" ) {
	
									print('running extra optimization')
								
									mod_int_eo <- try(run_extra_optimization(mod_int, nlminb_loops = 3, newton_loops = 1)) 
							
											# if model with extra optimization (eo) threw an error, rerun with no newton loops
 											if (class(mod_int_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_int_eo <- try(run_extra_optimization(mod_int, nlminb_loops = 3, newton_loops = 0)) 
 											
											} else if (sanity(mod_int_eo)$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
													print(paste("int model for", sp, "with", y, "complete"))
		 											
													write_rds(mod_int_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
					
											} else if
												 (sanity(mod_int_eo)$hessian_ok ==      "TRUE" &
						  						sanity(mod_int_eo)$eigen_values_ok == "TRUE" &
					  							sanity(mod_int_eo)$nlminb_ok ==       "TRUE" &
					  							sanity(mod_int_eo)$range_ok ==        "TRUE" &
					  							sanity(mod_int_eo)$se_na_ok ==        "TRUE" &
					  							sanity(mod_int_eo)$sigmas_ok ==       "TRUE") {
	
											print(paste("int model for", sp, "with", y, "complete"))

											write_rds(mod_int_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
													

											 } else {
												 	print(paste('boo - extra optimization did not solve the issue(s)',
												 							" for int model for", sp, "with", y))
											 }
		 								
							# if original model object before optimization (mod_int) looks good aside from a few non-issues, save model			
									 	
					  	 } else if 
					  			 (sanity(mod_int)$hessian_ok ==      "TRUE" &
						  			sanity(mod_int)$eigen_values_ok == "TRUE" &
					  				sanity(mod_int)$nlminb_ok ==       "TRUE" &
					  				sanity(mod_int)$range_ok ==        "TRUE" &
					  				sanity(mod_int)$se_na_ok ==        "TRUE" &
					  				sanity(mod_int)$sigmas_ok ==       "TRUE") {
	
							write_rds(mod_int, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
									print(paste("int model for", sp, "with", y, "complete"))

		 									
					  	 } else {
					  	 	
									print(paste("boo - int model for", sp, "with", y, "has issues"))

					  	 } 
	}

		
	
	# run function
	
	sp <- unique(dat_all$species)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_int_func)
	
	
	## two models for pcod did not converge
	
	
	
	#### read in models ####
	
	file_list <- list.files(path = paste0(here(), file_path_all))

  prestring <- paste0(here(), file_path_all)
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i) # I used a for loop!
  }
  
  mod_list <- lapply(mod_names_list, read_rds)
  

  # separate models by species ####
  
	# pollock #
  
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yf", names(mod_list))]
	
	# check sanity()
	
	pol_s_list <- lapply(pol_mod_list, sanity)
	pcod_s_list <- lapply(pcod_mod_list, sanity)	
	yfin_s_list <- lapply(yfin_mod_list, sanity)
	
	# look at residuals for some models
	
	plot_resid_func <- function(mod){
		
		if (sanity(mod)$all_ok == "FALSE") {
		
			sims <- simulate(mod, nsim = 250)
			resids <- sims %>% dharma_residuals(mod, plot = FALSE)
			
			fwrite(resids, file = paste0(here(), file_path_all, mod, ".csv"))
			
			} else { 
				
				print("nothing needed")
				
				}
	}
	
	lapply(pcod_mod_list, plot_resid_func)
	
	
	# pollock
	presurvey_boxy_int_pollock <- read_rds(paste0(prestring, "presurvey_boxy_int_mod_pollock.rds"))
	sanity(presurvey_boxy_int_pollock)
						
	sims <- simulate(presurvey_boxy_int_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_pollock, plot = FALSE)
 
	yrprior_boxy_int_pollock <- read_rds(paste0(prestring, "yrprior_boxy_int_mod_pollock.rds"))
	sanity(yrprior_boxy_int_pollock)
						
	sims <- simulate(presurvey_boxy_int_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_pollock, plot = FALSE)
	
		ggplot(dharma_sims) +
				geom_point(aes(x = expected, y = observed)) +
				geom_abline(intercept = 0, slope = 1, color = "red") 
	
	# pcod
	presurvey_boxy_int_pcod <- read_rds(paste0(prestring, "presurvey_boxy_int_mod_pcod.rds"))
	sanity(presurvey_boxy_int_pcod)
						
	sims <- simulate(presurvey_boxy_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_pcod)
	
 	presurvey_btemp_int_pcod <- read_rds(paste0(prestring, "presurvey_btemp_int_mod_pcod.rds"))
	sanity(presurvey_btemp_int_pcod)
						
	sims <- simulate(presurvey_btemp_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pcod)

	yrprior_boxy_int_pcod <- read_rds(paste0(prestring, "yrprior_boxy_int_mod_pcod.rds"))
	sanity(yrprior_boxy_int_pcod)
						
	sims <- simulate(yrprior_boxy_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_int_pcod)

	
	# yfin
	yrprior_btemp_int_yfsole <- read_rds(paste0(prestring, "yrprior_btemp_int_mod_yfsole.rds"))
	sanity(yrprior_btemp_int_yfsole)
						
	sims <- simulate(yrprior_btemp_int_yfsole, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_yfsole)

	presurvey_btemp_int_yfsole <- read_rds(paste0(prestring, "presurvey_btemp_int_mod_yfsole.rds"))
	sanity(presurvey_btemp_int_yfsole)
						
	sims <- simulate(presurvey_btemp_int_yfsole, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_yfsole)

	presurvey_boxy_int_yfsole <- read_rds(paste0(prestring, "presurvey_boxy_int_mod_yfsole.rds"))
	sanity(presurvey_boxy_int_yfsole)
						
	sims <- simulate(presurvey_boxy_int_yfsole, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_yfsole)

	# refit two pcod mods
	
	# make mesh
	mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")	
	
	# set up prior
	pc <- pc_matern(range_gt = 210, sigma_lt = 0.05)
	
	# order age class
	levels_to_ord <- sort(unique(pollock_dat$age_f))
	pollock_dat$age_f_ord <- ordered(pollock_dat$age_f, levels = c(levels_to_ord))	
  	
					
	# model
	test_mod <- 
		sdmTMB(
			log_wt ~ 0 + age_f_ord + s(age0_btemp),
			data = pollock_dat,
			mesh = mesh,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			spatiotemporal = "off",
			control = sdmTMBcontrol(nlminb_loops = 3))

		sanity(test_mod)
		
		test_mod <- run_extra_optimization(test_mod, nlminb_loops = 1, newton_loops = 0)
					
		sims <- simulate(test_mod, nsim = 250)
		dharma_sims <- sims %>% dharma_residuals(test_mod)
 
		write_rds(test_mod, 
			file = paste0(here(), file_path_all, 
				"age0_btemp_no_int_mod_", "pollock", ".rds"))
	AIC(test_mod)
	
	
	#### compare models ####
	
	# pollock 
  lapply(pol_mod_list, AIC)
	# age0
		# temp: int model did not run
		# oxy: int model better
	# presurvey
		# temp: int model better
		# oxy: int model did not run
	# yrprior
		# temp: int model better
		# oxy: int model did not run
	# also - 
	
	# pcod
	lapply(pcod_mod_list, AIC)
	# age0
		# temp: int model did not run
		# oxy: int model did not run
	# presurvey
		# temp: int model did not run
		# oxy: int model did not run
	# yrprior
		# temp: int model better
		# oxy: int model did not run
	 
	# yfin
	lapply(yfin_mod_list, AIC)
	# age0
		# temp: int model did not run
		# oxy: no int model better
	# presurvey
		# temp: int model did not run
		# oxy: int model did not run
	# yrprior
		# temp: int model did not run
		# oxy: int model better
	
	# for temp metric
	
	pol_temp_comp <- mod_list[grep("temp", names(pol_mod_list))]

	lapply(pol_temp_comp, AIC)
	
	
	########################################
	# models with an interaction of temp
	#######################################
	
	sdmTMB_two_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(species == sp)
  			
					# order age class
  				levels_to_ord <- sort(unique(new_dat$age_f))
  				new_dat$age_f_ord <- ordered(new_dat$age_f, levels = c(levels_to_ord))	
  				
  				# drop unused factor levels of year (error when do this outside function)
  				new_dat$year_f <- droplevels(new_dat$year_f)
  				
					# make mesh
					mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
				
					# for mod name
					mod_name <- "_two_int_mod_"
					
					# set up which sanity checks are not really issues (but will check residual plots)
					not_probs <- c(5, 6) # gradients_ok & se_magnitude_ok
	
		# run models	
		
					print(paste('running two int model for', sp, "with", y))
		
					# set up formulas
					form1 <- paste0("log_wt_std ~ s(" x , y, ")")
			
 					# model without interaction 
					mod <- 
						try(
							sdmTMB(
								formula = as.formula(form1),
								data = new_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3)))

					# run and index sanity checks for later optimization
					s <- unlist(sanity(mod))
					s
		
					ind <- which(s == FALSE) %>% as.numeric()
	
		# deal with warnings and issues
					
					# if error, tell me
						if (class(mod) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (sanity(mod)[[9]] == "TRUE") { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod, 
									file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 						
		 						print(paste("no int model for", sp, "with", y, "complete"))

		 		 # if no error and sanity() does not look good aside from a few non-issues (see above), rerun model with extra optimization
		 						
						} else if (any(ind %!in% not_probs)) { 
					
		 						print('running extra optimization')
								
								mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 1)) 
								
								s_eo <- unlist(sanity(mod_eo))
								s_eo
		
								ind_eo <- which(s_eo == FALSE) %>% as.numeric()
								
	
								# if model with extra optimization (eo) threw an error, rerun with no newton loops
 										if (class(mod_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 0)) 
							
											s_eo <- unlist(sanity(mod_eo))
											s_eo
		
											ind_eo <- which(s_eo == FALSE) %>% as.numeric()
												
 													 if (any(ind_eo %!in% not_probs)) { # this is not working as intended
											
															print(paste0('boo - check sanity for no int model for', sp, "with", y))
 													 	
 													 } else {
 													 	
 													 		write_rds(mod_eo, 
 													 							file = paste0(here(), 
 													 							file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 						
 												   }
							
								# if model with extra optimization (eo) looks all good, save model
										} else if (sanity(mod_eo)$all_ok == "TRUE") {
		 				 	
		 									write_rds(mod_eo, 
												file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 						
		 									print(paste("no int model for", sp, "with", y, "complete"))
		 									
		 						# if model with extra optimization (eo) does not look all good aside from a few non-issues, tell me
										} else if (any(ind_eo %!in% not_probs)) { 
											
												print(paste0('boo - check sanity for no int model for', sp, "with", y))

												write_rds(mod_eo, 
													file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 									
											
		 					 # if model with extra optimization (eo) looks all good aside from a few non-issues, save model
										} else {
								
												write_rds(mod_eo, 
													file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 									
										}
								
											
							# if original model object before optimization (mod_int) looks good aside from a few non-issues, save model			
									 	
					  	 } else {
							
    								 		write_rds(mod_eo, 
													file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 													
		 										print(paste("no int model for", sp, "with", y, "complete"))
		 										
					  } 
	}
	
	
	# run function
	
	sp <- unique(dat_all$species)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_no_int_func)
