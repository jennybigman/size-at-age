# sdmTMB function - with CV, knots specified, 
	

	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/Jan 2024/spatially_exp_temp/"
	
	#### fit models without interaction ####
	
	sdmTMB_no_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp)
  			
  				# drop unused factor levels of year (error when do this outside function)
  				new_dat$year_f <- droplevels(new_dat$year_f)
  				
					# make mesh
					mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), cutoff = 20)
				
					# for mod name
					mod_name <- "_no_int_mod_"
					
		# run models	
		
					print(paste('running no int model for', sp, "with", y))
		
					# set up formulas
					form_no_int <- paste0("log_wt ~ 0 + age_f + s(" , y, ", k = 3)")

 					# model without interaction 
					mod_no_int <- 
						try(
							sdmTMB(
								formula = as.formula(form_no_int),
								data = new_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2020:2099))
					
					s <- sanity(mod_no_int, gradient_thresh = 0.05)
	
		# deal with warnings and issues
					
					# if error, tell me
						if (class(mod_no_int) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (s$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod_no_int, 
									file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
		 						
		 						print(paste("no int model for", sp, "with", y, "complete"))
		 	
					  } else if 
					  			 (s$hessian_ok ==      "TRUE" &
						  			s$eigen_values_ok == "TRUE" &
					  				s$nlminb_ok ==       "TRUE" &
					  				s$range_ok ==        "TRUE" &
					  				s$se_na_ok ==        "TRUE" &
					  				s$sigmas_ok ==       "TRUE") {
	
							write_rds(mod_no_int, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
							print(paste("no int model for", sp, "with", y, "complete"))

		 				
		 				} else if 
							(s$hessian_ok      != "TRUE" |
						 	 s$eigen_values_ok != "TRUE" |
					  	 s$nlminb_ok       != "TRUE" |
					  	 s$range_ok        != "TRUE" |
					  	 s$se_na_ok        != "TRUE" |
					  	 s$sigmas_ok       != "TRUE" ) {
	
									print('running extra optimization')
								
									mod_eo <- try(run_extra_optimization(mod_no_int, nlminb_loops = 3, newton_loops = 1)) 
							
											# if model with extra optimization (eo) threw an error, rerun with no newton loops
 											if (class(mod_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_eo <- try(run_extra_optimization(mod_no_int, nlminb_loops = 3, newton_loops = 0)) 
 											
											} else if (s$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
													print(paste("no int model for", sp, "with", y, "complete"))
		 											
													write_rds(mod_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
					
											} else if
												 (s$hessian_ok ==      "TRUE" &
						  						s$eigen_values_ok == "TRUE" &
					  							s$nlminb_ok ==       "TRUE" &
					  							s$range_ok ==        "TRUE" &
					  							s$se_na_ok ==        "TRUE" &
					  							s$sigmas_ok ==       "TRUE") {
	
											print(paste("no int model for", sp, "with", y, "complete"))

											write_rds(mod_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
													

											 } else {
												 	print('boo - extra optimization did not solve the issue(s)')
											 }
		 								
							# if original model object before optimization (mod_int) looks good aside from a few non-issues, save model			
									 	
					  	 } else if 
					  			 (s$hessian_ok ==      "TRUE" &
						  			s$eigen_values_ok == "TRUE" &
					  				s$nlminb_ok ==       "TRUE" &
					  				s$range_ok ==        "TRUE" &
					  				s$se_na_ok ==        "TRUE" &
					  				s$sigmas_ok ==       "TRUE") {
	
							write_rds(mod, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
									print(paste("no int model for", sp, "with", y, "complete"))

		 									
					 	 } else {
					   	
									print(paste("boo - no int model for", sp, "with", y, "has issues"))

					  	 } 
	}
	
	
	# run function

	sp <- unique(dat_all$short_name)
	
	#vars <- dat_all %>%
	#	select(contains(c("btemp", "boxy"))) %>%
	#	names() 
	
	# just temp
	vars <- dat_all %>%
		select(contains("temp")) %>%
		names() 


	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_no_int_func)

	
	#### fit models with an interaction ####
	
	sdmTMB_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp)
  			
  				# drop unused factor levels of year (error when do this outside function)
  				new_dat$year_f <- droplevels(new_dat$year_f)
  				
					# make mesh
					mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), cutoff = 30) 
				
					# for mod name
					mod_name <- "_int_mod_"
					
		# run models	
		
					print(paste('running int model for', sp, "with", y))
		
					# set up formulas
					form_int <- paste0("log_wt ~ 0 + age_f + s(" , y, ", by = age_f, k = 3)")

 					# model without interaction 
					mod_int <- 
						try(
							sdmTMB(
								formula = as.formula(form_int),
								data = new_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2020:2099,
								priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 200, sigma_lt = 2),
													matern_s = pc_matern(range_gt = 200, sigma_lt = 2))))
					
					s <- sanity(mod_int, gradient_thresh = 0.05)
	
		# deal with warnings and issues
					
					# if error, tell me
						if (class(mod_int) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (s$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod_int, 
									file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
		 						
		 						print(paste("int model for", sp, "with", y, "complete"))
		 	
					  } else if 
					  			 (s$hessian_ok ==      "TRUE" &
						  			s$eigen_values_ok == "TRUE" &
					  				s$nlminb_ok ==       "TRUE" &
					  				s$range_ok ==        "TRUE" &
					  				s$se_na_ok ==        "TRUE" &
					  				s$sigmas_ok ==       "TRUE") {
	
							write_rds(mod_int, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
							print(paste("int model for", sp, "with", y, "complete"))

		 				
		 				} else if 
							(s$hessian_ok      != "TRUE" |
						 	 s$eigen_values_ok != "TRUE" |
					  	 s$nlminb_ok       != "TRUE" |
					  	 s$range_ok        != "TRUE" |
					  	 s$se_na_ok        != "TRUE" |
					  	 s$sigmas_ok       != "TRUE" ) {
	
									print('running extra optimization')
								
									mod_int_eo <- try(run_extra_optimization(mod_int, nlminb_loops = 3, newton_loops = 1)) 
							
											# if model with extra optimization (eo) threw an error, rerun with no newton loops
 											if (class(mod_int_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_int_eo <- try(run_extra_optimization(mod_int, nlminb_loops = 3, newton_loops = 0)) 
 											
											} else if (s$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
													print(paste("int model for", sp, "with", y, "complete"))
		 											
													write_rds(mod_int_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
					
											} else if
												 (s$hessian_ok ==      "TRUE" &
						  						s$eigen_values_ok == "TRUE" &
					  							s$nlminb_ok ==       "TRUE" &
					  							s$range_ok ==        "TRUE" &
					  							s$se_na_ok ==        "TRUE" &
					  							s$sigmas_ok ==       "TRUE") {
	
											print(paste("int model for", sp, "with", y, "complete"))

											write_rds(mod_int_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
													

											 } else {
												 	print('boo - extra optimization did not solve the issue(s)')
											 }
		 								
							# if original model object before optimization (mod_int) looks good aside from a few non-issues, save model			
									 	
					  	 } else if 
					  			 (s$hessian_ok ==      "TRUE" &
						  			s$eigen_values_ok == "TRUE" &
					  				s$nlminb_ok ==       "TRUE" &
					  				s$range_ok ==        "TRUE" &
					  				s$se_na_ok ==        "TRUE" &
					  				s$sigmas_ok ==       "TRUE") {
	
							write_rds(mod_int, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
									print(paste("int model for", sp, "with", y, "complete"))

		 									
					 	 } else {
					   	
									print(paste("boo - int model for", sp, "with", y, "has issues"))

					  	 } 
	}
	
	
	# run function

	sp <- unique(dat_all$short_name)
	
#	vars <- dat_all %>%
#		select(contains(c("btemp", "boxy"))) %>%
#		names() 
	
	# just temp
	vars <- dat_all %>%
		select(contains("btemp")) %>%
		names() 

	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_int_func)
	
	

	## presurvey temp and yfin - model didn't converge
	#
	yfinsole_dat <- dat_all %>% 
		filter(short_name == "yfin")
	
	yfinsole_dat$age_f <- droplevels(yfinsole_dat$age_f)
	
	yfinsole_dat <- yfinsole_dat %>%
		mutate(wt_kg = weight/1000,
					 log_wt_kg = log10(wt_kg))
		
	mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), cutoff = 20)
		
			presurvey_btemp_no_int_mod_yfin <-
				sdmTMB(
					formula = log_wt ~ 0 + s(presurvey_btemp, by = age_f, k = 3),
					data = yfinsole_dat,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "iid",
					time = "year",
					share_range = FALSE,
					control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 0),
					priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 450, sigma_lt = 1),
														matern_s = pc_matern(range_gt = 125, sigma_lt = 1)))
			

	sanity(presurvey_btemp_no_int_mod_yfin)

	write_rds(presurvey_btemp_no_int_mod_yfin, file = paste0(here(), file_path_all, "presurvey_btemp_no_int_mod_yfin.rds"))


	####### pollock ####
	
	pollock_dat <- dat_all %>% 
		filter(short_name == "pollock")
	
	pollock_dat$age_f <- droplevels(pollock_dat$age_f)
		
	#mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
	mesh <- make_mesh(
		pollock_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 60,
			max.edge = 60,
			offset = c(60, 60)
		)
	
	plot(mesh)
	
	yrprior_btemp_int_mod_pollock <-
				sdmTMB(
					formula = log_wt ~ 0 + age_f + s(yrprior_btemp, by = age_f, k = 3),
					data = pollock_dat,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "iid",
					time = "year",
					#extra_time = 2020:2099,
					silent = FALSE,
					control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 0))
			

	sanity(yrprior_btemp_int_mod_pollock)

	write_rds(presurvey_btemp_no_int_mod_yfin, file = paste0(here(), file_path_all, "presurvey_btemp_no_int_mod_yfin.rds"))

