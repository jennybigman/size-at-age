# sdmTMB function - with CV, knots specified, and year as RE

	lat_quantiles <- function(df){
		
		first_q <- quantile(df$latitude, 0)
		sec_q <- quantile(df$latitude, 0.25)
		third_q <- quantile(df$latitude, 0.5)
		fourth_q <- quantile(df$latitude, 0.75)
		fifth_q <- 	quantile(df$latitude, 1)

		df <- df %>%
			mutate(fold = case_when(
				between(latitude, first_q, sec_q) ~ 1,
				between(latitude, sec_q, third_q) ~ 2,
				between(latitude, third_q, fourth_q) ~ 3,
				between(latitude, fourth_q, fifth_q) ~ 4))
	}
	
	dat_list <- list(pollock_dat, pcod_dat_trim, yfinsole_dat)
	
	dat_all <- lapply(dat_list, lat_quantiles) %>% bind_rows()
	
	# remove any rows with no age0 environmental data (the year in which some individuals were age 0 was prior to 1970, first year of hindcast)
	#dat_all <- dat_all %>%
	#	drop_na(age0_boxy)
	
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/with year as RE/"
	
	#### fit models without interaction ####
	
	# function to fit multiple models with no spatiotemporal RE but year RE
	
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
					mod_name <- "_no_int_mod_cv_"
					
					# set up which sanity checks are not really issues (but will check residual plots)
					#not_probs <- c(5, 6) # gradients_ok & se_magnitude_ok
	
		# run models	
		
					print(paste('running no int model for', sp, "with", y))
		
					# set up formulas
					form1 <- paste0("log_wt_std ~ s(" , y, ", k = 4) + (1|year_f)")
	
			
 					# model without interaction 
					mod <- 
						try(
							sdmTMB_cv(
								formula = as.formula(form1),
								data = new_dat,
								mesh = mesh,
								k_folds = max(new_dat$fold),
        				fold_ids = new_dat$fold,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3)))

	
		# deal with warnings and issues
					
					# if error, tell me
						if (class(mod) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (sanity(mod$models[[1]])$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod, 
									file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
		 						
		 						print(paste("no int model for", sp, "with", y, "complete"))
		 	
					  } else if 
					  			 (sanity(mod$models[[1]])$hessian_ok ==      "TRUE" &
						  			sanity(mod$models[[1]])$eigen_values_ok == "TRUE" &
					  				sanity(mod$models[[1]])$nlminb_ok ==       "TRUE" &
					  				sanity(mod$models[[1]])$range_ok ==        "TRUE" &
					  				sanity(mod$models[[1]])$se_na_ok ==        "TRUE" &
					  				sanity(mod$models[[1]])$sigmas_ok ==       "TRUE") {
	
							write_rds(mod, 
										file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
									
							print(paste("no int model for", sp, "with", y, "complete"))

		 				
		 				} else if 
							(sanity(mod$models[[1]])$hessian_ok      != "TRUE" |
						 	 sanity(mod$models[[1]])$eigen_values_ok != "TRUE" |
					  	 sanity(mod$models[[1]])$nlminb_ok       != "TRUE" |
					  	 sanity(mod$models[[1]])$range_ok        != "TRUE" |
					  	 sanity(mod$models[[1]])$se_na_ok        != "TRUE" |
					  	 sanity(mod$models[[1]])$sigmas_ok       != "TRUE" ) {
	
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
												 (sanity(mod_eo$models[[1]])$hessian_ok ==      "TRUE" &
						  						sanity(mod_eo$models[[1]])$eigen_values_ok == "TRUE" &
					  							sanity(mod_eo$models[[1]])$nlminb_ok ==       "TRUE" &
					  							sanity(mod_eo$models[[1]])$range_ok ==        "TRUE" &
					  							sanity(mod_eo$models[[1]])$se_na_ok ==        "TRUE" &
					  							sanity(mod_eo$models[[1]])$sigmas_ok ==       "TRUE") {
	
											print(paste("no int model for", sp, "with", y, "complete"))

											write_rds(mod_eo, 
														file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
													

											 } else {
												 	print('boo - extra optimization did not solve the issue(s)')
											 }
		 								
							# if original model object before optimization (mod_int) looks good aside from a few non-issues, save model			
									 	
					  	 } else if 
					  			 (sanity(mod$models[[1]])$hessian_ok ==      "TRUE" &
						  			sanity(mod$models[[1]])$eigen_values_ok == "TRUE" &
					  				sanity(mod$models[[1]])$nlminb_ok ==       "TRUE" &
					  				sanity(mod$models[[1]])$range_ok ==        "TRUE" &
					  				sanity(mod$models[[1]])$se_na_ok ==        "TRUE" &
					  				sanity(mod$models[[1]])$sigmas_ok ==       "TRUE") {
	
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
					
		# run models	
		
					print(paste('running int model for', sp, "with", y))
		
					# set up formulas
					form2 <- paste0("log_wt ~ age_f_ord + s(" , y, ", by = age_f_ord, k = 4) + (1|year_f)")

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
	
	
	##### READ IN MODELS AND CHECKS ####
	
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
	