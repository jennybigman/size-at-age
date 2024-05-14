# sdmTMB function - without CV, knots specified, 
	
	# mesh for each species - best to do this visually for model convergence

	## pollock mesh ####
	pollock_dat <- dat_all %>% 
		filter(short_name == "pollock")
	
	pollock_dat$age_f <- droplevels(pollock_dat$age_f)
		
	pol_mesh <- make_mesh(
		pollock_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 60,
			offset = c(70, 60)
		)
	
	plot(pol_mesh)
	
	## pcod mesh ####
	pcod_dat <- dat_all %>% 
		filter(short_name == "pcod")
	
	pcod_dat$age_f <- droplevels(pcod_dat$age_f)
		
	pcod_mesh <- make_mesh(
		pcod_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 30,
			offset = c(60, 70)
		)
	
	plot(pcod_mesh)
	
	## yellowfin mesh ####
	yfin_dat <- dat_all %>% 
		filter(short_name == "yfin")
	
	yfin_dat <- yfin_dat %>%
		filter(between(age, 3, 20))
	
	yfin_dat$age_f <- droplevels(yfin_dat$age_f)
		
	yfin_mesh <- make_mesh(
		yfin_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 60,
			offset = c(60, 70)
		)
	
	plot(yfin_mesh)
	
	dat_all <- list(pollock_dat, pcod_dat, yfin_dat) %>% bind_rows()

		
	# center response to help with convergence
	
	dat_all <- dat_all %>%
		group_by(species_name) %>%
		mutate(log_wt_c = as.vector(scale(log_wt, scale = FALSE)))

	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/Feb 2024 - NN/student/"
	
	#### fit models without interaction ####
	
	sdmTMB_no_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp)
  			
  				# drop unused factor levels of year (error when do this outside function)
  				new_dat$year_f <- droplevels(new_dat$year_f)
  				
					# assign mesh
					if (sp == "pcod") {
						
						mesh <- pcod_mesh
					
					} else if (sp == "pollock") {
							
						mesh <- pol_mesh
						
					} else {
						
						mesh <- yfin_mesh
					
					}
  				
					# for mod name
					mod_name <- "_no_int_mod_student_"
					
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
								family = student(),
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2020:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
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
	
	vars <- dat_all %>%
		ungroup %>%
		select(contains(c("btemp", "boxy"))) %>%
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
					mod_name <- "_int_mod_student_"
					
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
								family = student(),
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2020:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
					
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
	
	vars <- dat_all %>%
		ungroup() %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_int_func)
	
	# work on models that did not converge ####
	
	yfinsole_dat <- dat_all %>% 
		filter(short_name == "yfin")
	
	yfinsole_dat$age_f <- droplevels(yfinsole_dat$age_f)
	
	yfinsole_dat <- yfinsole_dat %>%
		mutate(wt_kg = weight/1000,
					 log_wt_kg = log10(wt_kg),
					 log_wt_kg_c = as.vector(scale(log_wt_kg, scale = FALSE)))
		
	yfin_mesh <- make_mesh(
		yfin_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			max.edge = 60,
			offset = c(70, 90)
		)
	
	plot(yfin_mesh)
	
	presurvey_boxy_int_mod_yfin <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f + s(presurvey_boxy, by = age_f, k = 3),
			data = yfinsole_dat,
			family = student(),
			mesh = yfin_mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			#extra_time = 2020:2099,
			share_range = FALSE,
			silent = FALSE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 400, sigma_lt = 2)))

	sanity(presurvey_boxy_int_mod_yfin)

	write_rds(presurvey_boxy_int_mod_yfin, 
						file = paste0(here(), 
						file_path_all, "presurvey_boxy_int_mod_yfin.rds"))

	
	
	###############################################################
	
	# AICs
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/student/")))

	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/student/"))
  

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  st_mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 s <- sanity(x)
  }
  
	s <- lapply(st_mod_list, sanity_func)
	
	
	# separate models by species #
  
	# pollock #
	st_pol_mod_list <- st_mod_list[grep("pol", names(st_mod_list))]
	
	# pcod #
	st_pcod_mod_list <- st_mod_list[grep("pcod", names(st_mod_list))]
		
	# yfin #
	st_yfin_mod_list <- st_mod_list[grep("yfin", names(st_mod_list))]
	
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	sp_mod_lists_st <- list(st_pol_mod_list, st_pcod_mod_list, st_yfin_mod_list)

	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = sp_mod_lists_st,
		y = vars
	)

	st_AICs <- map2(df_func$x, df_func$y, AIC_func)

	st_AIC_df <-	st_AICs %>% 
 		bind_cols() %>%
 		pivot_longer(cols = contains(c("temp", "oxy")),
 							 names_to = 'model', values_to = "AIC") %>%
 		rename(AIC_st = AIC)
 
 
 
	################ CHANGE BELOW HERE ##############################
	
		
	pollock_dat <- dat_all %>% 
		filter(short_name == "pollock")
	
	pol_mesh <- make_mesh(
		pollock_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 70,
			max.edge = 90,
			offset = c(70, 90)
		)
	
	plot(pol_mesh)
	
	presurvey_boxy_int_mod_pollock <-
		sdmTMB(
			formula = log_wt_c ~ 0 + age_f + s(presurvey_boxy, by = age_f, k = 3),
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			extra_time = 2020:2099,
			share_range = FALSE,
			silent = FALSE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 800, sigma_lt = 2)))

	sanity(presurvey_boxy_int_mod_pollock)

	write_rds(presurvey_boxy_int_mod_pollock, 
						file = paste0(here(), 
						file_path_all, "presurvey_boxy_int_mod_pollock.rds"))


