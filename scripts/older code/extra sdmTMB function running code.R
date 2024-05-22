# extra sdmTMB function running code

	####### check
	
	presurvey_boxy_int_mod_pollock <- read_rds(paste0(here("./output/model output/sdmTMB output/APR 2024 NEW/presurvey_boxy_int_mod_pollock.rds")))

	# run models that didn't converge ####
	
	# 1. yrprior boxy pcod
	
	## pcod mesh 
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
	
 	# model with interaction 
	yfin_dat <- yfin_dat %>%
		mutate(log_wt_c = as.vector(scale(log_wt, scale = FALSE)))
	
	yrprior_boxy_int_mod_yfin <- 
							sdmTMB(
								formula = log_wt ~ 0 + age_f * (yrprior_boxy + I(yrprior_boxy^2) + I(yrprior_boxy^3)),
								data = yfin_dat,
								mesh = yfin_mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2023:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 350, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 600, sigma_lt = 2)))#,
							#	control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
					
					
	s <- sanity(yrprior_boxy_int_mod_yfin, gradient_thresh = 0.05)
	
	
		# get data 
	mod_oxy_data <- pluck(yrprior_boxy_int_mod_yfin, 'data')

	# new dat for prediction
	new_dat_oxy_hind <- expand_grid(
		yrprior_boxy = seq(
			from = min(mod_oxy_data$yrprior_boxy),
			to = max(mod_oxy_data$yrprior_boxy),
			length.out = 25),
		age_f = sort(unique(mod_oxy_data$age_f)),
		year = 2004) # predict for one year b/c all the same and faster

	# predict
	oxy_hind_preds <- predict(
		yrprior_boxy_int_mod_yfin,
		newdata = new_dat_oxy_hind,
		se_fit = TRUE,
		re_form = NA,
		return_tmb_object = FALSE)
	 		
	# add se estimates
	oxy_hind_preds <- oxy_hind_preds %>% 
		mutate(low = est - est_se,
					 high = est + est_se,
					 species = "yfin")
	
 		
	p <- 
		ggplot(oxy_hind_preds, aes(yrprior_boxy, est)) +
		geom_ribbon(aes(ymin = low, ymax = high), 
								fill = "lightgrey", alpha = 0.4) +
		geom_line(color = "black") +
		facet_wrap( ~ age_f, scales = "free") +
		ylab("partial effect of\n(log) weight") +
		xlab("bottom oxygen\n(averaged June - June)") +
		theme_sleek()

	
	# without
	presurvey_boxy_no_int_mod_yfin <- 
							sdmTMB(
								formula = log_wt_c ~ 0 + age_f + s(presurvey_boxy, k = 3),
								data = yfin_dat,
								mesh = yfin_mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2023:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 350, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 600, sigma_lt = 2)))#,
							#	control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
					
					
	s <- sanity(presurvey_boxy_no_int_mod_yfin, gradient_thresh = 0.05)
	
	
	###### with smooth
		yrprior_boxy_int_mod_yfin <- 
							sdmTMB(
								formula = log_wt ~ 0 + age_f  + s(yrprior_boxy, by = age_f, k = 4),
								data = yfin_dat,
								mesh = yfin_mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2023:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 350, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 600, sigma_lt = 2)))#,

	sanity(yrprior_boxy_int_mod_yfin)
	###################################################################
	# for yfin, center response for convergence ####
	##################################################################
	
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/APR 2024 NEW/"
	
	# models without interaction ####
	
	sdmTMB_no_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp) %>%
						mutate(log_wt_c = as.vector(scale(log_wt, scale = FALSE)))
  			
					# assign mesh
					if (sp == "pcod") {
						
						mesh <- pcod_mesh
					
					} else if (sp == "pollock") {
							
						mesh <- pol_mesh
					
					} else if (sp == "atooth") {
						
						mesh <- atooth_mesh
						
					} else {
						
						mesh <- yfin_mesh
					
					}
  				
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
								extra_time = 2020:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
					s <- sanity(mod_no_int, gradient_thresh = 0.05)
	
	
		 						write_rds(mod_no_int, 
									file = paste0(here(), file_path_all, y, mod_name, sp, ".rds"))
		 						
		 						print(paste("no int model for", sp, "with", y, "complete"))
		 	
	}
	
	
	# run function

	#sp <- unique(dat_all$short_name)
	
	vars <- dat_all %>%
		ungroup %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 


	#df_func <- expand_grid(
	#	sp = sp,
	#	y = vars
	#)
	
	df_func <- expand_grid(
		sp = "yfin",
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_no_int_func)

	
	# models with an interaction ####
	
	sdmTMB_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp) %>%
						mutate(log_wt_c = as.vector(scale(log_wt, scale = FALSE)))
  			
					# assign mesh
					if (sp == "pcod") {
						
						mesh <- pcod_mesh
					
					} else if (sp == "pollock") {
							
						mesh <- pol_mesh
					
					} else if (sp == "atooth") {
						
						mesh <- atooth_mesh
						
					} else {
						
						mesh <- yfin_mesh
					
					}
  				
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


	#sp <- unique(dat_all$short_name)
	
	vars <- dat_all %>%
		ungroup %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 


	#df_func <- expand_grid(
	#	sp = sp,
	#	y = vars
	#)
	
	df_func <- expand_grid(
		sp = "yfin",
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_int_func)
	