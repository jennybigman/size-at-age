# pacific cod workshop

# sdmTMB function - with CV, knots specified, and year as RE
		
		first_q <- quantile(pcod_dat_trim$latitude, 0)
		sec_q <- quantile(pcod_dat_trim$latitude, 0.25)
		third_q <- quantile(pcod_dat_trim$latitude, 0.5)
		fourth_q <- quantile(pcod_dat_trim$latitude, 0.75)
		fifth_q <- 	quantile(pcod_dat_trim$latitude, 1)

		pcod_dat_trim <- pcod_dat_trim %>%
			mutate(fold = case_when(
				between(latitude, first_q, sec_q) ~ 1,
				between(latitude, sec_q, third_q) ~ 2,
				between(latitude, third_q, fourth_q) ~ 3,
				between(latitude, fourth_q, fifth_q) ~ 4))
	
	

	# remove any rows with no age0 environmental data (the year in which some individuals were age 0 was prior to 1970, first year of hindcast)
	#dat_all <- dat_all %>%
	#	drop_na(age0_boxy)
	
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/pcod/"
	
	#### fit models without interaction ####
	
	# function to fit multiple models with no spatiotemporal RE but year RE
	
	sdmTMB_no_int_func <- function(y){
		
		# wrangling and making a mesh 
		
					# order age class
  				levels_to_ord <- sort(unique(pcod_dat_trim$age_f))
  				pcod_dat_trim$age_f_ord <- ordered(pcod_dat_trim$age_f, levels = c(levels_to_ord))	
  				
  				# drop unused factor levels of year (error when do this outside function)
  				pcod_dat_trim$year_f <- droplevels(pcod_dat_trim$year_f)
  				
					# make mesh
					mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
				
					# for mod name
					mod_name <- "_no_int_mod_cv_"
					
		# run models	
		
					print(paste('running no int model for', "pcod", "with", y))
		
					# set up formulas
					form1 <- paste0("log_wt ~ age_f_ord + s(" , y, ", k = 4)")
	
			
 					# model without interaction 
					mod <- 
						try(
							sdmTMB_cv(
								formula = as.formula(form1),
								data = pcod_dat_trim,
								mesh = mesh,
								k_folds = max(pcod_dat_trim$fold),
        				fold_ids = pcod_dat_trim$fold,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3)))
					
					s <- sanity(mod$models[[1]], gradient_thresh = 0.05)
	
		# deal with warnings and issues
					
					# if error, tell me
						if (class(mod) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (s$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod, 
									file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
		 						
		 						print(paste("no int model for", "pcod", "with", y, "complete"))
		 	
					  } else if 
					  			 (s$hessian_ok ==      "TRUE" &
						  			s$eigen_values_ok == "TRUE" &
					  				s$nlminb_ok ==       "TRUE" &
					  				s$range_ok ==        "TRUE" &
					  				s$se_na_ok ==        "TRUE" &
					  				s$sigmas_ok ==       "TRUE") {
	
							write_rds(mod, 
										file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
									
							print(paste("no int model for", "pcod", "with", y, "complete"))

		 				
		 				} else if 
							(s$hessian_ok      != "TRUE" |
						 	 s$eigen_values_ok != "TRUE" |
					  	 s$nlminb_ok       != "TRUE" |
					  	 s$range_ok        != "TRUE" |
					  	 s$se_na_ok        != "TRUE" |
					  	 s$sigmas_ok       != "TRUE" ) {
	
									print('running extra optimization')
								
									mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 1)) 
							
											# if model with extra optimization (eo) threw an error, rerun with no newton loops
 											if (class(mod_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 0)) 
 											
											} else if (sanity(mod_eo)$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
													print(paste("no int model for", "pcod", "with", y, "complete"))
		 											
													write_rds(mod_eo, 
														file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
					
											} else if
												 (s$hessian_ok ==      "TRUE" &
						  						s$eigen_values_ok == "TRUE" &
					  							s$nlminb_ok ==       "TRUE" &
					  							s$range_ok ==        "TRUE" &
					  							s$se_na_ok ==        "TRUE" &
					  							s$sigmas_ok ==       "TRUE") {
	
											print(paste("no int model for", "pcod", "with", y, "complete"))

											write_rds(mod_eo, 
														file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
													

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
										file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
									
									print(paste("no int model for", "pcod", "with", y, "complete"))

		 									
					 	 } else {
					   	
									print(paste("boo - no int model for", "pcod", "with", y, "has issues"))

					  	 } 
	}
	
	
	# run function
	
	vars <- pcod_dat_trim %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()


	map(vars, sdmTMB_no_int_func)

	
	###################################################################################
	# Models with a factor-smooth interaction ####
	###################################################################################
	
	sdmTMB_int_func <- function(y){
		
		# wrangling and making a mesh 
		
					# order age class
  				levels_to_ord <- sort(unique(pcod_dat_trim$age_f))
  				pcod_dat_trim$age_f_ord <- ordered(pcod_dat_trim$age_f, levels = c(levels_to_ord))	
  				
  				# drop factor levels of year
  				pcod_dat_trim$year_f <- droplevels(pcod_dat_trim$year_f)
  				
  				# make mesh
					mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
				
					# set up prior
					pc <- pc_matern(range_gt = 300, sigma_lt = 0.4)
					
					# mod name
					mod_name <- "_int_mod_"
					
		# run models	
		
					print(paste('running int model for', "pcod", "with", y))
		
					# set up formulas
					form2 <- paste0("log_wt ~ age_f_ord + s(" , y, ", by = age_f_ord, k = 4)")

 					# model with interaction 
					mod_int <- 
						try(
							sdmTMB_cv(
								formula = as.formula(form2),
								data = pcod_dat_trim,
								mesh = mesh,
								k_folds = max(pcod_dat_trim$fold),
        				fold_ids = pcod_dat_trim$fold,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3)))
					

					# deal with warnings and issues
					
					# if error, tell me
						if (class(mod_int) == "try-error"){
		 						print(paste("error!"))
		 			
					# if no error and sanity() checks all good, save model and tell me

						} else if (
							sanity(mod_int$models[[1]], gradient_thresh = 0.05)$all_ok == "TRUE"
						
							)  { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod_int, 
									file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
		 						
		 						print(paste("int model for", "pcod", "with", y, "complete"))
		 	
					  } else if 
					  			 (sanity(mod_int$models[[1]], gradient_thresh = 0.05)$hessian_ok ==      "TRUE" &
						  			sanity(mod_int$models[[1]], gradient_thresh = 0.05)$eigen_values_ok == "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$nlminb_ok ==       "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$range_ok ==        "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$se_na_ok ==        "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$sigmas_ok ==       "TRUE") {
	
							write_rds(mod_int, 
										file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
									
							print(paste("int model for", "pcod", "with", y, "complete"))

		 				
		 				} else if 
							(sanity(mod_int$models[[1]], gradient_thresh = 0.05)$hessian_ok      != "TRUE" |
						 	 sanity(mod_int$models[[1]], gradient_thresh = 0.05)$eigen_values_ok != "TRUE" |
					  	 sanity(mod_int$models[[1]], gradient_thresh = 0.05)$nlminb_ok       != "TRUE" |
					  	 sanity(mod_int$models[[1]], gradient_thresh = 0.05)$range_ok        != "TRUE" |
					  	 sanity(mod_int$models[[1]], gradient_thresh = 0.05)$se_na_ok        != "TRUE" |
					  	 sanity(mod_int$models[[1]], gradient_thresh = 0.05)$sigmas_ok       != "TRUE" ) {
	
									print('running extra optimization')
								
									mod_int_eo <- try(run_extra_optimization(mod_int, nlminb_loops = 3, newton_loops = 1)) 
							
											# if model with extra optimization (eo) threw an error, rerun with no newton loops
 											if (class(mod_int_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_int_eo <- try(run_extra_optimization(mod_int, nlminb_loops = 3, newton_loops = 0)) 
 											
											} else if (s$all_ok == "TRUE")  { # if all sanity checks are good, save model
		 	 	
													print(paste("int model for", "pcod", "with", y, "complete"))
		 											
													write_rds(mod_int_eo, 
														file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
					
											} else if
												 (sanity(mod_int$models[[1]], gradient_thresh = 0.05)$hessian_ok ==      "TRUE" &
						  						sanity(mod_int$models[[1]], gradient_thresh = 0.05)$eigen_values_ok == "TRUE" &
					  							sanity(mod_int$models[[1]], gradient_thresh = 0.05)$nlminb_ok ==       "TRUE" &
					  							sanity(mod_int$models[[1]], gradient_thresh = 0.05)$range_ok ==        "TRUE" &
					  							sanity(mod_int$models[[1]], gradient_thresh = 0.05)$se_na_ok ==        "TRUE" &
					  							sanity(mod_int$models[[1]], gradient_thresh = 0.05)$sigmas_ok ==       "TRUE") {
	
											print(paste("int model for", "pcod", "with", y, "complete"))

											write_rds(mod_int_eo, 
														file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
													

											 } else {
												 	print(paste('boo - extra optimization did not solve the issue(s)',
												 							" for int model for", "pcod", "with", y))
											 }
		 								
							# if original model object before optimization (mod_int) looks good aside from a few non-issues, save model			
									 	
					  	 } else if 
					  			 (sanity(mod_int$models[[1]], gradient_thresh = 0.05)$hessian_ok ==      "TRUE" &
						  			sanity(mod_int$models[[1]], gradient_thresh = 0.05)$eigen_values_ok == "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$nlminb_ok ==       "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$range_ok ==        "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$se_na_ok ==        "TRUE" &
					  				sanity(mod_int$models[[1]], gradient_thresh = 0.05)$sigmas_ok ==       "TRUE") {
	
							write_rds(mod_int, 
										file = paste0(here(), file_path_all, y, mod_name, "pcod", ".rds"))
									
									print(paste("int model for", "pcod", "with", y, "complete"))

		 									
					  	 } else {
					  	 	
									print(paste("boo - int model for", "pcod", "with", y, "has issues"))

					  	 } 
	}

		
	
	# run function
	
	vars <- pcod_dat_trim %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()

	map(vars, sdmTMB_int_func)
	
	### run pcod model with yrprior_btemp
	
	levels_to_ord <- sort(unique(pcod_dat_trim$age_f))
  pcod_dat_trim$age_f_ord <- ordered(pcod_dat_trim$age_f, levels = c(levels_to_ord))	
  		
	mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 10)
				
	# set up prior
	pc <- pc_matern(range_gt = 75, sigma_lt = 0.2)
		
	form2_yr <- paste0("log_wt ~  age_f_ord + s(" , "yrprior_btemp", ", by = age_f_ord, k = 4)")

	yrprior_btemp_int_mod_pcod <- 
		sdmTMB_cv(
			formula = as.formula(form2_yr),
			data = pcod_dat_trim,
			mesh = mesh,
			k_folds = max(pcod_dat_trim$fold),
     	fold_ids = pcod_dat_trim$fold,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			spatiotemporal = "off",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	write_rds(yrprior_btemp_int_mod_pcod, 
			file = paste0(here(), file_path_all, "yrprior_btemp_int_mod_pcod", ".rds"))
											

	# also for presurvey_btemp
	
	pc <- pc_matern(range_gt = 75, sigma_lt = 0.2)
		
	form2 <- paste0("log_wt ~  age_f_ord + s(" , "presurvey_btemp", ", by = age_f_ord, k = 4)")

	presurvey_btemp_int_mod_pcod <- 
		sdmTMB_cv(
			formula = as.formula(form2),
			data = pcod_dat_trim,
			mesh = mesh,
			k_folds = max(pcod_dat_trim$fold),
     	fold_ids = pcod_dat_trim$fold,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			spatiotemporal = "off",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	write_rds(presurvey_btemp_int_mod_pcod, 
			file = paste0(here(), file_path_all, "presurvey_btemp_int_mod_pcod", ".rds"))
											

	##### READ IN MODELS AND CHECKS ####
	
	file_list <- list.files(path = paste0(here(), file_path_all))

  prestring <- paste0(here(), file_path_all)
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i) # I used a for loop!
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
	save_obj <- function(x){
		
  	 x$models[[1]]
  		}

	mods_list <-lapply(mod_list, save_obj)

	# check sanity()
	
	sanity_func <- function(x){
  	 sanity(x)
  }
  
	s <- lapply(mods_list, sanity_func)
	
	# plot residuals 
	
	plot_file_path <- paste0(here(), "/output/residuals/pcod mods/")

	resid_func <- function(x){
		
		sims <- simulate(x, nsim = 250)
		dharma_sims <- sims %>% dharma_residuals(x, plot = FALSE)

			}	
	
	sims_dat_list <- map(mods_list, resid_func)

	
	# plot
	resid_plot <- function(x, y){
		
		p <- 
			ggplot(x) +
			geom_point(aes(expected, observed)) +
			geom_abline(intercept = 0, slope = 1, color = "red")
		
		ggsave(filename = paste0(plot_file_path, y, ".png"),
					 plot = p,
					 width = 5, height = 5, units = "in")
		
	}
	
	plot_names <- names(mods_list)
	
	map2(sims_dat_list, plot_names, resid_plot)

	## AIC tables 
	
	pre_boxy <- mods_list[grep("presurvey_boxy", names(mods_list))]
	pre_btemp <- mods_list[grep("presurvey_btemp", names(mods_list))]
	yr_boxy <- mods_list[grep("yrprior_boxy", names(mods_list))]
	yr_btemp <- mods_list[grep("yrprior_btemp", names(mods_list))]
	
	lapply(pre_boxy, AIC) #no int
	lapply(pre_btemp, AIC) #no int
	lapply(yr_boxy, AIC) #int
	lapply(yr_btemp, AIC) #no int
	
	top_mods_list <- c(pre_boxy[2], pre_btemp[2], yr_boxy[1], yr_btemp[2])

	top_temp_mods <- top_mods_list[grep("btemp", names(top_mods_list))]
	
	top_oxy_mods <- top_mods_list[grep("boxy", names(top_mods_list))]
	
	lapply(top_temp_mods, AIC) #presurvey better
	lapply(top_oxy_mods, AIC) #yrprior better
	
	# plots 
	levels_to_ord <- sort(unique(pcod_dat_trim$age_f))
  pcod_dat_trim$age_f_ord <- ordered(pcod_dat_trim$age_f, levels = c(levels_to_ord))	

	# temp
	presurvey_btemp_int_mod_pcod <- readRDS(
		paste0(here(), file_path_all, "presurvey_btemp_int_mod_pcod", ".rds"))

	presurvey_btemp_int_mod_pcod <- presurvey_btemp_int_mod_pcod$models[[1]]
	
	new_dat <- expand_grid(
		age_f_ord = unique(pcod_dat_trim$age_f_ord),
		presurvey_btemp = seq(from = min(pcod_dat_trim$presurvey_btemp),
										 to = max(pcod_dat_trim$presurvey_btemp),
										 length.out = 100))
	
	pcod_temp_preds <- predict(presurvey_btemp_int_mod_pcod,
										 newdata = new_dat,
										 se_fit = TRUE,
										 re_form = NA)
	
	# high and low CIs
	pcod_temp_preds$low <- pcod_temp_preds$est + (qnorm(0.025) * pcod_temp_preds$est_se)
	pcod_temp_preds$high <- pcod_temp_preds$est + (qnorm(0.975) * pcod_temp_preds$est_se)
	
	pcod_temp_preds
		
	# plot
		pcod_temp_plot <-
			ggplot(pcod_temp_preds, aes(presurvey_btemp, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 6, scales = "free") +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			#labs(title = year) +
			black_facet_theme(x = 16, y = 18)

	ggsave(filename = paste0(here(), "/output/plots/pcod_temp_plot_PCODWKSHOP.png"),
				 pcod_temp_plot, width = 10, height = 6)


	# oxygen mod
	yrprior_boxy_int_mod_pcod <- readRDS(
		paste0(here(), file_path_all, "yrprior_boxy_int_mod_pcod", ".rds"))

	yrprior_boxy_int_mod_pcod <- yrprior_boxy_int_mod_pcod$models[[1]]
	
	new_dat <- expand_grid(
		age_f_ord = unique(pcod_dat_trim$age_f_ord),
		yrprior_boxy = seq(from = min(pcod_dat_trim$presurvey_btemp),
										 to = max(pcod_dat_trim$presurvey_btemp),
										 length.out = 100))
	
	pcod_oxy_preds <- predict(yrprior_boxy_int_mod_pcod,
										 newdata = new_dat,
										 se_fit = TRUE,
										 re_form = NA)
	
	# high and low CIs
	pcod_oxy_preds$low <- pcod_oxy_preds$est + (qnorm(0.025) * pcod_oxy_preds$est_se)
	pcod_oxy_preds$high <- pcod_oxy_preds$est + (qnorm(0.975) * pcod_oxy_preds$est_se)
		
	# plot
	pcod_oxy_plot <-
			ggplot(pcod_oxy_preds, aes(yrprior_boxy, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 6) +
			ylab("log scaled weight") +
			xlab("bottom oxygen (averaged June - June)") +
			#labs(title = year) +
			black_facet_theme(x = 16, y = 18)

	ggsave(filename = paste0(here(), "/output/plots/pcod_oxy_plot_PCODWKSHOP.png"),
				 pcod_oxy_plot, width = 10, height = 6)
	
	### plot spatial predictions
	
	pcod_dat_trim$longitude <- -1* pcod_dat_trim$longitude

	plot(pcod_dat_trim$longitude, pcod_dat_trim$latitude)

	grid <- pcod_dat_trim %>%
  st_as_sf(crs=4326,coords=c("longitude","latitude")) #%>%
  #st_transform(crs=32604)

	spatial_grid <- pcod_dat_trim %>%
		dplyr::select(latitude, longitude, haul_id) %>%
		distinct() %>%
		st_as_sf(crs = 4236, coords = c("longitude", "latitude"))
	
	buff_dist <- 200000
	
	buff <- st_buffer(spatial_grid, buff_dist, dissolve = TRUE) %>%
		st_union
	
	conv <- st_convex_hull(st_union(spatial_grid))
	
	gridinconv <- spatial_grid %>%
  	st_intersection(conv) %>%
  	st_transform(crs=4326) %>%
  	mutate(longitude=st_coordinates(.)[,1],
  	       latitude=st_coordinates(.)[,2]) %>%
  	st_drop_geometry() %>%
  	dplyr::select(-haul_id)


	plot(gridinconv)

	#write.csv(gridinconv,"Data/goagrid.csv",row.names=F)

	# aclim temps
	
	# read in CMIP6 ROMS output
  load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/allEBS_means/ACLIM_surveyrep_fut_mn.Rdata")
  
  # trim 
  vars <- c("temp_bottom5m", "oxygen_bottom5m")
  
  ACLIM_vars_fut <- ACLIM_surveyrep_fut %>%
  	filter(var %in% vars) %>%
  	group_by()
  	summarise(mean_)
  
  ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
			filter(., between(depth, 0, 250)) %>%
			filter(domain > 0) 


	tmp <- pcod_dat_trim %>% 
  	st_as_sf(coords = c("X", "Y"), 
  				 crs = suppressWarnings(sdmTMB::get_crs(pcod_dat_trim, ll_names = c("longitude", "latitude")))) %>% 
  	summarise(geometry = st_combine(geometry)) %>%
  	st_convex_hull() 

	proj_dt <- tmp %>% 
	  st_make_grid(cellsize = rep(100,2), square = TRUE, what = "centers") %>% 
	  st_sf() %>% 
	  st_filter(tmp) %>% 
	  st_coordinates() %>% 
	  as_tibble()
	
	plot(proj_dt)
	
	coords <- pcod_dat_trim %>%
		dplyr::select(X, Y, yrprior_boxy, yrprior_btemp, presurvey_boxy, presurvey_btemp)
	
	proj_df <- fuzzyjoin::fuzzy_left_join(proj_dt, coords)
	
	
	
	## predict
	
	

	
	#### try using the mesh
	
	df <- data.frame(X = mesh$mesh$loc[,1], Y = mesh$mesh$loc[,2],  depth = mean(pcod_2011$depth))



	

	