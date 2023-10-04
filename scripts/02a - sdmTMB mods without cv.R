# sdmTMB function - no CV for now

	dat_list <- list(pollock_dat, pcod_dat_trim, yfinsole_dat)
	
	dat_all <- dat_list %>% bind_rows()
	
	# remove any rows with no age0 environmental data (the year in which some individuals were age 0 was prior to 1970, first year of hindcast)
	dat_all <- dat_all %>%
		drop_na(age0_boxy)

	# function to fit multiple models with no spatiotemporal RE but year RE
	
	sdmTMB_yr_RE_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
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
					
					# set up which sanity checks are not really issues (but will check residual plots)
					not_probs <- c(5, 6) # gradients_ok & se_magnitude_ok
	
		# run models	
		
					print(paste('running no int model for', sp, "with", y))
		
					# set up formulas
					form1 <- paste0("log_wt_std ~ 0 + age_f_ord + s(" , y, ") + (1|year_f)")
			
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
						if (class(mod) == "try-error"){
		 						print(paste("error!"))
		 
						} else if (sanity(mod)[[9]] == "TRUE") { # if all sanity checks are good, save model
		 	 	
		 						write_rds(mod, 
									file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", # change to file path on local machine
														 y, "_no_int_mod_yr_RE_", sp, ".rds"))
		 						
		 						print(paste("no int model for", sp, "with", y, "complete"))
				
						} else if (any(ind %!in% not_probs)) { # if sanity checks are not good, may be able to ignore some but if not run extra optimization 
					
		 						print('running extra optimization')
								
								mod_eo <- try(run_extra_optimization(mod)) 
		
						if (class(mod_eo) == "try-error"){
		 				
    					print(paste("error!"))
					
						} else if (sanity(mod_eo)[[9]] == "TRUE") {
		 				 	
		 					write_rds(mod_eo, 
								file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", # change to file path on local machine
										 y, "_no_int_mod_yr_RE_", sp, ".rds"))
		 		
		 					print(paste("no int model for", sp, "with", y, "complete"))

					  } else {
			
    					print('boo')
					  } 
				}
		}
	
	
	# run function - this will take a while (>30 mins)
	
	sp <- unique(dat_all$species)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_yr_RE_func)
	
	############# EDIT BELOW HERE ##########################################################################
	
	# read in saved models to check fits
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/with year as RE/")))

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/with year as RE/"))
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i) # I used a for loop!
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # separate models by species ####
  
	# pollock #
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
	
	
	
	