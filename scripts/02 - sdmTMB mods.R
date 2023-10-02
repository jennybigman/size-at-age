# sdmTMB cv function

	plan(multisession) # run folds in parallel
	
	# set up spatial quantiles for cv and bind dfs
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
	
	dat_all$year_f <- as.factor(dat_all$year)
			
	# remove any rows with no age0 oxygen data (the year in which some individuals were age 0 was prior to 1970, first year of hindcast)
	dat_all <- dat_all %>%
		drop_na(age0_boxy)
	

	# function to fit multiple models
	
	sdmTMB_cv_func <- function(sp, y){
		
		# filter df by species
		new_dat <- dat_all %>% filter(species == sp)
  
		# order age class
  	levels_to_ord <- sort(unique(new_dat$age_f))
  	new_dat$age_f_ord <- ordered(new_dat$age_f, levels = c(levels_to_ord))	
  	
		# make mesh
		mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
	
		# set up prior
		pc <- pc_matern(range_gt = 300, sigma_lt = 0.4) # set up prior

		# run models		
		print(paste('running no int model for', sp, "with", y))
		
		# set up formulas
		form1 <- paste0("log_wt ~ 0 + age_f_ord + s(" , y, ")")
		form2 <- paste0("log_wt ~ 0 + age_f_ord + s(" , y, ", by = age_f_ord)")
		 
 		# model without interaction ### add folds
		mod_cv <- 
			try(
				sdmTMB_cv(	
 					formula = as.formula(form1),
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					k_folds = max(new_dat$fold),
        	fold_ids = new_dat$fold,
					control = sdmTMBcontrol(nlminb_loops = 1)),
					silent = FALSE)
		
		 if(class(mod_cv) == "try-error"){
    		print(paste("error!"))
		 	}
		 else{
    		print(paste("no int model for", sp, "with", y, "complete"))
  		}
		
		print(paste('running int model for', sp, "with", y))

		mod_int_cv <- 
			try(
				sdmTMB(	
 					formula = as.formula(form2),
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					priors = sdmTMBpriors(matern_s = pc),
					time = "year",
					spatiotemporal = "IID",
					k_folds = max(new_dat$fold),
        	fold_ids = new_dat$fold,
					control = sdmTMBcontrol(nlminb_loops = 3)),
					silent = FALSE)
		
			 if(class(mod_int_cv) == "try-error"){
    		print(paste("error!"))
		 	}
		 else{
    		print(paste("int model for", sp, "for", y, "complete"))
  		}

		# put model objects into a list for each species and variable
		mod_cv_list <- list(mod_cv, mod_int_cv)
		
	}
	
	sp = unique(dat_all$species)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	sdmTMB_mod_lists <- map2(df_func$sp, df_func$y, sdmTMB_cv_func)
	