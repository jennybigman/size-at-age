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
	
	# remove any rows with no age0 environmental data (the year in which some individuals were age 0 was prior to 1970, first year of hindcast)
	dat_all <- dat_all %>%
		drop_na(age0_boxy)

	# function to fit multiple models with no spatiotemporal RE but year RE
	
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
		form1 <- paste0("log_wt_std ~ 0 + age_f_ord + s(" , y, ") + (1|year_f)")
		form2 <- paste0("log_wt_std ~ 0 + age_f_ord + s(" , y, ", by = age_f_ord) + (1|year_f)")
		 
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

		
		 if(class(mod_cv) == "try-error"){
		 	
    		print(paste("error!"))
		 	}
		 else{
		 		
		 		s <- sanity(mod_cv)
		 	
		 		write_rds(mod_cv, 
				file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", 
										 y, "_no_int_mod_yr_RE_", sp, ".rds"))
		
    		print(paste("no int model for", sp, "with", y, "complete"))
  		}
		
		print(paste('running int model for', sp, "with", y))

		mod_int_cv <- 
			try(
				sdmTMB_cv(	
 					formula = as.formula(form2),
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "off",
					k_folds = max(new_dat$fold),
        	fold_ids = new_dat$fold,
					control = sdmTMBcontrol(nlminb_loops = 3)))
		
			 if(class(mod_int_cv) == "try-error"){
    		print(paste("error!"))
		 	}
		 else{
		 	
		 	write_rds(mod_int_cv, 
				file = paste0(here(), "/output/model output/sdmTMB output/with year as RE/", 
										 y, "_int_mod_yr_RE_", sp, ".rds"))
		
    		print(paste("int model for", sp, "for", y, "complete"))
		 }
	}
	
	sp <- unique(dat_all$species)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names()
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	map2(df_func$sp, df_func$y, sdmTMB_cv_yr_RE_func)
	
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
	
	
	
	