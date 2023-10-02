# function to fit sdmTMB mods 

	plan(multisession)

	# set up spatial quantiles and bind dfs
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
			
	# remove any rows with no age0 oxygen data
	dat_all <- dat_all %>%
		drop_na(age0_boxy)
	
	# function 
	sdmTMB_cv_func <- function(sp){
		
		# filter df by species
		new_dat <- dat_all %>% filter(species == sp)
		

		# make mesh
		mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), n_knots = 500, type = "kmeans")
	
		# set up prior
		pc <- pc_matern(range_gt = 300, sigma_lt = 0.4) # set up prior

		# run models		
		print(paste('running no int model for', sp))
 		
		mod_cv <- 
			try(
				sdmTMB_cv(	
 					log_wt ~ age_f + s(presurvey_btemp) + year_f,
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1)),
					silent = FALSE)
		
		 if(class(mod_cv) == "try-error"){
    		print(paste("error"))
		 	}
		 else{
    		print(paste("no int model for", sp,"complete"))
  		}
		
		print(paste('running int model for', sp))

		mod_int_cv <- 
			try(
				sdmTMB(	
 					log_wt ~ age_f + s(presurvey_btemp, by = age_f) + year_f,
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					priors = sdmTMBpriors(matern_s = pc),
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 3)),
					silent = FALSE)
		
			 if(class(mod_int_cv) == "try-error"){
    		print(paste("error"))
		 	}
		 else{
    		print(paste("int model for", sp,"complete"))
  		}

		# put model objects into a list for each species and variable
		mod_cv_list <- list(mod_cv, mod_int_cv)
		
	}
	
	species_mod <- unique(dat_all$species)
	
	presurvey_btemp_cv_mod_list <- lapply(species_mod, sdmTMB_cv_func)
	
	
	# extract, merge, and name models
	presurvey_mod_list1 <- c(presurvey_mod_list[[1]])
	presurvey_mod_list2 <- c(presurvey_mod_list[[2]])
	presurvey_mod_list3 <- c(presurvey_mod_list[[3]])

	presurvey_mod_lists <- c(presurvey_mod_list1,
													 presurvey_mod_list2,
													 presurvey_mod_list3)
	
	sp_name <- c("pol", "pol_int",
							 "pcod", "pcod_int",
							 "yfin", "yfin_int")
	
	mod_name <- function(x){
		paste0("lin_presurvey_btemp_", x)
	}
	
	mod_names <- lapply(sp_name, mod_name)
	
	names(presurvey_mod_lists) <- mod_names
	
	ex_list <- function(x) {
		list2env(x, globalenv())
	}
	
	sapply(presurvey_mod_lists, ex_list)
	
	# check models
	sanity(lin_presurvey_btemp_pol)
	sanity(lin_presurvey_btemp_pol_int)

	sanity(lin_presurvey_btemp_pcod)
	sanity(lin_presurvey_btemp_pcod_int)

	sanity(lin_presurvey_btemp_yfin)
	sanity(lin_presurvey_btemp_yfin_int)
