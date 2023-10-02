# function to fit sdmTMB mods 

	# put three dfs into one
	dat_all <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat) %>%
		drop_na(any_of(c("age0_btemp", "age0_boxy"))) %>%
		na.omit()
	
	# function 
	sdmTMB_no_int_func <- function(sp){
		
		new_dat <- dat_all %>% filter(species == sp)
		
		mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
		mod_cv <- 
			try(
				sdmTMB(	
 					formula = log_wt ~ age_f + s(age0_boxy),
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1)
					)
				)
		
		mod
		
	}
	
	# list of species and variables (predictors)
	species_mod <- unique(dat_all$species)

	vars_mod <- dat_all %>% 
		select(contains(c("btemp", "boxy"))) %>%
		names()
	
	df_func <- expand_grid(
		sp = species_mod,
		var = vars_mod
	)

#no_int_mod_list <- mapply(sdmTMB_no_int_func,
	#													x = species_mod,
	#													y = vars_mod,
	#													SIMPLIFY = FALSE)
		

	mod_list <- lapply(species_mod, sdmTMB_no_int_func) # works
	
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
