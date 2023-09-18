# function to fit sdmTMB mods 

	# put three dfs into one
	dat_all <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat)
	
	# function 
	sdmTMB_presurvey_func <- function(sp){
		
		new_dat <- dat_all %>% filter(species == sp)
		
		mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
		pc <- pc_matern(range_gt = 300, sigma_lt = 0.1) # set up prior
 		
		mod <- 
			try(
				sdmTMB(	
 					log_wt ~ age_f + presurvey_btemp,
					data = new_dat,
					mesh = mesh,
					priors = sdmTMBpriors(matern_s = pc),
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1)))
		
		mod_int <- 
			try(
				sdmTMB(	
 					log_wt ~ age_f * presurvey_btemp,
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					priors = sdmTMBpriors(matern_s = pc),
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1)))
		
		mod_list <- list(mod, mod_int)
		
	}
	
	species_mod <- unique(dat_all$species)
	
	presurvey_mod_list <- lapply(species_mod, sdmTMB_presurvey_func)
	
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
