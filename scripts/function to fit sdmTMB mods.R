# function to fit sdmTMB mods 

	# put three dfs into one
	dat_all <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat)
	
	# function 
	sdmTMB_no_int_func <- function(sp){
		
		new_dat <- dat_all %>% filter(species == sp)
		
		mesh <- make_mesh(new_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
		btemp_mod <- 
			try(
				sdmTMB(	
 					log_wt ~ age_f + s(presurvey_btemp),
					data = new_dat,
					mesh = mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1)))
	}
	
	sp_var_names <- expand_grid(
		sp = unique(dat_all$species),
		var = names(select(dat_all, contains(c("btemp", "boxy"))))
	)
	
	species_mod <- unique(dat_all$species)
	
	no_int_mod_list <- lapply(species_mod, sdmTMB_no_int_func)
	