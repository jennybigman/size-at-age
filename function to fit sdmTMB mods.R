# function to fit sdmTMB mods 

	# put three dfs into one

	dat_all <- bind_rows(pollock_dat, pcod_dat, yfinsole_dat)
	
	
	sdmTMB_func <- function(sp, var){
		
		new_dat <- dat_all <- filter(species == sp)
		
		presurvey_btemp <- 
			sdmTMB(	
 					log_wt ~ age_f + s(var) + s(jday),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1,
																	step.min = 0.01, step.max = 1))
	
		
	}