# try with cross validation

####################################################
	# presurvey bottom temp (averaged April - June) ####
	####################################################

	#### pollock ####
	
	presurvey_btemp_pol_nj <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(presurvey_btemp),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_btemp_pol_nj)
	
	# compare linear and smooth models 
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pol_nj, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pol_nj)
	
	saveRDS(presurvey_btemp_pol_nj, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol_nj.rds"))
 
 	# model with interaction	
  presurvey_btemp_int_pol_nj <- sdmTMB_cv(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f),
			#dispformula = ~ 0, 
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(presurvey_btemp_int_pol_nj) # looks great
 	
 	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_int_pol_nj, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol_nj)
