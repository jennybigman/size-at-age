# without jday and priors

	# read in models with jday

	presurvey_btemp_pol <- readRDS( 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol.rds"))
 
	
	pc <- pc_matern(range_gt = 300, sigma_lt = 1) 

	presurvey_btemp_pol_np <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(presurvey_btemp),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_btemp_pol_np)
	
	presurvey_btemp_pol_p <- 
		sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_btemp),
				data = pollock_dat,
				mesh = pol_mesh,
				priors = sdmTMBpriors(
					matern_s = pc),
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_btemp_pol_p)
	
	sims <- simulate(presurvey_btemp_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pol)

	# does having an effect of jday improve model fit?
	AIC(presurvey_btemp_pol)
	AIC(presurvey_btemp_pol_p)
	AIC(presurvey_btemp_pol_np)
	

	 presurvey_btemp_int_pol_nj <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f),
			#dispformula = ~ 0, 
			data = pollock_dat,
			mesh = pol_mesh2,
			priors = sdmTMBpriors(
				matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 2))

 	sanity(presurvey_btemp_int_pol) # looks great
 	
 	sims <- simulate(presurvey_btemp_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol)
 	
  saveRDS(presurvey_btemp_int_pol, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol.rds"))

  presurvey_btemp_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f),
			#dispformula = ~ 0, 
			data = yfinsole_dat,
			mesh = yfin_mesh,
			priors = sdmTMBpriors(
				matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 2))

  sanity(presurvey_btemp_int_yfin)
  
  sims <- simulate(presurvey_btemp_int_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_yfin)

 	