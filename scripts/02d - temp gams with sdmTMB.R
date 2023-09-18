# gams with sdmTMB 

	`%!in%` = Negate(`%in%`)

	# make mesh
	pcod_mesh <- make_mesh(pcod_dat, xy_cols = c("X", "Y"), cutoff = 20)
	pol_mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)
	yfin_mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
	# matern priors
	pc <- pc_matern(range_gt = 5, sigma_lt = 1) # range val too small
	
	# dataset for pcod has too few samples in age 1 and 2 so remove
  drop_age <- c(1, 2)
  pcod_dat_trim <- pcod_dat %>% filter(age %!in% drop_age)
  pcod_dat_trim$age_f <- droplevels(pcod_dat_trim$age_f)
  
  # adjust mesh due to errors
	pcod_mesh_trim <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

	####################################################
	# presurvey bottom temp (averaged April - June) ####
	####################################################

	#### pollock ####
	
	# refine mesh because model with factor-smooth interaction won't converge
	pol_mesh2 <- make_mesh(pollock_dat, c("X", "Y"), n_knots = 800, type = "kmeans")

	# model with no factor smooth interaction
	presurvey_btemp_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(presurvey_btemp) + s(jday_std), # consider linear effect here
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_btemp_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pol)
	
	saveRDS(presurvey_btemp_pol, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol.rds"))
 
	# model with factor-smooth interaction
	
	# set up priors
	pc <- pc_matern(range_gt = 6, sigma_lt = 1.7) # set up prior
 		
 	presurvey_btemp_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday_std),
			data = pollock_dat,
			mesh = pol_mesh,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(presurvey_btemp_int_pol) 
 	
 	sims <- simulate(presurvey_btemp_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol)
 	
  saveRDS(presurvey_btemp_int_pol, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol.rds"))
 
  #### pcod ####
  
  # model with no interaction
	presurvey_btemp_pcod <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_btemp) + s(jday_std),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(presurvey_btemp_pcod)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pcod)
	
	saveRDS(presurvey_btemp_pcod, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pcod.rds"))
 
	# model with factor-smooth interaction
	
	# adjust mesh
	pcod_mesh_trim2 <- make_mesh(pcod_dat_trim, c("X", "Y"), n_knots = 800, type = "kmeans")

	# set priors
	pc <- pc_matern(range_gt = 202, sigma_lt = 0.4) # set up prior

 	presurvey_btemp_int_pcod <- sdmTMB( ##### TROUBLE CONVERGING ############
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday_std),
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim2,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3))
 	
 	sanity(presurvey_btemp_int_pcod)
 	
 	sims <- simulate(presurvey_btemp_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pcod)
 	
  saveRDS(presurvey_btemp_int_pcod, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pcod.rds"))

  presurvey_btemp_int_pcod <- readRDS(
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pcod.rds"))


  #### yellowfin sole ####
  
  # model with no interaction
  
  # set up prior	
  pc <- pc_matern(range_gt = 277, sigma_lt = 0.2) # set up prior

	presurvey_btemp_yfin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_btemp) + s(jday_std),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				priors = sdmTMBpriors(matern_s = pc),
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(presurvey_btemp_yfin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_yfin)
	
	saveRDS(presurvey_btemp_yfin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_yfin.rds"))
 
	# model with factor-smooth interaction
	
	# add priors
	pc <- pc_matern(range_gt = 470, sigma_lt = 0.5) # set up prior

 	presurvey_btemp_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday_std),
			data = yfinsole_dat,
			mesh = yfin_mesh,
			spatial = "on",
			priors = sdmTMBpriors(matern_s = pc),
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 2))
 	
 	sanity(presurvey_btemp_int_yfin)
 	
 	sims <- simulate(presurvey_btemp_int_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_yfin)
 	
  saveRDS(presurvey_btemp_int_yfin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin.rds"))

  presurvey_btemp_int_yfin <- readRDS( 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin.rds"))

  ####################################################
	# yr prior btemp ####
	####################################################

	#### pollock ####

  # model with no interaction  
	yrprior_btemp_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(yrprior_btemp) + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(yrprior_btemp_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_pol)
	
	saveRDS(yrprior_btemp_pol, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_pol.rds"))
 
	# model with factor-smooth interaction
	
 	# set up prior
	pc <- pc_matern(range_gt = 210, sigma_lt = 0.3) # set up prior
	
 	yrprior_btemp_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_btemp, by = age_f) + s(jday_std),
			data = pollock_dat,
			mesh = pol_mesh,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(yrprior_btemp_int_pol) # ln_smooth_sigma SE may be large
 	
 	sims <- simulate(yrprior_btemp_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_pol)
 	
  saveRDS(yrprior_btemp_int_pol, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_int_pol.rds"))
 
  #### pcod ####
 
  # model with no interaction 
 	yrprior_btemp_pcod <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(yrprior_btemp) + s(jday_std),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(yrprior_btemp_pcod)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_pcod)
	
	saveRDS(yrprior_btemp_pcod, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_pcod.rds"))
 
	# model with factor smooth interaction
	
	# set priors
	pc <- pc_matern(range_gt = 210, sigma_lt = 0.4) # set up prior
	
 	yrprior_btemp_int_pcod <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_btemp, by = age_f) + s(jday_std),
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 
 	sanity(yrprior_btemp_int_pcod) # ln_smooth_sigma SE too large?
 	
 	sims <- simulate(yrprior_btemp_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_pcod)
 	
  saveRDS(yrprior_btemp_int_pcod, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_int_pcod.rds"))

  yrprior_btemp_int_pcod <- readRDS(
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_int_pcod.rds"))

  #### yellowfin sole ####
  
  # model with no interaction
	yrprior_btemp_yfin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(yrprior_btemp) + s(jday_std),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(yrprior_btemp_yfin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_yfin)
	
	saveRDS(yrprior_btemp_yfin, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_yfin.rds"))
 
	# model with factor smooth interaction
	
	# adjust mesh 
	yfin_mesh2 <- make_mesh(yfinsole_dat, c("X", "Y"), n_knots = 800, type = "kmeans")

	# set up prior
	pc <- pc_matern(range_gt = 270, sigma_lt = 0.2) # set up prior

 	yrprior_btemp_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_btemp, by = age_f) + s(jday_std),
			data = yfinsole_dat,
			mesh = yfin_mesh2,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 2,
															step.min = 0.01, step.max = 1)) ### won't converge

 	sanity(yrprior_btemp_int_yfin)
 	
 	sims <- simulate(yrprior_btemp_int_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_yfin)
 	
  saveRDS(yrprior_btemp_int_yfin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin.rds"))
  
  yrprior_btemp_int_yfin <- readRDS(
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin.rds"))


  ####################################################
	# age 0 ####
	####################################################

  #### pollock ####
  
  # model with no interaction
	age0_btemp_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(age0_btemp) + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(age0_btemp_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(age0_btemp_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_pol)
	
	saveRDS(age0_btemp_pol, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_pol.rds"))
 
 	# model with factor-smooth interaction
	
	# set up prior
	pc <- pc_matern(range_gt = 380, sigma_lt = 0.29) # set up prior

 	age0_btemp_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(age0_btemp, by = age_f) + s(jday_std),
			data = pollock_dat,
			mesh = pol_mesh,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

 	sanity(age0_btemp_int_pol) 
 	
 	sims <- simulate(age0_btemp_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_pol)
 	
  saveRDS(age0_btemp_int_pol, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_pol.rds"))
 
  ##### pcod ####
  
  apply(is.na(pcod_dat_trim), 2, any) # any cols have NAs
  which(is.na(pcod_dat_trim$age0_btemp)) # which rows have NAs
  
  # drop NAs
  
  pcod_dat_trim <- pcod_dat_trim %>%
  	drop_na(age0_btemp)
 
  # adjust mesh due to errors
	pcod_mesh_trim <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

	# model with no interaction
 	age0_btemp_pcod <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(age0_btemp) + s(jday_std),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(age0_btemp_pcod)
	
	# check residuals - simulation-based residuals
	sims <- simulate(age0_btemp_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_pcod)
	
	saveRDS(age0_btemp_pcod, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_pcod.rds"))
 
	# model with factor-smooth interaction
	
	# set up prior
	pc <- pc_matern(range_gt = 242, sigma_lt = 0.4) # set up prior

	# adjust mesh
	pcod_mesh2 <- make_mesh(pcod_dat_trim, c("X", "Y"), n_knots = 800, type = "kmeans")

	pcod_mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

 	age0_btemp_int_pcod <- sdmTMB(
			log_wt_std ~ age_f + s(age0_btemp, by = age_f) + s(jday_std),
			data = pcod_dat_trim,
			mesh = pcod_mesh2,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3))
 	
 	sanity(age0_btemp_int_pcod) # ln_smooth_sigma SE too large?
 	
 	#### trouble converging ####
 	
 	sims <- simulate(age0_btemp_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_pcod)
 	
  saveRDS(age0_btemp_int_pcod, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_pcod.rds"))

  age0_btemp_int_pcod <- readRDS(
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_pcod.rds"))

  # yellowfin sole ####
  
  # model with no interaction
	age0_btemp_yfin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(age0_btemp) + s(jday_std),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(age0_btemp_yfin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(age0_btemp_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_yfin)
	
	saveRDS(age0_btemp_yfin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_yfin.rds"))
 
	# model with factor-smooth interaction
	
	# set up priors
	pc <- pc_matern(range_gt = 294, sigma_lt = 0.2) # set up prior

	# adjust mesh 
	yfin_mesh2 <- make_mesh(yfinsole_dat, c("X", "Y"), n_knots = 800, type = "kmeans")

 	age0_btemp_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(age0_btemp, by = age_f) + s(jday_std),
			data = yfinsole_dat,
			mesh = yfin_mesh2,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1,
															step.min = 0.01, step.max = 1)) ### won't converge

 	sanity(age0_btemp_int_yfin)
 	
 	sims <- simulate(age0_btemp_int_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_yfin)
 	
  saveRDS(age0_btemp_int_yfin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_yfin.rds"))
  
  age0_btemp_int_yfin <- readRDS( 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_yfin.rds"))
  
  
  
  
  
  
  
  
  
  
  
  
  
 