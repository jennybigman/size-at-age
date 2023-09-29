
	# oxygen gams with sdmTMB without jday

	`%!in%` = Negate(`%in%`)

	# make mesh
	pcod_mesh <- make_mesh(pcod_dat, xy_cols = c("X", "Y"), cutoff = 20)
	pol_mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)
	yfin_mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
	# dataset for pcod has too few samples in age 1 and 2 so remove
  drop_age <- c(1, 2)
  pcod_dat_trim <- pcod_dat %>% filter(age %!in% drop_age)
  pcod_dat_trim$age_f <- droplevels(pcod_dat_trim$age_f)
  
  # adjust mesh due to errors
	pcod_mesh_trim <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

	####################################################
	# presurvey bottom oxygen (averaged April - June) ####
	####################################################

	#### pollock ####
	
	# no interaction
	presurvey_boxy_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(presurvey_boxy),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					reml = FALSE,
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_boxy_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_boxy_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_pol)
	
	saveRDS(presurvey_boxy_pol, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol.rds"))
 
 	# model with interaction
 	presurvey_boxy_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_boxy, by = age_f),
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			reml = FALSE,
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(presurvey_boxy_int_pol)
 	
 	sims <- simulate(presurvey_boxy_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_pol)
 	
  saveRDS(presurvey_boxy_int_pol, 
  				file = here("./output/model output/sdmTMB output/presurvey_boxy_int_pol.rds"))
 
  #### pcod ####
  
  # no interaction
	presurvey_boxy_pcod_nj <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_boxy),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(presurvey_boxy_pcod_nj)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_boxy_pcod_nj, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_pcod_nj)
	
	saveRDS(presurvey_boxy_pcod_nj, 
  				file = here("./output/model output/sdmTMB output/presurvey_boxy_pcod_nj.rds"))
 
	# adjust mesh
	pcod_mesh_trim2 <- make_mesh(pcod_dat_trim, c("X", "Y"), n_knots = 300, type = "kmeans")

	# set up priors
	pc <- pc_matern(range_gt = 234, sigma_lt = 0.4) # set up prior
		
	# model with interaction
 	presurvey_boxy_int_pcod <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_boxy, by = age_f),
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim2,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 	
 	sanity(presurvey_boxy_int_pcod) # needs priors
 	
 	sims <- simulate(presurvey_boxy_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_pcod)
 	
  saveRDS(presurvey_boxy_int_pcod_nj, 
  				file = here("./output/model output/sdmTMB output/presurvey_boxy_int_pcod_nj.rds"))

  #### yellowfin sole ####
  
  # no interaction
	presurvey_boxy_yfin_nj <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_boxy),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(presurvey_boxy_yfin_nj)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_boxy_yfin_nj, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_yfin_nj)
	
	saveRDS(presurvey_boxy_yfin_nj, 
  				file = here("./output/model output/sdmTMB output/presurvey_boxy_yfin_nj.rds"))
 
	# adjust mesh 
	yfin_mesh2 <- make_mesh(yfinsole_dat, c("X", "Y"), n_knots = 800, type = "kmeans")

	# set up priors
	pc <- pc_matern(range_gt = 317, sigma_lt = 0.2) # set up prior

 	presurvey_boxy_int_yfin_nj <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_boxy, by = age_f),
			data = yfinsole_dat,
			mesh = yfin_mesh2,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

 	sanity(presurvey_boxy_int_yfin_nj)
 	
 	sims <- simulate(presurvey_boxy_int_yfin_nj, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_yfin_nj)
 	
  saveRDS(presurvey_boxy_int_yfin_nj, 
  				file = here("./output/model output/sdmTMB output/presurvey_boxy_int_yfin_nj.rds"))

  ####################################################
	# yr prior btemp ####
	####################################################

	#### pollock ####
  
  # no interaction
	yrprior_boxy_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(yrprior_boxy),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(yrprior_boxy_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_boxy_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_pol)
	
	saveRDS(yrprior_boxy_pol, 
  				file = here("./output/model output/sdmTMB output/yrprior_boxy_pol.rds"))
 	
	# model with interaction	
 	yrprior_boxy_int_pol <- 
		sdmTMB(
			log_wt_std ~ age_f + s(yrprior_boxy, by = age_f),
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(yrprior_boxy_int_pol) 
 	
 	sims <- simulate(yrprior_boxy_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_int_pol)
 	
  saveRDS(yrprior_boxy_int_pol, 
  				file = here("./output/model output/sdmTMB output/yrprior_boxy_int_pol.rds"))
 
  
  #### pcod ####
  
 	yrprior_boxy_pcod <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(yrprior_boxy),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(yrprior_boxy_pcod)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_boxy_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_pcod)
	
	saveRDS(yrprior_boxy_pcod, 
  				file = here("./output/model output/sdmTMB output/yrprior_boxy_pcod.rds"))
 
	# adjust mesh
	pcod_mesh2 <- make_mesh(pcod_dat_trim, c("X", "Y"), n_knots = 800, type = "kmeans")

	pcod_mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

	# set up prior
	pc <- pc_matern(range_gt = 240, sigma_lt = 0.4) # set up prior

 	yrprior_boxy_int_pcod <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_boxy, by = age_f),
			data = pcod_dat_trim,
			mesh = pcod_mesh2,
			priors = sdmTMBpriors(matern_s = pc),
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3))
 	
 	sanity(yrprior_boxy_int_pcod) 
 	
 	#### trouble converging ####
 	
 	sims <- simulate(yrprior_boxy_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_int_pcod)
 	
  saveRDS(yrprior_boxy_int_pcod, 
  				file = here("./output/model output/sdmTMB output/yrprior_boxy_int_pcod.rds"))

  #### yellowfin sole ####
  
	yrprior_boxy_yfin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(yrprior_boxy),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 1)) 
	
	sanity(yrprior_boxy_yfin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_boxy_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_yfin)
	
	saveRDS(yrprior_boxy_yfin, 
  				file = here("./output/model output/sdmTMB output/yrprior_boxy_yfin.rds"))
 
	# adjust mesh 
	yfin_mesh2 <- make_mesh(yfinsole_dat, c("X", "Y"), n_knots = 800, type = "kmeans")
 ######## start here #####
 	yrprior_boxy_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_boxy, by = age_f),
			data = yfinsole_dat,
			mesh = yfin_mesh2,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1,
															step.min = 0.01, step.max = 1)) 

 	sanity(yrprior_boxy_int_yfin)
 	
 	sims <- simulate(yrprior_btemp_int_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_yfin)
 	
  saveRDS(yrprior_btemp_int_yfin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin.rds"))

  ####################################################
	# age 0 ####
	####################################################

  #### pollock ####
  
	age0_btemp_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(age0_btemp),
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
 
 		
 	age0_btemp_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(age0_btemp, by = age_f),
			data = pollock_dat,
			mesh = pol_mesh,
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

 	age0_btemp_pcod <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(age0_btemp),
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
 
	# adjust mesh
	pcod_mesh2 <- make_mesh(pcod_dat_trim, c("X", "Y"), n_knots = 800, type = "kmeans")

	pcod_mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

 	age0_btemp_int_pcod <- sdmTMB(
			log_wt_std ~ age_f + s(age0_btemp, by = age_f),
			dispformula = ~ 0, 
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim,
			spatial = "on",
							time = "year",
				spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 	
 	#presurvey_btemp_int_pcod <- sdmTMB::run_extra_optimization(presurvey_btemp_int_pcod,
 	#	nlminb_loops = 1L, newton_loops = 1L)

 	sanity(age0_btemp_int_pcod) # ln_smooth_sigma SE too large?
 	
 	#### trouble converging ####
 	
 	sims <- simulate(age0_btemp_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_pcod)
 	
  saveRDS(age0_btemp_int_pcod, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_pcod.rds"))

  # yellowfin sole ####
  
	age0_btemp_yfin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(age0_btemp),
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
 
	# adjust mesh 
	yfin_mesh2 <- make_mesh(yfinsole_dat, c("X", "Y"), n_knots = 800, type = "kmeans")

 	age0_btemp_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(age0_btemp, by = age_f),

						data = yfinsole_dat,
			mesh = yfin_mesh2,
			spatial = "on",
							time = "year",
				spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3,
															step.min = 0.01, step.max = 1)) ### won't converge

 	sanity(age0_btemp_int_yfin)
 	
 	sims <- simulate(age0_btemp_int_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_yfin)
 	
  saveRDS(age0_btemp_int_yfin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_yfin.rds"))
  
 
  
  
  
  
  
  
  
  
  
  
  
   #### linear ####
  
  # gams with sdmTMB 

	`%!in%` = Negate(`%in%`)

	# make mesh
	pcod_mesh <- make_mesh(pcod_dat, xy_cols = c("X", "Y"), cutoff = 20)
	pol_mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)
	yfin_mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
	# matern priors
	pc <- pc_matern(range_gt = 0.1, sigma_lt = 1)
	
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
	presurvey_btemp_pol_lin <- 
			sdmTMB(	
 					log_wt_std ~ age_f + presurvey_btemp + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_btemp_pol_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pol_lin)
	
	saveRDS(presurvey_btemp_pol_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol_lin.rds"))
 
 		
 	presurvey_btemp_int_pol_lin <- sdmTMB(
			log_wt_std ~ age_f * presurvey_btemp + s(jday_std),
			#dispformula = ~ 0, 
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(presurvey_btemp_int_pol_lin)
 	
 	sims <- simulate(presurvey_btemp_int_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol_lin)
 	
  saveRDS(presurvey_btemp_int_pol_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol_lin.rds"))
 