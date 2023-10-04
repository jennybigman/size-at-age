# oxygen gams with sdmTMB 

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
	# presurvey bottom oxygen (averaged April - June) ####
	####################################################

	#### pollock ####
	presurvey_boxy_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(presurvey_boxy) + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_btemp_pol)
	
	# compare to breakpoint (threshold in oxygen
	
		presurvey_boxy_bp_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + breakpt(presurvey_boxy) + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_boxy_bp_pol)
	
	# linear
	
	presurvey_boxy_lin_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + presurvey_boxy + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_boxy_lin_pol)
	
	AIC(presurvey_boxy_pol)
	AIC(presurvey_boxy_bp_pol)
	AIC(presurvey_boxy_lin_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pol)
	
	saveRDS(presurvey_btemp_pol, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol.rds"))
 
 		
 	presurvey_btemp_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday_std),
			#dispformula = ~ 0, 
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(presurvey_btemp_int_pol)
 	
 	sims <- simulate(presurvey_btemp_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol)
 	
  saveRDS(presurvey_btemp_int_pol, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol.rds"))
 
  #### pcod ####
  
	presurvey_btemp_pcod <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_btemp) + s(jday_std),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				spatiotemporal = "off",
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(presurvey_btemp_pcod)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pcod)
	
	saveRDS(presurvey_btemp_pcod, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pcod.rds"))
 
	# adjust mesh
	#pcod_mesh2 <- make_mesh(pcod_dat_trim, c("X", "Y"), n_knots = 800, type = "kmeans")

 	presurvey_btemp_int_pcod <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0, 
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim,
			spatial = "on",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 	
 	#presurvey_btemp_int_pcod <- sdmTMB::run_extra_optimization(presurvey_btemp_int_pcod,
 	#	nlminb_loops = 1L, newton_loops = 1L)

 	sanity(presurvey_btemp_int_pcod)
 	
 	#### trouble converging ####
 	
 	sims <- simulate(presurvey_btemp_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pcod)
 	
  saveRDS(presurvey_btemp_int_pcod, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pcod.rds"))

  #### yellowfin sole ####
  
	presurvey_btemp_yfin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_btemp) + s(jday_std),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				spatiotemporal = "off",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(presurvey_btemp_yfin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_yfin)
	
	saveRDS(presurvey_btemp_yfin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_yfin.rds"))
 
	# adjust mesh 
	yfin_mesh2 <- make_mesh(yfinsole_dat, c("X", "Y"), n_knots = 800, type = "kmeans")

 	presurvey_btemp_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0, 
			data = yfinsole_dat,
			mesh = yfin_mesh2,
			spatial = "on",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 2))

 	sanity(presurvey_btemp_int_yfin)
 	
 	sims <- simulate(presurvey_btemp_int_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_yfin)
 	
  saveRDS(presurvey_btemp_int_yfin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin.rds"))

  ####################################################
	# yr prior btemp ####
	####################################################

	#### pollock ####
	yrprior_btemp_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + s(yrprior_btemp) + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(yrprior_btemp_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_pol)
	
	saveRDS(yrprior_btemp_pol, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_pol.rds"))
 
 		
 	yrprior_btemp_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0, 
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(yrprior_btemp_int_pol) # ln_smooth_sigma SE may be large
 	
 	sims <- simulate(yrprior_btemp_int_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_pol)
 	
  saveRDS(yrprior_btemp_int_pol, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_int_pol.rds"))
 
  #### pcod ####
  
 	yrprior_btemp_pcod <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(yrprior_btemp) + s(jday_std),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				spatiotemporal = "off",
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(yrprior_btemp_pcod)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_pcod)
	
	saveRDS(yrprior_btemp_pcod, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_pcod.rds"))
 
	# adjust mesh
	pcod_mesh2 <- make_mesh(pcod_dat_trim, c("X", "Y"), n_knots = 800, type = "kmeans")

	pcod_mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

 	yrprior_btemp_int_pcod <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0, 
			data = pcod_dat_trim,
			mesh = pcod_mesh,
			spatial = "on",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 	
 	#presurvey_btemp_int_pcod <- sdmTMB::run_extra_optimization(presurvey_btemp_int_pcod,
 	#	nlminb_loops = 1L, newton_loops = 1L)

 	sanity(yrprior_btemp_int_pcod) # ln_smooth_sigma SE too large?
 	
 	#### trouble converging ####
 	
 	sims <- simulate(yrprior_btemp_int_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_pcod)
 	
  saveRDS(yrprior_btemp_int_pcod, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_int_pcod.rds"))

  #### yellowfin sole ####
  
	yrprior_btemp_yfin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + s(yrprior_btemp) + s(jday_std),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				spatiotemporal = "off",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(yrprior_btemp_yfin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_yfin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_yfin)
	
	saveRDS(yrprior_btemp_yfin, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_yfin.rds"))
 
	# adjust mesh 
	yfin_mesh2 <- make_mesh(yfinsole_dat, c("X", "Y"), n_knots = 800, type = "kmeans")

 	yrprior_btemp_int_yfin <- sdmTMB(
			log_wt_std ~ age_f + s(yrprior_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0, 
			data = yfinsole_dat,
			mesh = yfin_mesh2,
			spatial = "on",
			control = sdmTMBcontrol(nlminb_loops = 3,
															step.min = 0.01, step.max = 1)) ### won't converge

 	sanity(yrprior_btemp_int_yfin)
 	
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
 					log_wt_std ~ age_f + s(age0_btemp) + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(age0_btemp_pol)
	
	# check residuals - simulation-based residuals
	sims <- simulate(age0_btemp_pol, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_pol)
	
	saveRDS(age0_btemp_pol, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_pol.rds"))
 
 		
 	age0_btemp_int_pol <- sdmTMB(
			log_wt_std ~ age_f + s(age0_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0 + as.factor(year), 
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
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
 				log_wt_std ~ age_f + s(age0_btemp) + s(jday_std),
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				spatiotemporal = "off",
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
			log_wt_std ~ age_f + s(age0_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0, 
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim,
			spatial = "on",
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
 				log_wt_std ~ age_f + s(age0_btemp) + s(jday_std),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				spatiotemporal = "off",
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
			log_wt_std ~ age_f + s(age0_btemp, by = age_f) + s(jday_std),
			dispformula = ~ 0, 
			data = yfinsole_dat,
			mesh = yfin_mesh2,
			spatial = "on",
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
 