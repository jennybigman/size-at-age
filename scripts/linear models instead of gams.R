# exploring linear models instead of gams

	#### temperature ####
	
	`%!in%` = Negate(`%in%`)

	# make meshes
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
	# presurvey bottom temp (averaged April - June) ####
	####################################################

	#### pollock ####
	
	# no interaction
	presurvey_btemp_pol_lin <- 
			sdmTMB(	
 					log_wt_std ~ age_f + presurvey_btemp,
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_btemp_pol_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pol_lin)
	
	saveRDS(presurvey_btemp_pol_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol_lin.rds"))
 
 	# model with interaction	
 	presurvey_btemp_int_pol_lin <- sdmTMB(
			log_wt_std ~ age_f * presurvey_btemp,
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",			
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(presurvey_btemp_int_pol_lin)
 	
 	sims <- simulate(presurvey_btemp_int_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol_lin)
 	
  saveRDS(presurvey_btemp_int_pol_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol_lin.rds"))
 
  #### pcod ####
  
  # no interaction
	presurvey_btemp_pcod_lin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + presurvey_btemp,
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",				
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(presurvey_btemp_pcod_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_pcod_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pcod_lin)
	
	saveRDS(presurvey_btemp_pcod_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pcod_lin.rds"))
 
	# model with interaction	
 	presurvey_btemp_int_pcod_lin <- sdmTMB(
			log_wt_std ~ age_f * presurvey_btemp,
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",			
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 	
 	sanity(presurvey_btemp_int_pcod_lin)
 	
 	sims <- simulate(presurvey_btemp_int_pcod_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pcod)
 	
  saveRDS(presurvey_btemp_int_pcod_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pcod_lin.rds"))

  #### yellowfin sole ####
  
  # no interaction
	presurvey_btemp_yfin_lin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + presurvey_btemp,
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",				
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(presurvey_btemp_yfin_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(presurvey_btemp_yfin_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_yfin_lin)
	
	saveRDS(presurvey_btemp_yfin_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_yfin_lin.rds"))
 
 	# model with interaction	
 	presurvey_btemp_int_yfin_lin <- sdmTMB(
			log_wt_std ~ age_f * presurvey_btemp,
			data = yfinsole_dat,
			mesh = yfin_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",		
			control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))

 	sanity(presurvey_btemp_int_yfin_lin)
 	
 	sims <- simulate(presurvey_btemp_int_yfin_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_yfin_lin)
 	
  saveRDS(presurvey_btemp_int_yfin_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin_lin.rds"))

  
  ####################################################
	# yr prior btemp ####
	####################################################

	#### pollock ####
  
  # no interaction
	yrprior_btemp_pol_lin <- 
			sdmTMB(	
 					log_wt_std ~ age_f + yrprior_btemp,
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",		
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(yrprior_btemp_pol_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_pol_lin)
	
	saveRDS(yrprior_btemp_pol_lin, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_pol_lin.rds"))
 
 	# model with interaction	
 	yrprior_btemp_int_pol_lin <- 
 		sdmTMB(
			log_wt_std ~ age_f * yrprior_btemp,
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",		
			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1))

 	sanity(yrprior_btemp_int_pol_lin) 
 	
 	sims <- simulate(yrprior_btemp_int_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_pol_lin)
 	
  saveRDS(yrprior_btemp_int_pol_lin, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_int_pol_lin.rds"))
 
  #### pcod ####
  
  # no interaction	
 	yrprior_btemp_pcod_lin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + yrprior_btemp,
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",		
  			control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(yrprior_btemp_pcod_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_pcod_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_pcod_lin)
	
	saveRDS(yrprior_btemp_pcod_lin, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_pcod_lin.rds"))

  # model with interaction
 	yrprior_btemp_int_pcod_lin <- sdmTMB(
			log_wt_std ~ age_f * yrprior_btemp,
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",	
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 	
 	sanity(yrprior_btemp_int_pcod_lin) 
 	
 	sims <- simulate(yrprior_btemp_int_pcod_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_pcod_lin)
 	
  saveRDS(yrprior_btemp_int_pcod_lin, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_int_pcod_lin.rds"))

  #### yellowfin sole ####
  
  # no interaction
	yrprior_btemp_yfin_lin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + yrprior_btemp,
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",	
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(yrprior_btemp_yfin_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(yrprior_btemp_yfin_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_yfin_lin)
	
	saveRDS(yrprior_btemp_yfin_lin, 
  				file = here("./output/model output/sdmTMB output/yrprior_btemp_yfin_lin.rds"))

	# model with interaction	
 	yrprior_btemp_int_yfin_lin <- sdmTMB(
			log_wt_std ~ age_f * yrprior_btemp, 
			data = yfinsole_dat,
			mesh = yfin_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",	
			control = sdmTMBcontrol(nlminb_loops = 3,
															step.min = 0.01, step.max = 1)) 
 	
 	sanity(yrprior_btemp_int_yfin_lin)
 	
 	sims <- simulate(yrprior_btemp_int_yfin_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_yfin_lin)
 	
  saveRDS(yrprior_btemp_int_yfin_lin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin_lin.rds"))

  ####################################################
	# age 0 ####
	####################################################

  #### pollock ####
  
  # no interaction
	age0_btemp_pol_lin <- 
			sdmTMB(	
 					log_wt_std ~ age_f + age0_btemp,
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
  				time = "year",
					spatiotemporal = "IID",	
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(age0_btemp_pol_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(age0_btemp_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_pol_lin)
	
	saveRDS(age0_btemp_pol_lin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_pol_lin.rds"))
 
 	# model with interaction
 	age0_btemp_int_pol_lin <- 
 		sdmTMB(
			log_wt_std ~ age_f * age0_btemp,  
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
  		time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

 	sanity(age0_btemp_int_pol_lin) 
 	
 	sims <- simulate(age0_btemp_int_pol_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_pol_lin)
 	
  saveRDS(age0_btemp_int_pol_lin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_pol_lin.rds"))
 
  ##### pcod ####
  
  apply(is.na(pcod_dat_trim), 2, any) # any cols have NAs
  which(is.na(pcod_dat_trim$age0_btemp)) # which rows have NAs
  
  # drop NAs
  pcod_dat_trim <- pcod_dat_trim %>%
  	drop_na(age0_btemp)
 
  # adjust mesh due to errors
	pcod_mesh_trim <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

	# model without interaction
 	age0_btemp_pcod_lin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + age0_btemp,
				data = pcod_dat_trim,
				mesh = pcod_mesh_trim,
				spatial = "on",
 				time = "year",
				spatiotemporal = "IID",				
				control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1)) 
	
	sanity(age0_btemp_pcod_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(age0_btemp_pcod_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_pcod_lin)
	
	saveRDS(age0_btemp_pcod_lin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_pcod_lin.rds"))
 
	# model with interaction
 	age0_btemp_int_pcod_lin <- 
 		sdmTMB(
			log_wt_std ~ age_f * age0_btemp, 
			data = pcod_dat_trim,
			mesh = pcod_mesh_trim,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3))
 	
 	sanity(age0_btemp_int_pcod_lin)
 	
 	sims <- simulate(age0_btemp_int_pcod_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_pcod_lin)
 	
  saveRDS(age0_btemp_int_pcod_lin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_pcod_lin.rds"))

  # yellowfin sole ####
  
  # model with no interaction
	age0_btemp_yfin_lin <- 
			sdmTMB(	
 				log_wt_std ~ age_f + age0_btemp,
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
 				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1)) 
	
	sanity(age0_btemp_yfin_lin)
	
	# check residuals - simulation-based residuals
	sims <- simulate(age0_btemp_yfin_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_yfin_lin)
	
	saveRDS(age0_btemp_yfin_lin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_yfin_lin.rds"))
 
	# model with interaction
 	age0_btemp_int_yfin_lin <- 
 		sdmTMB(
			log_wt_std ~ age_f * age0_btemp, 
			data = yfinsole_dat,
			mesh = yfin_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1,
															step.min = 0.01, step.max = 1)) 
 	
 	sanity(age0_btemp_int_yfin_lin)
 	
 	sims <- simulate(age0_btemp_int_yfin_lin, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_yfin_lin)
 	
  saveRDS(age0_btemp_int_yfin_lin, 
  				file = here("./output/model output/sdmTMB output/age0_btemp_int_yfin_lin.rds"))