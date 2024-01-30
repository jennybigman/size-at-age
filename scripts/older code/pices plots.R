# PICES 2023 plots


	## resid checks for models that need it ####

	# pollock mods ####
	
	pol_s_list <- lapply(pol_mod_list, sanity)
	
	#age0_boxy_int_mod_pollock
	#age0_btemp_int_mod_pollock
	#age0_btemp_no_int_mod_pollock
	#presurvey_boxy_int_mod_pollock
	#yrprior_btemp_int_mod_pollock

  # 1. age0_boxy_int_mod_pollock
	
	age0_boxy_int_mod_pollock <- read_rds(
		file = paste0(here(), file_path_all, "age0_boxy_int_mod_pollock", ".rds"))
		
	sims <- simulate(age0_boxy_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_boxy_int_mod_pollock)
			
	#good
	
	# 2. age0_btemp_int_mod_pollock
	
	age0_btemp_int_mod_pollock <- read_rds(
		file = paste0(here(), file_path_all, "age0_btemp_int_mod_pollock", ".rds"))
		
	sanity(age0_btemp_int_mod_pollock)
	
	sims <- simulate(age0_btemp_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_mod_pollock)
			
	#good
	

	# 3. 	age0_btemp_no_int_mod_pollock
	
	age0_btemp_no_int_mod_pollock <- read_rds(
		file = paste0(here(), file_path_all, "age0_btemp_no_int_mod_pollock", ".rds"))
		
	sanity(age0_btemp_no_int_mod_pollock)
	
	sims <- simulate(age0_btemp_no_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_no_int_mod_pollock)
			
	#refit
	
	# 4. 	presurvey_boxy_int_mod_pollock
	
	presurvey_boxy_int_mod_pollock <- read_rds(
		file = paste0(here(), file_path_all, "presurvey_boxy_int_mod_pollock", ".rds"))
		
	sanity(presurvey_boxy_int_mod_pollock)
	
	sims <- simulate(presurvey_boxy_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_mod_pollock)
			
	#good
	
	# 5. 	yrprior_btemp_int_mod_pollock
	
	yrprior_btemp_int_mod_pollock <- read_rds(
		file = paste0(here(), file_path_all, "yrprior_btemp_int_mod_pollock", ".rds"))
		
	sanity(yrprior_btemp_int_mod_pollock)
	
	sims <- simulate(yrprior_btemp_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_mod_pollock)
			
	#refit
	
	# refit two mods

	# order age class
  levels_to_ord <- sort(unique(pollock_dat$age_f))
  pollock_dat$age_f_ord <- ordered(pollock_dat$age_f, levels = c(levels_to_ord))	
  
  # drop unused factor levels of year (error when do this outside function)
  pollock_dat$year_f <- droplevels(pollock_dat$year_f)
  
	# make mesh
	mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), n_knots = 200, type = "kmeans")
	
	# 1. age0_btemp_no_int_mod_pollock			
	age0_btemp_no_int_mod_pollock <- 
							sdmTMB(
								formula = log_wt_std ~ age_f_ord + s(age0_btemp),
								data = pollock_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(age0_btemp_no_int_mod_pollock)

	write_rds(age0_btemp_no_int_mod_pollock,
						file = paste0(here(), file_path_all, "age0_btemp_no_int_mod_pollock.rds"))
	
	# 2. yrprior_btemp_int_mod_pollock			
	yrprior_btemp_int_mod_pollock <- 
							sdmTMB(
								formula = log_wt_std ~ s(yrprior_btemp, by = age_f_ord),
								data = pollock_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(yrprior_btemp_int_mod_pollock)
	
	sims <- simulate(yrprior_btemp_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_mod_pollock)
	
	write_rds(yrprior_btemp_int_mod_pollock,
						file = paste0(here(), file_path_all, "yrprior_btemp_int_mod_pollock.rds"))
	
	
	
	# pcod mods ####
	
	# age0_btemp_int_mod_pcod
	# presurvey_boxy_int_mod_pcod
	# presurvey_btemp_int_mod_pcod
	# yrprior_boxy_int_mod_pcod
	# yrprior_btemp_int_mod_pcod
	
	# 1. age0_btemp_int_mod_pcod
	age0_btemp_int_mod_pcod <- read_rds(
		file = paste0(here(), file_path_all, "age0_btemp_int_mod_pcod", ".rds"))
		
	sanity(age0_btemp_int_mod_pcod)
	
	sims <- simulate(age0_btemp_int_mod_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_btemp_int_mod_pcod)
			
	#refit
	
	# 2. presurvey_boxy_int_mod_pcod
	presurvey_boxy_int_mod_pcod <- read_rds(
		file = paste0(here(), file_path_all, "presurvey_boxy_int_mod_pcod", ".rds"))
		
	sanity(presurvey_boxy_int_mod_pcod)
	
	sims <- simulate(presurvey_boxy_int_mod_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_mod_pcod)
			
	#good
	
	# 3. presurvey_btemp_int_mod_pcod
	presurvey_btemp_int_mod_pcod <- read_rds(
		file = paste0(here(), file_path_all, "presurvey_btemp_int_mod_pcod", ".rds"))
		
	sanity(presurvey_btemp_int_mod_pcod)
	
	sims <- simulate(presurvey_btemp_int_mod_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_mod_pcod)
			
	#refit
	
	# 4. yrprior_boxy_int_mod_pcod
	yrprior_boxy_int_mod_pcod <- read_rds(
		file = paste0(here(), file_path_all, "yrprior_boxy_int_mod_pcod", ".rds"))
		
	sanity(yrprior_boxy_int_mod_pcod)
	
	sims <- simulate(yrprior_boxy_int_mod_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_int_mod_pcod)
			
	#refit
	
	# 5. yrprior_btemp_int_mod_pcod
	yrprior_btemp_int_mod_pcod <- read_rds(
		file = paste0(here(), file_path_all, "yrprior_btemp_int_mod_pcod", ".rds"))
		
	sanity(yrprior_btemp_int_mod_pcod)
	
	sims <- simulate(yrprior_btemp_int_mod_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_mod_pcod)

	# good
	
	# refit
	#age0_btemp_int_mod_pcod
	#presurvey_btemp_int_mod_pcod
	#yrprior_boxy_int_mod_pcod
	
	# refit three mods
	pcod_dat_trim <- pcod_dat_trim %>%
		drop_na(age0_boxy)

	# order age class
  levels_to_ord <- sort(unique(pcod_dat_trim$age_f))
  pcod_dat_trim$age_f_ord <- ordered(pcod_dat_trim$age_f, levels = c(levels_to_ord))	
  
  # drop unused factor levels of year (error when do this outside function)
  pcod_dat_trim$year_f <- droplevels(pcod_dat_trim$year_f)
  
	# make mesh
	mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), n_knots = 200, type = "kmeans")
	
	# 1. age0_btemp_no_int_mod_pollock ### need to fit this with pcod_dat_trim
	age0_btemp_int_mod_pcod <- 
							sdmTMB(
								formula = log_wt_std ~ s(age0_btemp, by = age_f_ord),
								data = pcod_dat_trim,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(age0_btemp_int_mod_pcod)

	write_rds(age0_btemp_int_mod_pcod,
						file = paste0(here(), file_path_all, "age0_btemp_int_mod_pcod.rds"))
	
	# 2. presurvey_btemp_int_mod_pcod
	presurvey_btemp_int_mod_pcod <- 
						sdmTMB(
							formula = log_wt_std ~ s(presurvey_btemp, by = age_f_ord),
							data = pcod_dat_trim,
							mesh = mesh,
							spatial = "on",
							spatiotemporal = "off",
							control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(presurvey_btemp_int_mod_pcod)

	sims <- simulate(presurvey_btemp_int_mod_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_mod_pcod)

	write_rds(presurvey_btemp_int_mod_pcod,
						file = paste0(here(), file_path_all, "presurvey_btemp_int_mod_pcod.rds"))
	
	# 3. yrprior_boxy_int_mod_pcod ## get this to work with pcod_dat_trim
	yrprior_boxy_int_mod_pcod <- 
						sdmTMB(
							formula = log_wt_std ~ s(yrprior_boxy, by = age_f_ord),
							data = pcod_dat_trim,
							mesh = mesh,
							spatial = "on",
							spatiotemporal = "off",
							control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(yrprior_boxy_int_mod_pcod)

	sims <- simulate(yrprior_boxy_int_mod_pcod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_int_mod_pcod)

	write_rds(yrprior_boxy_int_mod_pcod,
						file = paste0(here(), file_path_all, "yrprior_boxy_int_mod_pcod.rds"))
	
	
	# yfin mods
	# presurvey_boxy_int_mod_yfsole
	# yrprior_btemp_int_mod_yfsole
	
	# 1. yrprior_btemp_int_mod_pcod
	presurvey_boxy_int_mod_yfsole <- read_rds(
		file = paste0(here(), file_path_all, "presurvey_boxy_int_mod_yfsole", ".rds"))
		
	sanity(presurvey_boxy_int_mod_yfsole)
	
	sims <- simulate(presurvey_boxy_int_mod_yfsole, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_boxy_int_mod_yfsole)

	# good
	
	# 2. yrprior_btemp_int_mod_yfsole
	yrprior_btemp_int_mod_yfsole <- read_rds(
		file = paste0(here(), file_path_all, "yrprior_btemp_int_mod_yfsole", ".rds"))
		
	sanity(yrprior_btemp_int_mod_yfsole)
	
	sims <- simulate(yrprior_btemp_int_mod_yfsole, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_int_mod_yfsole)

	# good
	
	
	################################################
	# AIC ####
	################################################
	
	# separate models by species ####
  
	# pollock #
  
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yf", names(mod_list))]

	## comparisons ##
	
	# 1. int or no int mods
	lapply(pol_mod_list, AIC)
	
	# temp
		# age0: int model lower AIC
		# presurvey: int model lower AIC
		# yrprior: int model lower AIC
	# oxygen
		# age0: int model lower AIC
		# presurvey: no int model lower AIC
		# yrprior: no int model lower AIC
	
	lapply(pcod_mod_list, AIC)
	# temp
		# age0: int model better
		# presurvey: no int model better
		# yrprior: no int model better
	# oxygen
		# age0: int model better
		# presurvey: no int model better
		# yrprior:  int model better
	
	lapply(yfin_mod_list, AIC)
	# temp
		# age0: int model better
		# presurvey: int model better
		# yrprior: int model better
	# oxygen
		# age0: int model better
		# presurvey: int model better
		# yrprior: int model better
	
	
	# oxy vs temp
	
	# pollock
	pol_mod_oxy_list <- pol_mod_list[grep("oxy", names(pol_mod_list))]
	lapply(pol_mod_oxy_list, AIC)
	# best oxy: presurvey_boxy_no_int_mod_pollock: 71895.81
	pol_mod_temp_list <- pol_mod_list[grep("temp", names(pol_mod_list))]
	lapply(pol_mod_temp_list, AIC)
	# best temp: age0_btemp_int_mod_pollock: 71323.73
	# best overall - temp (age0)
	
	# pcod
	pcod_mod_oxy_list <- pcod_mod_list[grep("oxy", names(pcod_mod_list))]
	lapply(pcod_mod_oxy_list, AIC)
	# best oxy: age0_boxy_int_mod_pcod: 38976.48
	pcod_mod_temp_list <- pcod_mod_list[grep("temp", names(pcod_mod_list))]
	lapply(pcod_mod_temp_list, AIC)
	# best temp: presurvey_btemp_no_int_mod_pcod: 39043.56

	# yfin
	yfin_mod_oxy_list <- yfin_mod_list[grep("oxy", names(yfin_mod_list))]
	lapply(yfin_mod_oxy_list, AIC)
	# best oxy: yrprior_boxy_int_mod_yfsole: 94981.41
	yfin_mod_temp_list <- yfin_mod_list[grep("temp", names(yfin_mod_list))]
	lapply(yfin_mod_temp_list, AIC)
	# best temp: yrprior_btemp_int_mod_yfsole: 94306.88
	
	# fit models with interactions
	
	# pollock
	
	# make mesh
	mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), n_knots = 250, type = "kmeans")

	pollock_two_int_mod <- 
							sdmTMB(
								formula = log_wt_std ~ t2(age0_btemp, presurvey_boxy, by = age_f_ord),
								data = pollock_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(pollock_two_int_mod)

	sims <- simulate(pollock_two_int_mod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(pollock_two_int_mod)

	write_rds(pollock_two_int_mod,
						file = paste0(here(), file_path_all, "pollock_two_int_mod.rds"))
	
	# pcod
	
	# make mesh
	mesh <- make_mesh(pcod_dat_trim_10, xy_cols = c("X", "Y"), n_knots = 250, type = "kmeans")
	
	pc <- pc_matern(range_gt = 26, sigma_lt = 0.85)

	pcod_two_int_mod <- 
							sdmTMB(
								formula = log_wt_std ~ t2(age0_boxy, presurvey_btemp, by = age_f_ord),
								data = pcod_dat_trim_10,
								mesh = mesh,
								priors = sdmTMBpriors(matern_s = pc),
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3))
	
	pcod_two_int_mod2 <- run_extra_optimization(pcod_two_int_mod, 
																							nlminb_loops = 3, newton_loops = 1)
	
	sanity(pcod_two_int_mod)

	sims <- simulate(pcod_two_int_mod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(pcod_two_int_mod)

	write_rds(pcod_two_int_mod,
						file = paste0(here(), file_path_all, "pcod_two_int_mod.rds"))
	
	
	# yfin

	# order age class
  levels_to_ord <- sort(unique(yfinsole_dat$age_f))
  yfinsole_dat$age_f_ord <- ordered(yfinsole_dat$age_f, levels = c(levels_to_ord))	

	# make mesh
	mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), n_knots = 250, type = "kmeans")

	yfsole_two_int_mod <- 
							sdmTMB(
								formula = log_wt_std ~ t2(yrprior_btemp, yrprior_boxy, by = age_f_ord),
								data = yfinsole_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "off",
								control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(yfsole_two_int_mod)

	sims <- simulate(yfsole_two_int_mod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yfsole_two_int_mod)

	write_rds(yfsole_two_int_mod,
						file = paste0(here(), file_path_all, "yfsole_two_int_mod.rds"))
	

	## AIC of best model vs two way int
	
	AIC(pollock_two_int_mod) # better
	# better than best model of temp or oxy (age0_btemp_int_mod_pollock: 71323.73)
	
	AIC(yfsole_two_int_mod) # better
	# # better than best model of temp or oxy (yrprior_btemp_int_mod_yfsole: 94306.88)
