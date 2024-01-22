	
	form_int <- paste0("")

	# 1. pollock yr prior btemp
	# 2. cod yr prior btemp
	# 3. cod presurvey boxy
	# 4. yfin yr prior btemp
	
	# 1. pollock yr prior btemp
	
	pollock_dat <- dat_all %>% filter(short_name == "pollock")
	
	mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)

	yrprior_btemp_int_mod_pollock <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f + s(yrprior_btemp, by = age_f, k = 3),
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			#control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1),
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 100, sigma_lt = 2),
				matern_s  = pc_matern(range_gt = 100, sigma_lt = 2)))
	
	sanity(yrprior_btemp_int_mod_pollock)


		write_rds(yrprior_btemp_int_mod_pollock, file = paste0(here(), file_path_all, "yrprior_btemp_int_mod_pollock.rds"))

	
	
	# 2. cod yr prior btemp
	pcod_dat <- dat_all %>% filter(short_name == "pcod")

	mesh <- make_mesh(pcod_dat, xy_cols = c("X", "Y"), n_knots = 300, type = "kmeans")

	yrprior_btemp_int_mod_pcod <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f + s(yrprior_btemp, by = age_f, k = 4),
			data = pcod_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			control = sdmTMBcontrol(nlminb_loops = 3),
			priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 5, sigma_lt = 1, range_prob = 0.05, sigma_prob = 0.05)))

	
	sanity(yrprior_btemp_int_mod_pcod)

	
	
	pars <- sdmTMB::get_pars(yrprior_btemp_int_mod_pcod)

	# rebuild  model updating some elements
	kappa_map <- factor(rep(NA, length(pars$ln_kappa)))

	# fit model	
	yrprior_btemp_int_mod_pcod <- update(
  	yrprior_btemp_int_mod_pcod,
  		control = sdmTMBcontrol(
    	start = list(
    	  ln_kappa = pars$ln_kappa #<
    	),
    	map = list(
    	  ln_kappa = kappa_map #<
    	)),do_fit = TRUE #<
	)
	
		write_rds(yrprior_btemp_int_mod_pcod, 
							file = paste0(here(), 
							file_path_all, "yrprior_btemp_int_mod_pcod.rds"))

	
	# 3. cod presurvey btemp
	mesh <- make_mesh(pcod_dat, xy_cols = c("X", "Y"), n_knots = 200, type = "kmeans")

	presurvey_btemp_int_mod_pcod <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f + s(presurvey_btemp, by = age_f, k = 4),
			data = pcod_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 5, sigma_lt = 1, range_prob = 0.05, sigma_prob = 0.05)))

	
	sanity(presurvey_btemp_int_mod_pcod)

		write_rds(presurvey_btemp_int_mod_pcod, 
										file = paste0(here(), file_path_all, "presurvey_btemp_int_mod_pcod.rds"))
	
			# four models had issues - all with an interaction ####

	# 4. yfin yrprior btemp
	mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), n_knots = 300, type = "kmeans")

	yrprior_btemp_int_mod_yfin <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f + s(yrprior_boxy, by = age_f, k = 4),
			data = yfinsole_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 5, sigma_lt = 1, range_prob = 0.05, sigma_prob = 0.05)),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

	sanity(yrprior_boxy_int_mod_yfin)
						