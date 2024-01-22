	yrprior_btemp_int_mod_pollock1 <-
		sdmTMB(
			formula = log_wt_std ~ 0 + s(presurvey_btemp, by = age_f, k = 4),
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 5, sigma_lt = 1, range_prob = 0.05, sigma_prob = 0.05)),
			control = sdmTMBcontrol(nlminb_loops = 3))
	
	sanity(yrprior_btemp_int_mod_pollock)
	
	yrprior_btemp_int_mod_pollock2 <- # won't run
		sdmTMB(
			formula = log_wt_std ~ 0 + age_f + s(presurvey_btemp, by = age_f, k = 4),
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 5, sigma_lt = 1, range_prob = 0.05, sigma_prob = 0.05)),
			control = sdmTMBcontrol(nlminb_loops = 3))
	
	yrprior_btemp_int_mod_pollock3 <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f + s(presurvey_btemp, by = age_f, k = 4),
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			priors = sdmTMBpriors(matern_st = pc_matern(range_gt = 5, sigma_lt = 1, range_prob = 0.05, sigma_prob = 0.05)),
			control = sdmTMBcontrol(nlminb_loops = 3))
	
	
	# linear so I can understand
	
	mod1 <-
		sdmTMB(
			formula =  log_wt_std ~ 0 + presurvey_btemp:age_f, 
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year")
	
	sanity(mod1)
	
	mod2 <- 
		sdmTMB(
			formula = log_wt_std ~ 0 + presurvey_btemp * age_f, 
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year")
	
	mod3 <-
		sdmTMB(
			formula = log_wt ~ 0 + presurvey_btemp * age_f,
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year")