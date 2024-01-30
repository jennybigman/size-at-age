# comparing models with different parameterizations of time

	# play with pcod data
	pcod2 <- pcod_dat

	# order age class
  levels_to_ord <- sort(unique(pcod2$age_f))
  pcod2$age_f_ord <- ordered(pcod2$age_f, levels = c(levels_to_ord))	
  
  # drop unused factor levels of year (error when do this outside function)
  pcod2$year_f <- droplevels(pcod2$year_f)
  
	# make mesh
	mesh <- make_mesh(pcod2, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
				
	# set up formulas
	form_no_yr <- paste0("log_wt_std ~ 0 + s(" , "presurvey_btemp", ", k = 3)")
	form_RE <- paste0("log_wt_std ~ 0 + s(" , "presurvey_btemp", ", k = 3) + (1|year_f)")
	form_FE <- paste0("log_wt_std ~ 0 + s(" , "presurvey_btemp", ", k = 3) + year_f")

	# model without any effect of time year ####
	mod_no_yr <-
		sdmTMB(
			formula = as.formula(form_no_yr),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

	# model with time-varying effect ####					
	mod_tv <-
		sdmTMB(
			formula = as.formula(form_no_yr),
			data = pcod2,
			mesh = mesh,
			time_varying = ~ 1,
			time = 'year',
			spatial = "on",
			spatiotemporal = "off",
			extra_time = 2020:2025,
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	# model with time-varying and spatiotemporal effect ####					
	mod_tv_st <-
		sdmTMB(
			formula = as.formula(form_no_yr),
			data = pcod2,
			mesh = mesh,
			time_varying = ~ 1,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			extra_time = 2020:2025,
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	# model with time-varying effect of temp specifically (won't converge) ####					
	mod_tv2 <-
		sdmTMB(
			formula = as.formula(form_no_yr),
			data = pcod2,
			mesh = mesh,
			time_varying = ~ 1 + presurvey_btemp,
			time = 'year',
			spatial = "on",
			spatiotemporal = "off",
			extra_time = 2020:2025,
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	# model with just spatiotemporal (and spatial effects)
	mod_st <-
		sdmTMB(
			formula = as.formula(form_no_yr),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

	# model with random effect of year
	mod_RE <-
		sdmTMB(
			formula = as.formula(form_RE),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			#extra_time = 2020:2025,
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	# model with fixed effect of year #### won't converge
	mod_FE <-
		sdmTMB(
			formula = as.formula(form_FE),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			#extra_time = 2020:2025,
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	# model with random effect of year and spatiotemporal effects ####
	mod_RE_st <-
		sdmTMB(
			formula = as.formula(form_RE),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

	# sanity
	sanity(mod)
	sanity(mod_tv)
	sanity(mod_st)
	sanity(mod_RE)
	sanity(mod_RE_st)
	sanity(mod_tv_st)
	
	# compare models
	
	AIC(mod)
	AIC(mod_tv)
	AIC(mod_st)
	AIC(mod_RE)
	AIC(mod_RE_st)
	AIC(mod_tv_st)
	
	
	# check residuals 
		
	sims <- simulate(mod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod)

	sims <- simulate(mod_tv, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_tv)
	
	sims <- simulate(mod_RE, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_RE)
	
	sims <- simulate(mod_RE_st, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_RE_st)

	sims <- simulate(mod_tv_st, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_tv_st)

	# plots
	yrs <- sort(unique(pcod2$year))
	
	
	
	# predict with time_varying mod_st
	new_dat <- expand_grid(
		#age_f_ord = unique(pcod2$age_f_ord),
		presurvey_btemp = seq(from = min(pcod2$presurvey_btemp),
										 to = max(pcod2$presurvey_btemp),
										 length.out = 100))#,
		#year = yrs)
	
	mod_no_yr_pred <- predict(mod_st,
										 #newdata = new_dat,
										 se_fit = TRUE,
										 re_form = NA,
										 return_tmb_object = FALSE)
	
	# high and low CIs
	test_preds$low <- test_preds$est + (qnorm(0.025) * test_preds$est_se)
	test_preds$high <- test_preds$est + (qnorm(0.975) * test_preds$est_se)
	
	# plot
	test_plot <-
			ggplot(test_preds, aes(year, est)) +
			geom_line() +
			#geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 6, scales = "free") 

	
	#### with an interaction #####
	
	# set up formulas
	form_int <- paste0("log_wt_std ~ 0 + s(" , "presurvey_btemp", ", by = age_f_ord, k = 4)")
		
	## model without time-varying effect ####
	
	# set prior
	pc_mod <- pc_matern(range_gt = 150, sigma_lt = 1)

	mod_int <-
		sdmTMB(
			formula = as.formula(form_int),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
					
	mod_int_tv <-
		sdmTMB(
			formula = as.formula(form_int),
			data = pcod2,
			mesh = mesh,
			time_varying = ~ 1,
			time = 'year',
			spatial = "on",
			spatiotemporal = "off",
			extra_time = 2020:2025)
	
	# sanity
	sanity(mod)
	sanity(mod_tv)

	# check residuals (especially for the model with the time-varying effect)
		
	# model w/o time-varying effect
	sims <- simulate(mod_int, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_int)

	# model w/ time-varying effect
	sims <- simulate(mod_int_tv, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_int_tv)

	# compare models
	
	AIC(mod_int)
	AIC(mod_int_tv)
	
	# predict with time_varying model
	new_dat <- expand_grid(
		age_f_ord = unique(pcod2$age_f_ord),
		presurvey_btemp = seq(from = min(pcod2$presurvey_btemp),
										 to = max(pcod2$presurvey_btemp),
										 length.out = 100),
		year = 2020:2025)
	
	test_preds_int <- predict(mod_int_tv,
										 newdata = new_dat,
										 se_fit = TRUE,
										 re_form = NA,
										 return_tmb_object = FALSE)
	
	# high and low CIs
	test_preds$low <- test_preds$est + (qnorm(0.025) * test_preds$est_se)
	test_preds$high <- test_preds$est + (qnorm(0.975) * test_preds$est_se)
	
	# plot
	test_plot <-
			ggplot(test_preds, aes(presurvey_btemp, est, group = year, color = year)) +
			geom_line() +
			#geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 6, scales = "free") 
	
		test_plot <-
			ggplot(test_preds, aes(year, est, group = presurvey_btemp, color = presurvey_btemp)) +
			geom_line() +
			#geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 6, scales = "free") 


	