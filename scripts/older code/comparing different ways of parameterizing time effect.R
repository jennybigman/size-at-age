# comparing models with and without time-varying effect no CV

	pcod2 <- pcod_dat

	# add quantiles for folds
	
		first_q <- quantile(pcod2$latitude, 0)
		sec_q <- quantile(pcod2$latitude, 0.25)
		third_q <- quantile(pcod2$latitude, 0.5)
		fourth_q <- quantile(pcod2$latitude, 0.75)
		fifth_q <- 	quantile(pcod2$latitude, 1)

		pcod2 <- pcod2 %>%
			mutate(fold = case_when(
				between(latitude, first_q, sec_q) ~ 1,
				between(latitude, sec_q, third_q) ~ 2,
				between(latitude, third_q, fourth_q) ~ 3,
				between(latitude, fourth_q, fifth_q) ~ 4))
	
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/test/"
	
	# order age class
  levels_to_ord <- sort(unique(pcod2$age_f))
  pcod2$age_f_ord <- ordered(pcod2$age_f, levels = c(levels_to_ord))	
  
  # drop unused factor levels of year (error when do this outside function)
  pcod2$year_f <- droplevels(pcod2$year_f)
  
	# make mesh
	mesh <- make_mesh(pcod2, xy_cols = c("X", "Y"), n_knots = 400, type = "kmeans")
				
	# set up formulas
	form1 <- paste0("log_wt_std ~ 0 + s(" , "presurvey_btemp", ", k = 3)")
	## model without time-varying effect ####
	
	# set prior
	pc_mod <- pc_matern(range_gt = 150, sigma_lt = 1)

	mod <-
		sdmTMB(
			formula = as.formula(form1),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
					
	mod_tv <-
		sdmTMB(
			formula = as.formula(form1),
			data = pcod2,
			mesh = mesh,
			time_varying = ~ 1,
			time = 'year',
			spatial = "on",
			spatiotemporal = "off",
			extra_time = 2020:2025,
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
						
	mod_tv2 <-
		sdmTMB(
			formula = as.formula(form1),
			data = pcod2,
			mesh = mesh,
			time_varying = ~ 1,
			time = 'year',
			spatial = "on",
			spatiotemporal = "off",
			extra_time = 2020:2025,
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	mod_st <-
		sdmTMB(
			formula = as.formula(form1),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			#priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

		
	# sanity
	sanity(mod)
	sanity(mod_tv)
	sanity(mod_st)

	# check residuals (especially for the model with the time-varying effect)
		
	# model w/o time-varying effect
	sims <- simulate(mod, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod)

	# model w/ time-varying effect
	sims <- simulate(mod_tv, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_tv)

	# compare models
	
	AIC(mod)
	AIC(mod_tv)
	AIC(mod_st)
	
	## what about a model with a random effect of year? about the same 

	# is a model with a random effect of year a better fit?
	form_RE <- paste0("log_wt_std ~ 0 + s(" , "presurvey_btemp", ") + (1|year_f)")

	mod_RE <-
		sdmTMB(
			formula = as.formula(form_RE),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			#extra_time = 2020:2025,
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	sanity(mod_RE)
	AIC(mod_RE)
	
	# predict with time_varying model
	new_dat <- expand_grid(
		#age_f_ord = unique(pcod2$age_f_ord),
		presurvey_btemp = seq(from = min(pcod2$presurvey_btemp),
										 to = max(pcod2$presurvey_btemp),
										 length.out = 100),
		year = 2020:2025)
	
	test_preds <- predict(mod_tv,
										 newdata = new_dat,
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
			extra_time = 2020:2025, # model won't converge without this
			priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
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


	