# comparing models with and without time-varying effect

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
	mesh <- make_mesh(pcod2, xy_cols = c("X", "Y"), n_knots = 300, type = "kmeans")
				
	# set up formulas
	form1 <- paste0("log_wt_std ~ 0 + s(" , "presurvey_btemp", ", k = 4)")
		
	## model without time-varying effect ####
	
	# set prior
	pc_mod <- pc_matern(range_gt = 150, sigma_lt = 1)

	mod <-
		sdmTMB_cv(
			formula = as.formula(form1),
			data = pcod2,
			mesh = mesh,
			k_folds = max(pcod2$fold),
      fold_ids = pcod2$fold,
			spatial = "on",
			spatiotemporal = "off",
			priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
					
	mod_tv <-
		sdmTMB_cv(
			formula = as.formula(form1),
			data = pcod2,
			mesh = mesh,
			time_varying = ~ 1,
			time = 'year',
			k_folds = max(pcod2$fold),
      fold_ids = pcod2$fold,
			spatial = "on",
			spatiotemporal = "off",
			extra_time = 2020:2025,
			priors = sdmTMBpriors(matern_s = pc_mod),
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	# sanity
	sanity(mod$models[[1]])
	sanity(mod_tv$models[[1]])

	# check residuals (especially for the model with the time-varying effect)
		
	# model w/o time-varying effect
	mod1 <- mod$models[[1]]
	sims <- simulate(mod1, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod1)

	# model w/ time-varying effect
	mod2 <- mod_tv$models[[1]]
	sims <- simulate(mod2, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod2)

	# compare models
	
	mod$sum_loglik
	mod_tv$sum_loglik	

	# can you predict with a time varying effect without inputting a year? NO
	new_dat <- expand_grid(
		age_f_ord = unique(pcod2$age_f_ord),
		presurvey_btemp = seq(from = min(pcod2$presurvey_btemp),
										 to = max(pcod2$presurvey_btemp),
										 length.out = 50),
		year = 2020)
	
	test_preds <- predict(mod2,
										 newdata = new_dat,
										 se_fit = TRUE,
										 re_form = NA,
										 return_tmb_object = FALSE,
										 extra_time = 2020:2025)
	
	
	# is a model with a random effect of year a better fit?
	form_RE <- paste0("log_wt ~ 0 + age_f_ord + s(" , "presurvey_btemp", ") + (1|year_f)")

	mod_RE <-
		sdmTMB_cv(
			formula = as.formula(form_RE),
			data = pcod2,
			mesh = mesh,
			k_folds = max(pcod2$fold),
      fold_ids = pcod2$fold,
			spatial = "on",
			spatiotemporal = "off",
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))
	
	mod3 <- mod_RE$models[[1]]
	
	sanity(mod3)
	
	mod_RE$sum_loglik

