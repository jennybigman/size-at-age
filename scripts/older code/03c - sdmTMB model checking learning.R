#03 model checking exploration

 presurvey_btemp_int_pol <- readRDS(
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol.rds"))
 
 tidy(presurvey_btemp_int_pol, conf.int = TRUE)
 tidy(presurvey_btemp_int_pol, "ran_pars", conf.int = TRUE) 
 
 pollock_dat$residuals <- residuals(presurvey_btemp_int_pol, 
 																		type = "response")
 
 qqnorm(pollock_dat$residuals)
 qqline(pollock_dat$residuals)
 # issue in tails?
 
 ggplot(pollock_dat, aes(X, Y, col = residuals)) +
 	geom_point() +
 	facet_wrap( ~ age_f) +
 	scale_colour_gradient2() +
 	coord_fixed()
 # these look okay to me
 
 # MCMC-based residuals
	set.seed(123)
	samps <- sdmTMBextra::predict_mle_mcmc(presurvey_btemp_int_pol, 
																				 mcmc_warmup = 100, mcmc_iter = 101)
	
	r <- residuals(presurvey_btemp_int_pol, mcmc_samples = samps)
	qqnorm(r)
	qqline(r)
	# don't work for models with dispformula?
 
	rq_res <- residuals(presurvey_btemp_int_pol)
	# again, don't work with models with dispformula
	
	# simulation based residuals with the sdmTMB extra pkg
	simulate(presurvey_btemp_int_pol, nsim = 500) %>%
  	sdmTMBextra::dharma_residuals(presurvey_btemp_int_pol)	
	
	sims <- simulate(presurvey_btemp_int_pol, nsim = 500)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol)

	## using DHARMa tools
	
	# simulations from the fitted model to use with simulation-based residuals
	# returns matrix where each row is a row of data and each col is a simulation draw
	sims <- simulate(presurvey_btemp_int_pol, nsim = 500)
	
	# predicted response for the resid vs fitted plot should ideally not include random effects:
	pred_fixed <- presurvey_btemp_int_pol$family$linkinv(predict(presurvey_btemp_int_pol)$est_non_rf)
	
	d_resids <- DHARMa::createDHARMa(
		simulatedResponse = sims,
		observedResponse = pollock_dat$log_wt_std,
		fittedPredictedResponse = pred_fixed
	)
	
	plot(d_resids)

	DHARMa::testResiduals(d_resids)	

	DHARMa::testSpatialAutocorrelation(d_resids, x = pollock_dat$X, y = pollock_dat$Y)	
	# have several obs per location so need unique x,y vals
	
	d_resids_recalc <- DHARMa::recalculateResiduals(d_resids)
	
	DHARMa::testSpatialAutocorrelation(d_resids_recalc, x = pollock_dat$X, y = pollock_dat$Y)
	# still didn't work...need to explore
	
	
	# look at residuals per age class
	sims <- simulate(presurvey_btemp_int_pol, nsim = 250)
	
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_int_pol)
	
	resid_mat <- sims - pollock_dat$log_wt_std

	resid_long <- resid_mat %>%
		as.data.frame() %>%
		cbind(pollock_dat %>% select(age_f), .) %>%
		pivot_longer(-age_f, names_to = "iter", values_to = "resid_est")
	
	resid_dat <- resid_long %>% 
		group_by(age_f) %>%
		summarise(mean_resid = mean(resid_est),
							high_resid = quantile(resid_est, 0.975),
							low_resid = quantile(resid_est, 0.025))
	
	ggplot(resid_dat) +
		geom_pointrange(aes(x = age_f, y = mean_resid, 
												ymin = low_resid, ymax = high_resid)) +
		geom_hline(yintercept = 0) +
		theme_sleek()
	
	# using DHARMa pkg
	
	sims <- simulateResiduals(presurvey_btemp_int_pol, plot = FALSE)
	resids <- residuals(sims)

	pol_resids <- bind_cols(pollock_dat, resids) %>% rename(resids = "...40")
	
