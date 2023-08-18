# model comparison of sdmTMB mods
	
	## read in models ####
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output")))

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/"))
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
	# pollock ####
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	lapply(pol_mod_list, AIC)
	
	unlist(pol_mod_list)
	
	# model checks
	presurvey_btemp_int_pol <- pol_mod_list[[1]]
	
	sanity(presurvey_btemp_int_pol)
 
	# coefs
	tidy(presurvey_btemp_int_pol, conf.int = TRUE)
	tidy(presurvey_btemp_int_pol, "ran_pars", conf.int = TRUE)
 
	# inspect randomized quantile residuals
	pollock_dat$resids <- residuals(presurvey_btemp_int_pol)
	
	qqnorm(pollock_dat$resids)
	qqline(pollock_dat$resids)
	
	ggplot(pollock_dat, aes(X, Y, col = resids)) +
		scale_color_gradient2() +
		geom_point() +
		facet_wrap(~age_f) +
		coord_fixed()
	
	samps <- sdmTMBextra::predict_mle_mcmc(presurvey_btemp_int_pol, 
																				 mcmc_warmup = 100, mcmc_iter = 101)
	
	r <- residuals(presurvey_btemp_int_pol,
								 "mle-mcmc", mcmc_samples = samps)
	qqnorm(r, ylim = c(-20, 10))
	qqline(r)
	
	# look at spatial and spatiotemporal random effects
	nd <- expand.grid(
		age_f = unique(pollock_dat$age_f),
		presurvey_btemp = pollock_dat$presurvey_btemp,
		jday = pollock_dat$jday
	)
	preds <- predict(presurvey_btemp_int_pol, newdata = nd)
	
	
	# pcod ####
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	lapply(pcod_mod_list, AICc)
	
	# yfin ####
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
		
	lapply(yfin_mod_list, AICc)
