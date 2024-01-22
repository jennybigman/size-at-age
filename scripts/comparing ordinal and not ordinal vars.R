

	test <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f + s(presurvey_btemp, by = age_f, k = 4),
			data = pcod2,
			mesh = mesh,
			spatial = "off",
			spatiotemporal = "off")

	test2 <-
		sdmTMB(
			formula = log_wt ~ 0 + age_f_ord + s(presurvey_btemp, by = age_f_ord, k = 4),
			data = pcod2,
			mesh = mesh,
			spatial = "off",
			spatiotemporal = "off")

	#linear
	
		test <-
		sdmTMB(
			formula = log_wt_std ~ 0 + age_f_ord * presurvey_btemp,
			data = pcod2,
			mesh = mesh,
			spatial = "off",
			spatiotemporal = "off")
		
		# brms
	brm_test <- brm(log_wt_std ~ 0 + age_f_ord + s(presurvey_btemp, by = age_f_ord, k = 4),
													data = pcod2,
													family = gaussian(),
													save_pars = save_pars(all = TRUE),
                  				warmup = 20, iter = 100, chains = 4, cores = 4)
														

	preds <- predict(test)
	
			ggplot(preds, aes(presurvey_btemp, est)) +
			geom_line(color = "black") +
			facet_wrap(~ age_f_ord) +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			theme_sleek()
			
	gamm_test <- gamm4(log_wt_std ~  s(presurvey_btemp, by = age_f_ord, k=4),
                        	data=pcod2, REML=TRUE) 