# carrying one model through to prediction

	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/test/"
	
	# play with pcod data
	pcod2 <- pcod_dat

	# order age class
  levels_to_ord <- sort(unique(pcod2$age_f))
  pcod2$age_f_ord <- ordered(pcod2$age_f, levels = c(levels_to_ord))	

  # drop unused factor levels of year (error when do this outside function)
  pcod2$year_f <- droplevels(pcod2$year_f)
  
	# make mesh
	mesh <- make_mesh(pcod2, xy_cols = c("X", "Y"), n_knots = 200, type = "kmeans")
		
	# set up formulas
	form_no_yr <- paste0("log_wt ~ 0 + age_f + s(" , "presurvey_btemp", ", k = 4)")
	
	# model with no interaction and just spatiotemporal (and spatial effects)
	mod_st <-
		sdmTMB(
			formula = as.formula(form_no_yr),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
			extra_time = 2020:2099)
	#
	#write_rds(mod_st, file = paste0(here(), file_path_all, "pcod", "_mod_s_st", "_presurvey_btemp", ".rds"))
	
	mod_st <- read_rds(file = paste0(here(), file_path_all, "pcod", "_mod_s_st", "_presurvey_btemp", ".rds"))		
	
	# model with interaction and spatiotemporal effects

	# set up formulas
	#form_int <- paste0("log_wt ~ 0 + age_f + s(" , "presurvey_btemp", ", by = age_f, k = 4)")
	#	
	mod_int_st <-
		sdmTMB(
			formula = as.formula(form_int),
			data = pcod2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year")
#
	#write_rds(mod_int_st, file = paste0(here(), file_path_all, "pcod", "_mod_int_s_st", "_presurvey_btemp", ".rds"))

	mod_int_st <- read_rds(file = paste0(here(), file_path_all, "pcod", "_mod_int_s_st", "_presurvey_btemp", ".rds"))		

	# model checks
	sanity(mod_st)
	sanity(mod_int_st)
	
	AIC(mod_st)
	AIC(mod_int_st)

	sims <- simulate(mod_st, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_st)
	
	sims <- simulate(mod_int_st, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(mod_int_st)

	# fits - no interaction
	
	# nonspatially
	new_dat <- expand_grid(
		presurvey_btemp = seq(from = min(pcod2$presurvey_btemp),
										 to = max(pcod2$presurvey_btemp),
										 length.out = 25),
		year = sort(unique(pcod2$year)))
	
	non_spatial_preds <- predict(mod_st,
											         newdata = new_dat,
															 se_fit = TRUE,
															 re_form = NA,
															 return_tmb_object = FALSE)
	# high and low CIs
	non_spatial_preds$low <- non_spatial_preds$est + (qnorm(0.025) * non_spatial_preds$est_se)
	non_spatial_preds$high <- non_spatial_preds$est + (qnorm(0.975) * non_spatial_preds$est_se)
	
	# average across years
	non_spatial_preds_avg <- non_spatial_preds %>%
		group_by(presurvey_btemp) %>%
		summarise(mean_est = mean(est),
							mean_est_se = mean(est_se),
							mean_high = mean(high),
							mean_low = mean(low))
		
	pcod_no_int_temp_plot <-
			ggplot(non_spatial_preds_avg, aes(presurvey_btemp, mean_est)) +
			geom_ribbon(aes(ymin = mean_low, ymax = mean_high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			theme_sleek()


	# spatially - predicted values at locations of observations #### NEED TO DO A SMOOTH GRID
	

	# fits - with interaction
	
	# nonspatially
	new_dat <- expand_grid(
		age_f = unique(pcod2$age_f),
		presurvey_btemp = sort(unique(pcod2$presurvey_btemp)),
		year = 2000:2001)
			#sort(unique(pcod2$year))) # same 
	
	temp_preds_st <- predict(mod_int_st,
											    newdata = new_dat,
													se_fit = TRUE,
													re_form = NA,
													return_tmb_object = FALSE)

	
	# high and low CIs
	non_spatial_preds_int$low <- non_spatial_preds_int$est + (qnorm(0.025) * non_spatial_preds_int$est_se)
	non_spatial_preds_int$high <- non_spatial_preds_int$est + (qnorm(0.975) * non_spatial_preds_int$est_se)
	
	# average across years
	non_spatial_preds_int_avg <- non_spatial_preds_int %>%
		group_by(age_f, presurvey_btemp) %>%
		summarise(mean_est = mean(est),
							mean_est_se = mean(est_se),
							mean_high = mean(high),
							mean_low = mean(low))
		
	pcod_int_temp_plot <-
			ggplot(non_spatial_preds_int_avg, aes(presurvey_btemp, mean_est)) +
			geom_ribbon(aes(ymin = mean_low, ymax = mean_high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f) +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			theme_sleek()
	
	# spatially -- need to make prediction grid
	
	#### without ST effect so can forecast
	
	#form_int <- paste0("log_wt ~ 0 + age_f + s(" , "presurvey_btemp", ", by = age_f, k = 4)")
#
	#mod_int_s <-
	#	sdmTMB(
	#		formula = as.formula(form_int),
	#		data = pcod2,
	#		mesh = mesh,
	#		spatial = "on")
#
	#write_rds(mod_int_s, file = paste0(here(), file_path_all, "pcod", "_mod_int_s", "_presurvey_btemp", ".rds"))

	#mod_int_s <- read_rds(file = paste0(here(), file_path_all, "pcod", "_mod_int_s", "_presurvey_btemp", ".rds"))		

	### don't need to use a non spatiotemporal model since predictions don't take into account RE
	
	# nonspatially
	new_dat_hind <- expand_grid(
		age_f = unique(pcod2$age_f),
		presurvey_btemp = sort(unique(pcod2$presurvey_btemp)),
		year = 2000) # pick any year
	
	temp_hind_preds <- predict(mod_int_st,
											         newdata = new_dat_hind,
															 se_fit = TRUE,
															 re_form = NA,
															 return_tmb_object = FALSE)

	
	# high and low CIs
	temp_hind_preds$low <- temp_hind_preds$est + (qnorm(0.025) * temp_hind_preds$est_se)
	temp_hind_preds$high <- temp_hind_preds$est + (qnorm(0.975) * temp_hind_preds$est_se)
	
	# match to year
	pcod_temps <- pcod_dat %>%
		select(year, presurvey_btemp) %>%
		distinct()
	
	# remove year from preds
	temp_hind_preds <- temp_hind_preds %>% select(-year)
	
	temp_hind_preds <- left_join(temp_hind_preds, pcod_temps)
		
	pcod_int_temp_plot <-
			ggplot(temp_hind_preds, aes(year, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free") +
			ylab("log weight") +
			xlab("bottom temperature (averaged June - June)") +
			theme_sleek()
	
	# forecasting
	 
  proj_var_2020 <- ACLIM_weekly_fut %>%
    filter(var %in% vars) %>%
  	filter(year >= 2020) %>%
  	filter(basin == "SEBS")
 
	# summarize forecasted temps 
	fut_temps <- proj_var_2020 %>%
		filter(var == "temp_bottom5m") %>%
		group_by(year, RCP) %>%
		summarise(temp = mean(val_biascorrected)) 
	
	# low emission
	temps_ssp126 <- fut_temps %>% filter(RCP == "ssp126")
	
	new_dat_ssp126 <- expand_grid(
		age_f = unique(pcod2$age_f),
		presurvey_btemp = temps_ssp126$temp,
		year = 2000) # pick any year
	
	temp_fut_preds_ssp126 <- predict(
		mod_int_st,
		newdata = new_dat_ssp126,
		se_fit = TRUE,
		re_form = NA,
		return_tmb_object = FALSE)

	
	# high and low CIs
	temp_fut_preds_ssp126$low <- 
		temp_fut_preds_ssp126$est + (qnorm(0.025) * temp_fut_preds_ssp126$est_se)
	temp_fut_preds_ssp126$high <- 
		temp_fut_preds_ssp126$est + (qnorm(0.975) * temp_fut_preds_ssp126$est_se)
	
	# match to year
	temps_ssp126$presurvey_btemp <- temps_ssp126$temp
	
	temp_fut_preds_ssp126 <- temp_fut_preds_ssp126 %>% select(-year)
	
	temp_fut_preds_ssp126 <- left_join(temp_fut_preds_ssp126, temps_ssp126)
	
	# high emission
	temps_ssp585 <- fut_temps %>% filter(RCP == "ssp585")
	
	new_dat_ssp585 <- expand_grid(
		age_f = unique(pcod2$age_f),
		presurvey_btemp = temps_ssp585$temp,
		year = 2000) # pick any year
	
	temp_fut_preds_ssp585 <- predict(
		mod_int_st,
		newdata = new_dat_ssp585,
		se_fit = TRUE,
		re_form = NA,
		return_tmb_object = FALSE)

	temp_fut_preds_ssp585 <- temp_fut_preds_ssp585 %>% select(-year)

	# high and low CIs
	temp_fut_preds_ssp585$low <- 
		temp_fut_preds_ssp585$est + (qnorm(0.025) * temp_fut_preds_ssp585$est_se)
	temp_fut_preds_ssp585$high <- 
		temp_fut_preds_ssp585$est + (qnorm(0.975) * temp_fut_preds_ssp585$est_se)
	
	# match to year
	temps_ssp585$presurvey_btemp <- temps_ssp585$temp
	
	temp_fut_preds_ssp585 <- left_join(temp_fut_preds_ssp585, temps_ssp585)
	
	# plot
	pcod_int_temp_plot <-
			ggplot(temp_hind_preds, aes(year, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
		  geom_ribbon(data = temp_fut_preds_ssp126,
		  						aes(ymin = low, ymax = high), 
									fill = "lightblue", alpha = 0.4) +	
		 geom_ribbon(data = temp_fut_preds_ssp585,
		  						aes(ymin = low, ymax = high), 
									fill = "coral", alpha = 0.4) +	
			geom_line(color = "black") +
			geom_line(data = temp_fut_preds_ssp126,
								aes(year, est, color = "lightblue")) +
			geom_line(data = temp_fut_preds_ssp585,
								aes(year, est, color = "coral")) +
			facet_wrap(~ age_f, scales = "free") +
			ylab("log weight") +
			xlab("bottom temperature (averaged June - June)") +
			theme_sleek()
	
	
	
	
	# predicting with RE?

	grid <- pcod_dat %>%
		distinct_at(vars(X, Y)) 
	
	new_dat <- expand_grid(
		age_f = 3,
		presurvey_btemp = seq(from = min(pcod2$presurvey_btemp),
										 to = max(pcod2$presurvey_btemp),
										 length.out = 25),
		X = grid$X,
		Y = grid$Y)
	
	non_spatial_preds_int <- predict(mod_int_s,
											         newdata = new_dat,
															 se_fit = FALSE,
															 #re_form = NA,
															 return_tmb_object = FALSE)

	
	# high and low CIs
	non_spatial_preds_int$low <- non_spatial_preds_int$est + (qnorm(0.025) * non_spatial_preds_int$est_se)
	non_spatial_preds_int$high <- non_spatial_preds_int$est + (qnorm(0.975) * non_spatial_preds_int$est_se)
	
	# average across years
	non_spatial_preds_int_avg <- non_spatial_preds_int %>%
		group_by(age_f, presurvey_btemp) %>%
		summarise(mean_est = mean(est),
							mean_est_se = mean(est_se),
							mean_high = mean(high),
							mean_low = mean(low))
		
	pcod_int_temp_plot <-
			ggplot(non_spatial_preds_int_avg, aes(presurvey_btemp, mean_est)) +
			geom_ribbon(aes(ymin = mean_low, ymax = mean_high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free") +
			ylab("log weight") +
			xlab("bottom temperature (averaged June - June)") +
			theme_sleek()
	