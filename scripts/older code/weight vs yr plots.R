
	# weight vs year plots ####

	# create a df with avg temps for each year in the hindcast for each age class
	new_dat_func <- function(x){
		
		age_sp_dat <- pcod_dat %>%
			filter(age_f == x) %>%
			select(age_f, year, yrprior_btemp) %>%
			distinct(year, .keep_all = TRUE)
		
	}
		
	ages <- unique(pcod_dat$age_f)
	
	pcod_temp_new_dat <- lapply(ages, new_dat_func) %>% bind_rows()
	
	# predictions of weight-at-age during hindcast
	pcod_temp_preds <- predict(yrprior_btemp_int_mod_pcod,
											      newdata = pcod_temp_new_dat,
													  se_fit = TRUE,
													  re_form = NA,
													  return_tmb_object = FALSE)
	
	pcod_temp_preds <- pcod_temp_preds %>%
		mutate(low = est - est_se,
					 high = est + est_se)
	

	# forecasts
	proj_var_2020 <- ACLIM_weekly_fut %>%
    filter(var %in% vars) %>%
  	filter(year >= 2020) %>%
  	filter(basin == "SEBS")
 
	# summarize forecasted temps 
	fut_temps <- proj_var_2020 %>%
		filter(var == "temp_bottom5m") %>%
		group_by(year, RCP) %>%
		summarise(temp = mean(val_biascorrected)) 
	
	new_dat_proj <- crossing(
		age_f = unique(pcod_dat$age_f),
		year = fut_temps$year
	)
	
	new_dat_proj <- left_join(fut_temps, new_dat_proj, by = "year") %>%
		rename(yrprior_btemp = temp)
	
	new_dat_proj_low <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp126") %>%
		dplyr::select(-RCP) 
	
	#new_dat_proj_high <- new_dat_proj %>% filter(RCP == "ssp585")

	pred_yrs <- unique(fut_temps$year)
	
	temp_fut_preds_low <- predict(
		yrprior_btemp_int_mod_pcod,
		newdata = new_dat_proj_low,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	temp_fut_preds_low <- temp_fut_preds_low %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP126")
	
	new_dat_proj_high <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp585") %>%
		dplyr::select(-RCP) 
	
	temp_fut_preds_high <- predict(
		yrprior_btemp_int_mod_pcod,
		newdata = new_dat_proj_high,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	temp_fut_preds_high <- temp_fut_preds_high %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP585")
	
	temp_fut_preds <- bind_rows(temp_fut_preds_ssp126, temp_fut_preds_ssp585) 
	
	# plot
	pcod_yrprior_temp_plot <-
			ggplot() +
			geom_ribbon(data = temp_fut_preds, 
				aes(x = year, ymin = low, ymax = high, fill = RCP), alpha = 0.1) +
			geom_line(dat = temp_fut_preds, aes(x = year, y = est, color = RCP), alpha = 0.5) +
			geom_line(data = temp_hind_preds, aes(x = year, y = est), color = "black") +
			facet_wrap(~ age_f, scales = "free_y", ncol = 5) +
			ylab("Partial effect of\n(log) weight") +
			xlab("Year") +
			theme_sleek()

	ggsave(pcod_yrprior_temp_plot, file = paste0(here(), file_path_plots, "pcod_temp_plot.png"),
				 height = 5, width = 10, units = "in")
	

