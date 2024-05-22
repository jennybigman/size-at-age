# 05 - forecasting

	# plot predictions of size-at-age from temperature
	# this requires the hindcast predictions estimated in previous script (temp_hind_plot_dfs)
	# and a list of the models with yrprior_btemp that are read in in the previous script
	
	pollock_hind_preds <- temp_hind_plot_dfs[[1]]
	pcod_hind_preds <- temp_hind_plot_dfs[[2]]
	yfin_hind_preds <- temp_hind_plot_dfs[[3]]
	
	# read in model and plot file paths
	file_path_mods <- "/output/model output/sdmTMB output/Jan 2024/"
	file_path_plots <- "/output/plots/Jan 2024/"

	# forecast weight for future temps

  #### pollock #### WHAT'S GOING ON WITH THIS MODEL
  
  # read in model
	yrprior_btemp_int_mod_pollock <- read_rds(paste0(here(), file_path_mods, "yrprior_btemp_int_mod_pollock.rds"))
	
	# read in forecast from Bering10k
	ROMS_proj <- readRDS(file = "./data/ROMS_proj.rds")
	
	ROMS_proj_temps <- ROMS_proj %>% 
		filter(var == "temp") %>%
		group_by(year) %>%
		

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
	
	# create new df of years for each age class
	new_dat_proj <- crossing(
		age_f = unique(pollock_dat$age_f),
		year = fut_temps$year
	)
	
	# add in the projected temps
	new_dat_proj <- left_join(fut_temps, new_dat_proj, by = "year") %>%
		rename(yrprior_btemp = temp)
	
	# low emission scenario ##
	
	# wrangle df into df for predict function (incl scaling temp vals b/c model was fitted with scaled temp vals)
	new_dat_proj_low <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp126") %>%
		dplyr::select(-RCP) %>%
		mutate(yrprior_btemp = (scale(yrprior_btemp) %>% as.vector))

	# object of years to forecast
	pred_yrs <- unique(fut_temps$year)
	
	# forecast
	temp_fut_preds_low <- predict(
		yrprior_btemp_int_mod_pollock,
		newdata = new_dat_proj_low,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	# add in se estimates and RCP column
	temp_fut_preds_low <- temp_fut_preds_low %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP126")
	
	# do it all again for high emission scenario
	new_dat_proj_high <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp585") %>%
		dplyr::select(-RCP) %>%
		mutate(yrprior_btemp = (scale(yrprior_btemp) %>% as.vector))

	# object of years to forecast
	pred_yrs <- unique(fut_temps$year)
	
	# forecast
	temp_fut_preds_high <- predict(
		yrprior_btemp_int_mod_pollock,
		newdata = new_dat_proj_high,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	# add in se estimates and RCP column
	temp_fut_preds_high <- temp_fut_preds_high %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP585")
	
	pollock_temp_fut_preds <- bind_rows(temp_fut_preds_low, temp_fut_preds_high) 
	
	#### pacific cod ####
	
	# read in model
	yrprior_btemp_int_mod_pcod <- read_rds(paste0(here(), file_path_mods, "yrprior_btemp_int_mod_pcod.rds"))
	
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
	
	# create new df of years for each age class
	new_dat_proj <- crossing(
		age_f = unique(pcod_dat$age_f),
		year = fut_temps$year
	)
	
	# add in the projected temps
	new_dat_proj <- left_join(fut_temps, new_dat_proj, by = "year") %>%
		rename(yrprior_btemp = temp)
	
	# low emission scenario ##
	
	# wrangle df into df for predict function (incl scaling temp vals b/c model was fitted with scaled temp vals)
	new_dat_proj_low <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp126") %>%
		dplyr::select(-RCP) %>%
		mutate(yrprior_btemp = (scale(yrprior_btemp) %>% as.vector))

	# object of years to forecast
	pred_yrs <- unique(fut_temps$year)
	
	# forecast
	temp_fut_preds_low <- predict(
		yrprior_btemp_int_mod_pcod,
		newdata = new_dat_proj_low,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	# add in se estimates and RCP column
	temp_fut_preds_low <- temp_fut_preds_low %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP126")
	
	# do it all again for high emission scenario
	new_dat_proj_high <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp585") %>%
		dplyr::select(-RCP) %>%
		mutate(yrprior_btemp = (scale(yrprior_btemp) %>% as.vector))

	# object of years to forecast
	pred_yrs <- unique(fut_temps$year)
	
	# forecast
	temp_fut_preds_high <- predict(
		yrprior_btemp_int_mod_pcod,
		newdata = new_dat_proj_high,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	# add in se estimates and RCP column
	temp_fut_preds_high <- temp_fut_preds_high %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP585")
	
	pcod_temp_fut_preds <- bind_rows(temp_fut_preds_low, temp_fut_preds_high) 
	
	#### yellowfin ####
	
	# read in model
	yrprior_btemp_int_mod_yfin <- read_rds(paste0(here(), file_path_mods, "yrprior_btemp_int_mod_yfin.rds"))
	
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
	
	# create new df of years for each age class
	new_dat_proj <- crossing(
		age_f = unique(yfinsole_dat$age_f),
		year = fut_temps$year
	)
	
	# add in the projected temps
	new_dat_proj <- left_join(fut_temps, new_dat_proj, by = "year") %>%
		rename(yrprior_btemp = temp)
	
	# low emission scenario ##
	
	# wrangle df into df for predict function (incl scaling temp vals b/c model was fitted with scaled temp vals)
	new_dat_proj_low <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp126") %>%
		dplyr::select(-RCP) %>%
		mutate(yrprior_btemp = (scale(yrprior_btemp) %>% as.vector))

	# object of years to forecast
	pred_yrs <- unique(fut_temps$year)
	
	# forecast
	temp_fut_preds_low <- predict(
		yrprior_btemp_int_mod_yfin,
		newdata = new_dat_proj_low,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	# add in se estimates and RCP column
	temp_fut_preds_low <- temp_fut_preds_low %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP126")
	
	# do it all again for high emission scenario
	new_dat_proj_high <- new_dat_proj %>% 
		ungroup() %>%
		filter(RCP == "ssp585") %>%
		dplyr::select(-RCP) %>%
		mutate(yrprior_btemp = (scale(yrprior_btemp) %>% as.vector))

	# object of years to forecast
	pred_yrs <- unique(fut_temps$year)
	
	# forecast
	temp_fut_preds_high <- predict(
		yrprior_btemp_int_mod_yfin,
		newdata = new_dat_proj_high,
		se_fit = TRUE,
		re_form = NA,
		extra_time = pred_yrs,
		return_tmb_object = FALSE)

	# add in se estimates and RCP column
	temp_fut_preds_high <- temp_fut_preds_high %>%
		mutate(low = est - est_se,
					 high = est - est_se,
					 RCP = "SSP585")
	
	yfin_temp_fut_preds <- bind_rows(temp_fut_preds_low, temp_fut_preds_high) 
	
	#### plots ####
	
	#### pacific cod ####
	pcod_weight_yr_plot <-
		ggplot() +
		geom_ribbon(data = pcod_temp_fut_preds, 
								aes(x = year, ymin = low, ymax = high, fill = RCP), alpha = 0.1) +
		geom_line(dat = pcod_temp_fut_preds, aes(x = year, y = est, color = RCP), alpha = 0.5) +
		geom_line(data = pcod_hind_preds, aes(x = year, y = est), color = "black") +
		facet_wrap(~ age_f, scales = "free_y", ncol = 5) +
		ylab("Partial effect of\n(log) weight") +
		xlab("Year") +
		theme_sleek()

	ggsave(pcod_weight_yr_plot, file = paste0(here(), file_path_plots, "pcod_yr_weight_plot.png"),
				 height = 5, width = 10, units = "in")

	#### pacific cod ####
	yfin_weight_yr_plot <-
		ggplot() +
		geom_ribbon(data = yfin_temp_fut_preds, 
								aes(x = year, ymin = low, ymax = high, fill = RCP), alpha = 0.1) +
		geom_line(dat = yfin_temp_fut_preds, aes(x = year, y = est, color = RCP), alpha = 0.5) +
		geom_line(data = yfin_hind_preds, aes(x = year, y = est), color = "black") +
		facet_wrap(~ age_f, scales = "free_y", ncol = 5) +
		ylab("Partial effect of\n(log) weight") +
		xlab("Year") +
		theme_sleek()

	ggsave(yfin_weight_yr_plot, file = paste0(here(), file_path_plots, "yfin_yr_weight_plot.png"),
				 height = 5, width = 10, units = "in")
