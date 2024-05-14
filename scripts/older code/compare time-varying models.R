#  playing around with time_varying intercepts

	# pick a species
	pollock_dat <- dat_all %>% 
		filter(short_name == "pollock") 

	mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)
	
	et <- c(1992:1998, 2020:2099)
	
	mod_tv <-
		sdmTMB(
			log_wt_std ~ 0 + s(yrprior_btemp, by = age_f, k = 3),
			time_varying = ~ 1,
			time_varying_type = "ar1",
			time = "year",
			data = pollock_dat,
			mesh = mesh,
			spatial = "off")

	sanity(mod_tv)
	
	mod_tv_s <-
		sdmTMB(
			log_wt_std ~ 0  + s(yrprior_btemp, by = age_f, k = 3),
			time_varying = ~ 1,
			time_varying_type = "ar1",
			time = "year",
			data = pollock_dat,
			mesh = mesh,
			silent = FALSE,
			spatial = "on")

	sanity(mod_tv_s)

		
	mod_s_st <-
		sdmTMB(
			log_wt_std ~ 0 + s(yrprior_btemp, by = age_f, k = 3),
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year")

	sanity(mod_s_st)

	mod_s_st_ar1 <-
		sdmTMB(
			log_wt_std ~ 0 + s(yrprior_btemp, by = age_f, k = 3),
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "ar1",
			silent = FALSE,
			time = "year")

	sanity(mod_s_st_ar1)

# spatial and spatiotemporal gave me the same result
	
	mod_tv_s_st <-
		sdmTMB(
			log_wt ~ 0 + age_f + s(yrprior_btemp, by = age_f, k = 3),
			time_varying = ~ 1,
			time_varying_type = "ar1",
			time = "year",
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid")

	sanity(mod_tv_s_st)
	
		
	mod_tv_s_st_ar1 <-
		sdmTMB(
			log_wt ~ 0 + age_f + s(yrprior_btemp, k = 3),
			time_varying = ~ 1,
			time_varying_type = "ar1",
			time = "year",
			data = pollock_dat,
			mesh = mesh,
			spatial = "on",
			silent = FALSE,
			spatiotemporal = "ar1")

	sanity(mod_tv_s_st_ar1)
	
mod_st_ar1 <-
		sdmTMB(
			log_wt ~ 0 + age_f + s(yrprior_btemp, k = 3),
			spatial = "on",
			spatiotemporal = "ar1",
			time = "year",
			data = pollock_dat,
			mesh = mesh,
			spatial = "off")

	sanity(mod_tv)

	###################
	
	# testing forecasting with basin-averaged temps from ACLIM

	# read in CMIP6 ROMS output - averaged by basin annually
  load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/allEBS_means/ACLIM_weekly_fut_mn.Rdata")
 
  # forecasts
  
  vars <- c("temp_bottom5m", "oxygen_bottom5m")
  
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
		mod1_tv,
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
	