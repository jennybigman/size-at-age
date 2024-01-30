# 04c - visualizations of sdmTMB mods

	# plots
	
	# plot not in function since not working
	file_path_mods <- "/output/model output/sdmTMB output/Jan 2024/"
	
	file_path_plots <- "/output/plots/Jan 2024/"

	
	#### pollock ####
	yrprior_btemp_int_mod_pollock <- read_rds(paste0(here(), file_path_mods, "yrprior_btemp_int_mod_pollock.rds"))
	yrprior_boxy_int_mod_pollock <- read_rds(paste0(here(), file_path_mods, "yrprior_boxy_int_mod_pollock.rds"))

	# temp ####
	new_dat <- expand_grid(
		yrprior_btemp = seq(from = min(pollock_dat$yrprior_btemp),
												to = max(pollock_dat$yrprior_btemp),
												length.out = 25),
		age_f = sort(unique(pollock_dat$age_f)),
		year = 2000) # predict for one year b/c all the same and faster
	
	pol_temp_preds <- predict(yrprior_btemp_int_mod_pollock,
											      newdata = new_dat,
													  se_fit = TRUE,
													  re_form = NA,
													  return_tmb_object = FALSE)
	
	pol_temp_preds <- pol_temp_preds %>%
		mutate(high = est + est_se,
					 low = est - est_se)
	
	# plot
	pol_int_temp_plot <-
			ggplot(pol_temp_preds, aes(yrprior_btemp, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free_y") +
			ylab("partial effect of\n(log) weight") +
			xlab("bottom temperature\n(averaged June - June)") +
			theme_sleek()

	ggsave(pol_int_temp_plot, file = paste0(here(), file_path_plots, "pollock_temp.png"),
				 height = 5, width = 10, units = "in")
	
	
	# oxygen ####
	new_dat <- expand_grid(
		yrprior_boxy = seq(from = min(pollock_dat$yrprior_boxy),
												to = max(pollock_dat$yrprior_boxy),
												length.out = 25),
		age_f = sort(unique(pollock_dat$age_f)),
		year = 2000) # predict for one year b/c all the same and faster
	
	pol_oxy_preds <- predict(yrprior_boxy_int_mod_pollock,
											      newdata = new_dat,
													  se_fit = TRUE,
													  re_form = NA,
													  return_tmb_object = FALSE)
	
	# high and low CIs
	pol_oxy_preds$low <- pol_oxy_preds$est + (qnorm(0.025) * pol_oxy_preds$est_se)
	pol_oxy_preds$high <- pol_oxy_preds$est + (qnorm(0.975) * pol_oxy_preds$est_se)
	
	# plot
	pol_int_oxy_plot <-
			ggplot(pol_oxy_preds, aes(yrprior_boxy, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free_y") +
			ylab("partial effect of\n(log) weight") +
			xlab("bottom oxygen\n(averaged June - June)") +
			theme_sleek()

	ggsave(pol_int_oxy_plot, file = paste0(here(), file_path_plots, "pollock_oxy.png"),
				 height = 5, width = 10, units = "in")
	
	
	#### pcod ####
	yrprior_btemp_int_mod_pcod <- read_rds(paste0(here(), file_path_mods, "yrprior_btemp_int_mod_pcod.rds"))
	yrprior_boxy_int_mod_pcod <- read_rds(paste0(here(), file_path_mods, "yrprior_boxy_int_mod_pcod.rds"))

	# temp ####
	new_dat <- expand_grid(
		yrprior_btemp = seq(from = min(pcod_dat$yrprior_btemp),
												to = max(pcod_dat$yrprior_btemp),
												length.out = 25),
		age_f = sort(unique(pcod_dat$age_f)),
		year = 2000) # predict for one year b/c all the same and faster
	
	pcod_temp_preds <- predict(yrprior_btemp_int_mod_pcod,
											      newdata = new_dat,
													  se_fit = TRUE,
													  re_form = NA,
													  return_tmb_object = FALSE)
	
	# high and low CIs
	pcod_temp_preds$low <-  pcod_temp_preds$est + (qnorm(0.025) * pcod_temp_preds$est_se)
	pcod_temp_preds$high <- pcod_temp_preds$est + (qnorm(0.975) * pcod_temp_preds$est_se)
	
	# plot
	pcod_int_temp_plot <-
			ggplot(pcod_temp_preds, aes(yrprior_btemp, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free_y") +
			ylab("partial effect of\n(log) weight") +
			xlab("bottom temperature\n(averaged June - June)") +
			theme_sleek()

	ggsave(pcod_int_temp_plot, file = paste0(here(), file_path_plots, "pcod_temp.png"),
				 height = 5, width = 10, units = "in")
	
	
	# oxygen ####
	new_dat <- expand_grid(
		yrprior_boxy = seq(from = min(pcod_dat$yrprior_boxy),
												to = max(pcod_dat$yrprior_boxy),
												length.out = 25),
		age_f = sort(unique(pcod_dat$age_f)),
		year = 2000) # predict for one year b/c all the same and faster
	
	pcod_oxy_preds <- predict(yrprior_boxy_int_mod_pcod,
											      newdata = new_dat,
													  se_fit = TRUE,
													  re_form = NA,
													  return_tmb_object = FALSE)
	
	# high and low CIs
	pcod_oxy_preds$low <-  pcod_oxy_preds$est + (qnorm(0.025) * pcod_oxy_preds$est_se)
	pcod_oxy_preds$high <- pcod_oxy_preds$est + (qnorm(0.975) * pcod_oxy_preds$est_se)
	
	# plot
	pcod_int_oxy_plot <-
			ggplot(pcod_oxy_preds, aes(yrprior_boxy, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free_y") +
			ylab("partial effect of\n(log) weight") +
			xlab("bottom oxygen\n(averaged June - June)") +
			theme_sleek()

	ggsave(pcod_int_oxy_plot, file = paste0(here(), file_path_plots, "pcod_oxy.png"),
				 height = 5, width = 10, units = "in")
	
	
	#### yfin ####
	yrprior_btemp_int_mod_yfin <- read_rds(paste0(here(), file_path_mods, "yrprior_btemp_int_mod_yfin.rds"))
	yrprior_boxy_int_mod_yfin <- read_rds(paste0(here(), file_path_mods, "yrprior_boxy_int_mod_yfin.rds"))

	# temp ####
	new_dat <- expand_grid(
		yrprior_btemp = seq(from = min(yfinsole_dat$yrprior_btemp),
												to = max(yfinsole_dat$yrprior_btemp),
												length.out = 25),
		age_f = sort(unique(yfinsole_dat$age_f)),
		year = 2000) # predict for one year b/c all the same and faster
	
	yfin_temp_preds <- predict(yrprior_btemp_int_mod_yfin,
											      newdata = new_dat,
													  se_fit = TRUE,
													  re_form = NA,
													  return_tmb_object = FALSE)
	
	# high and low CIs
	yfin_temp_preds$low <-  yfin_temp_preds$est + (qnorm(0.025) * yfin_temp_preds$est_se)
	yfin_temp_preds$high <- yfin_temp_preds$est + (qnorm(0.975) * yfin_temp_preds$est_se)
	
	# plot
	yfin_int_temp_plot <-
			ggplot(yfin_temp_preds, aes(yrprior_btemp, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free_y") +
			ylab("partial effect of\n(log) weight") +
			xlab("bottom temperature\n(averaged June - June)") +
			theme_sleek()

	ggsave(yfin_int_temp_plot, file = paste0(here(), file_path_plots, "yfin_temp.png"),
				 height = 5, width = 10, units = "in")
	
	
	# oxygen ####
	new_dat <- expand_grid(
		yrprior_boxy = seq(from = min(yfinsole_dat$yrprior_boxy),
												to = max(yfinsole_dat$yrprior_boxy),
												length.out = 25),
		age_f = sort(unique(yfinsole_dat$age_f)),
		year = 2000) # predict for one year b/c all the same and faster
	
	yfin_oxy_preds <- predict(yrprior_boxy_int_mod_yfin,
											      newdata = new_dat,
													  se_fit = TRUE,
													  re_form = NA,
													  return_tmb_object = FALSE)
	
	# high and low CIs
	yfin_oxy_preds$low <-  yfin_oxy_preds$est + (qnorm(0.025) * yfin_oxy_preds$est_se)
	yfin_oxy_preds$high <- yfin_oxy_preds$est + (qnorm(0.975) * yfin_oxy_preds$est_se)
	
	# plot
	yfin_int_oxy_plot <-
			ggplot(yfin_oxy_preds, aes(yrprior_boxy, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f, scales = "free_y") +
			ylab("partial effect of\n(log) weight") +
			xlab("bottom oxygen\n(averaged June - June)") +
			theme_sleek()

	ggsave(yfin_int_oxy_plot, file = paste0(here(), file_path_plots, "yfin_oxy.png"),
				 height = 5, width = 10, units = "in")
	
	
	########### projections
	
	# 05 - forecasting

	# read in models with an interaction

	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/")))
	drop_list <- file_list[grep("no_int", file_list)]
	int_mod_list_names <- setdiff(file_list, drop_list)
	
	prestring <- paste0(here(), ("/output/model output/sdmTMB output//Jan 2024/"))
	 
	 mod_names_list <- list()
	 
	 for(i in int_mod_list_names){
	 	mod_names_list[[i]] <- paste0(prestring, i)
	 }
	 
	 int_mods <- lapply(mod_names_list, readRDS)
	
	 yr_oxy_mods <- int_mods[grep("yrprior_boxy", names(int_mods))]
	 yr_temp_mods <- int_mods[grep("yrprior_btemp", names(int_mods))]
	 
	 
	 pretemp_df_func <- function(x){
	 	
	 if (names(pretemp_mods) == names(pretemp_mods)[grep("pollock", names(pretemp_mods))] {
	 	
	 	# new dat
	 	new_dat_hind <- expand_grid(
			age_f = 1,
			presurvey_btemp = sort(unique(pollock_dat$presurvey_btemp)),
			year = unique(pollock_dat$presurvey_btemp)) # pick any year

	 	# predict
	 	hind_preds <- predict(
	 		names(pretemp_mods)[grep("pollock", names(pretemp_mods))],
			newdata = new_dat_hind,
			se_fit = TRUE,
			re_form = NA,
			return_tmb_object = FALSE)
	 	
	 	pol_hind_preds <- pol_hind_preds %>% 
	 		select(-year) %>% 
	 		mutate(low = est - est_se,
	 					 high = est + est_se)

		# match to year
		pol_hind_dat <- pollock_dat %>%
			select(year, presurvey_btemp) %>%
			distinct()

		pol_hind_preds <- left_join(pol_hind_preds, pol_hind_dat)

	 }
	 

	species <- unique(dat_all$short_name) 

	df_func <- expand_grid(
		sp = species,
		mod = temp_mod_list
	)

	temp_plot_dfs <- map2(df_func$sp, df_func$mod, temp_pred_plot_function)

	 
# code

	# nonspatially
	

	
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
		mod_int_s,
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
	
	
	
	
##################### draft function below
	
	# pull out all int models

#	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/")))
#	drop_list <- file_list[grep("no_int", file_list)]
#	int_mod_list_names <- setdiff(file_list, drop_list)
#
#  prestring <- paste0(here(), ("/output/model output/sdmTMB output//Jan 2024/"))
#  
#  mod_names_list <- list()
#  
#  for(i in int_mod_list_names){
#  	mod_names_list[[i]] <- paste0(prestring, i)
#  }
#  
#  int_mods <- lapply(mod_names_list, readRDS)
#  
# 
#	# pollock
#	
#  pol_mod_list <- int_mods %>% keep(grepl("pol", names(int_mods)))
#  pol_mod_list <- pol_mod_list %>% keep(grepl("yrprior", names(pol_mod_list)))
#	
#	pol_plot_dfs_func <- function(x, y) {
#		
#  new_dat <- expand_grid(
#		y = seq(from = min(pollock_dat$y),
#										 to = max(pollock_dat$y),
#										 length.out = 50),
#		year = sort(unique(pollock_dat$year)))
#	
#	non_spatial_preds <- predict(x,
#											         newdata = new_dat,
#															 se_fit = TRUE,
#															 re_form = NA,
#															 return_tmb_object = FALSE)
#	# high and low CIs
#	non_spatial_preds$low <- non_spatial_preds$est + (qnorm(0.025) * non_spatial_preds$est_se)
#	non_spatial_preds$high <- non_spatial_preds$est + (qnorm(0.975) * non_spatial_preds$est_se)
#	
#	# average across years
#	non_spatial_preds_avg <- non_spatial_preds %>%
#		group_by(y) %>%
#		summarise(mean_est = mean(est),
#							mean_est_se = mean(est_se),
#							mean_high = mean(high),
#							mean_low = mean(low))
#	
#	}
#	
	#vars <- dat_all %>%
	#	select(contains("yrprior")) %>%
	#	names() 
	#
	#df_func <- expand_grid(
	#	x = pol_mod_list,
	#	y = vars
	#)
#
	#pol_plot_dfs <- map2(df_func$x, df_func$y, pol_plot_dfs_func)
#
	










#############################################################################################################

# pull out all int models

	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/")))
	drop_list <- file_list[grep("no_int", file_list)]
	int_mod_list_names <- setdiff(file_list, drop_list)

  prestring <- paste0(here(), ("/output/model output/sdmTMB output//Jan 2024/"))
  
  mod_names_list <- list()
  
  for(i in int_mod_list_names){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  int_mods <- lapply(mod_names_list, readRDS)
  
  # pollock
  
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

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   int_mod_plot_df_func <- function(sp, mod){
  	
  	sp_dat <- dat_all %>% filter(short_name == sp)
  	
  	# new df for predicting
		new_dat <- expand_grid(
			presurvey_btemp = seq(from = min(sp_dat$presurvey_btemp),
											 to = max(sp_dat$presurvey_btemp),
											 length.out = 50),
			year = sort(unique(sp_dat$year)))

				
		# predict
		non_spatial_preds <- predict(mod,
												         newdata = new_dat,
																 se_fit = TRUE,
																 re_form = NA,
																 return_tmb_object = FALSE)
		
		# add high and low CIs
		non_spatial_preds$low <- non_spatial_preds$est + (qnorm(0.025) * non_spatial_preds$est_se)
		non_spatial_preds$high <- non_spatial_preds$est + (qnorm(0.975) * non_spatial_preds$est_se)
	
		# average across years since ignoring random effect
		non_spatial_preds_avg <- non_spatial_preds %>%
			group_by(presurvey_btemp) %>%
			summarise(mean_est = mean(est),
								mean_est_se = mean(est_se),
								mean_high = mean(high),
								mean_low = mean(low))
	
  }
  
	pol_mod_list  <-  int_mods[grep("pol", names(int_mods))]
	
	pol_df_func <- expand_grid(
		sp = "pollock",
		mod = pol_mod_list
	)
	
  int_mod_plot_df_func
  
	
	pcod_mod_list <-  int_mods[grep("pcod", names(int_mods))]
	yfin_mod_list <-  int_mods[grep("yfin", names(int_mods))]
	

	
	pcod_no_int_temp_plot <-
			ggplot(non_spatial_preds_avg, aes(presurvey_btemp, mean_est)) +
			geom_ribbon(aes(ymin = mean_low, ymax = mean_high), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			theme_sleek()


















	# plot pollock model with factor-smooth interaction, spatial & spatiotemporal effects, and no jday
	
	# read in model
	presurvey_btemp_int_pol_nj <- readRDS( 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol_nj.rds"))
 
	# from scratch
	
	new_dat <- expand.grid(
		age_f = unique(pollock_dat$age_f), 
		presurvey_btemp = seq(from = min(pollock_dat$presurvey_btemp),
													to = max(pollock_dat$presurvey_btemp),
													length.out = 100)) #,
	#	year = 1993) # what year do I want to show?
	
	
	pred <- predict(presurvey_btemp_int_pol_nj,
									newdata = new_dat,
									se_fit = TRUE,
									re_form = NA)
	
	ggplot(pred, aes(presurvey_btemp, est)) +
		geom_line() +
		facet_wrap(~ age_f)
	
	# for all years
	
	plot_df_func <- function(year){
	
		# create new data to estimate fitted values
		new_dat <- expand.grid(
			age_f = unique(pollock_dat$age_f), 
			presurvey_btemp = 
				seq(from = min(pollock_dat$presurvey_btemp),
						to = max(pollock_dat$presurvey_btemp),
						length.out = 100),
			year = year) 
	
		# data frame of fitted vals with SE
		fits <- predict(presurvey_btemp_int_pol_nj,
										newdata = new_dat,
										se_fit = TRUE,
										re_form = NA)
		
		# high and low CIs
		fits$low <- fits$est + (qnorm(0.025) * fits$est_se)
		fits$high <- fits$est + (qnorm(0.975) * fits$est_se)
		
		fits
		
		fit_plot <-
			ggplot(fits, aes(presurvey_btemp, est)) +
			geom_line() +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f, ncol = 5) +
			ylab("log scaled weight-at-age") +
			xlab("Bottom temperature (averaged April - June)") +
			labs(title = year) +
			theme_sleek()

		fit_plot
		
	}
	
	# vector of years to plot
	years_plot <- sort(unique(pollock_dat$year))

	# list of plots for each year in vector above
	pol_plot_dfs <- lapply(years_plot, plot_df_func)	
	
	names(pol_plot_dfs) <- years_plot
	
	# save all plots
	
  plot_name <- function(year){
  	paste0(here(), "/output/plots/", year, ".png")
  }
   
  plot_names <- sapply(years_plot, plot_name)
  
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y))
    }
  
	mapply(ggsave_func, x = pol_plot_dfs, y = plot_names)

	
	#### spatial predictions ####
	
	# create grid for mapping - don't use every XY, create a new grid and use code in examples on sdmTMB site
	# a reduction grid - not every single point 
	
	bs_grid <- pollock_dat %>%
		select(X, Y, presurvey_btemp, age_f) %>%
		mutate(year = 1993)
	
	space_preds <- predict(presurvey_btemp_int_pol_nj,
												 newdata = bs_grid)
	
	plot_map <- function(dat, column){
		ggplot(dat, aes(X, Y, fill = {{column}})) +
			geom_raster() +
			coord_fixed()
	}
	
	# predictions of all fixed and random effects
	plot_map(space_preds, est) +
		scale_color_viridis_c(
			na.value = "yellow",
			limits = c(0, quantile(space_preds$est), 0.95)) +
		facet_wrap( ~age_f, ncol = 5) +
		ggtitle("Prediction (fixed and random effects)")
	
	# predictions of just fixed effects
	plot_map(predictions, est_non_rf) +
		scale_fill_viridis_c() +
		ggtitle("Prediction (fixed effects only)")
	
	# predictions of just spatial random effects (not accounted for by fixed effects)
	plot_map(predictions, omega_s) +
		scale_fill_gradient2() +
		ggtitle("Spatial random effects only")
	
	# predictions of just spatiotemporal effects
	plot_map(predictions, epsilon_st) +
		scale_fill_gradient2() +
		facet_wrap( ~year) +
		ggtitle("Spatiotemporal random effects only")