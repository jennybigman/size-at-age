	# pcod ####
	
	# what years are forecasted
	pcod_yrs <- pluck(forecast_yrs_dfs, 1) %>%
		rename(year = forecast_yrs)
	
	pcod_obs <- pcod_dat %>%
		select(age, log_wt, year, age_f)
	
	pcod_obs_rep <- replicate(length(pcod_yrs$year), pcod_obs, simplify = FALSE) %>% bind_rows()

	pcod_preds <- pred_df %>% filter(short_name == "pcod")
	
	pcod_preds_no_cov <- pred_df_no_cov %>% filter(short_name == "pcod") %>%
		rename(mean_est_nocov = mean_est,
					 mean_est_se_nocov = mean_se) %>%
		select(-mean_sd, -mod_run, -short_name)
	
	pcod_all <- bind_cols(pcod_preds, pcod_preds_no_cov, pcod_obs_rep)
	
	pcod_all_trim <- pcod_all %>% filter(year %in% pcod_yrs$year)

	
	sep_df_fun <- function(years, ages) {
		
		
		df <- pcod_all_trim %>%
			filter(year == years) %>%
			filter(age == ages)
		
		
	}
	
	ages <- sort(unique(pcod_all_trim$age))

	fdf <- crossing(
		years = sort(unique(pcod_all_trim$year)),
		ages = ages
	)
	
	dfs <- map2(fdf$years, fdf$ages, sep_df_fun) 
	
	# remove empty lists
	dfs_trim <- dfs[-110]
 
	all_preds_sum <- dfs_trim %>% 
		bind_rows() %>%
		group_by(age_f, year) %>%
		summarise(mean_est = mean(mean_est),
							mean_se = mean(mean_se),
							mean_est_nocov = mean(mean_est_nocov),
							mean_est_se_nocov = mean(mean_est_se_nocov),
							mean_log_wt = mean(log_wt),
							mean_obs_se = (sd(log_wt))/(sqrt(length(log_wt))))
	
	pcod_preds_all <- 
		ggplot(data = all_preds_sum, aes(x = year, y = mean_est)) +
		geom_point(aes(x = year, y = mean_est), 
							 color = "black", shape = 21, alpha = 0.5) +
		geom_linerange(aes(ymin = mean_est - mean_se, ymax = mean_est + mean_se), 
									 color = "black", alpha = 0.5) +
		geom_point(aes(x = year, y = mean_log_wt), 
							 color = "black", alpha = 0.5) +
		geom_linerange(aes(x = year, y = mean_log_wt, 
											 ymin = mean_log_wt - mean_obs_se, 
											 ymax = mean_log_wt + mean_obs_se), 
									 color = "black", alpha = 0.5) +
		geom_point(aes(x = year, y = mean_est_nocov), 
							 color = "red", shape = 21, alpha = 0.5) +
		geom_linerange(aes(ymin = mean_est_nocov - mean_est_se_nocov, ymax = mean_est_nocov + mean_est_se_nocov), 
									 color = "red", alpha = 0.5) +
		facet_wrap(~ age_f, scales = "free") +
		theme_sleek() 
		