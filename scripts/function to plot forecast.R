	# function to plot forecast
	
	# create dfs of predicted 
	forecast_df_fun <- function(sp){

		dat <- dat_all %>% filter(short_name == sp)
	
		f_yrs <- forecast_yrs_dfs %>% filter(short_name == sp) %>%
			rename(year = forecast_yrs)
	
		obs <- dat %>%
			select(age, log_wt, year, age_f)
	
		obs_rep <- replicate(length(f_yrs$year), obs, simplify = FALSE) %>% bind_rows()

		preds <- pred_df %>% filter(short_name == sp)
		
		preds_no_cov <- pred_df_no_cov %>% filter(short_name == sp) %>%
			rename(mean_est_nocov = mean_est,
						 mean_est_se_nocov = mean_se) %>%
			select(-mean_sd, -mod_run, -short_name)

	preds_all <- bind_cols(preds, preds_no_cov, obs_rep)
	
	preds_all_trim <- preds_all %>% filter(year %in% f_yrs$year)
	
	}
	
	sp <- unique(dat_all$short_name)
	sp <- sp[sp != "atooth"]
	
	forecast_dfs <- purrr::map(sp, forecast_df_fun)
	
	plot_fun <- function(df){
		
		sp <- unique(df$short_name)

		all_preds_sum <- df %>% 
			group_by(age_f, year) %>%
			summarise(mean_est = mean(mean_est),
								mean_se = mean(mean_se),
								mean_est_nocov = mean(mean_est_nocov),
								mean_est_se_nocov = mean(mean_est_se_nocov),
								mean_log_wt = mean(log_wt),
								mean_obs_se = (sd(log_wt))/(sqrt(length(log_wt))))
		
		p <- 
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
			ggtitle(sp) +
			theme_sleek() 
		
		
	}
	
	plots <- map(forecast_dfs, plot_fun)
	
	
	ggsave(here("output", "plots", "May 2024", "forecast skill", paste0("pcod_custom_forecast", ".png")), 
	 	 	height = 6, width = 12)

	