	# function to iteratively extract predictions for the forecast year and plot against observations

	forecast_yrs_all <- forecast_yrs_dfs %>% 
				bind_rows() 
	
	set_up_df_fun <- function(sp){
	
		yrs <- forecast_yrs_all %>% 
				filter(short_name == sp) %>%
				rename(year = forecast_yrs)
	
		obs <- dat_all %>%
			filter(short_name == sp) %>%
			select(age, log_wt, year, age_f)
	
		obs_rep <- replicate(length(yrs$year), obs, simplify = FALSE) %>% bind_rows()

		preds <- pred_df %>% filter(short_name == sp)
	
		preds_obs <- bind_cols(preds, obs_rep)
	
		preds_obs_trim <- preds_obs %>% filter(year %in% yrs$year)
		
		preds_obs_trim

	}
	
	sp <- unique(dat_all$short_name)[-1]
	
	preds_obs_dfs <- map(sp, set_up_df_fun)
	
	preds_obs_df <- 	preds_obs_dfs %>% bind_rows()

	
	# use pmap
	sep_df_fun <- function(sp, ages, yrs) {
		
		df <- preds_obs_df %>%
			filter(short_name == sp) %>%
			filter(year == yrs) %>%
			filter(age == ages)
		
		
	}
	
	yrs_df <- forecast_yrs_all %>%
		select(short_name, forecast_yrs)
	
	ages_df <- dat_all %>%
		select(short_name, age) %>%
		distinct_all()
	
	pmap_df <- left_join(yrs_df, ages_df, by = "short_name")
	
	pmap_list <- list(pmap_df$short_name, pmap_df$age, pmap_df$forecast_yrs)
	
	dfs <- pmap(pmap_list, sep_df_fun)

	
	### did this work? 
	#dfs <- map2(fdf$years, fdf$ages, sep_df_fun) 
	
	dfs <- map(preds_obs_dfs, sep_df_fun)
	
	# remove empty lists
 dfs_trim <- dfs[-109]
 dfs_trim <- dfs_trim[-314]

	# compute correlation coefficient
	cor_test_fun <- function(df){
		
		cor <- cor.test(df$log_wt, df$mean_est, method = 'spearman')
		
		rho <- try(cor$estimate)
		
		new_dat <- tibble(
			"year" = df$year, 
			"age_f" = df$age,
			"rho" = rho) %>%
			distinct_all()
		
		new_dat
	}
	
	
	cor_dfs <- map(dfs_trim, cor_test_fun) 
	
	cor_df <- cor_dfs %>% bind_rows()
	
	pcod_cor_plot <- 
		ggplot(cor_df) + 
		geom_line(aes(x = year, y = rho)) +
		facet_wrap(~ age_f, nrow = 2) +
		theme_sleek()
	
	
	all_preds_sum <- dfs_trim %>% 
		bind_rows() %>%
		group_by(age_f, year) %>%
		summarise(mean_est = mean(mean_est),
							mean_se = mean(mean_se),
							mean_log_wt = mean(log_wt),
							mean_obs_se = (sd(log_wt))/(sqrt(length(log_wt))))
	
	pcod_preds <- 
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
		facet_wrap(~ age_f, scales = "free") +
		theme_sleek() 
		
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	 