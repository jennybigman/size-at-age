	#### PLOTS ####
	
	# pcod ####
	
	# what years are forecasted
	dat <- dat_all %>% filter(short_name == "pollock")
	
	dat_yrs <- forecast_yrs_dfs %>% filter(short_name == "pollock") %>%
		rename(year = forecast_yrs)
	
	dat_obs <- dat %>%
		select(age, log_wt, year, age_f)
	
	dat_obs_rep <- replicate(length(dat_yrs$year), dat_obs, simplify = FALSE) %>% bind_rows()

	dat_preds <- pred_df %>% filter(short_name == "pollock")
	
	dat_all <- bind_cols(dat_preds, dat_obs_rep)
	
	dat_all_trim <- dat_all %>% filter(year %in% dat_yrs$year)

	
	sep_df_fun <- function(years, ages) {
		
		
		df <- dat_all_trim %>%
			filter(year == years) %>%
			filter(age == ages)
		
		
	}
	
	ages <- sort(unique(dat_all_trim$age))

	fdf <- crossing(
		years = sort(unique(dat_all_trim$year)),
		ages = ages
	)
	
	dfs <- map2(fdf$years, fdf$ages, sep_df_fun) 
	
	# remove empty lists
	dim_fun <- function(df, num){
		
		dims <- dim(df)
		dim1 <- dims[1]
		dim2 <- dims[2]
		dims <- tibble(dim1, dim2)
		dims$list_no <- num
		
		dims
	}
	
	num <- length(dfs)
	
	fdf <- tibble(
		df = dfs,
		num = 1:num
	)
	
	dims <- map2(fdf$df, fdf$num, dim_fun) %>% bind_rows()
	
	lists_drop <- dims %>%
		filter(if_any(everything(), ~ .x == 0))
	
	drop <- lists_drop$list_no

	dfs_keep <- dfs[-drop]
	
	all_preds_sum <- dfs_keep %>% 
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
		facet_wrap(~ age_f, scales = "free_y") +
		theme_sleek() +
		ylab("Predicted mean (log) weight (g)") +
	 	xlab("Year") 
	
		 ggsave(here("output", "plots", "May 2024", "forecast skill", paste0("pcod_custom_forecast", ".png")), 
	 	 			 height = 6, width = 12)

	
		 
		 	# try group split
	
	split_fun <- function(df){
		
		df <- df %>% 
			mutate(year_f = as.factor(year)) %>%
			group_by(age_f, year_f, short_name) %>%
			group_split()
		
	}
	
	age_yr_sp_dfs <- purrr::map(forecast_dfs, split_fun)

	
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
	dim_fun <- function(df, num){
		
		dims <- dim(df)
		dim1 <- dims[1]
		dim2 <- dims[2]
		dims <- tibble(dim1, dim2)
		dims$list_no <- num
		
		dims
	}
	
	num <- length(dfs)
	
	fdf <- tibble(
		df = dfs,
		num = 1:num
	)
	
	dims <- map2(fdf$df, fdf$num, dim_fun) %>% bind_rows()
	
	lists_drop <- dims %>%
		filter(if_any(everything(), ~ .x == 0))
	
	drop <- lists_drop$list_no

	dfs_keep <- dfs[-drop]
	
		
	#### PLOTS ####
	
	# pcod ####
	
	# what years are forecasted
	pcod_dat <- dat_all %>% filter(short_name == "pcod")
	
	pcod_yrs <- forecast_yrs_dfs %>% filter(short_name == "pcod") %>%
		rename(year = forecast_yrs)
	
	pcod_obs <- pcod_dat %>%
		select(age, log_wt, year, age_f)
	
	pcod_obs_rep <- replicate(length(pcod_yrs$year), pcod_obs, simplify = FALSE) %>% bind_rows()

	pcod_preds <- pred_df %>% filter(short_name == "pcod")
	
	pcod_all <- bind_cols(pcod_preds, pcod_obs_rep)
	
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
	dim_fun <- function(df, num){
		
		dims <- dim(df)
		dim1 <- dims[1]
		dim2 <- dims[2]
		dims <- tibble(dim1, dim2)
		dims$list_no <- num
		
		dims
	}
	
	num <- length(dfs)
	
	fdf <- tibble(
		df = dfs,
		num = 1:num
	)
	
	dims <- map2(fdf$df, fdf$num, dim_fun) %>% bind_rows()
	
	lists_drop <- dims %>%
		filter(if_any(everything(), ~ .x == 0))
	
	drop <- lists_drop$list_no

	dfs_keep <- dfs[-drop]
	
	all_preds_sum <- dfs_keep %>% 
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
		facet_wrap(~ age_f, scales = "free_y") +
		theme_sleek() +
		ylab("Predicted mean (log) weight (g)") +
	 	xlab("Year") 
	
		 ggsave(here("output", "plots", "May 2024", "forecast skill", paste0("pcod_custom_forecast", ".png")), 
	 	 			 height = 6, width = 12)

	
		
	#### correlation coefficient ####
	
	
	
	
