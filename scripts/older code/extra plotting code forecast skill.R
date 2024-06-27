
	
	
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
		
		all_preds_sum <- all_preds_sum %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
		all_preds_sum <- all_preds_sum %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
		
		all_preds_sum$age_label <- paste0("Age ", all_preds_sum$age_f)
	

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
	 		facet_wrap(~ fct_reorder(age_label, age), scales = "free_y", ncol = 4) +
			ylab("Predicted mean (log) weight (g)") +
	 		xlab("Year") +
			ggtitle(sp) +
			theme_sleek() +
			theme(
				legend.position = "bottom"
			)
		
		plot_name <- paste0(sp, "_forecast_skill.png")

		ggsave(here("output", "plots", "May 2024", "forecast skill", plot_name), 
	 	 	height = 6, width = 12)

		
	}
	
	map(forecast_dfs, plot_fun)
	
	

	
	
	
	
	
	
	
	#########################################################
	#########################################################
	
	#### create and plot dfs of preds and obs for each year of each model for each age class ####

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
	
	forecast_df <- forecast_dfs %>% bind_rows()
	
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
	
	
	# customize axes
	
	pcod_plot1 <- plots[[1]]
	
	# set up labels in each facet
	pcod_fcast <- forecast_df %>% filter(short_name == "pcod")
	
	age_labs <- paste0("Age", " ", unique(pcod_fcast$age_f))
	age_f <- unique(sort(pcod_fcast$age_f))
	year_f <- pcod_fcast %>% 
						group_by(age_f) %>% 
						summarise(year_f = floor(max(year)) - 15)
	year_f <- as_vector(year_f$year_f)
	
	mean_est <- pcod_fcast %>% 
							group_by(age_f) %>% 
							summarise(mean_est = max(mean_est))
	mean_est <- mean_est$mean_est
	
	label_df <- tibble(
		year_f = as.factor(year_f),
		mean_est = mean_est,
		lab = age_labs,
		age_f = age_f
	)
	
	pcod_scales <- list(
		scale_y_continuous(
			breaks = c(1.6, 1.8)), 
		scale_y_continuous(
			breaks = c(2.5, 2.7)), 
		scale_y_continuous(
			breaks = c(2.9, 3.1)),
		scale_y_continuous(
			breaks = c(3.15, 3.25)),
		scale_y_continuous(
			breaks = c(3.4, 3.5)),
		scale_y_continuous(
			breaks = c(3.55, 3.65)),
		scale_y_continuous(
			breaks = c(3.65, 3.75)),
		scale_y_continuous(
			breaks = c(3.75, 3.85)),
		scale_y_continuous(
			breaks = c(3.8, 3.9)),
		scale_y_continuous(
			breaks = c(3.9, 4.0))
)
	
	pcod_plot2 <- pcod_plot1 + 
		facetted_pos_scales(y = pcod_scales) +
		geom_text(data = label_df, label = label_df$lab, size = 2) 

	 
	ggsave(here("output", "plots", "May 2024", "forecast skill", paste0("pcod_custom_forecast", ".png")), 
	 	 	height = 6, width = 12)

	
	# pollock #
	
		
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
		
		all_preds_sum <- all_preds_sum %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
		all_preds_sum <- all_preds_sum %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
		
		all_preds_sum$age_label <- paste0("Age ", all_preds_sum$age_f)
	

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
	 		facet_wrap(~ fct_reorder(age_label, age), scales = "free_y", ncol = 4) +
			ylab("Predicted mean (log) weight (g)") +
	 		xlab("Year") +
			ggtitle(sp) +
			theme_sleek() +
			theme(
				legend.position = "bottom"
			)
		
		plot_name <- paste0(sp, "_forecast_skill.png")

		ggsave(here("output", "plots", "May 2024", "forecast skill", plot_name), 
	 	 	height = 6, width = 12)

		
	}
	
	map(forecast_dfs, plot_fun)
	
	
	
	
	
	
	
	
	
