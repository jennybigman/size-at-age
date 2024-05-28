	# predictions generated from these models
	preds_fun <- function(mod, x){
		
		nsims = 100
		
		preds <- predict(mod, se_fit = FALSE, nsim = nsims)
	
		preds_df <- preds %>%
			as_tibble()
			
		preds_mean <- preds_df %>%
			rowwise() %>%
			summarise(mean_est = mean(c_across(1:ncol(preds))))
		
		preds_sd <- preds_df %>%
			rowwise() %>%
			summarise(mean_sd = sd(c_across(1:ncol(preds))))
		
		preds_se <- preds_sd %>%
			mutate(mean_se = mean_sd/(sqrt(nsims)))
						 
		preds_sum <- bind_cols(preds_mean, preds_se) 
						 
		preds_sum$mod_run <- as.factor(x)
		
		preds_sum
		
	}
	
	
	apply_fun <- function(mod_list, dat){
		
		mods <- mod_list$models
		num_mods <- length(mods)
		preds <- purrr::map2(mods, 1:num_mods, preds_fun) %>% 
			bind_rows() 
	
		preds

	}
	
	mod_list <- list(atooth_top_mod_cv, pcod_top_mod_cv, pollock_top_mod_cv, yfin_top_mod_cv)
	
	pred_dfs <- purrr::map(mod_list, apply_fun)
	
	
	bind_fun <- function(pred_df, obs_df){
		
		ages <- obs_df %>%
			select(age, age_f, year, log_wt, short_name)
	
		num_mods <- length(unique(pred_df$mod_run))
		
		ages_rep <- replicate(num_mods, ages, simplify = FALSE) %>% bind_rows()
	
		all_df <- bind_cols(ages_rep, pred_df)
		
		all_df
		
	}
	
	fdf <- tibble(
		pred_df = pred_dfs,
		obs_df = list(atooth_dat, pcod_dat, pollock_dat, yfin_dat)
	)
	
	all_dfs <- map2(fdf$pred_df, fdf$obs_df, bind_fun)

	
	file_path_plots <- here("./output/plots/May 2024/forecast skill/")
		
	plot_fun <- function(df){
		
		pred_sum  <- df %>%
			group_by(year, age_f, mod_run, age) %>%
			summarise(mean_est = mean(mean_est))
		
		obs_sum <- df %>%
			group_by(year, age_f, age) %>%
			summarise(mean_log_wt = mean(log_wt))
	
		name <- unique(df$short_name)
	
		# plot
		p <- 
			ggplot(pred_sum, aes(x = year)) +
			#geom_ribbon(aes(ymin = low, ymax = high, fill = mod_run, alpha = 0.4)) +
			geom_line(aes(x = year, y = mean_est, group = mod_run, color = mod_run)) + 
			geom_line(data = obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
			facet_wrap(~ reorder(age_f, age), scales = "free") +
			ggtitle(name) +
			theme_sleek()
		
		plot_name <- paste0(name, "_forecast_skill_May2024.png")

		ggsave(p, file = paste0(file_path_plots, plot_name))
	
	}
	
	purrr::map(all_dfs, plot_fun)	
	
	
	
	