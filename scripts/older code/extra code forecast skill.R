# extra code for forecast skill

	
	
	
	# plot observed vs predicted for each weight
	
	# atooth
	atooth_fdf <- pred_df %>% filter(short_name == "atooth")

	atooth_obs <- atooth_dat %>%
		select(age, log_wt, year, age_f)
	
	atooth_comb <- bind_cols(atooth_fdf, atooth_obs)
	
	p <- 
		ggplot(atooth_comb) +
		geom_point(aes(x = log_wt, y = mean_est), alpha = 0.7) +
		facet_wrap(~age_f, scales = "free") +
		theme_sleek() 
		
	
	p + ggh4x::facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(1, 2)),
      scale_y_continuous(limits = c(1.5, 2.5)),
      scale_y_continuous(limits = c(2, 3)),
      scale_y_continuous(limits = c(2, 3)),
      scale_y_continuous(limits = c(2, 3)),
      scale_y_continuous(limits = c(2.5, 3.5)),
      scale_y_continuous(limits = c(2.5, 3.5)),
      scale_y_continuous(limits = c(3, 4)),
      scale_y_continuous(limits = c(3, 4)),
      scale_y_continuous(limits = c(3, 4)),
      scale_y_continuous(limits = c(3, 4))
      ),
		x = list(
      scale_x_continuous(limits = c(1, 2)),
      scale_x_continuous(limits = c(1.5, 2.5)),
      scale_x_continuous(limits = c(2, 3)),
      scale_x_continuous(limits = c(2, 3)),
      scale_x_continuous(limits = c(2, 3)),
      scale_x_continuous(limits = c(2.5, 3.5)),
      scale_x_continuous(limits = c(2.5, 3.5)),
      scale_x_continuous(limits = c(3, 4)),
      scale_x_continuous(limits = c(3, 4)),
      scale_x_continuous(limits = c(3, 4)),
      scale_x_continuous(limits = c(3, 4)))
      )
      
     
		
	
	###############################################
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
			geom_point(aes(x = year, y = mean_est, group = mod_run, color = mod_run)) + 
			geom_point(data = obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
			facet_wrap(~ reorder(age_f, age), scales = "free") +
			ggtitle(name) +
			theme_sleek()
		
		plot_name <- paste0(name, "_forecast_skill_May2024.png")

		ggsave(p, file = paste0(file_path_plots, plot_name))
	
	}
	
	purrr::map(all_dfs, plot_fun)	
	