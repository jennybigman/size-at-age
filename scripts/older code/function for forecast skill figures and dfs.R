	pred_fun <- function(mod){
	
		preds <- predict(mod, se_fit = TRUE)
	
	}
	
	atooth_preds <- purrr::map(atooth_top_mod_cv$models, pred_fun)
	atooth_preds <- atooth_preds %>% bind_rows()
	
	pcod_preds <- purrr::map(pcod_top_mod_cv$models, pred_fun)
	pcod_preds <- pcod_preds %>% bind_rows()

	pollock_preds <- purrr::map(pollock_top_mod_cv$models, pred_fun)
	pollock_preds <- pollock_preds %>% bind_rows()

	yfin_preds <- purrr::map(yfin_top_mod_cv$models, pred_fun)
	yfin_preds <- yfin_preds %>% bind_rows()

	
	pred_dfs <- list(atooth_preds, pcod_preds, pollock_preds, yfin_preds)
	
	# write to file
	write_fun <- function(df){
		
		file_name <- paste0(here(), "/data/", unique(df$short_name), "_preds.csv")
		write_csv(df, file = file_name)
		
	}
	
	purrr::map(pred_dfs, write_fun)
	
	
	file_path_plots <- paste0(here(), "/output/plots/May 2024/forecast skill/")

	atooth_preds <- read.csv(file = here("./data/atooth_preds.csv"))
	
	
	
	preds_plot_fun <- function(df){
		
	# mean weight-at-age by year
	preds_sum <- atooth_preds %>%
		mutate(cv_fold_f = as.factor(cv_fold)) %>%
		group_by(year, age_f, cv_fold_f, age) %>%
		summarise(mean_est = mean(est))
	
	obs_sum <- atooth_dat %>%
		group_by(year, age_f, age) %>%
		summarise(mean_log_wt = mean(log_wt))

	name <- unique(atooth_dat$short_name)

	# plot
	p <- 
		ggplot(preds_sum, aes(x = year)) +
		#geom_ribbon(aes(ymin = low, ymax = high, fill = mod_run, alpha = 0.4)) +
			geom_line(aes(x = year, y = mean_est, group = cv_fold_f, color = cv_fold_f)) + 
			geom_line(data = obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
			facet_wrap(~ reorder(age_f, age), scales = "free") +
			ggtitle(name) +
			theme_sleek()
	
	ggsave(filename = paste0(file_path_plots, name, ".png"),
				 plot = p, width = 10, height = 7, units = "in")
	
	}
	
	purrr::map(pred_dfs, preds_plot_fun)
	
	