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
	
	atooth_mods <- atooth_top_mod_cv$models
	num_mods <- length(atooth_mods)
	atooth_preds <- purrr::map2(atooth_mods, 1:num_mods, preds_fun) #%>% 
		#bind_rows() 
	
	ages <- atooth_dat %>%
		select(age, age_f, year, log_wt)
	
	ages_rep <- replicate(5, ages, simplify = FALSE) %>% bind_rows()
	
	atooth_test <- bind_cols(ages_rep, atooth_preds)
	
	atooth_test_sum <- atooth_test %>%
		group_by(year, age_f, mod_run, age) %>%
		summarise(mean_est = mean(mean_est))

	p <- ggplot(atooth_test_sum, aes(x = year)) +
		#geom_ribbon(aes(ymin = low, ymax = high, fill = mod_run, alpha = 0.4)) +
		geom_line(aes(x = year, y = mean_est, group = mod_run, color = mod_run)) + 
		geom_line(data = atooth_obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
		facet_wrap(~ reorder(age_f, age), scales = "free") +
		ggtitle(name) +
		theme_sleek()

	
	
	
	
	atooth_preds_list <- atooth_preds %>%
		group_split(mod_run)

	test <- bind_cols(atooth_preds_list)
	
	
	
	
	
	
	
	#atooth_all <- bind_cols(atooth_dat, atooth_preds)
	
	#########
	atooth_preds_sum <- atooth_all %>%
		group_by(year, age_f, mod_run, age) %>%
		summarise(mean_est = mean(est))
	
	atooth_obs_sum <- atooth_dat %>%
		group_by(year, age_f, age) %>%
		summarise(mean_log_wt = mean(log_wt))
	
	#atooth_obs_sum <- atooth_preds %>%
	#	group_by(year, age_f, age) %>%
	#	summarise(mean_log_wt = mean(log_wt))
	
	name <- unique(atooth_preds$short_name)
	
	# plot
	ggplot(atooth_preds_sum, aes(x = year)) +
		#geom_ribbon(aes(ymin = low, ymax = high, fill = mod_run, alpha = 0.4)) +
		geom_line(aes(x = year, y = mean_est, group = mod_run, color = mod_run)) + 
		geom_line(data = atooth_obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
		facet_wrap(~ reorder(age_f, age), scales = "free") +
		ggtitle(name) +
		theme_sleek()

	
		
	
	
	
	
	# predictions generated from these models
	preds_fun <- function(mod, x){
		
		preds <- predict(atooth_top_mod_cv$models[[1]], se_fit = TRUE)
		
		preds$mod_run <- as.factor(x)
		#preds <- preds %>% 
	 	#	mutate(low = est - est_se,
	 	#				 high = est + est_se)
	 	
		preds
		
	}

	
	# read in pred dfs (large files)
	atooth_preds <- read.csv(file = here("./data/atooth_preds.csv"))
	
	#### arrowtooth ####
	atooth_mods <- atooth_top_mod_cv$models
	num_mods <- length(atooth_mods)
	atooth_preds <- purrr::map2(atooth_mods, 1:num_mods, preds_fun) %>% 
		bind_rows() 

	# mean weight-at-age by year
	atooth_preds_sum <- atooth_preds %>%
		group_by(year, age_f, mod_run, age) %>%
		summarise(mean_est = mean(est))
	
	atooth_obs_sum <- atooth_dat %>%
		group_by(year, age_f, age) %>%
		summarise(mean_log_wt = mean(log_wt))
	
	name <- unique(atooth_preds$short_name)
	
	# plot
	atooth_preds_plot <- 
		ggplot(atooth_preds_sum, aes(x = year)) +
		#geom_ribbon(aes(ymin = low, ymax = high, fill = mod_run, alpha = 0.4)) +
		geom_line(aes(x = year, y = mean_est, group = mod_run, color = mod_run)) + 
		geom_line(data = atooth_obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
		facet_wrap(~ reorder(age_f, age), scales = "free") +
		ggtitle(name) +
		theme_sleek()

	#### pcod ####
	atooth_mods <- atooth_top_mod_cv$models
	num_mods <- length(atooth_mods)
	atooth_preds <- purrr::map2(atooth_mods, 1:num_mods, preds_fun) %>% 
		bind_rows() 

	# mean weight-at-age by year
	atooth_preds_sum <- atooth_preds %>%
		group_by(year, age_f, mod_run, age) %>%
		summarise(mean_est = mean(est))
	
	atooth_obs_sum <- atooth_dat %>%
		group_by(year, age_f, age) %>%
		summarise(mean_log_wt = mean(log_wt))
	
	name <- unique(atooth_preds$short_name)
	
	# plot
	atooth_preds_plot <- 
		ggplot(atooth_preds_sum, aes(x = year)) +
		#geom_ribbon(aes(ymin = low, ymax = high, fill = mod_run, alpha = 0.4)) +
		geom_line(aes(x = year, y = mean_est, group = mod_run, color = mod_run)) + 
		geom_line(data = atooth_obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
		facet_wrap(~cv_fold_f) + 
		#facet_wrap(~ reorder(age_f, age), scales = "free") +
		ggtitle(name) +
		theme_sleek()


	

	
	
	
	
	
	
	
	
		# run models for forecasting - find missing years
	max_yr <- max(atooth_dat$year)
	min_yr <- min(atooth_dat$year)
	yrs <- sort(unique(atooth_dat$year))
	all_yrs <- min_yr:max_yr
	yrs_missing <- setdiff(all_yrs, yrs)