	preds_fun <- function(mod, yr){
		
		mod_dat <- mod$data
		
		mod_dat_trim <- mod_dat %>% 
			select(age, age_f, year, year_f, short_name,
						 X, Y, latitude, longitude,
						 contains("temp"), contains("oxy")) %>%
			filter(year == yr)
		
		nsims = 20 # increase to at least 200
		
		preds <- predict(mod, se_fit = FALSE, nsim = nsims, newdata = mod_dat_trim) 
		
		preds_df <- preds %>%
			as_tibble()
		
		preds_df <- bind_cols(preds_df, mod_dat_trim)
		
		preds_df
		
	}
	
	###### for top models (with temp/oxy) #####
	mods <- pcod_top_mod_cv$models
	yrs <- forecast_yrs_dfs %>% filter(short_name == "pcod")
	yrs <- yrs$forecast_yrs
	
	fdf <- tibble(
		mod = mods,
		yr = yrs
	)
	
  pcod_preds <- map2(fdf$mod, fdf$yr, preds_fun)
	
  pcod_preds_df <- pcod_preds %>% bind_rows()
  
  ###### for  models without temp/oxy #####
	mods <- pcod_no_cov_cv$models
	yrs <- forecast_yrs_dfs %>% filter(short_name == "pcod")
	yrs <- yrs$forecast_yrs
	
	fdf <- tibble(
		mod = mods,
		yr = yrs
	)
	
  pcod_preds_no_cov <- map2(fdf$mod, fdf$yr, preds_fun)
	
  pcod_preds_no_cov_df <- pcod_preds_no_cov %>% bind_rows()
 
  # take average and sd 
  
  pcod_preds_split <- pcod_preds_df %>% group_split(age_f, year)
  pcod_preds_no_cov_split <- pcod_preds_no_cov_df %>% group_split(age_f, year)

  
  avg_sd_fun <- function(df){
  
  	df <- df %>% 
  	select(-contains("temp"), -contains("oxy"))
	
  	df_sum <- df %>%
			group_by(age_f, year, short_name) %>%
			summarise_at(vars(contains("V")), ~ mean(.x))
  
  	age <- df_sum$age_f
  	year <- df_sum$year
  
  	df_sum <- df_sum %>% 
  		ungroup() %>%
  		select(contains("V"))
  	
  	df_sum <- df_sum %>%
  		t() %>%
  		as_tibble() %>%
			summarise(mean = mean(V1), 
								sd = sd(V1)) 
  
  	df_sum$age <- age
  	df_sum$year <- year
  	
  	df_sum
  	
  }
  
  pcod_preds_top <- map(pcod_preds_split, avg_sd_fun) %>% bind_rows()
  
  pcod_preds_no_cov <- map(pcod_preds_no_cov_split, avg_sd_fun) %>% bind_rows()
  
  # observations 
  pcod_obs <- dat_all %>% filter(short_name == "pcod")

	pcod_obs_sum <- pcod_obs %>%
			group_by(age, age_f, year, year_f) %>%
			summarise_at(vars(log_wt),list(mean = ~mean(.), se = ~sd(./sqrt(.))))

  
  ggplot() +
  	geom_point(data = pcod_obs_sum, 
  						 aes(x = year, y = mean), color = "#1E88E5", alpha = 0.5) +
		geom_linerange(data = pcod_obs_sum, 
									aes(x = year, ymin = mean - se, ymax = mean + se),
									color = "#1E88E5", alpha = 0.5) +
  	geom_point(data = pcod_preds_top, 
  						 aes(x = year, y = mean), color = "#FFC107", alpha = 0.5) +
		geom_linerange(data = pcod_preds_top, 
									aes(x = year, ymin = mean - sd, ymax = mean + sd),
									color = "#FFC107", alpha = 0.5) +
  	geom_point(data = pcod_preds_no_cov, 
  						 aes(x = year, y = mean), color = "#004D40", alpha = 0.5) +
		geom_linerange(data = pcod_preds_no_cov, 
									aes(x = year, ymin = mean - sd, ymax = mean + sd),
									color = "#004D40", alpha = 0.5) +
  	facet_wrap(~age, scales = "free")
  