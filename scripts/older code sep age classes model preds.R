 sep_age_preds_fun <- function(x){
	 	
			mod <- pluck(all_mods, x)
			
			mod_dat <- mod$data
			
			sp <- unique(mod_dat$short_name)
	 		
			var_form <- mod$formula
			     
			var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
			
			mod_dat_trim <- mod_dat %>%
				ungroup %>%
				select(var)

			# new dat for prediction
	 		newdat <- expand_grid(
				var = seq(
					from = min(mod_dat_trim[, 1]),
					to = max(mod_dat_trim[ ,1]),
					length.out = 25),
				age_f = sort(unique(mod_dat$age_f)),
				year = 2004) # need all years for plotting weight at age vs year

	 		newdat <- newdat %>%
	 			rename("{var}" := var)
	 		
	 		# predict
	 		preds <- predict(
	 			mod,
				newdata = newdat,
				se_fit = TRUE,
				re_form = NA,
				return_tmb_object = FALSE)
	 		
	 		# add se estimates
	 		preds <- preds %>% 
	 			mutate(low = est - est_se,
	 						 high = est + est_se,
	 						 species = sp)
	 		
	 }

	preds_sep_age <- map(1:length(all_mods), sep_age_preds_fun)
	
	all_preds_sep_age <- preds_sep_age %>% bind_rows()
	
	
	
	
	##########################################################
	
	  preds_fun <- function(mod){
		
  	mod_dat <- mod$data
		
		sp <- unique(mod_dat$short_name)
		age <- unique(mod_dat$age_f)
		
		mod_dat_trim <- mod_dat %>% 
			select(age, age_f, year, year_f, short_name,
						 X, Y, latitude, longitude,
						 contains("temp"), contains("oxy"))
		
		nsims = 200
		
		preds <- try(predict(mod, se_fit = FALSE, nsim = nsims, newdata = mod_dat_trim))
		
		preds_df <- preds %>%
			as_tibble()
		
		preds_df <- bind_cols(preds_df, mod_dat_trim)
		
		preds_df$short_name <- sp
		
		preds_df
		
	}
	
	all_pred_dfs <- map(all_mods, preds_fun)
	
	all_pred_df <- all_pred_dfs %>% bind_rows()
	
	
		#####################
	
	# calculate averages
	
	preds_split_dfs <- all_pred_df %>% group_split(age_f, short_name)
  
  avg_sd_fun <- function(df){
  
  	age <- df$age
  	age_f <- df$age_f
  	short_name <- df$short_name
  	presurvey_btemp <- df$presurvey_btemp
  	presurvey_boxy <- df$presurvey_boxy
  	yrprior_btemp <- df$yrprior_btemp
  	yrprior_boxy <- df$yrprior_boxy
  	
  	df_trim <- df %>% 
  		select(-contains("pre")) %>%
  		select(contains("V"))
  	
		mean <- df_trim %>%
			rowwise() %>%
			summarise(mean_est = mean(c_across(1:ncol(df_trim)))) 
		
		sd <- df_trim %>%
			rowwise() %>%
			summarise(mean_sd = sd(c_across(1:ncol(df_trim))))

		preds_sum <- tibble(mean, sd, age, age_f, short_name, 
											  presurvey_btemp, presurvey_boxy, 
												yrprior_btemp, yrprior_boxy) 
  	
  }
  
  preds_top <- map(preds_split_dfs, avg_sd_fun) %>% bind_rows()
  
  
  #### PLOTS #### 
  
  # atooth - presurvey box
  # pcod - presurvey btemp
  # pollock - presurvey btemp
  # yfin - yrprior btemp
	#####################
	
	# calculate averages
	
	preds_split_dfs <- all_pred_df %>% group_split(age_f, short_name)
  
  avg_sd_fun <- function(df){
  
  	age <- df$age
  	age_f <- df$age_f
  	short_name <- df$short_name
  	presurvey_btemp <- df$presurvey_btemp
  	presurvey_boxy <- df$presurvey_boxy
  	yrprior_btemp <- df$yrprior_btemp
  	yrprior_boxy <- df$yrprior_boxy
  	
  	df_trim <- df %>% 
  		select(-contains("pre")) %>%
  		select(contains("V"))
  	
		mean <- df_trim %>%
			rowwise() %>%
			summarise(mean_est = mean(c_across(1:ncol(df_trim)))) 
		
		sd <- df_trim %>%
			rowwise() %>%
			summarise(mean_sd = sd(c_across(1:ncol(df_trim))))

		preds_sum <- tibble(mean, sd, age, age_f, short_name, 
											  presurvey_btemp, presurvey_boxy, 
												yrprior_btemp, yrprior_boxy) 
  	
  }
  
  preds_top <- map(preds_split_dfs, avg_sd_fun) %>% bind_rows()
  
  
  #### PLOTS #### 
  
  # atooth - presurvey box
  # pcod - presurvey btemp
  # pollock - presurvey btemp
  # yfin - yrprior btemp
