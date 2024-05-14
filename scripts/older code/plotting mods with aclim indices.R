# plots for spatially-averaged temp/oxy using ACLIM indices
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/")))

	file_list <- stringr::str_subset(file_list, '.rds')

	prestring <- paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/"))
	 
	mod_names_list <- list()
	 
	 for(i in file_list){
	 	mod_names_list[[i]] <- paste0(prestring, i)
	 }
	 
	 mods <- lapply(mod_names_list, readRDS)
	
	# pollock #
	pol_mods <- mods[grep("pol", names(mods))]
	
	# pcod #
	pcod_mods <- mods[grep("pcod", names(mods))]
		
	# yfin #
	yfin_mods <- mods[grep("yfin", names(mods))]
	
		
	## compare models with and without an interaction ##
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	mod_lists <- list(pol_mods, pcod_mods, yfin_mods)

	vars <- dat_all %>%
		ungroup() %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = mod_lists,
		y = vars
	)

	ACLIM_AICs <- map2(df_func$x, df_func$y, AIC_func)


	# df to compare
	ACLIM_AICs_df <-	ACLIM_AICs %>% 
 		bind_cols() %>%
 		pivot_longer(cols = contains(c("temp", "oxy")),
 								 names_to = 'model', values_to = "AIC") %>%
 		rename(AIC_aclim = AIC)
 
	 mod_split_func <- function(species){
 	
 		species_AICs <- ACLIM_AICs_df %>%
 			filter(str_detect(model, species))
	
 		temp_sp_AIC <- species_AICs %>% 
 			filter(str_detect(model,"temp"))
 		
 		oxy_sp_AIC <- species_AICs %>% 
 			filter(str_detect(model, "oxy"))
 		
 		sp_AIC <- list(temp_sp_AIC, oxy_sp_AIC)
 		
	}
 
	names <- unique(dat_all$short_name)
 
	sp_AIC_dfs <- lapply(names, mod_split_func) 
 
	pol_comp_temp <- sp_AIC_dfs[[1]][1] %>% bind_rows()
 
	pol_comp_oxy  <- sp_AIC_dfs[[1]][2] %>% bind_rows()
	
	pcod_comp_temp <- sp_AIC_dfs[[2]][1] %>% bind_rows()
	
	pcod_comp_oxy <- sp_AIC_dfs[[2]][2] %>% bind_rows()
	
	yfin_comp_temp <- sp_AIC_dfs[[3]][1] %>% bind_rows()
	
	yfin_comp_oxy <- sp_AIC_dfs[[3]][2] %>% bind_rows()
 
	AIC_list <- list(
		pol_comp_temp, pol_comp_oxy,
		pcod_comp_temp, pcod_comp_oxy,
		yfin_comp_temp, yfin_comp_oxy)

	# extract the model with the lowest AIC from each
	lowest_AIC_func <- function(df){
		
		mod <- df$model[df$AIC_aclim == min(df$AIC_aclim)]
	}
 
	lowest_AIC <- sapply(AIC_list, lowest_AIC_func) %>% 
 		as_tibble() %>%
 		rename(model = value)
 
	mod_names <- lowest_AIC$model
 
	# read files in
	file_list <- mod_names
	
	prestring <- paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/"))
	 
	mod_names_list <- list()
	 
	 for(i in file_list){
	 	mod_names_list[[i]] <- paste0(prestring, i)
	 }
	 
	mods <- lapply(mod_names_list, readRDS)

	temp_mods <- mods[grep("btemp", names(mods))]
	oxy_mods <-  mods[grep("boxy", names(mods))]
	 
	 # function to plot weight vs temp by age class
	 
	 weight_temp_df_func <- function(sp){
	 	
			mod_temp <- temp_mods[grep(sp, names(temp_mods))] ## change back to sp
			
			mod_temp_obj <- pluck(mod_temp, 1)
			
			mod_temp_data <- pluck(mod_temp_obj, 'data')
			
			var_form <- mod_temp_obj$formula
			
			var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
			
			temp_df <- mod_temp_data %>%
				ungroup %>%
				select(var)

			# new dat for prediction
	 		new_dat_temp_hind <- expand_grid(
				var = seq(
					from = min(temp_df[, 1]),
					to = max(temp_df[ ,1]),
					length.out = 25),
				age_f = sort(unique(mod_temp_data$age_f)),
				year = 2000) # need all years for plotting weight at age vs year

	 	new_dat_temp_hind <- new_dat_temp_hind %>%
	 		rename("{var}" := var)
	 		
	 		# predict
	 		temp_hind_preds <- predict(
	 			mod_temp_obj,
				newdata = new_dat_temp_hind,
				se_fit = TRUE,
				re_form = NA,
				return_tmb_object = FALSE)
	 		
	 		# add se estimates
	 		temp_hind_preds <- temp_hind_preds %>% 
	 			mutate(low = est - est_se,
	 						 high = est + est_se,
	 						 species = sp)
	 		
	 		
	 }

	species <- unique(dat_all$short_name) 

	temp_ACLIM_dfs <- purrr::map(species, weight_temp_df_func)

	
	# function to plot weight vs oxygen by age class
	 
	 weight_oxy_df_func <- function(sp){
	 	
			mod_oxy <- oxy_mods[grep(sp, names(oxy_mods))]
			
			mod_oxy_obj <- pluck(mod_oxy, 1)
			
			mod_oxy_data <- pluck(mod_oxy_obj, 'data')

			var_form <- mod_oxy_obj$formula
			
			var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
			
			oxy_df <- mod_oxy_data %>%
				ungroup %>%
				select(var)

	 		# new dat for prediction
	 		new_dat_oxy_hind <- expand_grid(
				var = seq(
					from = min(oxy_df[, 1]),
					to = max(oxy_df[ ,1]),
					length.out = 25),
				age_f = sort(unique(mod_oxy_data$age_f)),
				year = 2000) # need all years for plotting weight at age vs year

	 	new_dat_oxy_hind <- new_dat_oxy_hind %>%
	 		rename("{var}" := var)
	 		
	 	# predict
	 	oxy_hind_preds <- predict(
	 			mod_oxy_obj,
				newdata = new_dat_oxy_hind,
				se_fit = TRUE,
				re_form = NA,
				return_tmb_object = FALSE)
	 		
	 		# add se estimates
	 		oxy_hind_preds <- oxy_hind_preds %>% 
	 			mutate(low = est - est_se,
	 						 high = est + est_se,
	 						 species = sp)
	 		
	}

	species <- unique(dat_all$short_name) 

	oxy_ACLIM_dfs <- purrr::map(species, weight_oxy_df_func)
	
	# weight vs temp plots
	
	temp_ACLIM_dfs
	
	weight_temp_plot_func <- function(df){

	# plot
			weight_temp_plot <-
					ggplot(df, aes(yrprior_btemp, est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ age_f, scales = "free_y") +
					ylab("partial effect of\n(log) weight") +
					xlab("bottom temperature\n(averaged June - June)") +
					theme_sleek()
	
	
			species_name <- unique(df$species)
			
			plot_name <- paste0(species_name, "_aclim_temp.png")
			
			ggsave(weight_temp_plot, file = paste0(file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
	}
	
	purrr::map(temp_ACLIM_dfs, weight_temp_plot_func)

	
	
		# weight vs oxygen plots
	
		oxy_ACLIM_dfs_trim <- oxy_ACLIM_dfs[c(1,2)]
		oxy_ACLIM_df <- oxy_ACLIM_dfs[[3]]

	 	weight_oxy_plot_func <- function(df){
		
	 		# plot
			weight_oxy_plot <-
					ggplot(df, aes(x = yrprior_boxy, y = est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ age_f, scales = "free_y") +
					ylab("partial effect of\n(log) weight") +
					xlab("bottom oxygen") +
					theme_sleek()
			
			species_name <- unique(df$species)
			
			plot_name <- paste0(species_name, "_aclim_oxy.png")
			
			ggsave(weight_oxy_plot, file = paste0(file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
	 	}
	 	
	purrr::map(oxy_ACLIM_dfs_trim, weight_oxy_plot_func)

	# last one
	
		weight_oxy_plot <-
					ggplot(oxy_ACLIM_df, aes(x = presurvey_boxy, y = est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ age_f, scales = "free_y") +
					ylab("partial effect of\n(log) weight") +
					xlab("bottom oxygen") +
					theme_sleek()
			
			species_name <- unique(df$species)
			
			plot_name <- paste0("yfin_aclim_oxy.png")
			
			ggsave(weight_oxy_plot, file = paste0(file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
	
	 