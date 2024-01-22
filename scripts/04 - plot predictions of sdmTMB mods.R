# 05 - forecasting

	# read in models with an interaction

	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/")))
	drop_list <- file_list[grep("no_int", file_list)]
	int_mod_list_names <- setdiff(file_list, drop_list)
	
	prestring <- paste0(here(), ("/output/model output/sdmTMB output//Jan 2024/"))
	 
	 mod_names_list <- list()
	 
	 for(i in int_mod_list_names){
	 	mod_names_list[[i]] <- paste0(prestring, i)
	 }
	 
	 int_mods <- lapply(mod_names_list, readRDS)
	
	 yr_oxy_mods <- int_mods[grep("yrprior_boxy", names(int_mods))]
	 yr_temp_mods <- int_mods[grep("yrprior_btemp", names(int_mods))]
	 
	 # function to plot weight vs temp by age class
	 
	 weight_temp_df_func <- function(sp){
	 	
			mod_temp <- yr_temp_mods[grep(sp, names(yr_temp_mods))]
			
			mod_temp_obj <- pluck(mod_temp, 1)
			
			mod_temp_data <- pluck(mod_temp_obj, 'data')

	 		# new dat for prediction
	 		new_dat_temp_hind <- expand_grid(
				yrprior_btemp = seq(
					from = min(mod_temp_data$yrprior_btemp),
					to = max(mod_temp_data$yrprior_btemp),
					length.out = 25),
				age_f = sort(unique(mod_temp_data$age_f)),
				year = 2000) # predict for one year b/c all the same and faster

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

	temp_hind_plot_dfs <- map(species, weight_temp_df_func)

	
	# function to plot weight vs oxygen by age class
	 
	 weight_oxy_df_func <- function(sp){
	 	
			mod_oxy <- yr_oxy_mods[grep(sp, names(yr_oxy_mods))]
			
			mod_oxy_obj <- pluck(mod_oxy, 1)
			
			mod_oxy_data <- pluck(mod_oxy_obj, 'data')

	 		# new dat for prediction
	 		new_dat_oxy_hind <- expand_grid(
				yrprior_boxy = seq(
					from = min(mod_oxy_data$yrprior_btemp),
					to = max(mod_oxy_data$yrprior_btemp),
					length.out = 25),
				age_f = sort(unique(mod_oxy_data$age_f)),
				year = 2000) # predict for one year b/c all the same and faster

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

	oxy_hind_plot_dfs <- map(species, weight_oxy_df_func)
	
	 
	# weight vs temp plots
	
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
			
			plot_name <- paste0(species_name, "_temp.png")
			
			ggsave(weight_temp_plot, file = paste0(here(), file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
	}

		# weight vs oxygen plots

	 	weight_oxy_plot_func <- function(df){
		
	# plot
			weight_oxy_plot <-
					ggplot(df, aes(yrprior_boxy, est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ age_f, scales = "free_y") +
					ylab("partial effect of\n(log) weight") +
					xlab("bottom oxygen\n(averaged June - June)") +
					theme_sleek()
			
			species_name <- unique(df$species)
			
			plot_name <- paste0(species_name, "_oxy.png")
			
			ggsave(weight_oxy_plot, file = paste0(here(), file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
	}
	 
	
	 
	
	
	