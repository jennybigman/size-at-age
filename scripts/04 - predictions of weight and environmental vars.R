# 04 - predictions of weight and environmental variables (temp, oxy)

	# read in models with an interaction

	lowest_AIC <- read_csv(file = here("./data/AICs_nn.csv")) %>%
		rename(model = value)
	
	file_list <- lowest_AIC[["model"]]
	
	prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/"))
	 
	mod_names_list <- list()
	 
	 for(i in file_list){
	 	mod_names_list[[i]] <- paste0(prestring, i)
	 }
	 
	 mods <- lapply(mod_names_list, readRDS)
	
	 temp_mods <- mods[grep("btemp", names(mods))]
	 oxy_mods <- mods[grep("boxy", names(mods))]
	 
	 # function to plot weight vs temp by age class
	 
	 weight_temp_df_func <- function(sp){
	 	
			mod_temp <- temp_mods[grep("pollock", names(temp_mods))] ## change back to sp
			
			mod_temp_obj <- pluck(mod_temp, 1)
			
			mod_temp_data <- pluck(mod_temp_obj, 'data')
			
			var_form <- mod_temp_obj$formula
			
			var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
			
			temp_df <- mod_temp_data %>% select(var)

			# new dat for prediction
	 		new_dat_temp_hind <- expand_grid(
				var = seq(
					from = min(temp_df[, 1]),
					to = max(temp_df[ ,1]),
					length.out = 25),
				age_f = sort(unique(mod_temp_data$age_f)),
				year =2000) # need all years for plotting weight at age vs year

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

	temp_hind_plot_dfs <- map(species, weight_temp_df_func)

	
	# function to plot weight vs oxygen by age class
	 
	 weight_oxy_df_func <- function(sp){
	 	
			mod_oxy <- oxy_mods[grep(sp, names(oxy_mods))]
			
			mod_oxy_obj <- pluck(mod_oxy, 1)
			
			mod_oxy_data <- pluck(mod_oxy_obj, 'data')

	 		# new dat for prediction
	 		new_dat_oxy_hind <- expand_grid(
				yrprior_boxy = seq(
					from = min(mod_oxy_data$yrprior_boxy),
					to = max(mod_oxy_data$yrprior_boxy),
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
	
	#### PLOTS ####
	
	file_path_plots <- paste0(here(), "/plots/")
	
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
			
			ggsave(weight_temp_plot, file = paste0(file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
	}
	
	temp_plot_dfs <- list(temp_hind_plot_dfs[[2]], temp_hind_plot_dfs[[3]])
	
	map(temp_plot_dfs, weight_temp_plot_func)

	
	# for pollock
	
	pollock_plot_df <- temp_hind_plot_dfs[[1]]

	pollock_temp_plot <-
					ggplot(pollock_plot_df, aes(presurvey_btemp, est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ age_f, scales = "free_y") +
					ylab("partial effect of\n(log) weight") +
					xlab("bottom temperature\n(averaged May - June)") +
					theme_sleek()
	
	ggsave(here("./plots/pollock_temp.png"), pollock_temp_plot,
				 	 height = 5, width = 10, units = "in")

	

		# weight vs oxygen plots

	 	weight_oxy_plot_func <- function(df, var){
		
	# plot
			weight_oxy_plot <-
					ggplot(df, aes({{ var }}, est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ age_f, scales = "free_y") +
					ylab("partial effect of\n(log) weight") +
					xlab("bottom oxygen\n(averaged June - June)") +
					theme_sleek()
			
			species_name <- unique(df$species)
			
			plot_name <- paste0(species_name, "_oxy.png")
			
			ggsave(weight_oxy_plot, file = paste0(file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
	}
	 
	map(oxy_hind_plot_dfs, weight_oxy_plot_func)

	
	 
	
	
	