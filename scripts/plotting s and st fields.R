# spatial and spatiotemporal latent fields

	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/")))
	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/"))
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  NN_mod_list <- lapply(mod_names_list, readRDS)
  
  # make a prediction grid
  pol_survey_grid <- survey_grid_full %>%
  	rename(presurvey_btemp = temp)
  
  pollock_dat <- dat_all %>%
  	filter(short_name == "pollock")
 
  pred_df_func <- function(x){
  	
  	pol_survey_grid <- pol_survey_grid %>%
  		mutate(age_f = x)
  	
  }
  
  ages <- sort(unique(pollock_dat$age_f))

  pol_pred_df <- lapply(ages, pred_df_func) %>% bind_rows()

  presurvey_btemp_int_mod_pollock <- NN_mod_list[[8]]
  
  preds <- predict(presurvey_btemp_int_mod_pollock, newdata = pol_pred_df)
  
  plot_map <- function(dat, column) {
  	ggplot(dat, aes(lat, lon, fill = {{ column }})) +
  	  geom_raster() +
  	  coord_fixed()
  }
  
  plot_map(preds, omega_s) +
  	scale_fill_gradient2() +
  	ggtitle("Spatial random effects only")
  
  plot_map(preds, epsilon_st) +
  	scale_fill_gradient2() +
  	facet_wrap(~year) +
  	ggtitle("Spatiotemporal random effects only")