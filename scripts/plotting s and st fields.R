# plot spatial and spatiotemporal latent fields

	# set up spatial polygon
	
	# plot temps
	reg = map_data("world2Hires")
	reg = subset(reg, region %in% c('USSR', 'USA'))
	
	# convert lat longs
	reg$long = (360 - reg$long)*-1
	
	# set map limits
	lons = c(-181, -160)
	lats = c(52, 65)

	# quick plotting function from sdmTMB vignette
	plot_map <- function(dat, column) {
	 	ggplot(dat, aes(lon, lat, fill = {{ column }})) +
	 	  geom_raster() +
			geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  		         fill = "darkgrey", color = NA) +
			coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
			theme_sleek()
	}
	
	# read in prediction grid of Bering 10k bottom temp
	grids_all <- fread(file = here("./data/grids_all.csv"))


	################################################################################
	#### spatially explicit - spatially explicit yearly temp
	################################################################################
	
	# load models
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/")))
	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/"))
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  NN_mod_list <- lapply(mod_names_list, readRDS)
  
  # function to plot spatial field for each model
  preds_func <- function(mod){
  	
  		# grab data
  		mod_dat <- mod$data
  		
  		# which environmental variable?
  		form <- mod$formula
	
  		var <- gsub(".*[()]([^,]+)[,].*", "\\1", form)
  		
  		# filter grid by variable and year
  		
  		mod_yrs <- sort(unique(mod_dat$year))
  			
  		mod_grid <- grid_full %>%
  			filter(year %in% mod_yrs) %>%
  			rename("{var}" := temp)
  		
  		pred_dat <- expand_grid(
  			age_f = sort(unique(mod_dat$age_f)),
  			year = sort(unique(mod_dat$year))
  		)
  		
  		pred_df <- left_join(pred_dat, mod_grid) %>% na.omit
  		
  		preds <- predict(mod, pred_df)
  		
  }
  
  preds_all <- purrr::map(NN_mod_list, preds_func)
  
  
  # plot spatial and spatiotemporal fields
  plot_func <- function(df, mod_name){
  	
  	plot_s <-
  		plot_map(df, omega_s) +
  		scale_fill_gradient2(limits = c(-0.29, 0.13)) +
  		ggtitle(paste0("spatial field: ", mod_name, "_sp_exp"))
   
  	plot_name_s <- paste0(mod_name, "_sp_field_nn")

		ggsave(plot_s,
					 file = paste0(here(), "/plots/spatial fields/", plot_name_s, ".png"),
					 height = 5, width = 5, units = "in")
  
  	plot_st <-
  		plot_map(df, epsilon_st) +
  		scale_fill_gradient2(limits = c(-0.53, 0.31)) +
  		facet_wrap(~ year) +
  		ggtitle(paste0("spatioemporal field: ", mod_name, "_sp_exp"))
  	
    plot_name_st <- paste0(mod_name, "_spt_field_nn")

		ggsave(plot_st, 
					 file = paste0(here(), "/plots/spatiotemporal fields/", plot_name_st, ".png"),
					 height = 10, width = 15, units = "in")

  }
  
  mod_names <- names(NN_mod_list)
  
  rm_func <- function(name){
  	
	mod_names <- name %>% str_remove(".rds")
	 
  }
  
  mod_names <- purrr::map(mod_names, rm_func)

  
  fdf <- tibble(
  	df = preds_all,
  	mod_name = mod_names
  )
  
	purrr::map2(fdf$df, fdf$mod_name, plot_func)
	
	
	################################################################################
	#### non spatially explicit - spatially averaged yearly temp
	################################################################################

	# load models
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/")))
	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/"))
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  nspe_mod_list <- lapply(mod_names_list, readRDS)
  
  # function to plot spatial field for each model
  preds_func <- function(mod){
  	
  		# grab data
  		mod_dat <- mod$data
  		
  		# which environmental variable?
  		form <- mod$formula
	
  		var <- gsub(".*[()]([^,]+)[,].*", "\\1", form)
  		
  		# filter grid by variable and year
  		
  		mod_yrs <- sort(unique(mod_dat$year))
  			
  		mod_grid <- grid_full %>%
  			filter(year %in% mod_yrs) %>%
  			rename("{var}" := temp)
  		
  		pred_dat <- expand_grid(
  			age_f = sort(unique(mod_dat$age_f)),
  			year = sort(unique(mod_dat$year))
  		)
  		
  		pred_df <- left_join(pred_dat, mod_grid) %>% na.omit
  		
  		preds <- predict(mod, pred_df)
  		
  }
  
  preds_all_nspe <- purrr::map(nspe_mod_list, preds_func)
  
  
  # plot spatial and spatiotemporal fields
  plot_func_nspe <- function(df, mod_name){
  	
  	plot_s <-
  		plot_map(df, omega_s) +
  		scale_fill_gradient2(limits = c(-0.29, 0.13)) +
  		ggtitle(paste0("spatial field: ", mod_name, "_nsp_exp"))
   
  	plot_name_s <- paste0(mod_name, "_sp_field_nspe")

		ggsave(plot_s,
					 file = paste0(here(), "/plots/spatial fields/", plot_name_s, ".png"),
					 height = 5, width = 5, units = "in")
  
  	plot_st <-
  		plot_map(df, epsilon_st) +
  		scale_fill_gradient2(limits = c(-0.53, 0.31)) +
  		facet_wrap(~ year) +
  		ggtitle(paste0("spatioemporal field: ", mod_name, "_nsp_exp"))
  	
    plot_name_st <- paste0(mod_name, "_spt_field_nspe")

		ggsave(plot_st, 
					 file = paste0(here(), "/plots/spatiotemporal fields/", plot_name_st, ".png"),
					 height = 10, width = 15, units = "in")

  }
  
  mod_names <- names(nspe_mod_list)
  
  
  rm_func <- function(name){
  	
		mod_names <- name %>% str_remove(".rds")
	 
  }
  
  
  mod_names <- purrr::map(mod_names, rm_func)

  
  fdf <- tibble(
  	df = preds_all_nspe,
  	mod_name = mod_names
  )
  
	purrr::map2(fdf$df, fdf$mod_name, plot_func_nspe)
	