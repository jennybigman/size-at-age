	sp_dat_list <- dat_all %>% group_split(short_name)
	
	# file path
	file_path_all <- "/output/model output/sdmTMB output/May 2024/year models/"

	# function to run each model by age class
	sdmTMB_yr_mod_fun <- function(df){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# model without interaction 
		mod_yr <- 
			try(sdmTMB(log_wt ~ 0 + age_f * year_f,
				data = df,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
			  time = "year",
			  silent = FALSE))
					
		s <- sanity(mod_yr, gradient_thresh = 0.05)
	
		mod_name <- "_yr_f_mod_"
		
		sp <- unique(df$short_name)
		
		write_rds(mod_yr, 
			file = paste0(here(), file_path_all, sp, mod_name, ".rds"))
		 					
		
	}
	
	purrr::map(sp_dat_list, sdmTMB_yr_mod_fun)
	
	##### model output #####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/")))

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  yr_mod_list <- lapply(mod_names_list, readRDS)
  
	
  # sanity
  
  sanity_fun <- function(mod){
  	
  	s <- sanity(mod)
  	
  }
  
  s <- purrr::map(yr_mod_list, sanity_fun)
	
	# predictions
  
  pred_fun <- function(mod){
  	
		preds <- predict(mod, re_form = NA, se_fit =  TRUE) # take out re_form = NA
		
  }
  
  preds <- purrr::map(yr_mod_list, pred_fun) %>% bind_rows()
  
  preds_list <- preds %>% group_split(short_name)
  
  plot_path <- here("output", "plots", "May 2024", "year models")
  
  # plot
  pred_plot_fun <- function(df){
  	
  	sp <- unique(df$short_name)
  	
		p <- 
			ggplot(df) +
  		geom_point(aes(x = year, y = log_wt), color = "lightgrey", alpha = 0.8) +
			geom_ribbon(aes(x = year, ymin = est - est_se, ymax = est + est_se),
									color = "grey", alpha = 0.5) + 
  		geom_line(aes(x = year, y = est), color = "black") +
			facet_wrap(~ age_f, scales = "free") +
  		ggsidekick::theme_sleek()
	
	ggsave(p, file = paste0(plot_path, "/", sp, "_yr_mod_preds.png"))
  
  }
  
  purrr::map(preds_list, pred_plot_fun)
  

  
  
  
  