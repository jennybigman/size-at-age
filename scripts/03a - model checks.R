
# model comparison of sdmTMB mods

	## read in models ####
	
	# polynomial - spatially explicit ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/poly models/")))

	file_list <- stringr::str_subset(file_list, '.rds')
	
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/poly models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  poly_mod_list <- lapply(mod_names_list, readRDS)
  
  # remove '.rds' from names
  names(poly_mod_list) <- str_replace_all(names(poly_mod_list), '.rds', '')
  
  poly_2_mods <- poly_mod_list[grep("2", names(poly_mod_list))]
  poly_3_mods <- poly_mod_list[grep("3", names(poly_mod_list))]

  # check sanity 
  
  sanity_func <- function(x){
	 	 s <- sanity(x)
	 }
	 
		s <- lapply(poly_3_mods, sanity_func)
	
 	#### plot residuals #### 

	resid_fun <- function(mod){
		
		sims <- simulate(mod, nsim = 500, type = 'mle-mvn') 
		plot_df <- dharma_residuals(sims, mod, plot = FALSE)

	}
	
  poly_2_resids  <- purrr::map(poly_2_mods, resid_fun) 
  poly_3_resids  <- purrr::map(poly_3_mods, resid_fun) 

	# save these plots
	file_path_plots <- paste0(here(), "/output/plots/May 2024/residual plots for poly models/")
				 
	ggsave_fun <- function(df, name){
		
		p <- 
				ggplot(df) +
				geom_point(aes(x = expected, y = observed)) +
				geom_abline(intercept = 0, slope = 1, color = "red") +
			ggtitle(name)
			
			ggsave(filename = paste0(file_path_plots, name, ".png"),
						 plot = p,
						 width = 5, height = 5, units = "in")
	
	}
	
	purrr::map2(poly_2_resids, names(poly_2_resids), ggsave_fun)
	purrr::map2(poly_3_resids, names(poly_3_resids), ggsave_fun)

	#### they all look ok ####
	
	

	
	# gams - spatially explicit ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/smooth models/")))

	file_list <- stringr::str_subset(file_list, '.rds')
	
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/smooth models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  gam_mod_list <- lapply(mod_names_list, readRDS)
  
  # remove '.rds' from names
  names(gam_mod_list) <- str_replace_all(names(gam_mod_list), '.rds', '')
  
  ## check sanity 
  #
  #	sanity_func <- function(x){
	# 	 s <- sanity(x)
	# }
	# 
	#	s <- lapply(mod_list, sanity_func)
	
  # some of these models have issues - check residuals ####
  
  ## pollock #
	#pol_mod_list <- gam_mod_list[grep("pol", names(gam_mod_list))]
#
	## pcod #
	#pcod_mod_list <- gam_mod_list[grep("pcod", names(gam_mod_list))]
	#	
	## yfin #
	#yfin_mod_list <- gam_mod_list[grep("yfin", names(gam_mod_list))]
	#
	## atooth #
	#atooth_mod_list <- gam_mod_list[grep("atooth", names(gam_mod_list))]
	#
	#
	##### plot residuals #### 
#
	#resid_fun <- function(mod){
	#	
	#sims <- simulate(mod, nsim = 500, type = 'mle-mvn') 
	#plot_df <- dharma_residuals(sims, mod, plot = FALSE)
#
	#}
		
#	pol_resids  <- purrr::map(pol_mod_list, resid_fun) #what's going on with pollock
#	pcod_resids <- purrr::map(pcod_mod_list, resid_fun)
#	yfin_resids <- purrr::map(yfin_mod_list, resid_fun)
#	atooth_resids <- purrr::map(atooth_mod_list, resid_fun)
	
	## save these plots
	#file_path_plots <- paste0(here(), "/output/plots/May 2024/residual plots for gam models/")
	#			 
	#ggsave_fun <- function(df, name){
	#	
	#	p <- 
	#			ggplot(df) +
	#			geom_point(aes(x = expected, y = observed)) +
	#			geom_abline(intercept = 0, slope = 1, color = "red") +
	#		ggtitle(name)
	#		
	#		ggsave(filename = paste0(file_path_plots, name, ".png"),
	#					 plot = p,
	#					 width = 5, height = 5, units = "in")
	#
	#}
	#
#	purrr::map2(pol_resids, names(pol_resids), ggsave_fun)
#	purrr::map2(pcod_resids, names(pcod_resids), ggsave_fun)
#	purrr::map2(yfin_resids, names(yfin_resids), ggsave_fun)
#	purrr::map2(atooth_resids, names(atooth_resids), ggsave_fun)

	#### they all look ok ####
	
	

	