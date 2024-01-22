# model comparison of sdmTMB mods

	## read in models ####
	
	# non spatially explicit ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/")))

	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/"))
  

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 sanity(x)
  }
  
	s <- lapply(mod_list, sanity_func)
	
	
  # separate models by species #
  
	# pollock #
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
	
		
	## compare models with and without an interaction ##
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	
	sp_mod_lists <- list(pol_mod_list, pcod_mod_list, yfin_mod_list)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = sp_mod_lists,
		y = vars
	)

	npe_AICs <- map2(df_func$x, df_func$y, AIC_func)

	# which temp and which oxygen metric? - yr prior temp & oxygen better than presurvey for all species
	# oxygen or temp? temp better predictor of changes in size at age for all three species
	
	
	# spatially explicit ####

	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/spatially_exp_temp/")))

  prestring <- paste0(here(), ("/output/model output/sdmTMB output//Jan 2024/spatially_exp_temp/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 sanity(x)
  }
  
	s <- lapply(mod_list, sanity_func)
	
	
  # separate models by species ####
  
	# pollock #
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
	
		
	#### compare models with and without an interaction ####
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	
	sp_mod_lists <- list(pol_mod_list, pcod_mod_list, yfin_mod_list)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = sp_mod_lists,
		y = vars
	)

	AICs <- map2(df_func$x, df_func$y, AIC_func)

	# which temp and which oxygen metric? - yr prior temp & oxygen better than presurvey for all species
	# oxygen or temp? temp better predictor of changes in size at age for all three species
	
	#### survey rep models ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/roms_survey_rep/")))

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Jan 2024/roms_survey_rep/"))
  

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 sanity(x)
  }
  
	s <- lapply(mod_list, sanity_func)
	
	
  # separate models by species ####
  
	# pollock #
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
	
		
	#### compare models with and without an interaction ####
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	
	sp_mod_lists <- list(pol_mod_list, pcod_mod_list, yfin_mod_list)
	
	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = sp_mod_lists,
		y = vars
	)

	npe_AICs <- map2(df_func$x, df_func$y, AIC_func)

# for spatially explicit models
 sp_exp_AICs <-	AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains("temp"), names_to = 'model', values_to = "AIC_sp_exp") 
 
 sp_exp_AICs <- sp_exp_AICs[grepl("rds", sp_exp_AICs$model), ]
 
 non_sp_exp_AICs <-	npe_AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains("temp"), names_to = 'model', values_to = "AIC_nsp_exp")

 AIC_comp <- left_join(non_sp_exp_AICs, sp_exp_AICs)
 