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

	vars <- c("presurvey_btemp", "yrprior_btemp")

	df_func <- expand_grid(
		x = sp_mod_lists,
		y = vars
	)

	nspe_AICs <- map2(df_func$x, df_func$y, AIC_func)

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
	
	vars <- c("presurvey_btemp", "yrprior_btemp")

	df_func <- expand_grid(
		x = sp_mod_lists,
		y = vars
	)

	spe_AICs <- map2(df_func$x, df_func$y, AIC_func)

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
	
	vars <- "yr_btemp"


	df_func <- expand_grid(
		x = sp_mod_lists,
		y = vars
	)

	rsr_AICs <- map2(df_func$x, df_func$y, AIC_func)

# for spatially explicit models
 spe_AICs <-	spe_AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains("temp"), names_to = 'model', values_to = "AIC_sp_exp") 
 
 spe_AICs <- spe_AICs[grepl("rds", spe_AICs$model), ]
 
 # non spatially explicit
 nspe_AICs <-	nspe_AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains("temp"), names_to = 'model', values_to = "AIC_nsp_exp")

 nspe_AICs <- nspe_AICs[grepl("rds", nspe_AICs$model), ]

 # roms survey replicated
 rsr_AICs_bind <-	rsr_AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains("temp"), names_to = 'model', values_to = "AIC_rsr")

 rsr_AICs_bind <- rsr_AICs_bind[grepl("rds", rsr_AICs_bind$model), ]

 rsr_AICs_yr <- rsr_AICs_bind
 rsr_AICs_yr$model <- gsub("yr_btemp", "yrprior_btemp", rsr_AICs_yr$model)
 rsr_AICs_yr$model <- gsub("_rsr", "", rsr_AICs_yr$model)

 rsr_AICs_pre <- rsr_AICs_bind
 rsr_AICs_pre$model <- gsub("yr_btemp", "presurvey_btemp", rsr_AICs_pre$model)
 rsr_AICs_pre$model <- gsub("_rsr", "", rsr_AICs_pre$model)

 
 rsr_AICs_mod <- bind_rows(rsr_AICs_yr, rsr_AICs_pre)

 # change yr btemp to yr prior just for comparison purposes
 
 AIC_comp <- left_join(nspe_AICs, spe_AICs)

 AIC_comp_all <- left_join(AIC_comp, rsr_AICs_mod) 
 