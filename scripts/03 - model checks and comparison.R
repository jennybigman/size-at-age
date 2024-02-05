# model comparison of sdmTMB mods

	## read in models ####
	
	# non spatially explicit ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/")))

	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/"))
  

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

	AICs <- map2(df_func$x, df_func$y, AIC_func)


# df to compare
 AIC_df <-	AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains(c("temp", "oxy")),
 							 names_to = 'model', values_to = "AIC") 
 
 mod_split_func <- function(species){
 	
 	species_AICs <- AIC_df %>%
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
 	
 	mod <- df$model[df$AIC == min(df$AIC)]
 }
 
 lowest_AIC <- lapply(AIC_list, lowest_AIC_func)
 