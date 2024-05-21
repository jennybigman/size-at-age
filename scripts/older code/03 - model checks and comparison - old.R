# model comparison of sdmTMB mods

	## read in models ####
	
	# spatially explicit ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/")))

	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/"))
  

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  NN_mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 sanity(x)
  }
  
	s <- lapply(mod_list, sanity_func)
	
	
  # separate models by species #
  
	# pollock #
	nn_pol_mod_list <- NN_mod_list[grep("pol", names(NN_mod_list))]
	
	# pcod #
	nn_pcod_mod_list <- NN_mod_list[grep("pcod", names(NN_mod_list))]
		
	# yfin #
	nn_yfin_mod_list <- NN_mod_list[grep("yfin", names(NN_mod_list))]
	
		
	## compare models with and without an interaction ##
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	sp_mod_lists_nn <- list(nn_pol_mod_list, nn_pcod_mod_list, nn_yfin_mod_list)

	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = sp_mod_lists_nn,
		y = vars
	)

	NN_AICs <- map2(df_func$x, df_func$y, AIC_func)


# df to compare
 NN_AIC_df <-	NN_AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains(c("temp", "oxy")),
 							 names_to = 'model', values_to = "AIC") %>%
 	rename(AIC_nn = AIC)
 
 mod_split_func <- function(species){
 	
 	species_AICs <- NN_AIC_df %>%
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
 
 lowest_AIC <- sapply(AIC_list, lowest_AIC_func) %>% as_tibble()
 
 write_csv(lowest_AIC, file = "./data/AICs_nn.csv")
 