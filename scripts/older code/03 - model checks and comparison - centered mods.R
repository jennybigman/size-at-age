# model comparison of sdmTMB mods

	## read in models ####
	
	# spatially explicit ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/C/")))

	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - NN/C/"))
  

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  C_NN_mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 s <- sanity(x)
  }
  
	s <- lapply(C_NN_mod_list, sanity_func)
	
	
  # separate models by species #
  
	# pollock #
	Cnn_pol_mod_list <- C_NN_mod_list[grep("pol", names(C_NN_mod_list))]
	
	# pcod #
	Cnn_pcod_mod_list <- C_NN_mod_list[grep("pcod", names(C_NN_mod_list))]
		
	# yfin #
	Cnn_yfin_mod_list <- C_NN_mod_list[grep("yfin", names(C_NN_mod_list))]
	
	
	#### plot residuals ####
	
	
	
	
	## compare models with and without an interaction ##
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	sp_mod_lists_nn_C <- list(Cnn_pol_mod_list, Cnn_pcod_mod_list, Cnn_yfin_mod_list)

	vars <- dat_all %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = sp_mod_lists_nn_C,
		y = vars
	)

	C_NN_AICs <- map2(df_func$x, df_func$y, AIC_func)


# df to compare
 C_NN_AIC_df <-	C_NN_AICs %>% 
 	bind_cols() %>%
 	pivot_longer(cols = contains(c("temp", "oxy")),
 							 names_to = 'model', values_to = "AIC") %>%
 	rename(AIC_nn_c = AIC)
 
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
 	
 	mod <- df$model[df$AIC_nn == min(df$AIC_nn)]
 }
 
 lowest_AIC <- sapply(AIC_list, lowest_AIC_func) %>% as_tibble()
 
 write_csv(lowest_AIC, file = "./data/AICs_nn.csv")
 
 
 #### non spatially explicit but averaged over grid using NN ####
 
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - SP_AVG_NN/C/")))

	file_list <- stringr::str_subset(file_list, '.rds')
	
	file_list <- stringr::str_subset(file_list, "yrprior_btemp_int_mod_yfin.rds", negate = TRUE)

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/Feb 2024 - SP_AVG_NN/C/"))
  

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  SPAVG_mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 s <- sanity(x)
  }
  
	s <- lapply(SPAVG_mod_list, sanity_func)
	
	
  # separate models by species #
  
	# pollock #
	spavg_pol_mod_list <- SPAVG_mod_list[grep("pol", names(SPAVG_mod_list))]
	
	# pcod #
	spavg_pcod_mod_list <- SPAVG_mod_list[grep("pcod", names(SPAVG_mod_list))]
		
	# yfin #
	spavg_yfin_mod_list <- SPAVG_mod_list[grep("yfin", names(SPAVG_mod_list))]
	
		
	## compare models with and without an interaction ##
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	spavg_mod_lists <- list(spavg_pol_mod_list, spavg_pcod_mod_list, spavg_yfin_mod_list)

	vars <- dat_all %>%
		ungroup() %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		x = spavg_mod_lists,
		y = vars
	)

	SPAVG_AICs <- map2(df_func$x, df_func$y, AIC_func)


	# df to compare
	SPAVG_AIC_df <-	SPAVG_AICs %>% 
 		bind_cols() %>%
 		pivot_longer(cols = contains(c("temp", "oxy")),
 							   names_to = 'model', values_to = "AIC") %>%
 		rename(AIC_spavg = AIC)
 
	all_mod_names <- NN_AIC_df$model
	
	mods_spavg <- SPAVG_AIC_df$model
	
	model <- setdiff(all_mod_names, mods_spavg)
	
	missing_row <- data.frame(model, AIC_spavg = NA)
	
	SPAVG_AIC_df <- bind_rows(SPAVG_AIC_df, missing_row)
	
	# model won't converge so add NA
	SPAVG_AIC_df$AIC_spavg[SPAVG_AIC_df$model == "yrprior_boxy_int_mod_yfin.rds"] <- NA
	

	#### compare spatially explicit and spatially averaged models ####
	
	all_AICs <- left_join(NN_AIC_df, SPAVG_AIC_df)
	
	
	
##########################
 mod_split_func <- function(species){
 	
 	species_AICs <- SPAVG_AIC_df %>%
 		filter(str_detect(model, species))
 
 	temp_sp_AIC <- species_AICs %>% 
 		filter(str_detect(model,"temp"))
 	
 	oxy_sp_AIC <- species_AICs %>% 
 		filter(str_detect(model, "oxy"))
 	
 	sp_AIC <- list(temp_sp_AIC, oxy_sp_AIC)
 	
 }
 
 names <- unique(dat_all$short_name)
 
 spavg_AIC_dfs <- lapply(names, mod_split_func) 
 
 pol_comp_temp <-  spavg_AIC_dfs[[1]][1] %>% bind_rows()
 pol_comp_oxy  <-  spavg_AIC_dfs[[1]][2] %>% bind_rows()
 pcod_comp_temp <- spavg_AIC_dfs[[2]][1] %>% bind_rows()
 pcod_comp_oxy <-  spavg_AIC_dfs[[2]][2] %>% bind_rows()
 yfin_comp_temp <- spavg_AIC_dfs[[3]][1] %>% bind_rows()
 yfin_comp_oxy <-  spavg_AIC_dfs[[3]][2] %>% bind_rows()
 
 AIC_list <- list(
 	pol_comp_temp, pol_comp_oxy,
	pcod_comp_temp, pcod_comp_oxy,
	yfin_comp_temp, yfin_comp_oxy)

 # extract the model with the lowest AIC from each
 lowest_AIC_func <- function(df){
 	
 	df <- na.omit(df)
 	
 	mod <- df$model[df$AIC_spavg == min(df$AIC_spavg)]
 }
 
 lowest_AIC_spavg <- sapply(AIC_list, lowest_AIC_func) %>% as_tibble()
 
 write_csv(lowest_AIC_spavg, file = "./data/AICs_spavg.csv")
 
 