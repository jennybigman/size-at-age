
# model comparison of sdmTMB mods

	## read in models ####
	
	# spatially explicit ####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/APR 2024 NEW/")))

	file_list <- stringr::str_subset(file_list, '.rds')
	
	# only keep yr prior models
	#keep_list <- file_list[grep("yrprior", file_list)]
	#file_list <- lubridate::intersect(file_list, keep_list)

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/APR 2024 NEW/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # remove '.rds' from names
  names(mod_list) <- str_replace_all(names(mod_list), '.rds', '')
  
  # check sanity 
  
  	sanity_func <- function(x){
	 	 s <- sanity(x)
	 }
	 
		s <- lapply(mod_list, sanity_func)
	
	
  # separate models by species #
  
	# pollock #
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]

	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
	
	# atooth #
	atooth_mod_list <- mod_list[grep("atooth", names(mod_list))]
	
	
	#### plot residuals #### does not work anymore as of April 2024

	resid_fun <- function(mod){
		
	sims <- simulate(mod, nsim = 500, type = 'mle-mvn') 
	plot_df <- dharma_residuals(sims, mod, plot = FALSE)

	}
		
	pol_resids  <- purrr::map(pol_mod_list, resid_fun) #what's going on with pollock
	pcod_resids <- purrr::map(pcod_mod_list, resid_fun)
	yfin_resids <- purrr::map(yfin_mod_list, resid_fun)
	atooth_resids <- purrr::map(atooth_mod_list, resid_fun)
	
	# save these plots
	file_path_plots <- paste0(here(), "/plots/April 2024 plots/")
				 
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
	
	purrr::map2(pol_resids, names(pol_resids), ggsave_fun)
	purrr::map2(pcod_resids, names(pcod_resids), ggsave_fun)
	purrr::map2(yfin_resids, names(yfin_resids), ggsave_fun)
	purrr::map2(atooth_resids, names(atooth_resids), ggsave_fun)

	## compare models with and without an interaction ##
	
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	sp_mod_lists <- list(pol_mod_list, pcod_mod_list, yfin_mod_list, atooth_mod_list)

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
 
 
 #### compare different variables and way of summarizing ####

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
 pcod_comp_oxy  <- sp_AIC_dfs[[2]][2] %>% bind_rows()

 yfin_comp_temp <- sp_AIC_dfs[[3]][1] %>% bind_rows()
 yfin_comp_oxy <- sp_AIC_dfs[[3]][2] %>% bind_rows()

 atooth_comp_temp <- sp_AIC_dfs[[4]][1] %>% bind_rows()
 atooth_comp_oxy <- sp_AIC_dfs[[4]][2] %>% bind_rows()
 
 AIC_list <- list(
 	atooth_comp_temp, atooth_comp_oxy,
 	pol_comp_temp, pol_comp_oxy,
	pcod_comp_temp, pcod_comp_oxy,
	yfin_comp_temp, yfin_comp_oxy)

 # extract the model with the lowest AIC from each
 lowest_AIC_func <- function(df){
 	
 	mod <- df$model[df$AIC == min(df$AIC)]
 }
 
 lowest_AIC <- sapply(AIC_list, lowest_AIC_func) %>% as_tibble()
 
 write_csv(lowest_AIC, file = "./data/AICs.csv")
 
 # which explains weight at age better - temp or oxygen?
 
 drop <- AIC_df$model[grep("no_int", AIC_df$model)]
 
 var_comp_df <- AIC_df %>% 
 	filter(model %!in% drop)
 
 # split by species
 sp_split_func <- function(species){
 	
 	species_AICs <- var_comp_df %>%
 		filter(str_detect(model, species))
 }
 
 names <- unique(dat_all$short_name)
 
 sp_comp_dfs <- lapply(names, sp_split_func) 

 pol_comp <- sp_comp_dfs[1] %>% bind_rows() #oxygen
 pcod_comp <- sp_comp_dfs[2] %>% bind_rows() #temp
 yfin_comp <- sp_comp_dfs[3] %>% bind_rows() #temp but no oxygen mod
 atooth_comp <- sp_comp_dfs[4] %>% bind_rows() #temp

 # lowest 
 lowest_AIC_fun <- function(df){
 
	mod <- df$model[df$AIC == min(df$AIC)]
	
	mod
	
 }
 
 top_mod_list <- purrr::map(list(pol_comp, pcod_comp, yfin_comp, atooth_comp), lowest_AIC_fun) %>% 
 	unlist() %>%
 	as_tibble() %>%
 	rename(mod = value)

 write.csv(top_mod_list, file = "./data/top_mod_list.csv")
 
 # for the models with presurvey temp/oxy that converged, which way of summarizing temp is better
 
 	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/APR 2024 NEW/")))

	file_list <- stringr::str_subset(file_list, '.rds')
	
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/APR 2024 NEW/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  	
  # separate models by species #
  
	# pollock #
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]

	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
	
	# atooth #
	atooth_mod_list <- mod_list[grep("atooth", names(mod_list))]
	
		
	AIC_func <- function(x, y){
		
		mods_list <- x[grep(y, names(x))]
		AIC_list <- lapply(mods_list, AIC)
	
	}
	
	sp_mod_lists <- list(pol_mod_list, pcod_mod_list, yfin_mod_list, atooth_mod_list)

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
 
 
 #### compare different variables and way of summarizing ####

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
 
 atooth_comp <- sp_AIC_dfs[[1]] %>% 
 	bind_rows() %>%
 	filter(!str_detect(model, "no_int"))
 
 pol_comp <- sp_AIC_dfs[[2]] %>% 
 	bind_rows() %>%
 	filter(!str_detect(model, "no_int"))
 
 pcod_comp <- sp_AIC_dfs[[3]] %>% 
 	bind_rows() %>%
 	filter(!str_detect(model, "no_int"))

 yfin_comp <- sp_AIC_dfs[[4]] %>% 
 	bind_rows() %>%
 	filter(!str_detect(model, "no_int"))

