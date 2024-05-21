
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
  
  ## check sanity 
  #
  #	sanity_func <- function(x){
	# 	 s <- sanity(x)
	# }
	# 
	#	s <- lapply(mod_list, sanity_func)
	
  # calculate AICs
	AIC_func <- function(x){
		
		mods <- poly_mod_list[grep(x, names(poly_mod_list))]
		AIC_list <- lapply(mods, AIC)
	
	}
	
	sp <- unique(dat_all$short_name)

	poly_AICs <- purrr::map(sp, AIC_func)
	
	AIC_df_messy_poly <-	poly_AICs %>% 
 		bind_cols() %>%
 		pivot_longer(cols = contains(c("temp", "oxy")),
 								 names_to = 'model', values_to = "AIC") %>%
		mutate(model_short = 
						case_when(grepl("presurvey_btemp", model) ~ "presurvey_btemp",
								 		  grepl("yrprior_btemp", model)   ~ "yrprior_btemp",
								 		  grepl("presurvey_boxy", model)  ~ "presurvey_boxy",
								 		  grepl("yrprior_boxy", model)    ~ "yrprior_boxy")) %>%
		mutate(species =
					 	case_when(grepl("pcod", model) ~ "pcod",
					 						grepl("atooth", model) ~ "atooth",
					 						grepl("yfin", model) ~ "yfin",
					 						grepl("pollock", model) ~ "pollock")) %>%
		mutate(poly_type = 
					 	case_when(grepl("2", model) ~ "2",
					 						grepl("3", model) ~ "3"))
	
	AIC_df_messy_poly2 <- AIC_df_messy_poly %>%
		filter(grepl("2", model)) %>%
		mutate(mod_type = "poly2",
					 mod_param = case_when(
					 	!grepl("no", model) ~ "int",
					 	grepl("no", model) ~ "no_int"
					 )) %>%
		select(-model)
	
		
	AIC_df_messy_poly3 <- AIC_df_messy_poly %>%
		filter(grepl("3", model)) %>%
		mutate(mod_type = "poly3",
					 mod_param = case_when(
					 	!grepl("no", model) ~ "int",
					 	grepl("no", model) ~ "no_int"
					 )) %>%
		select(-model)
	
	AIC_df_messy_poly <- bind_rows(AIC_df_messy_poly2, AIC_df_messy_poly3)

	top_poly_mods <- AIC_df_messy_poly %>%
		group_by(species, model_short) %>%
		slice_min(order_by = AIC)
	
 
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
	##### plot residuals #### does not work anymore as of April 2024
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
	
	
	# calculate AICs
	AIC_func <- function(x){
		
		mods <- gam_mod_list[grep(x, names(gam_mod_list))]
		AIC_list <- lapply(mods, AIC)
	
	}
	
	sp <- unique(dat_all$short_name)

	gam_AICs <- purrr::map(sp, AIC_func)
	
	AIC_df_messy_gam <-	gam_AICs %>% 
 		bind_cols() %>%
 		pivot_longer(cols = contains(c("temp", "oxy")),
 								 names_to = 'model', values_to = "AIC") %>%
		mutate(model_short = 
						case_when(grepl("presurvey_btemp", model) ~ "presurvey_btemp",
								 		  grepl("yrprior_btemp", model)   ~ "yrprior_btemp",
								 		  grepl("presurvey_boxy", model)  ~ "presurvey_boxy",
								 		  grepl("yrprior_boxy", model)    ~ "yrprior_boxy")) %>%
		mutate(species =
					 	case_when(grepl("pcod", model) ~ "pcod",
					 						grepl("atooth", model) ~ "atooth",
					 						grepl("yfin", model) ~ "yfin",
					 						grepl("pollock", model) ~ "pollock")) %>%
		mutate(mod_type = "gam") %>%
		mutate(mod_param = case_when(
					 	!grepl("no", model) ~ "int",
					 	grepl("no", model) ~ "no_int"
					 )) %>%
		select(-model)
				 
	top_gam_mods <- AIC_df_messy_gam %>%
		group_by(species, model_short) %>%
		slice_min(order_by = AIC)
	
	############# compare ############
	
	top_poly_mods_all <- top_poly_mods %>%
		rename(AIC_poly = AIC) %>%
		select(-mod_type, -mod_param)
	
	top_gam_mods_all <- top_gam_mods %>%
		rename(AIC_gam = AIC) %>%
		select(-mod_type, - mod_param)
	
	top_mods <- full_join(top_poly_mods_all, top_gam_mods_all)
	
	### AIC is lower for poly models
	
	# lowest AIC
	
	top_mods_sp <- top_poly_mods %>%
		group_by(species) %>%
		slice_min(order_by = AIC)
		
		
	