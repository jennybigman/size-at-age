# compare polynomial models with and without autoregressive ST effects & AR1 time-varying effect (on the mean weight at age)	

########################################################
# Polynomial models without AR effects ####
########################################################

	## read in models ####
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

  # calculate AICs
	AIC_func <- function(x){
		

		mods <- poly_mod_list[grep(x, names(poly_mod_list))]
		AIC_list <- lapply(mods, AIC)
	
	}
	
	sp <- unique(dat_all$short_name)

	poly_AICs <- purrr::map(sp, AIC_func)

	# make nice tables
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
					 						grepl("3", model) ~ "3")) %>%
		mutate(mod_param = case_when(
					 	!grepl("no", model) ~ "int",
					 	grepl("no", model) ~ "no_int"))
	
	poly_AIC_sum_2vs3 <- AIC_df_messy_poly %>%
		group_by(species, model_short, mod_param) %>%
		slice_min(order_by = AIC)
	
	poly_AIC_sum_2vs3_table <- poly_AIC_sum_2vs3 %>%
		select(-model) %>%
		mutate(
			"Environmental variable" = case_when(
				model_short == "presurvey_boxy" ~ "oxygen - presurvey ",
				model_short == "presurvey_btemp" ~ "temperature - presurvey ",
				model_short == "yrprior_boxy" ~ "oxygen - year prior ",
				model_short == "yrprior_btemp" ~ "temperature - year prior "),
			"Interaction?" = case_when(
				mod_param == "int" ~ "yes",
				mod_param == "no_int" ~ "no"),
			"Species name" = case_when(
				species == "atooth" ~ "arrowtooth flounder",
				species == "pcod" ~ "Pacific cod",
				species == "pollock" ~ "Walleye pollock",
				species == "yfin" ~ "yellowfin flounder")
			) %>%
		ungroup() %>%
		select(-model_short, -mod_param, -species) %>%
		select(-poly_type, everything()) %>%
		select(-AIC, everything()) %>%
		rename("Polynomial order" = poly_type) %>%
		select('Species name', everything())
		
	glimpse(poly_AIC_sum_2vs3_table)

	########################################################
	# Polynomial models with AR effects ####
	########################################################

	## read in models ####
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/poly models/AR1/ST_TVE/")))

	file_list <- stringr::str_subset(file_list, '.rds')
	
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/poly models/AR1/ST_TVE/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  poly_ar_mod_list <- lapply(mod_names_list, readRDS)
 
 # remove '.rds' from names
  names(poly_ar_mod_list) <- str_replace_all(names(poly_ar_mod_list), '.rds', '')

  # calculate AICs
	AIC_func <- function(x){
		
		mods <- poly_ar_mod_list[grep(x, names(poly_ar_mod_list))]
		AIC_list <- lapply(mods, AIC)
	
	}
	
	sp <- unique(dat_all$short_name)

	poly_ar_AICs <- purrr::map(sp, AIC_func)

	# make nice tables
	AIC_df_messy_poly_ar <-	poly_ar_AICs %>% 
 		bind_cols() %>%
 		pivot_longer(cols = contains(c("temp", "oxy")),
 								 names_to = 'model', values_to = "AIC_ar") %>%
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
					 						grepl("3", model) ~ "3")) %>%
		mutate(mod_param = case_when(
					 	!grepl("no", model) ~ "int",
					 	grepl("no", model) ~ "no_int"))
	
	
	AIC_df_messy_poly_ar <- AIC_df_messy_poly_ar %>% select(-model)
	AIC_df_messy_poly <- AIC_df_messy_poly %>% select(-model)
	
	AIC_comp_poly_AR <- left_join(AIC_df_messy_poly, AIC_df_messy_poly_ar)
	
	AIC_comp_poly_AR$diff <- AIC_comp_poly_AR$AIC - AIC_comp_poly_AR$AIC_ar
	
	
	