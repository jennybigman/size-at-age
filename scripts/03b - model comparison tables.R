# quick model comparison

	# Polynomial models ####
	
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

	kbl(poly_AIC_sum_2vs3_table, digits = 2) %>%
		kable_classic(full_width = TRUE) %>%
		save_kable(file = "output/tables/poly_mod_comparison.png")
	
	top_poly_mods <- poly_AIC_sum_2vs3 %>%
		group_by(species, model_short) %>%
		slice_min(order_by = AIC) %>%
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
	
	glimpse(top_poly_mods)
		
#	kbl(top_poly_mods, digits = 2) %>%
#		kable_minimal() %>%
#		save_kable(file = "output/tables/top_poly_mods.png")

	# GAM models ####
	
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
	
	# calculate AICs
	AIC_func <- function(x){
		
		mods <- gam_mod_list[grep(x, names(gam_mod_list))]
		AIC_list <- lapply(mods, AIC)
	
	}
	
	sp <- unique(dat_all$short_name)

	gam_AICs <- purrr::map(sp, AIC_func)
	
	# make nice tables
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
		mutate(mod_type = "GAM") %>%
		mutate(mod_param = case_when(
					 	!grepl("no", model) ~ "int",
					 	grepl("no", model) ~ "no_int"
					 )) %>%
		select(-model)
	
	gam_int_vs_no_int_table <- AIC_df_messy_gam %>%	
		mutate(
			"Environmental variable" = case_when(
				model_short == "presurvey_boxy" ~ "oxygen - presurvey",
				model_short == "presurvey_btemp" ~ "temperature - presurvey",
				model_short == "yrprior_boxy" ~ "oxygen - year prior",
				model_short == "yrprior_btemp" ~ "temperature - year prior"),
			"Interaction?" = case_when(
				mod_param == "int" ~ "yes",
				mod_param == "no_int" ~ "no"),
			"Species name" = case_when(
				species == "atooth" ~ "arrowtooth flounder",
				species == "pcod" ~ "Pacific cod",
				species == "pollock" ~ "Walleye pollock",
				species == "yfin" ~ "yellowfin flounder")
			)	%>%
		ungroup() %>%
		select(-model_short, -mod_param, -species, - mod_type) %>%
		select(-AIC, everything()) %>%
		select('Species name', everything())
	
	kbl(gam_int_vs_no_int_table, digits = 2) %>%
			kable_minimal() %>%
			save_kable(file = "output/tables/gam_mod_comparison.png")

	top_gam_mods <- AIC_df_messy_gam %>%
		group_by(species, model_short) %>%
		slice_min(order_by = AIC)
	
	top_gam_mods_table <- top_gam_mods %>%
		mutate(
			"Environmental variable" = case_when(
				model_short == "presurvey_boxy" ~ "oxygen - presurvey",
				model_short == "presurvey_btemp" ~ "temperature - presurvey",
				model_short == "yrprior_boxy" ~ "oxygen - year prior",
				model_short == "yrprior_btemp" ~ "temperature - year prior"),
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
			select(-AIC, everything()) %>%
			select('Species name', everything()) %>%
		rename("Model type" = mod_type)
	
	glimpse(top_gam_mods_table)
		
	kbl(top_gam_mods_table, digits = 2) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/top_gam_mods.png")
	
	#### Compare polynomial & gam models ####
	
	top_poly_mods_comp <- top_poly_mods %>%
		rename(AIC_poly = AIC) %>%
		select(-`Polynomial order`, -`Interaction?`)
	
	top_gam_mods_comp <- top_gam_mods_table %>%
		rename(AIC_gam = AIC) %>%
		select(-`Model type`, -`Interaction?`)

	all_top_mods <- left_join(top_gam_mods_comp, top_poly_mods_comp)
	
			
	kbl(all_top_mods, digits = 2) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/top_mods_comp.png")
	
	### so need to plot models from top_poly_mods
	
	# find which way of summarizing the environmental variable is more supported
	
	top_poly_mods <- poly_AIC_sum_2vs3 %>%
		group_by(species, model_short) %>%
		slice_min(order_by = AIC)
		
	top_mods <- top_poly_mods %>%
		mutate(
			envr_var = case_when(
				model_short == "presurvey_boxy" ~ "oxygen",
				model_short == "presurvey_btemp" ~ "temperature",
				model_short == "yrprior_boxy" ~ "oxygen",
				model_short == "yrprior_btemp" ~ "temperature"),
			sum_type = case_when(
				model_short == "presurvey_boxy" ~ "pre-survey",
				model_short == "presurvey_btemp" ~ "pre-survey",
				model_short == "yrprior_boxy" ~ "yrprior",
				model_short == "yrprior_btemp" ~ "yrprior"
			)) %>%
		ungroup() %>%
		select(-model_short)
		
	
	top_mods_var <- top_mods %>%
		group_by(species, envr_var) %>%
		slice_min(order_by = AIC)
	
	top_mods_sp <- top_mods %>%
		group_by(species) %>%
		slice_min(order_by = AIC)
	
	kbl(top_mods_sp, digits = 2) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/top_mods_sp.png")
	
	
	# plot all models from top_mods_var
	
	top_mods_list <- top_mods_var %>% 
		ungroup() %>%
		select(model) %>%
		mutate(model = paste0(model, ".rds"))
	
	write_csv(top_mods_list, file = "./data/top_mods_list.csv")
		