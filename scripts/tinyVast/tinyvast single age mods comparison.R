# quick model comparison

	file_path <- paste0(here(), "/output/model output/tinyVAST/sep age models/")
	file_path_AR <- paste0(here(), "/output/model output/tinyVAST/sep age models/AR term/")
	
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	# AIC function
	AIC_fun <- function(mod) {
  	
	  	AIC <- AIC(mod)
	  	AIC
  	
  }
  
		
	## read in models ####
	file_list <- list.files(path = file_path)
	file_list <- stringr::str_subset(file_list, ".rds")
	stacked_mods <- purrr::map2(file_path, file_list, read_mod_fun)

	file_list_AR <- list.files(path = file_path_AR)
	file_list_AR <- stringr::str_subset(file_list_AR, ".rds")
	AR_stacked_mods <- purrr::map2(file_path_AR, file_list_AR, read_mod_fun)


	#### AICs ####
	stacked_AICs <- purrr::map(stacked_mods, AIC_fun)
  stacked_AICs <- tibble(file_list, stacked_AICs) %>% rename(AIC = stacked_AICs)
  stacked_AICs$AIC <- unlist(stacked_AICs$AIC)
  
	AR_stacked_AICs <- purrr::map(AR_stacked_mods, AIC_fun)
  AR_stacked_AICs <- tibble(file_list_AR, AR_stacked_AICs) %>% rename(AIC_AR = AR_stacked_AICs)
  AR_stacked_AICs$AIC_AR <- unlist(AR_stacked_AICs$AIC_AR)
  AR_stacked_AICs <- AR_stacked_AICs %>%
  	rename(file_list = file_list_AR)
  
  stacked_comp <- left_join(stacked_AICs, AR_stacked_AICs, by = "file_list")
	
  stacked_comp <- stacked_comp %>% 
  	mutate(diff = AIC - AIC_AR)
	
	
  
   # no cov mods
  AIC_func <- function(x){
		
		mods <- no_cov_mods[grep(x, names(no_cov_mods))]
		AIC_list <- sapply(mods, AIC)
		
	
	}
	
	sp <- unique(dat_all$short_name)

	no_cov_mod_AICs <- purrr::map(sp, AIC_func) %>% 
		flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
	
	no_cov_mod_AICs$AIC <- unlist(no_cov_mod_AICs$AIC)

	# temp mods
  AIC_func <- function(x){
		
		mods <- temp_mods[grep(x, names(temp_mods))]
		AIC_list <- sapply(mods, AIC)
		
	
	}
	
	sp <- unique(dat_all$short_name)

	temp_mod_AICs <- purrr::map(sp, AIC_func) %>% 
		flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
	
	temp_mod_AICs$AIC <- unlist(temp_mod_AICs$AIC)
	
	# oxy mods
	
  AIC_func <- function(x){
		
		mods <- oxy_mods[grep(x, names(oxy_mods))]
		AIC_list <- sapply(mods, AIC)
		
	
	}
	
	sp <- unique(dat_all$short_name)

	oxy_mod_AICs <- purrr::map(sp, AIC_func) %>% 
		flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
	
	oxy_mod_AICs$AIC <- unlist(oxy_mod_AICs$AIC)
	
	
	#### wrangle ####
	
	# no cov mods
	
	models <- no_cov_mod_AICs$model
	model_type <- str_extract(models, "no_cov")
	species <-  str_extract(models, "atooth|pcod|yfin|pollock")
	cov <- NA
	
	no_cov_mod_AICs$model_type <- model_type
	no_cov_mod_AICs$species <- species
	no_cov_mod_AICs$cov <- cov

	ages <- sub(".*age_", "", models)
	ages <-  sub("_no_cov*", "", ages)

	no_cov_mod_AICs$age <- ages
	
	no_cov_mod_AICs <- no_cov_mod_AICs %>% select(-model)
	
	# temp
	
	models <- temp_mod_AICs$model
	model_type <- str_extract(models, "poly3|poly2|gam|lin|no_cov")
	species <-  str_extract(models, "atooth|pcod|yfin|pollock")
	cov <- "temp"
	
	temp_mod_AICs$model_type <- model_type
	temp_mod_AICs$species <- species
	temp_mod_AICs$cov <- cov

	ages <- sub(".*age_", "", models)
	temp_mod_AICs$age <- ages
	
	temp_mod_AICs <- temp_mod_AICs %>% select(-model)
	
	# oxy	
	models <- oxy_mod_AICs$model
	model_type <- str_extract(models, "poly3|poly2|gam|lin|no_cov")
	species <-  str_extract(models, "atooth|pcod|yfin|pollock")
	cov <- "oxy"
	
	oxy_mod_AICs$model_type <- model_type
	oxy_mod_AICs$species <- species
	oxy_mod_AICs$cov <- cov

	ages <- sub(".*age_", "", models)

	oxy_mod_AICs$age <- ages
	
	oxy_mod_AICs <- oxy_mod_AICs %>% select(-model)
	
	# bind together to compare
	
	temp_mod_comp <- bind_rows(temp_mod_AICs, no_cov_mod_AICs) %>%
		select(-cov) %>%
		pivot_wider(
			names_from = model_type,
			values_from = AIC
		) %>%
		mutate(age = as.numeric(age)) %>%
		arrange(species, age)
	
	oxy_mod_comp <- bind_rows(oxy_mod_AICs, no_cov_mod_AICs) %>%
		select(-cov) %>%
		pivot_wider(
			names_from = model_type,
			values_from = AIC
		) %>%
		mutate(age = as.numeric(age)) %>%
		arrange(species, age)
	

	kbl(oxy_mod_comp, digits = 2) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/oxy_mod_comp_sag_tvt.png")

	
	kbl(temp_mod_comp, digits = 2) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/temp_mod_comp_sag_tvt.png")

	
	######
	
	t <- temp_mod_AICs %>% 
		rename(AIC_temp = AIC) %>%
		select(-cov)
	
	o <- oxy_mod_AICs %>% 
		rename(AIC_oxy = AIC) %>%
		select(-cov)
	
	n <- no_cov_mod_AICs %>% 
		rename(AIC_no_cov = AIC) %>%
		select(-cov, - model_type)
	
	AIC_single_mods <- left_join(t, o) %>%
		left_join(., n) %>%
		pivot_wider(
			names_from = model_type,
			values_from = c(AIC_temp, AIC_oxy)
		) %>%
		mutate(age = as.numeric(age)) %>%
		arrange(species, age)
	
	sp_AICs_stacked <- AIC_single_mods %>%
		group_split(species)
	
	atooth_AICs <- sp_AICs_stacked[[1]]
	pcod_AICs <- sp_AICs_stacked[[2]]
	pol_AICs <- sp_AICs_stacked[[3]]
	yfin_AICs <- sp_AICs_stacked[[4]]
