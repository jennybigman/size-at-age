# quick model comparison

	file_path <- paste0(here(), "/output/model output/tinyVAST/sep age models/")

	# Polynomial models ####
	
	## read in models ####
	
	file_list <- list.files(path = file_path)
	
	
	no_cov_mods <- stringr::str_subset(file_list, 'no')
		
	temp_mods <- stringr::str_subset(file_list, 'temp')
	temp_mods <- stringr::str_subset(temp_mods, 'gam', negate = TRUE)

	oxy_mods <- stringr::str_subset(file_list, 'oxy')
	oxy_mods <- stringr::str_subset(oxy_mods, 'gam', negate = TRUE)


	#### read in mods ####
	
	# no cov mods
  mod_names_list <- list()
  
  for(i in no_cov_mods){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
	no_cov_mods <- lapply(mod_names_list, readRDS)
	
	# remove '.rds' from names
  names(no_cov_mods) <- str_replace_all(names(no_cov_mods), '.rds', '')
 
  # temp mods
  mod_names_list <- list()
  
  for(i in temp_mods){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
	temp_mods <- lapply(mod_names_list, readRDS)
	
	# remove '.rds' from names
  names(temp_mods) <- str_replace_all(names(temp_mods), '.rds', '')

  # oxy mods
  mod_names_list <- list()
  
  for(i in oxy_mods){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
	oxy_mods <- lapply(mod_names_list, readRDS)
	
	# remove '.rds' from names
  names(oxy_mods) <- str_replace_all(names(oxy_mods), '.rds', '')

	#### AICs ####
  
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

	
	
	
