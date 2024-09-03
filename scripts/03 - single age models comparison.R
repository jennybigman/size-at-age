 # comparing models with separate age classes and time-varying intercepts

	file_path <- "/output/model output/sdmTMB output/August 2024 TV single age models/"

	## read in models ####
	file_list <- list.files(path = paste0(here(), file_path))

	file_list <- stringr::str_subset(file_list, '.rds')
	
  prestring <- paste0(here(), file_path)

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # remove models that did not converge
  #drop <- which(mod_list %like% 'Error')
  #
  #drop <- as.numeric(drop)
  #
  #mod_list_trim <- mod_list[-drop]
  #
	
  names_mods <- names(mod_list)
	
	# remove models that did not pass sanity checks
	sanity_fun <- function(num, name) {
		
		mod <- mod_list[[num]]
		
		s <- sanity(mod)
		
			hess    <- s$hessian_ok 
			eigen   <- s$eigen_values_ok            
			nl      <- s$nlminb_ok       		         
			range 	<- s$range_ok       
			#se_na 	<- s$se_na_ok         
			sigma 	<- s$sigmas_ok        
		
	
			#df <- tibble(name, hess, eigen, nl, range, se_na, sigma)
			df <- tibble(name, hess, eigen, nl, range, sigma)

			df
		
}

	num = 1:length(mod_list)
	
	fdf <- tibble(
		num = num,
		name = names_mods
	)

	mod_df <- map2(fdf$num, fdf$name, sanity_fun) %>% bind_rows()	 	
	
	mod_df_drop <- mod_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- mod_df_drop$name

	mod_list_keep <- mod_df %>% 
		filter(name %!in% drop)
	
	mod_list_keep <- mod_list_keep$name

	#saveRDS(mod_list_keep, file = paste0(here(), file_path, "single_age_mod_converged_list.rda"))
	
	### read in models that did converge ####
	#mod_list_keep <- readRDS(file = paste0(here(), file_path, "single_age_mod_converged_list.rda"))

	no_cov_mods <- stringr::str_subset(mod_list_keep, 'no')
		
	temp_mods <- stringr::str_subset(mod_list_keep, 'temp')
	temp_mods <- stringr::str_subset(temp_mods, 'no', negate = TRUE)

	oxy_mods <- stringr::str_subset(mod_list_keep, 'oxy')
	
	#### read in mods ####
	
	prestring <- paste0(here(), file_path)

	# no cov mods
  mod_names_list <- list()
  
  for(i in no_cov_mods){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
	no_cov_mods <- lapply(mod_names_list, readRDS)
	
	# remove '.rds' from names
  names(no_cov_mods) <- str_replace_all(names(no_cov_mods), '.rds', '')
 
  # temp mods
  mod_names_list <- list()
  
  for(i in temp_mods){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
	temp_mods <- lapply(mod_names_list, readRDS)
	
	# remove '.rds' from names
  names(temp_mods) <- str_replace_all(names(temp_mods), '.rds', '')

  # oxy mods
  mod_names_list <- list()
  
  for(i in oxy_mods){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
	oxy_mods <- lapply(mod_names_list, readRDS)
	
	# remove '.rds' from names
  names(oxy_mods) <- str_replace_all(names(oxy_mods), '.rds', '')

  #### AICS ####
  
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
	#ages <- str_remove(ages, pattern = "\\_yrprior.*")

	oxy_mod_AICs$age <- ages
	
	oxy_mod_AICs <- oxy_mod_AICs %>% select(-model)

	# bind together to compare
	
	temp_mod_comp_s <- bind_rows(temp_mod_AICs, no_cov_mod_AICs) %>%
		select(-cov) %>%
		pivot_wider(
			names_from = model_type,
			values_from = AIC
		) %>%
		mutate(age = as.numeric(age)) %>%
		arrange(species, age)
	
	oxy_mod_comp_s <- bind_rows(oxy_mod_AICs, no_cov_mod_AICs) %>%
		select(-cov) %>%
		pivot_wider(
			names_from = model_type,
			values_from = AIC
		) %>%
		mutate(age = as.numeric(age)) %>%
		arrange(species, age)
	
	
	kbl(temp_mod_comp, digits = 3) %>%
		kable_classic(full_width = TRUE) %>%
		save_kable(file = "output/tables/single_age_temp_mods_comparison.png")

	
	kbl(oxy_mod_comp, digits = 3) %>%
		kable_classic(full_width = TRUE) %>%
		save_kable(file = "output/tables/single_age_temp_mods_comparison.png")

	# are gams and poly 3 equally supported?
	test <- temp_mod_comp %>%
		mutate(diff = gam - poly3)
	# no
	
	# compare mods
	
	# for all models with a diff in AIC of > 2, which is best
	
	test <- temp_mod_comp %>%
		rowwise() %>%
		mutate(
			min_AIC = min(c(gam, lin, poly2, poly3, no_cov)),
			max_AIC = max(c(gam, lin, poly2, poly3, no_cov)),
			max_diff = min_AIC - max_AIC)
	
	
	
	temp_mod_comp <- temp_mod_comp %>%
		mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
		mutate(top_mod = case_when(
			no_cov < gam & no_cov < lin & no_cov < poly2 & no_cov < poly3 ~ "no_cov",
			gam < no_cov & gam < lin & gam < poly2 & gam < poly3  ~ "gam",
			lin < no_cov & lin < gam & lin < poly2 & lin < poly3  ~ "lin",
			poly2 < no_cov & poly2 < lin & poly2 < gam & poly2 < poly3 ~ "poly2",
			poly3 < no_cov & poly3 < lin & poly3 < gam & poly3 < poly2 ~ "poly3"
		))
	
	#temp_mod_comp_sum <- bind_rows(temp_mod_AICs, no_cov_mod_AICs) %>%
	#	group_by(species, age) %>%
	#	summarise(max_AIC = max(AIC),
	#					  min_AIC = min(AIC)) %>%
	#	mutate(max_diff = max_AIC - min_AIC)
	
	test2 <- oxy_mod_comp %>%
		rowwise() %>%
		mutate(
			min_AIC = min(c(gam, lin, poly2, poly3, no_cov)),
			max_AIC = max(c(gam, lin, poly2, poly3, no_cov)),
			max_diff = min_AIC - max_AIC)

	oxy_mod_comp <- oxy_mod_comp %>%
		mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
		mutate(top_mod = case_when(
			no_cov < gam & no_cov < lin & no_cov < poly2 & no_cov < poly3 ~ "no_cov",
			gam < no_cov & gam < lin & gam < poly2 & gam < poly3  ~ "gam",
			lin < no_cov & lin < gam & lin < poly2 & lin < poly3  ~ "lin",
			poly2 < no_cov & poly2 < lin & poly2 < gam & poly2 < poly3 ~ "poly2",
			poly3 < no_cov & poly3 < lin & poly3 < gam & poly3 < poly2 ~ "poly3"
		))
	
	# retain only species and ages where temp explains size-at-age
	temp_mods <- temp_mod_comp %>%
		filter(top_mod != "no_cov") 
	
	# is the top model (with temp) > 2 AIC units better than without temp
	temp_mods_plot <- temp_mods %>%
		mutate(no_cov_AIC = no_cov) %>%
		rowwise() %>%
		mutate(min_AIC = min(c(gam, lin, poly2, poly3)),
					 diff = no_cov_AIC - min_AIC) %>%
		filter(diff > 2)
	
	temp_top_mods <- temp_mods_plot %>%
		select(species, age, top_mod)

	temp_top_mods <- paste(temp_top_mods$species, temp_top_mods$age, temp_top_mods$top_mod, sep = "_")		
		
	saveRDS(temp_top_mods, file = paste0(here(), file_path, "temp_top_mods.rda"))

  