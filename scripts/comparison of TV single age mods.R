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
	
	# none to remove
	
	### read in models that did converge ####

	#no_cov_mods <- stringr::str_subset(mod_list_keep, 'no')
	#	
	#temp_mods <- stringr::str_subset(mod_list_keep, 'temp')
	#temp_mods <- stringr::str_subset(temp_mods, 'no', negate = TRUE)
#
	#oxy_mods <- stringr::str_subset(mod_list_keep, 'oxy')
	

  #### AICS ####
  
  # no cov mods
  AIC_func <- function(x){
		
		mods <- mod_list[grep(x, names(mod_list))]
		AIC_list <- sapply(mods, AIC)
		
	
	}
	
	sp <- unique(dat_all$short_name)

	all_mod_AICs <- purrr::map(sp, AIC_func) %>% 
		flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
	
	all_mod_AICs$AIC <- unlist(all_mod_AICs$AIC)
	
	#### wrangle ####
	
	# no cov mods
	
	models <- all_mod_AICs$model
	model_type <- str_extract(models, "poly3|poly2|gam|lin|no_cov")
	species <-  str_extract(models, "atooth|pcod|yfin|pollock")
	cov <- str_extract(models, "yrprior_btemp|yrprior_boxy|no_cov")
	
	all_mod_AICs$model_type <- model_type
	all_mod_AICs$species <- species
	all_mod_AICs$cov <- cov

	ages <- sub(".*age_", "", models)
	ages <- str_remove(ages, pattern = "\\.rds.*")

	all_mod_AICs$age <- ages
	
	all_mod_AICs <- all_mod_AICs %>% select(-model)
	
	# to compare
	
	no_cov_mods <- all_mod_AICs %>% 
		filter(model_type == "no_cov")
	
	temp_mods <- all_mod_AICs %>%
		filter(cov == "yrprior_btemp")
	
	oxy_mods <- all_mod_AICs %>%
		filter(cov == "yrprior_boxy")
	
	temp_mod_comp <- bind_rows(temp_mods, no_cov_mods) %>%
		select(-cov) %>%
		pivot_wider(
			names_from = model_type,
			values_from = AIC
		) %>%
		mutate(age = as.numeric(age)) %>%
		arrange(species, age)
	
	oxy_mod_comp <- bind_rows(oxy_mods, no_cov_mods) %>%
		select(-cov) %>%
		pivot_wider(
			names_from = model_type,
			values_from = AIC
		) %>%
		mutate(age = as.numeric(age)) %>%
		arrange(species, age)
	
	#kbl(temp_mod_comp, digits = 3) %>%
	#	kable_classic(full_width = TRUE) %>%
	#	save_kable(file = "output/tables/single_age_temp_mods_comparison.png")
#
	#
	#kbl(oxy_mod_comp, digits = 3) %>%
	#	kable_classic(full_width = TRUE) %>%
	#	save_kable(file = "output/tables/single_age_temp_mods_comparison.png")

	# XXXXXX
	
	# retain only species and ages where temp explains size-at-age
	#temp_mods <- temp_mod_comp %>%
	#	filter(top_mod != "no_cov") 
	
	# is the top model (with temp) > 2 AIC units better than without temp
	temp_mod_comp <- temp_mod_comp %>%
		rowwise() %>%
		mutate(min_AIC = min(c(lin, poly2, poly3)), # add gam back in 
					 diff = no_cov - min_AIC) %>%
		filter(diff > 2)
	
	temp_top_mods <- temp_mods_plot %>%
		select(species, age, top_mod)

	temp_top_mods <- paste(temp_top_mods$species, temp_top_mods$age, temp_top_mods$top_mod, sep = "_")		
		
	saveRDS(temp_top_mods, file = paste0(here(), file_path, "temp_top_mods.rda"))

	
	
	
	
	
	
	
	
	
	
	
	
	
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
	

  