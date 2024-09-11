	file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
	file_list <- list.files(path = paste0(file_path))
	
	file_list <- stringr::str_subset(file_list, '.rds')

	#no_cov_mods <- stringr::str_subset(file_list, 'no')
	#poly2_mods_names <- stringr::str_subset(file_list, 'poly2')
	#poly3_mods <- stringr::str_subset(file_list, 'poly3')
	lin_mods <- stringr::str_subset(file_list, 'lin')

	
	mod_path_fun <- function(x){
		
		mod_path <- paste0(file_path, x)
		mod_path
	}
	
	#no_cov_mod_paths <- mod_path_fun(no_cov_mods)[-1]
	#poly2_mod_paths <- mod_path_fun(poly2_mods_names)
	#poly3_mod_paths <- mod_path_fun(poly3_mods)
	lin_mod_paths <- mod_path_fun(lin_mods)


  #no_cov_mods <- lapply(no_cov_mod_paths, readRDS)
	#poly2_mods <- lapply(poly2_mod_paths, readRDS)
	#poly3_mods <- lapply(poly3_mod_paths, readRDS)
	lin_mods <- lapply(lin_mod_paths, readRDS)

	
	# atooth
	  
	atooth_int_poly2_oxy_pars <- lin_mods[[1]]$internal$parlist$alpha_j 
	
	ints <- atooth_int_poly2_oxy_pars[1:11]
	poly2_1 <- atooth_int_poly2_oxy_pars[12]
	poly2_2 <- atooth_int_poly2_oxy_pars[13]
	poly2_1s <- atooth_int_poly2_oxy_pars[14:23]
	poly2_2s <- atooth_int_poly2_oxy_pars[24:33]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(10, 11, 12, 2:9)
	
	atooth_oxy_poly2 <- tibble(sp = rep("atooth", length(ints)), 
														 age, 
														 var = "yrprior_boxy", 
														 ints, 
														 poly2_1s, 
														 poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	 atooth_int_poly2_temp_pars <- poly2_mods[[3]]$internal$parlist$alpha_j 
	
	ints <-     atooth_int_poly2_temp_pars[1:11]
	poly2_1 <-  atooth_int_poly2_temp_pars[12]
	poly2_2 <-  atooth_int_poly2_temp_pars[13]
	poly2_1s <- atooth_int_poly2_temp_pars[14:23]
	poly2_2s <- atooth_int_poly2_temp_pars[24:33]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(10, 11, 12, 2:9)
	
	atooth_temp_poly2 <- tibble(sp = rep("atooth", length(ints)), age, var = "yrprior_btemp", ints, poly2_1s, poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	atooth_poly2_vars <- bind_rows(atooth_oxy_poly2, atooth_temp_poly2)
 
	atooth_poly2_vars$term[atooth_poly2_vars$term == "ints"] <- "intercept"
	atooth_poly2_vars$term[atooth_poly2_vars$term == "poly2_1s"] <- "poly2-1"
	atooth_poly2_vars$term[atooth_poly2_vars$term == "poly2_2s"] <- "poly2-2"

	
	atooth_par_comp <- left_join(atooth_pars_single, atooth_poly2_vars)
	
	atooth_par_comp$est_com <- round(atooth_par_comp$est_com, 3)
	
	atooth_par_comp <- atooth_par_comp %>%
		mutate(par_diff = abs(est_com - est_sm))
	