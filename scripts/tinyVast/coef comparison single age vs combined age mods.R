# comparing coefficients between single age and combined age models

	# read in single age mods
	file_path <- paste0(here(), "/output/model output/tinyVAST/sep age models/")

	file_list <- list.files(path = file_path)
	
	# remove gam models
	file_list <- stringr::str_subset(file_list, 'gam', negate = TRUE)

	no_cov_mods <- stringr::str_subset(file_list, 'no')
	poly2_mods <- stringr::str_subset(file_list, 'poly2')
	poly3_mods <- stringr::str_subset(file_list, 'poly3')
	lin_mods <- stringr::str_subset(file_list, 'lin')

	#### read in mods ####
	
	mod_path_fun <- function(x){
		
		mod_path <- paste0(file_path, x)
		mod_path
	}
	
	no_cov_mod_paths <- mod_path_fun(no_cov_mods)
	poly2_mod_paths <- mod_path_fun(poly2_mods)
	poly3_mod_paths <- mod_path_fun(poly3_mods)
	lin_mod_paths <- mod_path_fun(lin_mods)


  no_cov_mods <- lapply(no_cov_mod_paths, readRDS)
	poly2_mods <- lapply(poly2_mod_paths, readRDS)
	poly3_mods <- lapply(poly3_mod_paths, readRDS)
	  lin_mods <- lapply(lin_mod_paths, readRDS)

	
	par_fun <- function(mod){
		
		d <- mod$data
		sp <- unique(d$short_name)
		age <- unique(d$age)
		formula <- mod$formula
		var <- gsub(".*[()]([^,]+)[,].*", "\\1", formula)[3]

		p <- mod$internal$parlist$alpha_j
		
		pars <- tibble(sp, age, var, p)
		
	}
	
	no_cov_pars <- purrr::map_dfr(no_cov_mods, par_fun) %>%
		mutate(mod_type = "no_cov",
					 cat = "single") %>%
		select(-var)
	
	lin_pars <- purrr::map_dfr(lin_mods, par_fun) %>%
		mutate(mod_type = "lin",
					 cat = "single",
					 term = rep(c("intercept", "slope"), times = nrow(lin_pars)/2))

	poly2_pars <- purrr::map_dfr(poly2_mods, par_fun) %>%
		mutate(mod_type = "poly2",
					 cat = "single",
					 term = rep(c("intercept", "poly2-1", "poly2-2"), times = nrow(poly2_pars)/3))

	poly3_pars <- purrr::map_dfr(poly3_mods, par_fun) %>%
		mutate(mod_type = "poly3",
					 cat = "single",
					 term = rep(c("intercept", "poly3-1", "poly3-2", "poly3-3"), times = nrow(poly3_pars)/4))

	output_path <- paste0(here(), "/output/model output/tinyVAST/par comps/")
	
	saveRDS(no_cov_pars, file = paste0(output_path, "no_cov_pars_single_age.rds"))
	saveRDS(lin_pars, file = paste0(output_path, "lin_pars_single_age.rds"))
	saveRDS(poly2_pars, file = paste0(output_path, "poly2_pars_single_age.rds"))
	saveRDS(poly3_pars, file = paste0(output_path, "poly3_pars_single_age.rds"))

	#no_cov_sm <- readRDS(file = paste0(output_path, "no_cov_pars_single_age.rds"))
	poly2_sm <- readRDS(file = paste0(output_path, "poly2_pars_single_age.rds")) 
	poly2_sm$p <- round(poly2_sm$p, 3)
	
	atooth_pars_single <- poly2_sm %>% filter(sp == "atooth") %>%
		rename(est_sm = p) %>%
		select(-cat, -mod_type)
	
	pcod_pars_single <- poly2_sm %>% filter(sp == "pcod") %>%
		rename(est_sm = p) %>%
		select(-cat, -mod_type)

		
	pol_pars_single <- poly2_sm %>% filter(sp == "pollock") %>%
		rename(est_sm = p) %>%
		select(-cat, -mod_type)

			
	yfin_pars_single <- poly2_sm %>% filter(sp == "yfin") %>%
		rename(est_sm = p) %>%
		select(-cat, -mod_type)


	# pars from combined models
	file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
	file_list <- list.files(path = paste0(file_path))
	
	file_list <- stringr::str_subset(file_list, '.rds')

	#no_cov_mods_names <- stringr::str_subset(file_list, 'no')
	poly2_mods_names <- stringr::str_subset(file_list, 'poly2')
	#poly3_mods_names <- stringr::str_subset(file_list, 'poly3')
#	lin_mods_names <- stringr::str_subset(file_list, 'lin')

	#### read in mods ####
	
	mod_path_fun <- function(x){
		
		mod_path <- paste0(file_path, x)
		mod_path
	}
	
	#no_cov_mod_paths <- mod_path_fun(no_cov_mods_names)[-1]
	poly2_mod_paths <- mod_path_fun(poly2_mods_names)
	#poly3_mod_paths <- mod_path_fun(poly3_mods_names)
	#lin_mod_paths <- mod_path_fun(lin_mods_names)


  #no_cov_mods <- lapply(no_cov_mod_paths, readRDS)
	poly2_mods <- lapply(poly2_mod_paths, readRDS)
	#poly3_mods <- lapply(poly3_mod_paths, readRDS)
	#  lin_mods <- lapply(lin_mod_paths, readRDS)

	
	# atooth
	  
	atooth_int_poly2_oxy_pars <- poly2_mods[[1]]$internal$parlist$alpha_j 
	
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
	
	  
		kbl(atooth_par_comp) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/atooth_par_comp_poly_tvt.png")
		
	
	# pcod ####
		

	pcod_int_poly2_oxy_pars <- poly2_mods[[5]]$internal$parlist$alpha_j 
	
	ints <-     pcod_int_poly2_oxy_pars[1:10]
	poly2_1 <-  pcod_int_poly2_oxy_pars[11]
	poly2_2 <-  pcod_int_poly2_oxy_pars[12]
	poly2_1s <- pcod_int_poly2_oxy_pars[13:21]
	poly2_2s <- pcod_int_poly2_oxy_pars[22:30]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(1, 10, 2:9)
	
	pcod_oxy_poly2 <- tibble(sp = rep("pcod", length(ints)), 
														 age, 
														 var = "yrprior_boxy", 
														 ints, 
														 poly2_1s, 
														 poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	pcod_int_poly2_temp_pars <- poly2_mods[[7]]$internal$parlist$alpha_j 
	
	ints <-     pcod_int_poly2_temp_pars[1:10]
	poly2_1 <-  pcod_int_poly2_temp_pars[11]
	poly2_2 <-  pcod_int_poly2_temp_pars[12]
	poly2_1s <- pcod_int_poly2_temp_pars[13:21]
	poly2_2s <- pcod_int_poly2_temp_pars[22:30]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(1, 10, 2:9)
	
	pcod_temp_poly2 <- tibble(sp = rep("pcod", length(ints)), age, var = "yrprior_btemp", ints, poly2_1s, poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	pcod_poly2_vars <- bind_rows(pcod_oxy_poly2, pcod_temp_poly2)
 
	pcod_poly2_vars$term[pcod_poly2_vars$term == "ints"] <- "intercept"
	pcod_poly2_vars$term[pcod_poly2_vars$term == "poly2_1s"] <- "poly2-1"
	pcod_poly2_vars$term[pcod_poly2_vars$term == "poly2_2s"] <- "poly2-2"

	pcod_par_comp <- left_join(pcod_pars_single, pcod_poly2_vars)
	
	pcod_par_comp$est_com <- round(pcod_par_comp$est_com, 3)
	
	pcod_par_comp <- pcod_par_comp %>%
		mutate(par_diff = abs(est_com - est_sm))
	
	  
	kbl(pcod_par_comp) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/pcod_par_comp_poly_tvt.png")


	# pollock 
	
	pol_int_poly2_oxy_pars <- poly2_mods[[9]]$internal$parlist$alpha_j 
	
	ints <-     pol_int_poly2_oxy_pars[1:20]
	poly2_1 <-  pol_int_poly2_oxy_pars[21]
	poly2_2 <-  pol_int_poly2_oxy_pars[22]
	poly2_1s <- pol_int_poly2_oxy_pars[23:41]
	poly2_2s <- pol_int_poly2_oxy_pars[42:60]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(1, 10:19, 2, 20, 3:9)
	
	pol_oxy_poly2 <- tibble(sp = rep("pollock", length(ints)), 
														 age, 
														 var = "yrprior_boxy", 
														 ints, 
														 poly2_1s, 
														 poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	pol_int_poly2_temp_pars <- poly2_mods[[11]]$internal$parlist$alpha_j 
	
	ints <-     pol_int_poly2_temp_pars[1:20]
	poly2_1 <-  pol_int_poly2_temp_pars[21]
	poly2_2 <-  pol_int_poly2_temp_pars[22]
	poly2_1s <- pol_int_poly2_temp_pars[23:41]
	poly2_2s <- pol_int_poly2_temp_pars[42:60]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(1, 10:19, 2, 20, 3:9)
	
	pol_temp_poly2 <- tibble(sp = rep("pollock", length(ints)), age, var = "yrprior_btemp", ints, poly2_1s, poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	pol_poly2_vars <- bind_rows(pol_oxy_poly2, pol_temp_poly2)
 
	pol_poly2_vars$term[pol_poly2_vars$term == "ints"] <- "intercept"
	pol_poly2_vars$term[pol_poly2_vars$term == "poly2_1s"] <- "poly2-1"
	pol_poly2_vars$term[pol_poly2_vars$term == "poly2_2s"] <- "poly2-2"

	pol_par_comp <- left_join(pol_pars_single, pol_poly2_vars)
	
	pol_par_comp$est_com <- round(pol_par_comp$est_com, 3)
	
	pol_par_comp <- pol_par_comp %>%
		mutate(par_diff = abs(est_com - est_sm))
	
	  
	kbl(pol_par_comp) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/pol_par_comp_poly_tvt.png")
	
	# yfin
	
	yfin_int_poly2_oxy_pars <- poly2_mods[[13]]$internal$parlist$alpha_j 
	
	ints <-     yfin_int_poly2_oxy_pars[1:28]
	poly2_1 <-  yfin_int_poly2_oxy_pars[29]
	poly2_2 <-  yfin_int_poly2_oxy_pars[30]
	poly2_1s <- yfin_int_poly2_oxy_pars[31:57]
	poly2_2s <- yfin_int_poly2_oxy_pars[58:84]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(10:29, 3, 30, 4:9)
	
	yfin_oxy_poly2 <- tibble(sp = rep("yfin", length(ints)), 
														 age, 
														 var = "yrprior_boxy", 
														 ints, 
														 poly2_1s, 
														 poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	yfin_int_poly2_temp_pars <- poly2_mods[[15]]$internal$parlist$alpha_j 
	
	ints <-     yfin_int_poly2_temp_pars[1:28]
	poly2_1 <-  yfin_int_poly2_temp_pars[29]
	poly2_2 <-  yfin_int_poly2_temp_pars[30]
	poly2_1s <- yfin_int_poly2_temp_pars[31:57]
	poly2_2s <- yfin_int_poly2_temp_pars[58:84]
	
	poly2_1s <- c(poly2_1, poly2_1 + poly2_1s)
	poly2_2s <- c(poly2_2, poly2_2 + poly2_2s)
	age <- c(10:29, 3, 30, 4:9)
	
	yfin_temp_poly2 <- tibble(sp = rep("yfin", length(ints)), 
														 age, 
														 var = "yrprior_btemp", 
														 ints, 
														 poly2_1s, 
														 poly2_2s) %>%
		pivot_longer(cols = ints:poly2_2s,
								 names_to = "term",
								 values_to = "est_com")
	  
	yfin_poly2_vars <- bind_rows(yfin_oxy_poly2, yfin_temp_poly2)
 
	yfin_poly2_vars$term[yfin_poly2_vars$term == "ints"] <- "intercept"
	yfin_poly2_vars$term[yfin_poly2_vars$term == "poly2_1s"] <- "poly2-1"
	yfin_poly2_vars$term[yfin_poly2_vars$term == "poly2_2s"] <- "poly2-2"

	yfin_par_comp <- left_join(yfin_pars_single, yfin_poly2_vars)
	
	yfin_par_comp$est_com <- round(yfin_par_comp$est_com, 3)
	
	yfin_par_comp <- yfin_par_comp %>%
		mutate(par_diff = abs(est_com - est_sm))
	
	  
	kbl(yfin_par_comp) %>%
		kable_minimal() %>%
		save_kable(file = "output/tables/yfin_par_comp_poly_tvt.png")