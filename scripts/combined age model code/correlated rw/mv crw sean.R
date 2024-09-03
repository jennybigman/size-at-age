# correlated random walks & separate ST fields

	# path to save models
	file_path <- paste0(here(), "/output/model output/sdmTMB output/August 2024 combined age models/correlated rw/")

	n_year_age <- length(unique(a_d$age)) * length(unique(a_d$year))

	#### no covariate ####
	fit_no_cov <-
		sdmTMB(
			log_wt ~ 0 + age_f,
			data = a_d,
			mesh = mesh_a,
			spatial = "on",
			spatiotemporal = "off",
			spatial_varying = ~ 0 + year_age,
			groups = "age_f",
			control = sdmTMBcontrol(
				profile = "b_j",
				#map = list(ln_tau_Z = y_map_vec),
				map = list(ln_tau_Z = factor(rep(1, n_year_age))),
				multiphase = FALSE
			),
			time = "year",
			silent = FALSE
		)	
	
	sanity(fit_no_cov)
		
	write_rds(fit_no_cov, 
					file = paste0(file_path, "atooth_", "fit_no_cov_crw.rds"))
	
	
	#no cov-DONE
	#lin-DONE
	#poly2
	#poly3
	
	crw_mod_fun <- function(y){

	# no interaction
	fit_no_int <-
			try(sdmTMB(
				formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")),
				data = a_d,
				mesh = mesh_a,
				spatial = "on",
				spatiotemporal = "off",
				spatial_varying = ~ 0 + year_age,
				groups = "age_f",
				control = sdmTMBcontrol(
					profile = "b_j",
					#map = list(ln_tau_Z = y_map_vec),
					map = list(ln_tau_Z = factor(rep(1, n_year_age))),
					multiphase = FALSE
				),
				time = "year",
				silent = FALSE
			))
	
		try(sanity(fit_no_int))
	
	mod_name <- paste0(y, "_poly2_crw")
	
		try(write_rds(fit_no_int, 
					file = paste0(file_path, "atooth_", mod_name, ".rds")))

		# interaction
		fit_int <-
			try(sdmTMB(
				formula = as.formula(paste0("log_wt ~ 0 + age_f * poly(", y,", 3, raw = TRUE)")),
				data = a_d,
				mesh = mesh_a,
				spatial = "on",
				spatiotemporal = "off",
				spatial_varying = ~ 0 + year_age,
				groups = "age_f",
				control = sdmTMBcontrol(
					profile = "b_j",
					#map = list(ln_tau_Z = y_map_vec),
					map = list(ln_tau_Z = factor(rep(1, n_year_age))),
					multiphase = FALSE
				),
				time = "year",
				silent = FALSE
			))
	
		try(sanity(fit_int))
	
		mod_name <- paste0(y, "_int_poly3_crw")

		try(write_rds(fit_int, 
					file = paste0(file_path, "atooth_", mod_name, ".rds")))
		
	}
	
	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, crw_mod_fun)

	
	
	# read in, check, compare models #### error when trying to do all at once so separate by species

	## read in models ####
	file_list <- list.files(path = paste0(file_path))

	# pcod
	pcod_list <- stringr::str_subset(file_list, 'pcod')
	
	
  mod_names_list <- list()
  
  for(i in pcod_list){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
  pcod_mod_list <- lapply(mod_names_list, readRDS)
  
  # sanity
  names_mods <- names(pcod_mod_list)
  
  sanity_fun <- function(num, name) {
		
		mod <- pcod_mod_list[[num]]
		
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

	num = 1:length(pcod_mod_list)
	
	fdf <- tibble(
		num = num,
		name = names_mods
	)

	sanity_df <- map2_dfr(fdf$num, fdf$name, sanity_fun)  	
	
  # AIC
  AICs <- map_dfr(pcod_mod_list, AIC) %>%
  	flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
  
  AICs$AIC <- unlist(AICs$AIC)
	
  # pollock 
	pol_list <- stringr::str_subset(file_list, 'pol_')
	
	
  mod_names_list <- list()
  
  for(i in pol_list){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
  pol_mod_list <- lapply(mod_names_list, readRDS)
  
  # sanity
  names_mods <- names(pol_mod_list)
  
  sanity_fun <- function(num, name) {
		
		mod <- pol_mod_list[[num]]
		
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

	num = 1:length(pol_mod_list)
	
	fdf <- tibble(
		num = num,
		name = names_mods
	)

	sanity_df <- map2_dfr(fdf$num, fdf$name, sanity_fun)  	
	
  # AIC
  AICs <- map_dfr(pol_mod_list, AIC) %>%
  	flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
  
  AICs$AIC <- unlist(AICs$AIC)
	
	#### atooth ####
  
	atooth_list <- stringr::str_subset(file_list, 'atooth')
	
  mod_names_list <- list()
  
  for(i in atooth_list){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
  atooth_mod_list <- lapply(mod_names_list, readRDS)
  
  # sanity
  names_mods <- names(atooth_mod_list)
  
  sanity_fun <- function(num, name) {
		
		mod <- pcod_mod_list[[num]]
		
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

	num = 1:length(pcod_mod_list)
	
	fdf <- tibble(
		num = num,
		name = names_mods
	)

	sanity_df <- map2_dfr(fdf$num, fdf$name, sanity_fun)  	
	
  # AIC
  AICs <- map_dfr(pcod_mod_list, AIC) %>%
  	flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
  
  AICs$AIC <- unlist(AICs$AIC)
	
  
  
  
  
  
  
  ## yfin ####
  
	yfin_list <- stringr::str_subset(file_list, 'yfin_')
	
  yfin_fit_no_cov_crw <- readRDS(file = paste0(file_path, yfin_list[[1]]))
  yfin_yrprior_boxy_int_lin_crw <- readRDS(file = paste0(file_path, yfin_list[[2]]))
  	


  # sanity
  names_mods <- names(yfin_mod_list)
  
  sanity_fun <- function(num, name) {
		
		mod <- pol_mod_list[[num]]
		
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

	num = 1:length(pol_mod_list)
	
	fdf <- tibble(
		num = num,
		name = names_mods
	)

	sanity_df <- map2_dfr(fdf$num, fdf$name, sanity_fun)  	
	
  # AIC
  AICs <- map_dfr(pol_mod_list, AIC) %>%
  	flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
  
  AICs$AIC <- unlist(AICs$AIC)
  
  
  
  
  
  
  
  
  
  # pollock ####
  
	d <- pollock_dat
	d <- mutate(d, age = ifelse(age >= 13, 13, age))
	d <- d |>
		filter(age_f %in% age)
	d$age_f <- droplevels(d$age_f)
	d <- droplevels(d)
	d$year_age <- factor(paste(d$year, d$age))
	(all_time <- seq(min(d$year), max(d$year)))
	
	mesh <- make_mesh(
		d, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
		cutoff = 40,
		offset = c(60, 70)
	)
	
	# what about allowing each age to have its own SD?
	# create a map vector for that...
	mm <- model.matrix(~0 + year_age, data = d) |> colnames()
	#map_vec <- gsub("year_age[0-9]+ ", "", mm)
	map_vec <- gsub("^\\S+ ", "", mm)
	
	# but share last 2 are too data sparse in last age bin:
	table(d$age, d$year)
	# mirror their SD estimate:
	#map_vec[map_vec == "12"] <- "11"
	#map_vec[map_vec == "8"] <- "7"

	map_vec <- factor(map_vec)
	

	#### no covariate ####
	fit_no_cov <-
		sdmTMB(
			log_wt ~ 0 + age_f,
			data = d,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			spatial_varying = ~ 0 + year_age,
			groups = "age_f",
			control = sdmTMBcontrol(
				profile = "b_j",
				map = list(ln_tau_Z = map_vec),
				multiphase = FALSE
			),
			time = "year",
			silent = FALSE
		)	
	
	sanity(fit_no_cov)
		
	write_rds(fit_no_cov, 
					file = paste0(file_path, "pol_", "fit_no_cov_crw.rds"))
	
	# no interaction
	
	fit_no_int <-
		sdmTMB(
			log_wt ~ 0 + age_f + poly(yrprior_boxy, 3, raw = TRUE),
			data = d,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			spatial_varying = ~ 0 + year_age,
			groups = "age_f",
			control = sdmTMBcontrol(
				profile = "b_j",
				map = list(ln_tau_Z = map_vec),
				multiphase = FALSE
			),
			time = "year",
			silent = FALSE
		)	
	
	sanity(fit_no_int)
	
	mod_name <- "fit_oxy_poly3_crw"

	write_rds(fit_no_int, 
					file = paste0(file_path, "pol_", mod_name, ".rds"))
	


	# interaction
	
	fit_int <-
		sdmTMB(
			log_wt ~ 0 + age_f * poly(yrprior_btemp, 2, raw = TRUE),
			data = d,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			spatial_varying = ~ 0 + year_age,
			groups = "age_f",
			control = sdmTMBcontrol(
				profile = "b_j",
				map = list(ln_tau_Z = map_vec),
				multiphase = FALSE
			),
			time = "year",
			silent = FALSE
		)	
	
	sanity(fit_int)
	
	mod_name <- "fit_temp_int_poly2_crw"

	write_rds(fit_int, 
					file = paste0(file_path, "pol_", mod_name, ".rds"))
	

	# read in and compare models

	## read in models ####
	file_list <- list.files(path = paste0(file_path))

	#file_list <- stringr::str_subset(file_list, '.rds')
	
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  AICs <- map_dfr(mod_list, AIC) %>%
  	flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
	
  
  