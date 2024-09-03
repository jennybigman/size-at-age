# function to fit combined models using a model that approximates a single age model with a time-varying
# effect and has separate spatial and spatiotemporal fields for each age class using a hack to separate
# ST fields by age and year

	# path to save models
	file_path <- paste0(here(), "/output/model output/sdmTMB output/August 2024 combined age models/")

	# function to run models ####
	
	# temperature
	temp_mod_fun <- function(sp, end_age){
	
		d <- dat_all %>% filter(short_name == sp)
		d <- d %>% filter(age < end_age)

		mesh <- make_mesh(
			d, c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
			offset = c(60, 70)
		)

		d$fake_time <- factor(paste0(d$year, d$age))

		mod_name <- paste0(sp, "_poly3_noint_temp") # can change to poly2/poly3/etc.


		fit_cov <- 
			sdmTMB(
				formula = log_wt ~ age_f * year_f + age_f * poly(yrprior_btemp, 3, raw = TRUE), # can change to poly2/poly3/etc. and to no interaction (+)
				data = d,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "iid",
				control = sdmTMBcontrol(profile = "b_j"),
				time = "fake_time",
				share_range = FALSE,
				silent = FALSE,
				priors = sdmTMBpriors(
					matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
					matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
				)
			)
	
		write_rds(fit_cov, 
					file = paste0(file_path, mod_name,".rds"))
	
	}

	fdf <- tibble(
		sp = c("atooth", "pcod", "pollock"),
		end_age = c(12, 9, 13)
	)

	temp_mods_combined <- purrr::map2(fdf$sp, fdf$end_age, temp_mod_fun)

	# oxygen
	oxy_mod_fun <- function(sp, end_age){
	
		d <- dat_all %>% filter(short_name == sp)
		d <- d %>% filter(age < end_age)

		mesh <- make_mesh(
			d, c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
			offset = c(60, 70)
		)

		d$fake_time <- factor(paste0(d$year, d$age))

		mod_name <- paste0(sp, "_poly3_noint_oxygen") # can change to poly2/poly3/etc.


		fit_cov <- 
			sdmTMB(
				formula = log_wt ~ age_f * year_f + age_f + poly(yrprior_boxy, 3, raw = TRUE), # can change to poly2/poly3/etc. and to no interaction (+)
				data = d,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "iid",
				control = sdmTMBcontrol(profile = "b_j"),
				time = "fake_time",
				share_range = FALSE,
				silent = FALSE,
				priors = sdmTMBpriors(
					matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
					matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
				)
			)
		
		write_rds(fit_cov, 
					file = paste0(file_path, mod_name,".rds"))
		
	}

	fdf <- tibble(
		sp = c("atooth", "pcod", "pollock"),
		end_age = c(12, 9, 13)
	)

	oxy_mods_combined <- purrr::map2(fdf$sp, fdf$end_age, oxy_mod_fun)

	
	# no covariate model
	
	no_cov_mod_fun <- function(sp, end_age){
	
		d <- dat_all %>% filter(short_name == sp)
		d <- d %>% filter(age < end_age)

		mesh <- make_mesh(
			d, c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
			offset = c(60, 70)
		)

		d$fake_time <- factor(paste0(d$year, d$age))

		mod_name <- paste0(sp, "_no_cov")

		fit_no_cov <- 
			sdmTMB(
				formula = log_wt ~ age_f * year_f,
				data = d,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "iid",
				control = sdmTMBcontrol(profile = "b_j"),
				time = "fake_time",
				share_range = FALSE,
				silent = FALSE,
				priors = sdmTMBpriors(
					matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
					matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
				)
			)

		write_rds(fit_no_cov, 
						file = paste0(file_path, mod_name,".rds"))
			
		}

	fdf <- tibble(
		sp = c("atooth", "pcod", "pollock"),
		end_age = c(12, 9, 13)
	)

	no_cov_mods_combined <- purrr::map2(fdf$sp, fdf$end_age, no_cov_mod_fun)
	
	# for yfin, need to use age > 4, Age < 28 so run separately or modify this function

	### check sanity and calculate AICs ####
	
	# path to save models
	file_path <- paste0(here(), "/output/model output/sdmTMB output/August 2024 combined age models/")

	
	## read in models ####
	file_list <- list.files(path = paste0(file_path))

	#file_list <- stringr::str_subset(file_list, '.rds')
	
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # sanity checks
  
  names_mods <- names(mod_list)
  
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

	mod_df <- map2_dfr(fdf$num, fdf$name, sanity_fun) 
	
	#mod_df_drop <- mod_df %>%
	#	filter(if_any(everything(), ~ .x == FALSE))
	
	# plot residuals
	
	atooth_mods <- keep(mod_list, str_detect(names(mod_list), "atooth"))
	pcod_mods <- keep(mod_list, str_detect(names(mod_list), "pcod"))
	pollock_mods <- keep(mod_list, str_detect(names(mod_list), "pollock"))
	yfin_mods <- keep(mod_list, str_detect(names(mod_list), "yfin"))
	
	
	resid_fun <- function(mod){
		
		sims <- simulate(mod, nsim = 500, type = "mle-mvn") %>% dharma_residuals(mod) 
		plot_df <- dharma_residuals(sims, mod, plot = FALSE)

	}
	
  atooth_resids  <- purrr::map(atooth_mods, resid_fun) 
  pcod_resids  <- purrr::map(poly_3_mods, resid_fun) 

	
	
	# AICs ####
	
	AIC_func <- function(x){
		
		mods <- mod_list[grep(x, names(mod_list))]
		AIC_list <- sapply(mods, AIC)
		
	
	}
	
	sp <- unique(dat_all$short_name)

	comb_mod_AICs <- purrr::map(sp, AIC_func) %>% 
		flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
	
	comb_mod_AICs$AIC <- unlist(comb_mod_AICs$AIC)
