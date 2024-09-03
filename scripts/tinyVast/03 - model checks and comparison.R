# check residuals

	file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
	file_list <- list.files(path = paste0(file_path))
	
	
	# read AND reload mod function
	rr_mod_fun <- function(mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod <- try(reload_model(mod, check_gradient = TRUE))
		mod
		
	}
	
	
	# just read the rds file
	read_mod_fun <- function(mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	# residual function
	res_fun <- function(mod){
		
		y <- replicate(n = 100, expr = mod$obj$simulate()$y_i)
  	res <- DHARMa::createDHARMa(simulatedResponse = y,
  															observedResponse = mod$data$log_wt,
  															fittedPredictedResponse = fitted(mod))
  	res
	}
	
	# deviance explained function
	dev_exp_fun <- function(mod){
		
  	dev <- mod$deviance_explained
  	dev <- unlist(dev)
  	dev
	}
	
	# AIC function
	AIC_fun <- function(mod) {
  	
	  	AIC <- AIC(mod)
	  	AIC
  	
  }
  

	
	# pcod ####

	pcod_file_list <- stringr::str_subset(file_list, 'pcod')
	
	pcod_mods <- purrr::map(pcod_file_list, read_mod_fun)
	
	# plot residuals
	pcod_res <- purrr::map(mod, res_fun)
  
  # compare deviance explained
  
  pcod_dev_expl <- purrr::map(pcod_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  n <- pcod_file_list
  
  pcod_dev_exp <- tibble(n, pcod_dev_expl) %>%
  	rename(mod_name = n,
  				 dev_exp = V1)
  
  # all around 0.95
  
  # AICs
  
  pc_AICs <- purrr::map(pcod_mods, AIC_fun)
  	
  pc_AICs <- tibble(n, pc_AICs) %>% rename(AIC = pc_AICs)
  
  pc_AICs$AIC <- unlist(pc_AICs$AIC)
  
  
  # plot predictions - first need to rerun top model
  
  # need to refit model without expressions
   
  # predict
  nd <- expand.grid(
				y = seq(
					from = min(top_mod$data$yrprior_btemp, na.rm = TRUE),
					to = max(top_mod$data$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				age_f = unique(pcod_dat$age_f),
				year = 2004) 
  
  nd <- data.frame(nd)
  
  p <- predict(top_mod, newdata = nd, se.fit = FALSE)
  
  
  
  # atooth ####
  
	atooth_file_list <- stringr::str_subset(file_list, 'atooth')
	
	atooth_mods <- purrr::map(atooth_file_list, read_mod_fun)
	
  # plot residuals
	atooth_res <- purrr::map(atooth_mods, res_fun)
  
	# compare deviance explained
  atooth_dev_expl <- purrr::map(atooth_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  n <- atooth_file_list
  
  atooth_dev_expl <- tibble(n, atooth_dev_expl) %>%
  	rename(mod_name = n,
  				 dev_exp = V1)
  
  # AICs
  
  a_AICs <- purrr::map(atooth_mods, AIC_fun)
  	
  a_AICs <- tibble(n, a_AICs) %>% rename(AIC = a_AICs)
  
  a_AICs$AIC <- unlist(a_AICs$AIC)
  
  
  # pollock ####
  pol_file_list <- stringr::str_subset(file_list, 'pol_')
	
	pol_mods <- purrr::map(pol_file_list, read_mod_fun)
	
  # plot residuals
	pol_res <- purrr::map(pol_mods, res_fun)
  

	# compare deviance explained
  pol_dev_expl <- purrr::map(pol_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  n <- pol_file_list
  
  pol_dev_expl <- tibble(n, pol_dev_expl) %>%
  	rename(mod_name = n,
  				 dev_exp = V1)
  
  # AICs
  
  pol_AICs <- purrr::map(pol_mods, AIC_fun)
  	
  pol_AICs <- tibble(n, pol_AICs) %>% rename(AIC = pol_AICs)
  
  pol_AICs$AIC <- unlist(pol_AICs$AIC)
  
  # some issue with some of the pollock mods
  
  # predict
  
  
  m <- tinyVAST(
		data = pol_dat,
  	formula = log_wt ~ 0 + age_f * yrprior_btemp,
  	family = gaussian(),
  	#sem = "", 
  	#dsem = dsem,
  	#space_column = c("X", "Y"), 
  	#spatial_graph = mesh_v,
  	#time_column = "year",
  	#times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
  
  
  
  nd <- crossing(
				yrprior_btemp = seq(
					from = min(mod$data$yrprior_btemp),
					to = max(mod$data$yrprior_btemp),
					length.out = 25),
				age_f = unique(mod$data$age_f),
				year = 2004) 
  
  nd <- data.frame(nd)
  
  p <- predict(m, newdata = nd, se.fit = FALSE)
  


  #### yfin ####
  yfin_file_list <- stringr::str_subset(file_list, 'yfin')
	
	yfin_mods <- purrr::map(yfin_file_list, read_mod_fun)
	
  # plot residuals
	yfin_res <- purrr::map(yfin_mods, res_fun)
  
	plot(yfin_res[[1]])
  
	# compare deviance explained
  
  yfin_dev_expl <- purrr::map(yfin_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  n <- yfin_file_list
  
  yfin_dev_expl <- tibble(n, d) %>%
  	rename(mod_name = n,
  				 dev_exp = V1)
  
  # all around 0.95
  
  # AICs
  
  yfin_AICs <- purrr::map(yfin_mods, AIC_fun)
  	
  yfin_AICs <- tibble(n, yfin_AICs) %>% rename(AIC = yfin_AICs)
  
  yfin_AICs$AIC <- unlist(yfin_AICs$AIC)
  