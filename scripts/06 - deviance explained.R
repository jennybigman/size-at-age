# model skill - explanatory & predictive

	#### deviance explained ####
	
	# read models
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/April 2024/")))
	mods_keep <- lowest_AIC$value
	file_list <- file_list[grep(paste(mods_keep, collapse = "|"), file_list)]
		
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/April 2024/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # deviance explained
	pcod_oxy_mod <- mod_list[[1]]

	# null mod
	null_mod <- sdmTMB(log_wt ~ 1,
										 data = pcod_oxy_mod$data,
										 mesh = pcod_oxy_mod$spde) 
	
	sanity(null_mod)
	
	sat_mod <- sdmTMB(log_wt ~ as.factor(1:length(log_wt)),
										data = pcod_oxy_mod$data,
										mesh = pcod_oxy_mod$spde)
	
	# null deviance
	#n_dev <- -2 * (logLik(null_mod)) 
	#resid_dev <- 2 * logLik(pcod_oxy_mod)

	#1 - (resid_dev/n_dev)
	
	#####
	
	r_dev <- 2 * (logLik(sat_mod) - logLik(pcod_oxy_mod))
	n_dev <- 2 * (logLik(sat_mod) - logLik(null_mod))
	
	1 - (r_dev/n_dev)
	
	# calc_dev_exp_fun <- function(mod) {
	# 	
	# 	fit <- yrprior_btemp_int_mod_pollock$model[[1]]
	# 	dat <- yrprior_btemp_int_mod_pollock$data
	# 	mesh <- yrprior_btemp_int_mod_pollock$spde 
#
	# 	
	# 	
	# 	
	# 	sanity(null_mod)
	# 	
	# 	# centered response with ~1 works
	# 	
	 	#glm_test <- glm(log_wt ~ age_f, data = pollock_dat)
	 	
	 	null_dev <- -2 * as.numeric(logLik(null_mod))
  
  	log_lik <- as.numeric(unlist(logLik(yrprior_btemp_int_mod_pollock$model
  
  	resid_dev <- -2 * log_lik
  
  	dev_explained <- 100 * (null_dev - resid_dev)/null_dev
  
  	tibble(null_dev = null_dev, 
  				 resid_dev = resid_dev, 
  				 dev_explained = dev_explained)
	 	
	 	
	 }
	 
	 null_cv <- sdmTMB_cv(log_wt ~ age_f,
                 spatial="off",
                 mesh = fit$spde,
                 data = fit$data)
	 
	
	 #########
	calc_deviance_ensemble <- function(model_df){
  fit <- yrprior_btemp_int_mod_pollock$model[[1]]
  dat <- yrprior_btemp_int_mod_pollock$data
  mesh <- yrprior_btemp_int_mod_pollock$spde
  
  null <- sdmTMB(log_wt ~ age_f,
                 spatial="off",
                 mesh = mesh,
                 data = dat)
  
  sanity(null)
  
  
  null_dev <- -2 * as.numeric(logLik(null_mod))
  
  #log_lik <- unlist(lapply(yrprior_btemp_int_mod_pollock, logLik))
  
  log_lik <- unlist(logLik(yrprior_btemp_int_mod_pollock))
  
  resid_dev <- -2 * log_lik
  
  dev_explained <- 100 * (null_dev - resid_dev)/null_dev
  
  tibble(null_dev=null_dev,resid_dev=resid_dev,dev_explained=dev_explained)
}
	 
	 
	 2* (logLik(yrprior_btemp_int_mod_pollock) - logLik(null))
	 
	 -2*logLik(null)
	 
	 
	# fit a null model for each species

	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/null mods Feb 2024/"
	
	

	

	# read in null models
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/null mods Feb 2024/")))

	file_list <- stringr::str_subset(file_list, '.rds')

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/null mods Feb 2024/"))
  

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  null_mod_list <- lapply(mod_names_list, readRDS)
  
  # check sanity
  
	sanity_func <- function(x){
  	 s <- sanity(x)
  }
  
	s <- lapply(null_mod_list, sanity_func)
	
	###
	
	dev_exp_fun <- function(null_mod, mod){
		
		null_dev <- -2 * as.numeric(logLik(null_mod))
  
  	log_lik <- unlist(lapply(mod$model, logLik))
  
  resid_dev <- -2 * log_lik
  
  dev_explained <- 100 * (null_dev - resid_dev)/null_dev
  
  tibble(null_dev=null_dev,resid_dev=resid_dev,dev_explained=dev_explained)
	