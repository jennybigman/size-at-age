# deviance explained test

	yrprior_btemp_int_mod_pollock <- readRDS(file = 
		here("./output/model output/sdmTMB output/Feb 2024 - NN/yrprior_btemp_int_mod_pollock.rds"))
	
	mod_dat <-  yrprior_btemp_int_mod_pollock$data
	mod_mesh <- yrprior_btemp_int_mod_pollock$spde
		
	null_mod <- sdmTMB(log_wt ~ age_f,
	 									 data = mod_dat,
	 									 spatial = "on",
										 spatiotemporal = "IID",
							  		 time = "year",
	 									 mesh = mod_mesh) 
	
	glm_mod <- glm(log_wt ~ age_f, data = mod_dat)
	
	summary(glm_mod)
	
null_mod <- sdmTMB(log_wt ~ 1,
 									 data = mod_dat,
 									spatial = "off",
 									 mesh = mod_mesh) 
 	
	sanity(null_mod)
	
	null_dev <- -2 * as.numeric(logLik(null_mod))
	 
	resid_dev <- -2* as.numeric(logLik(yrprior_btemp_int_mod_pollock))
	 
	dev_explained <- 100 * (null_dev - resid_dev)/null_dev ## NEGATIVE?
	
	
	
	
	######### try CV mod -- WORKS
	mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 25)

	pcod$fyear <- as.factor(pcod$year)
	
	m_cv <- sdmTMB_cv(
  	density ~ 0 + s(depth_scaled) + fyear,
  	data = pcod,
  	mesh = mesh,
  	family = tweedie(link = "log"),
  	k_folds = 4
)
	
	null <- sdmTMB(density ~ 1,
                 spatial="off",
                 mesh = mesh,
                 data = m_cv$data)


	
	null_dev <- -2 * as.numeric(logLik(null))
  
  log_lik <- unlist(lapply(m_cv$model, logLik))
  
  resid_dev <- -2 * log_lik
  
  dev_explained <- 100 * (null_dev - resid_dev)/null_dev
  
  test_df <- tibble(null_dev=null_dev,resid_dev=resid_dev,dev_explained=dev_explained)
}
	
	################
	 	
	null_dev <- -2 * as.numeric(logLik(null_mod))
  
  log_lik <- sapply(yrprior_btemp_int_mod_pollock, logLik)
  
  resid_dev <- -2 * log_lik
  
  dev_explained <- 100 * (null_dev - resid_dev)/null_dev
  
  	tibble(null_dev = null_dev, 
  				 resid_dev = resid_dev, 
  				 dev_explained = dev_explained)
	 	