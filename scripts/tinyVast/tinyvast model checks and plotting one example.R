# example of one model
	
	######## PREDICT ERROR ###########
	
	# read in model
	mod <- readRDS(paste0(file_path, "pcod_yrprior_btemp_lin_tvt.rds"))
	mod <- reload_model(mod) # this step may not be needed
	
  # residuals
  y <- replicate(n = 100, expr = mod$obj$simulate()$y_i)
  
  res <- DHARMa::createDHARMa(simulatedResponse = y,
  														observedResponse = mod$data$log_wt,
  														fittedPredictedResponse = fitted(mod))
  plot(res)
  	
 	
  # predict
  
  #age_f <- unique(paste0("age_f", pcod_dat$age_f))

  nd <- crossing(
				yrprior_btemp = seq(
					from = min(mod$data$yrprior_btemp),
					to = max(mod$data$yrprior_btemp),
					length.out = 25),
				age_f = unique(mod$data$age_f),
				#age_f = age_f,
				year = 2004) 
  
  p <- predict(mod, newdata = nd, se.fit = FALSE)
  
  # ? error
  
  # output displays "age_fage_X but that doesn't work either...
  
  
  # try fitting model in session and predicting
  m <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f + yrprior_btemp,
  	family = gaussian(),
  	#sem = "", 
  	#dsem = NULL,
  	#space_column = c("X", "Y"), 
  	#spatial_graph = mesh_v,
  	#time_column = "year",
  	#times = 1993:2022,
  	#variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
  
  saveRDS(m, file = paste0(file_path, "ex_mod.rds"))
	
	# residuals
  y <- replicate(n = 100, expr = m$obj$simulate()$y_i)
  
  res <- DHARMa::createDHARMa(simulatedResponse = y,
  														observedResponse = m$data$log_wt,
  														fittedPredictedResponse = fitted(m))
  plot(res)
  
  # why so strange?
  	
	# predict
  nd <- crossing(
				yrprior_btemp = seq(
					from = min(m$data$yrprior_btemp),
					to = max(m$data$yrprior_btemp),
					length.out = 25),
				age_f = unique(m$data$age_f),
				year = 2004) 
  
  p <- predict(m, newdata = nd)
  
  # ? error still
  # tried other arguments (remove_origdata...)
  
  ########## reload_model error ###########
 
  
  