# top model checks and prediction

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
	
	
	# residuals
	
	pcod_resid <- res_fun(pcod_top_mod)
	plot(pcod_resid)	
	
	pol_resid <- res_fun(pol_top_mod)
	plot(pol_resid)	
	
	atooth_resid <- res_fun(atooth_top_mod)
	plot(atooth_resid)	
	
	yfin_resid <- res_fun(yfin_top_mod)
	plot(yfin_resid)	
	
	# deviance explained
	dev_exp_fun(pcod_top_mod)
	dev_exp_fun(atooth_top_mod)
	dev_exp_fun(pol_top_mod)	
	dev_exp_fun(yfin_top_mod)	

	# predictions
	
	# pcod
  nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(pcod_top_mod$data$yrprior_btemp, na.rm = TRUE),
					to = max(pcod_top_mod$data$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				age_f = unique(pcod_top_mod$data$age_f),
				year = 2004,
				X = pcod_top_mod$data$X[1],
				Y = pcod_top_mod$data$Y[1]) 
  
  nd <- data.frame(nd)
  
  #p <- predict(pcod_top_mod, newdata = nd, what = "p_g", se.fit = TRUE) # get error around each value?
  p <- predict(pcod_top_mod, newdata = nd, se.fit = FALSE) # get error around each value?

  preds <- tibble(nd, p) 
  
  preds <- preds %>%
		mutate(age = as.character(age_f))
  
	ages <- str_extract(preds2$age, "(?<=_)[0-9]+")
  
	preds <- preds %>%
		mutate(age = as.numeric(ages))
  
  ggplot(preds) +
  	#geom_ribbon() +
  	geom_line(aes(x = yrprior_btemp, y = p)) +
		facet_wrap(~ reorder(age_f, age), scales = "free_y")  
  
	# atooth
  mod_file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
  atooth_top_mod <- readRDS(paste0(mod_file_path, "yfin_yrprior_btemp_poly3_tvt.rds"))
  
  nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(atooth_top_mod$data$yrprior_btemp, na.rm = TRUE),
					to = max(atooth_top_mod$data$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				age_f = unique(atooth_top_mod$data$age_f)[1],
				year = 2004,
				X = atooth_top_mod$data$X[1],
				Y = atooth_top_mod$data$Y[1]) 
  
  nd <- data.frame(nd)
  
  p <- predict(atooth_top_mod, newdata = nd, what = "p_g", se.fit = TRUE) # get error around each value?
  p <- predict(atooth_top_mod, newdata = nd, se.fit = FALSE) # get error around each value?

  preds <- tibble(nd, p) 
  
  #preds <- preds %>%
	#	mutate(age = as.character(age_f))
  #
	#ages <- str_extract(preds2$age, "(?<=_)[0-9]+")
  
	#preds <- preds %>%
	#	mutate(age = as.numeric(ages))
  
  ggplot(preds) +
  	#geom_ribbon() +
  	geom_line(aes(x = yrprior_btemp, y = p)) 
  
  # pollock
   nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(pol_top_mod$data$yrprior_btemp, na.rm = TRUE),
					to = max(pol_top_mod$data$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				age_f = unique(pol_top_mod$data$age_f)[1],
				year = 2004,
				X = pol_top_mod$data$X[1],
				Y = pol_top_mod$data$Y[1]) 
  
  nd <- data.frame(nd)
  
  #p <- predict(pcod_top_mod, newdata = nd, what = "p_g", se.fit = TRUE) # get error around each value?
  p <- predict(pol_top_mod, newdata = nd, se.fit = FALSE) # get error around each value?

  preds <- tibble(nd, p) 
  
  #preds <- preds %>%
	#	mutate(age = as.character(age_f))
  #
	#ages <- str_extract(preds2$age, "(?<=_)[0-9]+")
  
	#preds <- preds %>%
	#	mutate(age = as.numeric(ages))
  
  ggplot(preds) +
  	#geom_ribbon() +
  	geom_line(aes(x = yrprior_btemp, y = p)) 
  
	# yfin
  
   nd <- expand.grid(
				yrprior_boxy = seq(
					from = min(yfin_top_mod$data$yrprior_boxy, na.rm = TRUE),
					to = max(yfin_top_mod$data$yrprior_boxy, na.rm = TRUE),
					length.out = 25),
				age_f = unique(yfin_top_mod$data$age_f)[1],
				year = 2004,
				X = yfin_top_mod$data$X[1],
				Y = yfin_top_mod$data$Y[1]) 
  
  nd <- data.frame(nd)
  
  p <- predict(pcod_top_mod, newdata = nd, what = "p_g", se.fit = TRUE) # get error around each value?
  p <- predict(yfin_top_mod, newdata = nd, se.fit = FALSE) # get error around each value?

  preds <- tibble(nd, p) 
  
  #preds <- preds %>%
	#	mutate(age = as.character(age_f))
  #
	#ages <- str_extract(preds2$age, "(?<=_)[0-9]+")
  
	#preds <- preds %>%
	#	mutate(age = as.numeric(ages))
  
  ggplot(preds) +
  	#geom_ribbon() +
  	geom_line(aes(x = yrprior_boxy, y = p)) 
  