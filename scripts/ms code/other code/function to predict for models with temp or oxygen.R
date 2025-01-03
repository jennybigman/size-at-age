	
	# predict 
	pred_fun <- function(mod){
	
		dat <- mod$data
		sp <- unique(dat$short_name)
		var_form <- mod$formula
			
		var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
		var <- var[3]
		
		if (var == "yrprior_btemp"){
		
			nd <- expand.grid(
					yrprior_btemp = seq(
						from = min_temp,
						to = max_temp,
						length.out = 25),
					age_f = unique(mod$data$age_f),
					year = 2004, # predict for a year
					X = X_pred,
					Y = Y_pred, 
					single_age = "shared") 
			
			nd <- data.frame(nd)
    	p <- predict(mod, newdata = nd)
  	
  		preds <- tibble(nd, p, sp, var) 
  		
  		#preds <- preds |> 
  		#	rename(value = yrprior_btemp)
  		
		} else if (var == "yrprior_boxy") {
			
					nd <- expand.grid(
					yrprior_boxy = seq(
						from = min_oxy,
						to = max_oxy,
						length.out = 25),
					age_f = unique(mod$data$age_f),
					year = 2004, 
					X = X_pred,
					Y = Y_pred,
					single_age = "shared") 
					
					
			nd <- data.frame(nd)
    	p <- predict(mod, newdata = nd)
  	
  		preds <- tibble(nd, p, sp, var) 
		 
  		#preds <- preds |> 
  		#	rename(value = yrprior_boxy)
					
		}
		
		 preds
	}