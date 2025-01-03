	# file list for mods

	#remotes::install_github("vast-lib/tinyVAST", ref = "dev", force = TRUE)

  file_paths <- paste0(here(), "/output/model output/tinyVAST/top mods/")
 
  file_list <- list.files(file_paths)
  
  # read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	# read in mods
	top_temp_mods <- purrr::map2(file_paths, file_list, read_mod_fun)

	# predict 
	pred_fun <- function(mod, mod_name){
	
		dat <- mod$data
		sp <- unique(dat$short_name)

		nd <- expand.grid(
			yrprior_btemp = seq(
				from = min_temp,
				to = max_temp,
				length.out = 25),
			age_f = unique(dat$age_f),
			year = 2004, # predict for any year
			X = X_pred,
			Y = Y_pred, 
			single_age = "shared") 
			
		nd <- data.frame(nd)
    p <- predict(mod, newdata = nd, what = "palpha_g")
  
  	preds <- tibble(nd, p, sp, mod_name) 
  		
		preds
		
	}
	
	# predictions
	
	top_temp_mod_preds <- purrr::map2_dfr(top_temp_mods, file_list, pred_fun)
