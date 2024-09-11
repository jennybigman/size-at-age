# predictions stacked analysis

	file_path <- paste0(here(), "/output/model output/tinyVAST/sep age models/")

	## read in models ####
	file_list <- list.files(path = file_path)
	
	names_no_cov_mods_st <- stringr::str_subset(file_list, 'no')
	names_temp_mods_st <- stringr::str_subset(file_list, 'temp')
	names_oxy_mods_st <- stringr::str_subset(file_list, 'oxy')

	# just read the rds file
	read_mod_fun <- function(mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}

	# predictions
	
	# no cov
	no_cov_st_mods <- purrr::map(names_no_cov_mods_st, read_mod_fun)	

	no_cov_st_preds <- purrr::map_dfr(no_cov_st_mods, \ (mod){
		
		dat <- mod$data
		dat <- dat %>% 
			select(log_wt, short_name, weight,
						 age, age_f, year, 
						 yrprior_btemp, yrprior_boxy)
		
		preds <- predict(mod)
		preds <- tibble(preds, dat)
		preds
		
	})
	
	no_cov_st_preds <- no_cov_st_preds %>%
		rename(no_cov_preds_st = preds)
	
	# temp mods
	temp_st_mods <- purrr::map(names_temp_mods_st, read_mod_fun)	

	temp_st_preds <- purrr::map_dfr(temp_st_mods, \ (mod){
		
		dat <- mod$data
		sp <- unique(dat$short_name)
		age <- unique(dat$age)
		
		nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(dat$yrprior_btemp, na.rm = TRUE),
					to = max(dat$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				year = dat$year[1],
				X = dat$X[1],
				Y = dat$Y[1],
				var = 1) # what is this? value doesn't seem to matter?
  
  	nd <- data.frame(nd)
 
  	p <- predict(mod, newdata = nd)
  	
  	preds <- tibble(p, sp, age, nd)
		
	})
	
	temp_st_preds$type <- "stacked"


	
	