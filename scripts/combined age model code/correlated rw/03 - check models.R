	# path to save models
	file_path <- paste0(here(), "/output/model output/sdmTMB output/August 2024 combined age models/correlated rw/")

	# read in and compare models

	## read in models ####
	file_list <- list.files(path = paste0(file_path))

	#file_list <- stringr::str_subset(file_list, '.rds')
	
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(file_path, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  names_mods <- names(mod_list)
  
  # check sanity()
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

	sanity_df <- map2_dfr(fdf$num, fdf$name, sanity_fun)  	
	
	mod_df_drop <- mod_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- mod_df_drop$name

  
  # check residuals
  
  
  # AICs
  AICs <- map_dfr(mod_list, AIC) %>%
  	flatten() %>%
		enframe() %>%
		rename(model = name,
					 AIC = value)
	