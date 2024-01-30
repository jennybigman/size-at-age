# put output into table

	##### READ IN MODELS AND CHECKS ####
	
	file_list <- list.files(path = paste0(here(), file_path_all))

  prestring <- paste0(here(), file_path_all)
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i) # I used a for loop!
  }
  
  mod_list <- lapply(mod_names_list, read_rds)
  
	# extract model from sdmTMB_cv object
	
	ex_mod <- function(x){
		
		mod <- x$models[[1]]
	}
	
	mod_list <- lapply(mod_list, ex_mod)

  # separate models by species ####
  
	# pollock #
  
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yf", names(mod_list))]

	
	# AIC
	
	pol_AICs <- lapply(pol_mod_list, AIC)
	
	model_ids <- names(pol_AICs)
	model_ids <- str_remove(model_ids, ".rds")
	model_AICs <- unlist(pol_AICs)

	pol_mod_AICs_all <- tibble(model_ids, model_AICs)	
	
	pol_mod_AICs_oxy <- pol_mod_AICs_all %>%
		filter(model_ids)