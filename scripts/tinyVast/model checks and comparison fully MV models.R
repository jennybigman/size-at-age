# model checks and comparisons joint MV models

	file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	
	file_list <- list.files(path = paste0(file_path))
	
	# just read the rds file
	read_mod_fun <- function(mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	
	# AIC function
	AIC_fun <- function(mod) {
  	
	  	AIC <- AIC(mod)
	  	AIC
  	
  }
  

	# pcod
	
	pcod_mods_names <- str_subset(file_list, "pcod")
	
	pcod_mv_mods <- purrr::map(pcod_mods_names, read_mod_fun)	
	
  pc_AICs <- purrr::map(pcod_mv_mods, AIC_fun)
  	
  pc_AICs <- tibble(pcod_mods_names, pc_AICs) %>% rename(AIC = pc_AICs)
  
  pc_AICs$AIC <- unlist(pc_AICs$AIC)
  
  # arrowtooth
  
	atooth_mods_names <- str_subset(file_list, "atooth")
	atooth_mods_names <- str_subset(atooth_mods_names, "atooth_", negate = TRUE) # forgot a _ in between sp name and covariate
	
	atooth_mv_mods <- purrr::map(atooth_mods_names, read_mod_fun)	
	
  a_AICs <- purrr::map(atooth_mv_mods, AIC_fun)
  	
  a_AICs <- tibble(atooth_mods_names, a_AICs) %>% rename(AIC = a_AICs)
  
  a_AICs$AIC <- unlist(a_AICs$AIC)
  