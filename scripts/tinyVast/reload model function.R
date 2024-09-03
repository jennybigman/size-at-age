	## error using reload_model when used in a function? ##

	# read in file 
	file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
	file_list <- list.files(path = paste0(file_path))

	pol_file_list <- stringr::str_subset(file_list, 'pol_')
	
	pol_list_trim <- pol_file_list[1:2]
	

	# read in mod function
	read_mod_fun <- function(mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod <- reload_model(mod, check_gradient = TRUE)
		mod
		
	}
	

	pol_mods <- purrr::map(pol_list_trim, read_mod_fun)
	
	
	###################################################
	
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
 
 saveRDS(m, file = paste0(file_path, "ex_mod1.rds"))

 m <- 
	tinyVAST(
	data = pcod_dat,
 	formula = log_wt ~ 0 + age_f * yrprior_btemp,
 	family = gaussian(),
 	#sem = "", 
 	#dsem = NULL,
 	#space_column = c("X", "Y"), 
 	#spatial_graph = mesh_v,
 	#time_column = "year",
 	#times = 1993:2022,
 	#variable_column = "age_f",
 	control = tinyVASTcontrol(profile = "alpha_j")) 
 
 
  saveRDS(m, file = paste0(file_path, "ex_mod2.rds"))

  
 m <- 
	tinyVAST(
	data = pcod_dat,
 	formula = log_wt ~ 0 + age_f * yrprior_btemp + year_f,
 	family = gaussian(),
 	#sem = "", 
 	#dsem = NULL,
 	#space_column = c("X", "Y"), 
 	#spatial_graph = mesh_v,
 	#time_column = "year",
 	#times = 1993:2022,
 	#variable_column = "age_f",
 	control = tinyVASTcontrol(profile = "alpha_j")) 
 
   saveRDS(m, file = paste0(file_path, "ex_mod3.rds"))

   
  # read in file 
	file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
	file_list <- list.files(path = paste0(file_path))

	file_list <- stringr::str_subset(file_list, 'ex_')
	

	# read in mod function
	read_mod_fun <- function(mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod <- reload_model(mod, check_gradient = TRUE)
		mod
		
	}
	

	mods <- purrr::map(file_list, read_mod_fun)
