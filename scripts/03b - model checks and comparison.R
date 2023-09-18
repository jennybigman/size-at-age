# model comparison of sdmTMB mods

	## read in models ####
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output")))

  prestring <- paste0(here(), ("/output/model output/sdmTMB output/"))
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
  # separate models by species ####
  
	# pollock #
	pol_mod_list <- mod_list[grep("pol", names(mod_list))]
	
	# pcod #
	pcod_mod_list <- mod_list[grep("pcod", names(mod_list))]
		
	# yfin #
	yfin_mod_list <- mod_list[grep("yfin", names(mod_list))]
	
		
	#### compare models with and without jday as a covariate ####
	
	lapply(pol_mod_list, AIC)
	# presurvey btemp - all models without jday lower AIC
	# age0 btemp - all models without jday lower AIC
	# yrprior btemp - all models without jday lower AIC
	
	lapply(pcod_mod_list, AIC)
	# presurvey btemp - all models without jday MUCH better fit
	# age0 btemp - only the two mods without interaction, equivalent
	# yrprior btemp - models with jday better fit
	
	lapply(yfin_mod_list, AIC)
	# presurvey btemp - models without jday lower AIC
	# age0 btemp - for models with no int, AIC is equivalent; model with int and jday won't converge
	# yrprior btemp - for models with no int, AIC is equivalent; models with ints won't converge
	
	
	#### compare models with and without int ####
	pol_mod_list_nj <- pol_mod_list[grep("nj", names(pol_mod_list))]
	lapply(pol_mod_list_nj, AIC)
	# presurvey btemp - model with int lower AIC
	# yrprior btemp - model with int lower AIC
	# age0 btemp - model with int lower AIC
	
	pcod_mod_list_nj <- pcod_mod_list[grep("nj", names(pcod_mod_list))]
	lapply(pcod_mod_list_nj, AIC)
	# presurvey btemp - model with int lower AIC
	# yrprior btemp - model with int lower AIC
	# age0 btemp - can't judge because model with int won't converge
	
	yfin_mod_list_nj <- yfin_mod_list[grep("nj", names(yfin_mod_list))]
	lapply(yfin_mod_list_nj, AIC)
	# presurvey btemp - model with int lower AIC
	# yrprior btemp - model with int won't converge
	# age0 btemp - model with int lower AIC
	
	