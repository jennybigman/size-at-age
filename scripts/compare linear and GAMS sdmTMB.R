# compare nonlinear and linear models

	# read in all models

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
	
	# nonlinear vs linear ####
	
	# pollock
	pol_mod_list_p <- pol_mod_list[grep("presurvey", names(pol_mod_list))]
	lapply(pol_mod_list_p, AIC)
	# for int model, GAM no jday lowest AIC
	# for no into model, GAM no jday lowest AIC
	
	pol_mod_list_y <- pol_mod_list[grep("yrprior", names(pol_mod_list))]
	lapply(pol_mod_list_y, AIC)
	# for int model, GAM no jday lowest AIC
	# for no into model, GAM no jday lowest AIC
	
	pol_mod_list_a <- pol_mod_list[grep("age0", names(pol_mod_list))]
	lapply(pol_mod_list_a, AIC)
	# for int model, GAM no jday lowest AIC
	# for no into model, GAM no jday lowest AIC
	
	
	# pcod
	pcod_mod_list_p <- pcod_mod_list[grep("presurvey", names(pcod_mod_list))]
	lapply(pcod_mod_list_p, AIC)
	# for int model, linear and GAM with jday equivalent
	# for no int model, linear and GAM with jday equivalent
	
	pcod_mod_list_y <- pcod_mod_list[grep("yrprior", names(pcod_mod_list))]
	lapply(pcod_mod_list_y, AIC)
	# for int model, linear best model
	# for no int model, linear best model
	
	pcod_mod_list_a <- pcod_mod_list[grep("age0", names(pcod_mod_list))]
	lapply(pcod_mod_list_a, AIC)
	# for int model, GAM no jday lowest AIC
	# for no into model, GAM no jday lowest AIC
	
	# yfin
	yfin_mod_list_p <- yfin_mod_list[grep("presurvey", names(yfin_mod_list))]
	lapply(yfin_mod_list_p, AIC)
	# for int model, GAM with no jday lowest AIC
	# for no int model, linear and GAM with no jday equivalent
	
	yfin_mod_list_y <- yfin_mod_list[grep("yrprior", names(yfin_mod_list))]
	lapply(yfin_mod_list_y, AIC)
	# for int model, ?
	# for no int model, linear and GAM with no jday equivalent
	
	yfin_mod_list_a <- yfin_mod_list[grep("age0", names(yfin_mod_list))]
	lapply(yfin_mod_list_a, AIC)
	# for int model, need int with jday
	# for no into model, two GAMS equivalent, without jday lower
	