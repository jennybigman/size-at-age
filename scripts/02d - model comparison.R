  
#### model comparisons ####

	#### comparing temp and temp by age effect ####

  # these models are fitted with ACLIM SEB temps ####
  
	# summer temps avg April - June annually ####
  
  # pollock
  
   pol_pretemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_pretemp_age_bam_ML.rds"))
   pol_pretemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_pretemp_int_age_bam_ML.rds"))

   AIC(pol_pretemp_age_bam_ML)
   AIC(pol_pretemp_int_age_bam_ML)
   
   # pcod
  
   pcod_pretemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_pretemp_age_bam_ML.rds"))
   pcod_pretemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_pretemp_int_age_bam_ML.rds"))

   AIC(pcod_pretemp_age_bam_ML)
   AIC(pcod_pretemp_int_age_bam_ML)
   
   # yfin sole
   
   yfin_pretemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_pretemp_age_bam_ML.rds"))
   yfin_pretemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_pretemp_int_age_bam_ML.rds"))

   AIC(yfin_pretemp_age_bam_ML)
   AIC(yfin_pretemp_int_age_bam_ML)
 
   
   #### year temp the year preceding survey ####
	
	# pollock
   
  pol_yrtemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_yrtemp_age_bam_ML.rds"))
  pol_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_yrtemp_int_age_bam_ML.rds"))
  
  AIC(pol_yrtemp_age_bam_ML) ## need to run
  AIC(pol_yrtemp_int_age_bam_ML)
  
	# pcod #
	
	pcod_yrtemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_yrtemp_age_bam_ML.rds"))
  pcod_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_yrtemp_int_age_bam_ML.rds"))
  
  AIC(pcod_yrtemp_age_bam_ML)
  AIC(pcod_yrtemp_int_age_bam_ML)
 
 # yellowfin # 
													
	yfin_yrtemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_yrtemp_age_bam_ML.rds"))
  yfin_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_yrtemp_int_age_bam_ML.rds"))
  
  AIC(yfin_yrtemp_age_bam_ML)
  AIC(yfin_yrtemp_int_age_bam_ML)

  #### temp during first year of life #### EDIT BELOW
	
	# pollock
   
  pol_yrtemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_yrtemp_age_bam_ML.rds"))
  pol_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_yrtemp_int_age_bam_ML.rds"))
  
  AIC(pol_yrtemp_age_bam_ML) ## need to run
  AIC(pol_yrtemp_int_age_bam_ML)
  
	# pcod #
	
	pcod_yrtemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_yrtemp_age_bam_ML.rds"))
  pcod_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_yrtemp_int_age_bam_ML.rds"))
  
  AIC(pcod_yrtemp_age_bam_ML)
  AIC(pcod_yrtemp_int_age_bam_ML)
 
 # yellowfin # 
													
	yfin_yrtemp_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_yrtemp_age_bam_ML.rds"))
  yfin_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_yrtemp_int_age_bam_ML.rds"))
  
  AIC(yfin_yrtemp_age_bam_ML)
  AIC(yfin_yrtemp_int_age_bam_ML)
  
  
  #### compare different temp metrics ####
  
  # pollock 
  
  pol_pretemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_pretemp_int_age_bam_ML.rds"))
  pol_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_yrtemp_int_age_bam_ML.rds"))
  pol_temp0_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pol_temp0_int_age_bam_ML.rds"))
  # linear effect of temp and age
  pol_presurvey_templin_int_age_bam <- readRDS(file = here("./output/model output/ACLIM temps/linear models/pol_presurvey_templin_int_age_bam.rds"))

  
  # with temp by age effect -- summer avg temp more supported
  AIC(pol_pretemp_int_age_bam_ML)
  AIC(pol_yrtemp_int_age_bam_ML)
  AIC(pol_temp0_int_age_bam_ML)
  AIC(pol_presurvey_templin_int_age_bam)

   # pcod 
  
  pcod_pretemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_pretemp_int_age_bam_ML.rds"))
  pcod_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_yrtemp_int_age_bam_ML.rds"))
  pcod_temp0_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/pcod_temp0_int_age_bam_ML.rds"))
  # linear effect of temp and age
  pcod_presurvey_templin_int_age_bam <- readRDS(file = here("./output/model output/ACLIM temps/linear models/pcod_presurvey_templin_int_age_bam.rds"))

  
  # with temp by age effect -- summer avg temp more supported
  AIC(pcod_pretemp_int_age_bam_ML)
  AIC(pcod_yrtemp_int_age_bam_ML)
  AIC(pcod_temp0_int_age_bam_ML)
  AIC(pcod_presurvey_templin_int_age_bam)

  
   # yfin 
  
	yfin_pretemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_pretemp_int_age_bam_ML.rds"))
  yfin_yrtemp_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_yrtemp_int_age_bam_ML.rds"))
  yfin_temp0_int_age_bam_ML <- readRDS(file = here("./output/model output/output Feb 2023/yfin_temp0_int_age_bam_ML.rds"))
  # linear effect of temp and age
  yfin_presurvey_templin_int_age_bam <- readRDS(file = here("./output/model output/ACLIM temps/linear models/yfin_presurvey_templin_int_age_bam.rds"))

  
  # with temp by age effect -- summer avg temp more supported
  AIC(yfin_pretemp_int_age_bam_ML)
  AIC(yfin_yrtemp_int_age_bam_ML)
  AIC(yfin_temp0_int_age_bam_ML)
  AIC(yfin_presurvey_templin_int_age_bam)

  #########################################################################
  # Oxygen models
  #########################################################################
  
  
  # read in all model objects
  file_list <- list.files(path = paste0(here(), ("/output/model output/ACLIM temps/ML output/Aug 2023")))

  prestring <- paste0(here(), ("/output/model output/ACLIM temps/ML output/Aug 2023/"))
  
  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  mod_list <- lapply(mod_names_list, readRDS)
  
	## compare mods
	
  # pcod
  pcod_oxy_list <- mod_list[grep("pcod", names(mod_list))]
	
	lapply(pcod_oxy_list, AICc)
	
	# pollock
	pol_oxy_list <- mod_list[grep("pol", names(mod_list))]
	
	lapply(pol_oxy_list, AICc)
	
	# yfin sole
	yfin_oxy_list <- mod_list[grep("yfin", names(mod_list))]
	
	lapply(yfin_oxy_list, AICc)

	### compare all temp and oxy mods for each species
	
	# pcod
	names(presurvey_boxy_int_fit)
	
	pcod_mod_list <- list(pcod_temp0_int_age_bam_ML, presurvey_boxy_int_fit[[1]])
	lapply(pcod_mod_list, AICc)
	# oxygen better

	# pollock
	names(presurvey_boxy_int_fit)

	pol_mod_list <- list(pol_pretemp_int_age_bam_ML, presurvey_boxy_int_fit[[2]])
	lapply(pol_mod_list, AICc)
	# temp better

	# yfin
	names(presurvey_boxy_int_fit)
	
	yfin_mod_list <- list(yfin_pretemp_int_age_bam_ML, presurvey_boxy_int_fit[[3]])
	lapply(yfin_mod_list, AICc)
	# temp better
	