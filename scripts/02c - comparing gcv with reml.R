  
#### GCV comparisons ####


# with ROMS temps from Pcod paper ####

	# pollock ####
  pol_temp_age_bam_gcv <- readRDS(here("./output/model output/pollock/pol_temp_age_bam_gcv.rds"))
  pol_temp_int_age_bam_gcv <- readRDS(here("./output/model output/pollock/pol_temp_int_age_bam_gcv.rds"))
  
  summary(pol_temp_age_bam_gcv)
  summary(pol_temp_int_age_bam_gcv)
  
  AIC(pol_temp_age_bam_gcv)
  AIC(pol_temp_int_age_bam_gcv)
  
  # pcod ####
  
  pcod_temp_age_bam_gcv <- readRDS(here("./output/model output/pollock/pcod_temp_age_bam_gcv.rds"))
  pcod_temp_int_age_bam_gcv <- readRDS(here("./output/model output/pollock/pcod_temp_int_age_bam_gcv.rds"))
  
  summary(pcod_temp_age_bam_gcv)
  summary(pcod_temp_int_age_bam_gcv)
  
  AIC(pcod_temp_age_bam_gcv)
  AIC(pcod_temp_int_age_bam_gcv)
  
   
  # yellowfin ####
  
  yfin_temp_age_bam_gcv <- readRDS(here("./output/model output/pollock/yfin_temp_age_bam_gcv.rds"))
  yfin_temp_int_age_bam_gcv <- readRDS(here("./output/model output/pollock/yfin_temp_int_age_bam_gcv.rds"))

  
  AIC(yfin_temp_age_bam_gcv)
  AIC(yfin_temp_int_age_bam_gcv)
  
  #### ACLIM temps ####
  
  # summer temps with SEBS and NEBS avg ####
  
  # pollock
  
   pol_temp_age_bam_ACLIM_gcv <- readRDS(file = here("./output/model output/ACLIM temps/GCV output/pol_temp_age_bam_ACLIM_gcv.rds"))
   pol_temp_int_age_bam_ACLIM_gcv <- readRDS(file = here("./output/model output/ACLIM temps/GCV output/pol_temp_int_age_bam_ACLIM_gcv.rds"))

   AIC(pol_temp_age_bam_ACLIM_gcv)
   AIC(pol_temp_int_age_bam_ACLIM_gcv)
   
   # pcod
  
   pcod_temp_age_bam_ACLIM_gcv <- readRDS(file = here("./output/model output/ACLIM temps/GCV output/pcod_temp_age_bam_ACLIM_gcv.rds"))
   pcod_temp_int_age_bam_ACLIM_gcv <- readRDS(file = here("./output/model output/ACLIM temps/GCV output/pcod_temp_int_age_bam_ACLIM_gcv.rds"))

   AIC(pcod_temp_age_bam_ACLIM_gcv)
   AIC(pcod_temp_int_age_bam_ACLIM_gcv)
   
   # yfin sole #### NEED TO DO ONCE MODELS DONE RUNNING####
   
   yfin_temp_age_bam_ACLIM_gcv <- readRDS(file = here("./output/model output/ACLIM temps/GCV output/yfin_temp_age_bam_ACLIM_gcv.rds"))
   yfin_temp_int_age_bam_ACLIM_gcv <- readRDS(file = here("./output/model output/ACLIM temps/GCV output/yfin_temp_int_age_bam_ACLIM_gcv.rds"))

   AIC(yfin_temp_age_bam_ACLIM_gcv)
   AIC(yfin_temp_int_age_bam_ACLIM_gcv)
   
#### REML comparisons ####

# with ROMS temps from Pcod paper ####

	# pollock ####
  pol_temp_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pol_temp_age_bam.rds"))
  pol_temp_int_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pol_temp_int_age_bam.rds"))
  
  AIC(pol_temp_age_bam)
  AIC(pol_temp_int_age_bam)
  
  # pcod ####
  
  pcod_temp_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pcod_temp_age_bam.rds"))
  pcod_temp_int_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/pcod_temp_int_age_bam.rds"))
  
  AIC(pcod_temp_age_bam)
  AIC(pcod_temp_int_age_bam)
  
  # yellowfin ####
  
  yfin_temp_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/yfin_temp_age_bam.rds"))
  yfin_temp_int_age_bam <- readRDS(here("./output/model output/ROMS temps from Pcod paper/yfin_temp_int_age_bam.rds"))

  AIC(yfin_temp_age_bam)
  AIC(yfin_temp_int_age_bam)
  
  #### ACLIM temps ####
  
  # summer temps with SEBS and NEBS avg ####
  
  # pollock
  
   pol_temp_age_bam_ACLIM <- readRDS(file = here("./output/model output/ACLIM temps/pol_temp_age_bam_ACLIM.rds"))
   pol_temp_int_age_bam_ACLIM <- readRDS(file = here("./output/model output/ACLIM temps/pol_temp_int_age_bam_ACLIM.rds"))

   AIC(pol_temp_age_bam_ACLIM)
   AIC(pol_temp_int_age_bam_ACLIM)
   
   # pcod
  
   pcod_temp_age_bam_ACLIM <- readRDS(file = here("./output/model output/ACLIM temps/pcod_temp_age_bam_ACLIM.rds"))
   pcod_temp_int_age_bam_ACLIM <- readRDS(file = here("./output/model output/ACLIM temps/pcod_temp_int_age_bam_ACLIM.rds"))

   AIC(pcod_temp_age_bam_ACLIM)
   AIC(pcod_temp_int_age_bam_ACLIM)
   
   # yfin sole
   
   yfin_temp_age_bam_ACLIM <- readRDS(file = here("./output/model output/ACLIM temps/yfin_temp_age_bam_ACLIM.rds"))
   yfin_temp_int_age_bam_ACLIM <- readRDS(file = here("./output/model output/ACLIM temps/yfin_temp_int_age_bam_ACLIM.rds"))

   AIC(yfin_temp_age_bam_ACLIM)
   AIC(yfin_temp_int_age_bam_ACLIM)
 
  # summer temps with just SEBS  ####
  
  # pollock
  
   pol_temp_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pol_temp_age_bam_ACLIM_SEBS.rds"))
   pol_temp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pol_temp_int_age_bam_ACLIM_SEBS.rds"))

   AIC(pol_temp_age_bam_ACLIM_SEBS)
   AIC(pol_temp_int_age_bam_ACLIM_SEBS)
   
   # pcod
  
   pcod_temp_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pcod_temp_age_bam_ACLIM_SEBS.rds"))
   pcod_temp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pcod_temp_int_age_bam_ACLIM_SEBS.rds"))

   AIC(pcod_temp_age_bam_ACLIM_SEBS)
   AIC(pcod_temp_int_age_bam_ACLIM_SEBS)
   
   # yfin sole
   
   yfin_temp_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_temp_age_bam_ACLIM_SEBS.rds"))
   yfin_temp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_temp_int_age_bam_ACLIM_SEBS.rds"))

   AIC(yfin_temp_age_bam_ACLIM_SEBS)
   AIC(yfin_temp_int_age_bam_ACLIM_SEBS)
 
   #### yearly temps ACLIM ####
	
	# pollock
   
  pol_yrtemp_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pol_yrtemp_age_bam_ACLIM_SEBS.rds"))
  pol_yrtemp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pol_yrtemp_int_age_bam_ACLIM_SEBS.rds"))
  
  AIC(pol_yrtemp_age_bam_ACLIM_SEBS)
  AIC(pol_yrtemp_int_age_bam_ACLIM_SEBS)
  
	# pcod #
	
	pcod_yrtemp_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pcod_yrtemp_age_bam_ACLIM_SEBS.rds"))
  pcod_yrtemp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pcod_yrtemp_int_age_bam_ACLIM_SEBS.rds"))
  
  AIC(pcod_yrtemp_age_bam_ACLIM_SEBS)
  AIC(pcod_yrtemp_int_age_bam_ACLIM_SEBS)
 
 # yellowfin # 
													
	yfin_yrtemp_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_yrtemp_age_bam_ACLIM_SEBS.rds"))
  yfin_yrtemp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_yrtemp_int_age_bam_ACLIM_SEBS.rds"))
  
  AIC(yfin_yrtemp_age_bam_ACLIM_SEBS)
  AIC(yfin_yrtemp_int_age_bam_ACLIM_SEBS)

  
  #### compare different temp metrics ####
  
  yfin_mods <- list(
  	yfin_temp_int_age_bam,
  	yfin_temp_age_bam_ACLIM,
  	yfin_temp_age_bam_ACLIM_SEBS,
  	yfin_yrtemp_int_age_bam_ACLIM_SEBS)
  
  yfin_AIC <- AIC(yfin_mods)
  
