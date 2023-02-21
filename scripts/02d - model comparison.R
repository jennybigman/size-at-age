  
#### model comparisons ####

	#### comparing temp and temp by age effect ####

  # these models are fitted with ACLIM SEB temps ####
  
	# summer temps avg April - June annually ####
  
  # pollock
  
   pol_temp_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_temp_age_bam_ACLIM_SEBS_ML.rds"))
   pol_temp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_temp_int_age_bam_ACLIM_SEBS_ML.rds"))

   AIC(pol_temp_age_bam_ACLIM_SEBS_ML)
   AIC(pol_temp_int_age_bam_ACLIM_SEBS_ML)
   
   # pcod
  
   pcod_temp_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_temp_age_bam_ACLIM_SEBS_ML.rds"))
   pcod_temp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_temp_int_age_bam_ACLIM_SEBS_ML.rds"))

   AIC(pcod_temp_age_bam_ACLIM_SEBS_ML)
   AIC(pcod_temp_int_age_bam_ACLIM_SEBS_ML)
   
   # yfin sole
   
   yfin_temp_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_temp_age_bam_ACLIM_SEBS_ML.rds"))
   yfin_temp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_temp_int_age_bam_ACLIM_SEBS_ML.rds"))

   AIC(yfin_temp_age_bam_ACLIM_SEBS_ML)
   AIC(yfin_temp_int_age_bam_ACLIM_SEBS_ML)
 
   
   #### temps avg for all months annually ACLIM ####
	
	# pollock
   
  pol_yrtemp_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
  pol_yrtemp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  
  AIC(pol_yrtemp_age_bam_ACLIM_SEBS_ML)
  AIC(pol_yrtemp_int_age_bam_ACLIM_SEBS_ML)
  
	# pcod #
	
	pcod_yrtemp_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
  pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  
  AIC(pcod_yrtemp_age_bam_ACLIM_SEBS_ML)
  AIC(pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML)
 
 # yellowfin # 
													
	yfin_yrtemp_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_yrtemp_age_bam_ACLIM_SEBS_ML.rds"))
  yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  
  AIC(yfin_yrtemp_age_bam_ACLIM_SEBS_ML)
  AIC(yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML)

  # with REML
  #yfin_yrtemp_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_yrtemp_age_bam_ACLIM_SEBS.rds"))
  #yfin_yrtemp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_yrtemp_int_age_bam_ACLIM_SEBS.rds"))
  
  #AIC(yfin_yrtemp_age_bam_ACLIM_SEBS)
  #AIC(yfin_yrtemp_int_age_bam_ACLIM_SEBS)

  #### compare different temp metrics ####
  
  # pollock 
  
  pol_temp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_temp_int_age_bam_ACLIM_SEBS_ML.rds"))
  pol_yrtemp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  pol_temp1_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_temp1_int_age_bam_ACLIM_SEBS_ML.rds"))
  pol_temp0_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pol_temp0_int_age_bam_ACLIM_SEBS_ML.rds"))

  # with temp by age effect -- summer avg temp more supported
  AIC(pol_temp_int_age_bam_ACLIM_SEBS_ML)
  AIC(pol_yrtemp_int_age_bam_ACLIM_SEBS_ML)
  AIC(pol_temp1_int_age_bam_ACLIM_SEBS_ML)
  AIC(pol_temp0_int_age_bam_ACLIM_SEBS_ML)

    # pcod 
  
  pcod_temp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_temp_int_age_bam_ACLIM_SEBS_ML.rds"))
  pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  pcod_temp1_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_temp1_int_age_bam_ACLIM_SEBS_ML.rds"))
  pcod_temp0_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/pcod_temp0_int_age_bam_ACLIM_SEBS_ML.rds"))

  # with temp by age effect -- temp age1 more supported
  AIC(pcod_temp_int_age_bam_ACLIM_SEBS_ML)
  AIC(pcod_yrtemp_int_age_bam_ACLIM_SEBS_ML)
  AIC(pcod_temp1_int_age_bam_ACLIM_SEBS_ML)
  AIC(pcod_temp0_int_age_bam_ACLIM_SEBS_ML)
  
    # yfin 
  
  yfin_temp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_temp_int_age_bam_ACLIM_SEBS_ML.rds"))
  yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML.rds"))
  yfin_temp1_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_temp1_int_age_bam_ACLIM_SEBS_ML.rds"))
  yfin_temp0_int_age_bam_ACLIM_SEBS_ML <- readRDS(file = here("./output/model output/ACLIM temps/ML output/yfin_temp0_int_age_bam_ACLIM_SEBS_ML.rds"))

    # with temp by age effect -- temp age1 more supported
  AIC(yfin_temp_int_age_bam_ACLIM_SEBS_ML)
  AIC(yfin_yrtemp_int_age_bam_ACLIM_SEBS_ML)
  AIC(yfin_temp1_int_age_bam_ACLIM_SEBS_ML)
  AIC(yfin_temp0_int_age_bam_ACLIM_SEBS_ML)
