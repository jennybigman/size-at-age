  
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
  
  
  