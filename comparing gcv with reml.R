  pol_temp_age_bam <- readRDS(here("./output/model output/pollock/pol_temp_age_bam.rds"))
  pol_temp_age_bam_gcv <- readRDS(here("./output/model output/pollock/pol_temp_age_bam_gcv.rds"))

  summary(pol_temp_age_bam)
  summary(pol_temp_age_bam_gcv)
  
  variance_comp(pol_temp_age_bam)
  variance_comp(pol_temp_age_bam_gcv)
  
  coef(pol_temp_age_bam)[1:10]
  coef(pol_temp_age_bam_gcv)[1:10]  

  mgcv::gam.vcomp(pol_temp_age_bam)
  variance_comp(pol_temp_age_bam)
  mgcv::gam.vcomp(pol_temp_age_bam_gcv)
  
  AIC(pol_temp_age_bam)
  AIC(pol_temp_age_bam_gcv)
  