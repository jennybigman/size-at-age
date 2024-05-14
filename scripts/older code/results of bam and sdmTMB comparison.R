# try to get same output bam vs sdmTMB

	#### BAM ####
	
	pol_bam <- readRDS(
	 				file = here("./output/model output/output Feb 2023/pol_pretemp_int_age_bam.rds"))
 
	
	# output
	pol_bam_sum <- summary(pol_pretemp_int_age_bam)[[4]] %>% as.data.frame()
	
