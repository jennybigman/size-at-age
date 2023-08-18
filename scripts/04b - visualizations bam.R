 # 03 - visualizations of bam() mods

	
	#### plotting model output ####
	
	# pollock ####
	
	pol_temp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pol_temp_int_age_bam_ACLIM_SEBS.rds"))

	pol_plot <- visreg(pol_temp_int_age_bam_ACLIM_SEBS, "SEBS_mean_sum_temp", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("mean temperature April - June (˚C)") +
		facet_wrap(~ age_f, ncol = 5) +
		theme_classic() 

	ggsave(file = here("./output/plots/pol_plot2.png"),
			 pol_plot)

		
	# pcod ####
	
	pcod_temp1_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pcod_temp1_int_age_bam_ACLIM_SEBS.rds"))

	pcod_plot <- visreg(pcod_temp1_int_age_bam_ACLIM_SEBS_ML, "temp_firstyr", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temperature (˚C) during\nfirst year of life") +
		facet_wrap(~ age_f, ncol = 6) 

	ggsave(file = here("./output/plots/pcod_plot.png"),
			 pcod_plot)
	
	# yfin sole ####
	
	yfin_temp1_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_temp1_int_age_bam_ACLIM_SEBS.rds"))

	yfin_plot <- visreg(yfin_temp1_int_age_bam_ACLIM_SEBS_ML, "temp_firstyr", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temperature (˚C) during\nfirst year of life") +
		facet_wrap(~ age_f, ncol = 9) 

	ggsave(file = here("./output/plots/yfin_plot.png"),
			 yfin_plot)
