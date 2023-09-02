 # 03 - visualizations of bam() mods

	
	#### plotting model output ####
	
	# pollock ####
	
	pol_temp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pol_temp_int_age_bam_ACLIM_SEBS.rds"))

	pollock_dat$SEBS_mean_sum_temp <- pollock_dat$presurvey_btemp

		pol_plot <- visreg(pol_temp_int_age_bam_ACLIM_SEBS, "SEBS_mean_sum_temp", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("mean temperature April - June (˚C)") +
		facet_wrap(~ age_f, ncol = 5, scales = "free") +
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
	
	pcod_pretemp_int_age_bam
	
	pcod_plot_df <- visreg(pcod_pretemp_int_age_bam, "presurvey_btemp", by = "age_f",
									 plot = FALSE)
	
	pcod_plot_df <- pcod_plot_df$fit 
	
	keepl <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
	
	pcod_plot_df <- pcod_plot_df %>%
		filter(age_f %in% keepl)
	
	pcod_plot_df$age_f <- droplevels(pcod_plot_df$age_f)
	
	pcod_plot_df$log_wt_std <- (pcod_plot_df$visregFit - (mean(pcod_plot_df$visregFit))/sd(pcod_plot_df$visregFit))

		ggplot(pcod_plot_df) +
		geom_line(aes(presurvey_btemp, log_wt_std, group = age_f, color = age_f)) +
		scale_color_manual(values = colors, name = "age class") +
		scale_x_continuous(
			name = "mean summer temperature (April - June) (˚C)",
			breaks = c(0, 1, 2),
			labels = c(0, 1, 2)
		) +
		scale_y_continuous(
			name = "partial effect on weight-at-age",
			breaks = c(-0.2, 0, 0.2),
			labels = c(-0.2, 0, 0.2)
		) +
		annotate("text", x = -0.2, y = 0.5, label = "Pacific cod") +
		theme(
			strip.text = element_text(color = "black"),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA),
			axis.text=element_text(colour = "black"),
  		axis.title= element_text(color = "black"),
  		axis.line = element_line(color = "black"),
  		axis.ticks = element_line(colour = "black"),
  		panel.background = element_rect(fill = "white"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "white", color = "white")) 
	
		
	
	
	# yfin sole ####
	
	yfin_temp1_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_temp1_int_age_bam_ACLIM_SEBS.rds"))

	yfin_plot <- visreg(yfin_temp1_int_age_bam_ACLIM_SEBS_ML, "temp_firstyr", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temperature (˚C) during\nfirst year of life") +
		facet_wrap(~ age_f, ncol = 9) 

	ggsave(file = here("./output/plots/yfin_plot.png"),
			 yfin_plot)
