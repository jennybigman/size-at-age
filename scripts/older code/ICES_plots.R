# for ICES report

	#### pollock ####
	
	pol_bam <- 	readRDS(file = here("./output/model output/output Feb 2023/pol_pretemp_int_age_bam.rds"))

	pol_plot_df <- (visreg(pol_bam, "presurvey_btemp", by = "age_f", 
												 plot = FALSE))$fit
	
	# colors
	
	pol_plot <-
		ggplot(pol_plot_df) +
		geom_line(aes(presurvey_btemp, visregFit, group = age_f, color = age_f)) +
		scale_color_viridis_d(name = "age class") +
		xlab("mean summer temperature (April - June) (˚C)") +
		ylab("scaled partial effect\non weight-at-age") +
		annotate("text", x = -1.3, y = 1.5, label = "Walleye pollock") +
		theme()
			strip.text = element_text(color = "black"),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA),
			axis.text.y=element_text(colour = "black"),
  		axis.title.y= element_text(color = "black"),
  		axis.line.y = element_line(color = "black"),
  		axis.ticks.y = element_line(colour = "black"),
			axis.text.x=element_blank(),
  		axis.title.x= element_blank(),
  		axis.line.x = element_blank(),
  		axis.ticks.x = element_blank(),
  		panel.background = element_rect(fill = "white"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "white", color = "white")) 
	
	#ggsave(pol_plot, file = here("./output/plots/pol_plot_ICES_REPORT.jpg"),
	#			 height = 5, width = 7, units = "in")
	
		ggplot(pol_plot_df) +
		geom_line(aes(presurvey_btemp, visregFit)) +
			facet_wrap(~ age_f) +
		scale_color_viridis_d(name = "age class")

	#### pcod ####
	
	# bam
	pcod_bam <- readRDS(file = here("./output/model output/output Feb 2023/pcod_pretemp_int_age_bam.rds"))

	pcod_plot_df <- visreg(pcod_pretemp_int_age_bam, "presurvey_btemp", by = "age_f", plot = FALSE)
	
	pcod_plot <-
		ggplot(pcod_plot_df$fit) +
		geom_line(aes(presurvey_btemp, visregFit, group = age_f, color = age_f)) +
		scale_color_viridis_d(name = "age class") +
		xlab("mean summer temperature (April - June) (˚C)") +
		ylab("scaled partial effect\non weight-at-age") +
		annotate("text", x = -1.4, y = -0.5, label = "Pacific cod") +
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
	
	#ggsave(pcod_plot, file = here("./output/plots/pcod_plot_ICES_REPORT.jpg"),
	#			 height = 5, width = 7, units = "in")


	#### yellowfin sole ####
	
	yfin_bam <- readRDS(file = here("./output/model output/output Feb 2023/yfin_pretemp_int_age_bam.rds"))

	yfin_plot_df <- (visreg(yfin_pretemp_int_age_bam,
									"presurvey_btemp", by = "age_f", 
									plot = FALSE, data = yfinsole_dat_trim))$fit

	yfin_plot <-
		ggplot(yfin_plot_df) +
		geom_line(aes(presurvey_btemp, visregFit, group = age_f, color = age_f)) +
		scale_color_viridis_d(name = "age class") +
		xlab("mean summer temperature (April - June) (˚C)") +
		ylab("scaled partial effect\non weight-at-age") +
		annotate("text", x = -1.4, y = 0.4, label = "Yellowfin sole") +
		guides(fill = guide_legend(title = "age class")) +
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
	
	ggsave(yfin_plot, file = here("./output/plots/yfin_plot_ICES_REPORT.jpg"),
				 height = 5, width = 7, units = "in")
	
ggplot(yfin_plot_df) +
		geom_line(aes(presurvey_btemp, visregFit)) +
			facet_wrap(~ age_f) +
		scale_color_viridis_d(name = "age class")
