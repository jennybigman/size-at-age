# comparison between brms and bam 

	# theme_set
	theme_set(
		theme_classic() +
		theme(
			plot.title = element_text(size = 8),
			axis.title = element_text(size = 6),
			axis.text = element_text(size = 4)))
	
	#### pollock ####
	
	# load models 
	
	# brms
	pol_temp_age_gam_brms <- readRDS(file = "./output/model output/poll_temp_age_gam.rds")
	pol_temp_int_age_gam_brms <- readRDS(file = "./output/model output/poll_temp_int_age_gam.rds")
	
	# bam
	pol_temp_age_bam <- readRDS(file = "./output/model output/pol_temp_age_bam.rds")
	pol_temp_int_age_bam <- readRDS(file = "./output/model output/pol_temp_int_age_bam.rda")

	# extract conditional smooths from brmsfit
	pol_temp_mod_ms <- conditional_smooths(pol_temp_age_gam_brms)
	pol_temp_mod_brms_plot <- plot(pol_temp_mod_ms)

	pol_temp_age_mod_ms <- conditional_smooths(pol_temp_int_age_gam_brms)
	pol_temp_age_mod_brms_plot <- plot(pol_temp_age_mod_ms)

	## julian day ##

	ylims <- c(-0.68, 0.041)

	pol_temp_mod_jday_bam_plot <- visreg(pol_temp_age_bam, "julian_day", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("bam") +
		ylim(ylims) 
		
	#ggplot_build(pol_temp_mod_jday_bam_plot)$layout$panel_scales_y[[1]]$range$range

	# temp age int mod #
	
	pol_temp_age_int_jday_bam_plot <- visreg(pol_temp_int_age_bam, "julian_day", gg = TRUE,
																				  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 

	#ggplot_build(pol_temp_age_int_jday_bam_plot)$layout$panel_scales_y[[1]]$range$range

	
	## brms ##

	# temp mod # ##### NOT SURE WHY YAXIS DIFF SCALE
	
	pol_temp_mod_brms_jday_plot <- pol_temp_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("brms") +
		ylim(ylims)
	
	#ggplot_build(pol_temp_mod_brms_jday_plot)$layout$panel_scales_y[[1]]$range$range

	# temp age int mod # ##### NOT SURE WHY YAXIS DIFF SCALE
	pol_temp_age_mod_brms_jday_plot <- pol_temp_age_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 
	
	#ggplot_build(pol_temp_age_mod_brms_jday_plot)$layout$panel_scales_y[[1]]$range$range

	## plot together 
	
	jday_top <-
		 wrap_elements(
		 	grid::textGrob(('temp model'), gp = gpar(fontsize = 6))) + pol_temp_mod_jday_bam_plot + 
		 	pol_temp_mod_brms_jday_plot  +
		  plot_layout(nrow = 1)
		  	
	jday_bot <-
		 wrap_elements(
		 	grid::textGrob(('temp age\n int model'), gp = gpar(fontsize = 6))) + pol_temp_age_int_jday_bam_plot + 
		 	pol_temp_age_mod_brms_jday_plot  +
		  plot_layout(nrow = 1)

	poll_jday <- jday_top/jday_bot
		
	ggsave(file = here("./output/plots/poll_jday_bam_vs_brms.pdf"),
				 poll_jday)

	## temp ##
	
	ylims <- c(-0.38, 0.23)

	pol_temp_mod_temp_bam_plot <- visreg(pol_temp_age_bam, "mean_sum_temp", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temp") +
		ggtitle("bam") +
		ylim(ylims) 
		
	#ggplot_build(pol_temp_mod_temp_bam_plot)$layout$panel_scales_y[[1]]$range$range

	
	## brms ##

	ylims_brms <- c(1.55, 1.85)
	
	# temp mod # ##### NOT SURE WHY YAXIS DIFF SCALE
	pol_temp_mod_brms_temp_plot <- pol_temp_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temp") +
		ggtitle("brms") +
		ylim(ylims) 

	
	#ggplot_build(pol_temp_mod_brms_temp_plot)$layout$panel_scales_y[[1]]$range$range


	## plot together 
	
	poll_mean_temp_plot <- pol_temp_mod_temp_bam_plot + pol_temp_mod_brms_temp_plot 
		
	ggsave(file = here("./output/plots/poll_mean_temp_plot_brms_vs_bam.pdf"),
				 poll_mean_temp_plot)
	

	#### temp by age ####
	
	# bam #
	
	ylims <- c(-1.75, 0.87)
	
	pol_temp_age_mod_temp_bam_plot <- visreg(pol_temp_int_age_bam, "mean_sum_temp", by = "age_f",
																				gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("mean summer temp") +
		labs(title = "bam") +
		facet_wrap(~ age_f, ncol = 5) +
		ylim(ylims) +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
		
	#ggplot_build(pol_temp_age_mod_temp_bam_plot)$layout$panel_scales_y[[1]]$range$range

	# brms # 
	
	pol_temp_age_mod_brms_temp <- pol_temp_age_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("mean summer temp") +
		facet_wrap(~ age_f, ncol = 5) +
		labs(title = "brms") +
		ylim(ylims) +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
		
	#ggplot_build(pol_temp_age_mod_brms_temp)$layout$panel_scales_y[[1]]$range$range

	pol_temp_by_age_plot <- pol_temp_age_mod_temp_bam_plot + pol_temp_age_mod_brms_temp
		
	ggsave(file = here("./output/plots/pol_temp_by_age_plot_bam_vs_brms.pdf"),
				 pol_temp_by_age_plot)
				 
	
	#### pcod ####
	
	# load models 
	
	# bam
	pcod_temp_age_bam <- load(file = "./output/model output/pcod_temp_age_bam.rda")
	pcod_temp_int_age_bam <- load(file = "./output/model output/pcod_temp_int_age_bam.rda")

	# brms ### fix
	pcod_temp_age_gam <- readRDS(file = "./output/model output/pcod_temp_age_gam.rds")
	pcod_temp_int_age_gam <- readRDS(file = "./output/model output/pcod_temp_int_age_gam.rds")

	pcod_temp_mod_ms <- conditional_smooths(pcod_temp_age_gam)
	pcod_temp_mod_brms_plot <- plot(pcod_temp_mod_ms)

	pcod_temp_age_mod_ms <- conditional_smooths(pcod_temp_int_age_gam)
	pcod_temp_age_mod_brms_plot <- plot(pcod_temp_age_mod_ms)

	## julian day ##

	ylims <- c(2.77, 2.87)

	pcod_temp_mod_jday_bam_plot <- visreg(, "julian_day", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("bam") +
		ylim(ylims) 
		
	#ggplot_build(pol_temp_mod_jday_bam_plot)$layout$panel_scales_y[[1]]$range$range

	# temp age int mod #
	
	pol_temp_age_int_jday_bam_plot <- visreg(pol_temp_int_age_bam, "julian_day", gg = TRUE,
																				  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 

	#ggplot_build(pol_temp_age_int_jday_bam_plot)$layout$panel_scales_y[[1]]$range$range

	
	## brms ##

	ylims_brms <- c(-0.08, 0.041)
	
	# temp mod # ##### NOT SURE WHY YAXIS DIFF SCALE
	pol_temp_mod_brms_jday_plot <- poll_temp_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("brms") +
		ylim(ylims_brms)
	
	#ggplot_build(pol_temp_mod_brms_jday_plot)$layout$panel_scales_y[[1]]$range$range

	# temp age int mod # ##### NOT SURE WHY YAXIS DIFF SCALE
	pol_temp_age_mod_brms_jday_plot <- poll_temp_age_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims_brms)
	
	#ggplot_build(pol_temp_age_mod_brms_jday_plot)$layout$panel_scales_y[[1]]$range$range

	## plot together 
	
	jday_top <-
		 wrap_elements(
		 	grid::textGrob(('temp model'), gp = gpar(fontsize = 6))) + pol_temp_mod_jday_bam_plot + 
		 	pol_temp_mod_brms_jday_plot  +
		  plot_layout(nrow = 1)
		  	
	jday_bot <-
		 wrap_elements(
		 	grid::textGrob(('temp age\n int model'), gp = gpar(fontsize = 6))) + pol_temp_age_int_jday_bam_plot + 
		 	pol_temp_age_mod_brms_jday_plot  +
		  plot_layout(nrow = 1)

	poll_jday <- jday_top/jday_bot
		
	ggsave(file = here("./output/plots/poll_jday.pdf"),
				 poll_jday)

	## temp ##
	
	ylims <- c(2.8, 2.94)

	pol_temp_mod_temp_bam_plot <- visreg(pol_temp_age_bam, "mean_sum_temp", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temp") +
		ggtitle("bam") +
		ylim(ylims) 
		
	#ggplot_build(pol_temp_mod_temp_bam_plot)$layout$panel_scales_y[[1]]$range$range

	
	## brms ##

	ylims_brms <- c(-0.07, 0.362)
	
	# temp mod # ##### NOT SURE WHY YAXIS DIFF SCALE
	pol_temp_mod_brms_temp_plot <- poll_temp_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temp") +
		ggtitle("brms")
	
	#ggplot_build(pol_temp_mod_brms_temp_plot)$layout$panel_scales_y[[1]]$range$range


	## plot together 
	
	poll_mean_temp_plot <- pol_temp_mod_temp_bam_plot + pol_temp_mod_brms_temp_plot 
		
	ggsave(file = here("./output/plots/poll_mean_temp_plot.pdf"),
				 poll_mean_temp_plot)
	

	#### temp by age ####
	
		# bam #
	
	poll_temp_age_mod_temp_bam_plot <- visreg(pol_temp_int_age_bam, "mean_sum_temp", by = "age_f",
																				gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("mean summer temp") +
		labs(title = "bam") +
		facet_wrap(~ age_f, ncol = 5) +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
		
	#ggplot_build(temp_age_mod_sst_bam_plot)$layout$panel_scales_y[[1]]$range$range

	# brms # 
	
	poll_temp_age_mod_brms_temp <- poll_temp_age_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("mean summer temp") +
		facet_wrap(~ age_f, ncol = 5) +
		labs(title = "brms") +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
	
	pol_temp_by_age_plot <- poll_temp_age_mod_temp_bam_plot + poll_temp_age_mod_brms_temp
		
	ggsave(file = here("./output/plots/pol_temp_by_age_plot.pdf"),
				 pol_temp_by_age_plot)
				 
	