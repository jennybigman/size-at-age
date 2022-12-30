# testing different formulation of rnadom effects with Krista's models

# code to run Krista's models in Oke et al 2022

	library(here)
	library(tidyverse)
	library(lme4)
	library(readr)
	library(lubridate)
	library(tidymv)
	library(mgcv)
	library(gamm4)
	library(cAIC4)
	library(visreg)
	library(mgcViz)
	library(lme4)
	library(MuMIn)
	library(brms)
  library(broom)
	library(tidybayes)
	library(patchwork)
	
	#### read in Krista's data ####
	lagdat <- read_csv(here("./data/Krista data/lagdat.csv"))
	
	lagdat$cohort <- lagdat$YEAR - lagdat$AGE

	table(lagdat$AGE)
	lagdat <- lagdat[which(lagdat$AGE<16),] #was asked if borrowing info across yrs would allow
	#more ages to be included

	lagdat$AGE <- as.factor(lagdat$AGE)
	lagdat$cohort <- as.factor(lagdat$cohort)
	
	# number of individuals per age
	lagdat_sum <- lagdat %>%
		group_by(AGE) %>%
		summarize(n())
	
	
	# prelim plots (my addition)
	ggplot(lagdat, aes(x = sst.amj, y = log_sc_weight)) +
		geom_point() +
		facet_wrap(~AGE)

	#### GAMs using gamm4 (package used in paper) ####

  # load
  base_mod_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/base_mod_KO.rds")
	temp_mod_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_mod_KO.rds")
	temp_age_int_mod_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_age_int_mod_KO.rds")
	
	#### GAMs using brms ####

  # load
	base_mod_brms_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/base_mod_brms_KO.rds")
	temp_mod_brms_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_mod_brms_KO.rds")
	temp_age_int_mod_brms_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_age_int_mod_brms_KO.rds")

	
	#### GAMs using bam() ####
	
	# 1. log, scaled weight at age ~  julian day + random effects of cohort +  haul nested within year
	base_mod_KO_bam <- bam(log_sc_weight ~  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  		   s(cohort, bs="re") +  s(HAUL, by = YEAR, bs = "re"),
                			   data=lagdat, REML=TRUE) 
	

	# 2. log, scaled weight at age ~  temp + julian day + random effects of cohort +  haul nested within year
	temp_mod_KO_bam <- bam(log_sc_weight ~  s(sst.amj, k=4) +  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  	 s(cohort, bs="re") + s(HAUL, by = YEAR, bs = "re"),
                		 data=lagdat, REML=TRUE) 

	
  # 3. log, scaled weight at age ~  + age*temp + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_KO_bam <- bam(log_sc_weight ~  s(sst.amj, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                        			   s(cohort, bs="re") + s(HAUL, by = YEAR, bs = "re"),
                        			   data=lagdat, REML=TRUE) 
	
  
  #### compare models via plotting ####
  
	# add dat object to all gamm4 objects
	base_mod_KO$gam$data <- lagdat
	temp_mod_KO$gam$data <- lagdat
	temp_age_int_mod_KO$gam$data <- lagdat

	# extract conditional smooths for brms models
	base_mod_ms <- conditional_smooths(base_mod_brms_KO)
	base_mod_brms_plot <- plot(base_mod_ms)

	temp_mod_ms <- conditional_smooths(temp_mod_brms_KO)
	temp_mod_brms_plot <- plot(temp_mod_ms)

	temp_age_mod_ms <- conditional_smooths(temp_age_int_mod_brms_KO)
	temp_age_mod_brms_plot <- plot(temp_age_mod_ms)

	# theme_set
	theme_set(
		theme_classic() +
		theme(
			plot.title = element_text(size = 8),
			axis.title = element_text(size = 6),
			axis.text = element_text(size = 4)))
	
	### julian day ###

	ylims <- c(-2.1, 0.31)
	
	## gamm4 ##
	
	# base mod #

	base_mod_jday_gamm4_plot <- visreg(base_mod_KO$gam, "julian", gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("gamm4") +
		ylim(ylims) 
	
	#ggplot_build(base_mod_jday_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(base_mod_jday_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	# temp mod #
	
	temp_mod_jday_gamm4_plot <- visreg(temp_mod_KO$gam, "julian", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 
	
	# temp age int mod #
	
	temp_age_int_jday_gamm4_plot <- visreg(temp_age_int_mod_KO$gam, "julian", gg = TRUE,
																				  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 

	#ggplot_build(temp_age_int_jday_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_age_int_jday_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	## bam ##
	
	base_mod_jday_bam_plot <- visreg(base_mod_KO_bam, "julian", gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("bam") +
		ylim(ylims) 
	
	#ggplot_build(base_mod_jday_bam_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(base_mod_jday_bam_plot)$layout$panel_scales_x[[1]]$range$range

	# temp mod #
	
	temp_mod_jday_bam_plot <- visreg(temp_mod_KO_bam, "julian", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 
		
	#ggplot_build(temp_mod_jday_bam_plot)$layout$panel_scales_y[[1]]$range$range

	# temp age int mod #
	
	temp_age_int_jday_bam_plot <- visreg(temp_age_int_mod_KO_bam, "julian", gg = TRUE,
																				  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 

	#ggplot_build(temp_age_int_jday_bam_plot)$layout$panel_scales_y[[1]]$range$range

	## brms ##

	# base mod #
	base_mod_brms_jday_plot <- base_mod_brms_plot[[2]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("brms") +
		ylim(ylims) 
	
	#ggplot_build(base_mod_brms_jday)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(base_mod_brms_jday)$layout$panel_scales_x[[1]]$range$range

	# temp mod #
	temp_mod_brms_jday_plot <- temp_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 
	
	#ggplot_build(temp_mod_brms_jday)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_brms_jday)$layout$panel_scales_x[[1]]$range$range

	# temp age int mod #
		
	temp_age_mod_brms_jday_plot <- temp_age_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(ylims) 
	
	#ggplot_build(temp_age_mod_brms_jday_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_age_mod_brms_jday_plot)$layout$panel_scales_x[[1]]$range$range
	
	# plot together
	
	jday_top <-
			wrap_elements(
		 	textGrob(('base model'), gp = gpar(fontsize = 6))) + base_mod_jday_gamm4_plot + 
		 	base_mod_brms_jday_plot + base_mod_jday_bam_plot +
		  plot_layout(nrow = 1)

	jday_mid <-
		 wrap_elements(
		 	grid::textGrob(('temp model'), gp = gpar(fontsize = 6))) + temp_mod_jday_gamm4_plot + 
		 	temp_mod_brms_jday_plot + temp_mod_jday_bam_plot +
		  plot_layout(nrow = 1)
		  	
	jday_bot <-
		 wrap_elements(
		 	grid::textGrob(('temp age\n int model'), gp = gpar(fontsize = 6))) + temp_age_int_jday_gamm4_plot + 
		 	temp_age_mod_brms_jday_plot + temp_age_int_jday_bam_plot +
		  plot_layout(nrow = 1)

	jday_KO_text <- jday_top/jday_mid/jday_bot
		
	ggsave(file = here("./output/plots/jday_KO_comparison_ranef_diffparam.pdf"),
				 jday_KO_text)
	
	## sst ##
	
	ylims <- c(-0.87, 0.34)
	# can only compare temp mods
	
	# gamm4 #
	
	temp_mod_sst_gamm4_plot <- visreg(temp_mod_KO$gam, "sst.amj", gg = TRUE,
																		 partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		labs(title = "temp mod") +
		ylim(ylims) +
		annotate(geom = "text", 
						 x = 2.05, y = 0.34,
						 label = "gamm4",
						 size = 2)
	
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	# bam #
	temp_mod_sst_bam_plot <- visreg(temp_mod_KO_bam, "sst.amj", gg = TRUE,
																		 partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		ylim(ylims) +
		annotate(geom = "text", 
						 x = 2.05, y = 0.34,
						 label = "bam",
						 size = 2)
	
	ggplot_build(temp_mod_sst_bam_plot)$layout$panel_scales_y[[1]]$range$range
	
	# brms # 
	
	temp_mod_brms_sst <- temp_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		ylim(ylims) +
		annotate(geom = "text", 
						 x = 2.05, y = 0.34,
						 label = "brms",
						 size = 2)
	
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_x[[1]]$range$range

	sst_KO <- temp_mod_sst_gamm4_plot + temp_mod_sst_bam_plot + temp_mod_brms_sst
		
	ggsave(file = here("./output/plots/sst_KO_comparison_diffparam.pdf"),
				 sst_KO)
	
	
	## sst by age ##
	ylims <- c(-2, 1)
	
	# can only compare temp age int mods
	
	# gamm4 #
	
	temp_age_mod_sst_gamm4_plot <- visreg(temp_age_int_mod_KO$gam, "sst.amj", by = "AGE",
																				gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		labs(title = "gamm4") +
		facet_wrap(~AGE) +
		ylim(ylims) +
		theme_update(
			strip.text = element_text(size = 2),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
	
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	# bam #
	
	temp_age_mod_sst_bam_plot <- visreg(temp_age_int_mod_KO_bam, "sst.amj", by = "AGE",
																				gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		labs(title = "bam") +
		facet_wrap(~AGE) +
		ylim(ylims) +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
		
	#ggplot_build(temp_age_mod_sst_bam_plot)$layout$panel_scales_y[[1]]$range$range

	# brms # 
	
	temp_age_mod_brms_sst <- temp_age_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		facet_wrap(~AGE) +
		labs(title = "brms") +
		ylim(ylims) +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
	
	
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_x[[1]]$range$range

	sst_age_KO <- temp_age_mod_sst_gamm4_plot + temp_age_mod_sst_bam_plot + temp_age_mod_brms_sst
		
	ggsave(file = here("./output/plots/sst_age_KO_comparison_diffparam.pdf"),
				 sst_age_KO)
				 
	