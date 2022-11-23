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
	
	#### read in Krista's data ####
	lagdat <- read_csv(here("./data/Krista data/lagdat.csv"))
	
	lagdat$cohort <- lagdat$YEAR - lagdat$AGE

	table(lagdat$AGE)
	lagdat <- lagdat[which(lagdat$AGE<16),] #was asked if borrowing info across yrs would allow
	#more ages to be included

	lagdat$AGE <- as.factor(lagdat$AGE)
	lagdat$cohort <- as.factor(lagdat$cohort)
	
	# prelim plots (my addition)
	ggplot(lagdat, aes(x = sst.amj, y = log_sc_weight)) +
		geom_point() +
		facet_wrap(~AGE)

	#### GAMs using gamm4 (package used in paper) ####
	# 1. log, scaled weight at age ~  julian day + random effects of cohort +  haul nested within year
	base_mod_KO <- gamm4(log_sc_weight ~  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  		 s(cohort, bs="re"),
                			 random=~(1|YEAR/HAUL), data=lagdat, REML=TRUE) 
	
	#summary(base_mod_KO$gam)
	#summary(base_mod_KO$mer)	
  #gam.check(base_mod_KO$gam)	
  #AICc(base_mod_KO$mer)

  # 2. log, scaled weight at age ~  temp + julian day + random effects of cohort +  haul nested within year
	temp_mod_KO <- gamm4(log_sc_weight ~  s(sst.amj, k=4) +  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(cohort, bs="re"),
                	 random=~(1|YEAR/HAUL), data=lagdat, REML=TRUE) 
	
	
	#summary(temp_mod_KO$gam)
  #AICc(temp_mod_KO$mer)
	
  # 3. log, scaled weight at age ~  + age*temp + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_KO <- gamm4(log_sc_weight ~  s(sst.amj, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                         s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL) , data=lagdat, REML=TRUE) 
	
	#summary(temp_age_int_mod_KO$gam)
	#summary(temp_age_int_mod_KO$mer)	
  #gam.check(temp_age_int_mod_KO$gam)	
  #AICc(temp_age_int_mod_KO$mer)
 
	# store data in gam obj
	temp_age_int_mod_KO$gam$data <- lagdat

  #### save and load model ouput ####
  
  # save
  saveRDS(base_mod_KO, 
  				file = here("./output/model output/pollock/base_mod_KO.rds"))
  saveRDS(temp_mod_KO, 
  				file = here("./output/model output/pollock/temp_mod_KO.rds"))
  saveRDS(temp_age_int_mod_KO, 
  				file = here("./output/model output/pollock/temp_age_int_mod_KO.rds"))
  
  # load
  base_mod_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/base_mod_KO.rds")
	temp_mod_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_mod_KO.rds")
	temp_age_int_mod_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_age_int_mod_KO.rds")
	
	#### GAMs using brms ####

  # 1. log, scaled weight at age ~  julian day + random effects of cohort +  haul nested within year
  base_mod_brms_KO <- brm(log_sc_weight ~  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  			  (1|cohort) + (1|YEAR/HAUL),
                					data = lagdat, 
                					family = gaussian(),
                					save_all_pars = TRUE,
                					warmup = 1000, iter = 5000,
                					chains = 4, cores = 4) 
  
  saveRDS(base_mod_brms_KO, 
  				file = here("./output/model output/pollock/base_mod_brms_KO.rds"))
  
  # 2. log, scaled weight at age ~  temp + julian day + random effects of cohort +  haul nested within year
	temp_mod_brms_KO <- brm(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + 
												  s(julian, k = 4) + (1|cohort) + (1|YEAR/HAUL),
													data = lagdat,
													family = gaussian(),
                					save_all_pars = TRUE,
                					warmup = 1000, iter = 5000,
                					chains = 4, cores = 4) 
  
  saveRDS(temp_mod_brms_KO, 
  				file = here("./output/model output/pollock/temp_mod_brms_KO.rds"))

  # 3. log, scaled weight at age ~  + age*temp + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_brms_KO <- brm(log_sc_weight ~  s(sst.amj, by=AGE, k=4) + 
																	t2(LONGITUDE, LATITUDE) + 
												  		    s(julian, k = 4) + (1|cohort) + (1|YEAR/HAUL),
																	data = lagdat,
																	family = gaussian(),
                									save_all_pars = TRUE,
                									warmup = 1000, iter = 5000,
                									chains = 4, cores = 4) 
		
  saveRDS(temp_age_int_mod_brms_KO, 
  				file = here("./output/model output/pollock/temp_age_int_mod_brms_KO.rds"))

  # load
	base_mod_brms_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/base_mod_brms_KO.rds")
	temp_mod_brms_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_mod_brms_KO.rds")
	temp_age_int_mod_brms_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/temp_age_int_mod_brms_KO.rds")

  
  # compare models via plotting
  
	
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

	## gamm4 ##
	
	# base mod #

	base_mod_jday_gamm4_plot <- visreg(base_mod_KO$gam, "julian", gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("base model") +
		ylim(-1.26, 0.31) 
	
	#ggplot_build(base_mod_jday_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(base_mod_jday_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	# temp mod #
	
	temp_mod_jday_gamm4_plot <- visreg(temp_mod_KO$gam, "julian", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("temp model") +
		ylim(-1.26, 0.31) 
	
	# temp age int mod #
	
	temp_age_int_jday_gamm4_plot <- visreg(temp_age_int_mod_KO$gam, "julian", gg = TRUE,
																				  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ggtitle("temp age int model") +
		ylim(-1.26, 0.31) 

	#ggplot_build(temp_age_int_jday_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_age_int_jday_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	## brms ##

	# base mod #
	base_mod_brms_jday <- base_mod_brms_plot[[2]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(-1.26, 0.31) 
	
	#ggplot_build(base_mod_brms_jday)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(base_mod_brms_jday)$layout$panel_scales_x[[1]]$range$range

	# temp mod #
	temp_mod_brms_jday <- temp_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(-1.26, 0.31) 
	
	#ggplot_build(temp_mod_brms_jday)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_brms_jday)$layout$panel_scales_x[[1]]$range$range

	# temp age int mod #
		
	temp_age_mod_brms_jday_plot <- temp_age_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		ylim(-1.26, 0.31) 
	
	#ggplot_build(temp_age_mod_brms_jday_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_age_mod_brms_jday_plot)$layout$panel_scales_x[[1]]$range$range

	# plot together
	
	jday_top <-
		 wrap_elements(
		 	grid::textGrob('gamm4')) + base_mod_jday_gamm4_plot + 
		 	temp_mod_jday_gamm4_plot + temp_age_int_jday_gamm4_plot +
		  plot_layout(nrow = 1)

	jday_bot <-
		 wrap_elements(
		 	grid::textGrob('brms')) + base_mod_brms_jday + 
		 	temp_mod_brms_jday + temp_age_mod_brms_jday_plot +
		  plot_layout(nrow = 1)

	jday_KO_text <- jday_top/jday_bot
		
	ggsave(file = here("./output/plots/jday_KO_comparison.pdf"),
				 jday_KO_text)
	
	
	## sst ##
	
	# can only compare temp mods
	
	# gamm4 #
	
	temp_mod_sst_gamm4_plot <- visreg(temp_mod_KO$gam, "sst.amj", gg = TRUE,
																		 partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		labs(title = "temp mod") +
		ylim(-0.87, 0.34) +
		annotate(geom = "text", 
						 x = 2.05, y = 0.34,
						 label = "gamm4",
						 size = 2)
	
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	# brms # 
	
	temp_mod_brms_sst <- temp_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		ylim(-0.87, 0.34) +
		annotate(geom = "text", 
						 x = 2.05, y = 0.34,
						 label = "brms",
						 size = 2)
	
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_x[[1]]$range$range

	sst_KO <- temp_mod_sst_gamm4_plot/temp_mod_brms_sst
		
	ggsave(file = here("./output/plots/sst_KO_comparison.pdf"),
				 sst_KO,
				 width = 2.5, height = 5, units = "in")
	
	
	## sst by age ##
	
	# can only compare temp age int mods
	
	# gamm4 #
	
	temp_age_mod_sst_gamm4_plot <- visreg(temp_age_int_mod_KO$gam, "sst.amj", by = "AGE",
																				gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		labs(title = "temp age int mod: gamm4") +
		facet_wrap(~AGE) +
		ylim(-1.5, 1) +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
	
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_sst_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	# brms # 
	
	temp_age_mod_brms_sst <- temp_age_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		facet_wrap(~AGE) +
		labs(title = "temp age int mod: brms") +
		ylim(-1.5, 1) +
		theme_update(
			strip.text = element_text(size = 5),
			strip.background = element_blank(),
			panel.border = element_rect(color = "black", fill = NA))
	
	
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_y[[1]]$range$range
	#ggplot_build(temp_mod_brms_sst)$layout$panel_scales_x[[1]]$range$range

	sst_age_KO <- temp_age_mod_sst_gamm4_plot + temp_age_mod_brms_sst
		
	ggsave(file = here("./output/plots/sst_age_KO_comparison.pdf"),
				 sst_age_KO)
				 
	