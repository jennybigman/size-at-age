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
  
	## gamm4 ##
	
	# need to add the data to the gam object first
	
	## base mod ##
	base_mod_KO$gam$data <- lagdat

	# julian day
	base_mod_jday_gamm4_plot <- visreg(base_mod_KO$gam, "julian", gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		labs(subtitle = "gamm4") +
		ylim(-1.25, 0.3) +
		theme_classic()
	
	ggplot_build(base_mod_jday_gamm4_plot)$layout$panel_scales_y[[1]]$range$range
	ggplot_build(base_mod_jday_gamm4_plot)$layout$panel_scales_x[[1]]$range$range

	
	# lat lon -- COME BACK TO THIS
	#base_mod_latlon_gamm4_plot <- visreg(base_mod_KO$gam, "LONGITUDE,LATITUDE", gg = TRUE,
	#																		  partial = FALSE, rug = FALSE) +
	#	ylab("partial effect log\nscaled weight-at-age") +
	#	xlab("julian day") +
	#	theme_classic()
	
	## temp mod ##
	temp_mod_KO$gam$data <- lagdat
	
	# julian day
	temp_mod_jday_gamm4_plot <- visreg(temp_mod_KO$gam, "julian", gg = TRUE,
																		  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		theme_classic()

	# sst
	temp_mod_sst_gamm4_plot <- visreg(temp_mod_KO$gam, "sst.amj", gg = TRUE,
																		 partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		theme_classic()
	
	## temp age int mod ##
	temp_age_int_mod_KO$gam$data <- lagdat
	
	# julian day
	temp_age_int_jday_gamm4_plot <- visreg(temp_age_int_mod_KO$gam, "julian", gg = TRUE,
																				  partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		theme_classic()

	# sst
	temp_mod_sst_gamm4_plot <- visreg(temp_age_int_mod_KO$gam, "sst.amj", by = "AGE", gg = TRUE,
																		 partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		theme_classic()
	
  # for models fitted with gamm4, model object is both a gam and mer, we need the gam
	
	## brms ##
	
	# base mod
	base_mod_ms <- conditional_smooths(base_mod_brms_KO)
	base_mod_brms_plot <- plot(base_mod_ms)
	
	base_mod_brms_jday <- base_mod_brms_plot[[2]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		labs(subtitle = "brms") +
		ylim(-1.25, 0.3) +
		theme_classic()
	
	ggplot_build(base_mod_brms_jday)$layout$panel_scales_y[[1]]$range$range
	ggplot_build(base_mod_brms_jday)$layout$panel_scales_x[[1]]$range$range

		
	base_mod_brms_latlon <- base_mod_brms_plot[[1]] +
		theme_classic()
	
	pp_check(base_mod_brms_KO)
	pp_check(base_mod_brms_KO, type = "ecdf_overlay")
	
	# temp mod
	temp_mod_ms <- conditional_smooths(temp_mod_brms_KO)
	temp_mod_brms_plot <- plot(temp_mod_ms)
	
	temp_mod_brms_jday <- temp_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		theme_classic()
	
	temp_mod_brms_sst <- temp_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		theme_classic()
	
	temp_mod_brms_latlon <- temp_mod_brms_plot[[2]] 

	# brms mod checks
	pp_check(temp_mod_brms_KO)
	pp_check(temp_mod_brms_KO, type = "ecdf_overlay")
	
	
	# temp age int model
	temp_age_mod_ms <- conditional_smooths(temp_age_int_mod_brms_KO)
	temp_age_mod_brms_plot <- plot(temp_age_mod_ms)
	
	temp_age_mod_brms_jday_plot <- temp_age_mod_brms_plot[[3]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("julian day") +
		theme_classic()
	
	temp_age_mod_brms_sst_plot <- temp_age_mod_brms_plot[[1]] +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("SST") +
		theme_classic()
	
	temp_age_mod_brms_latlon_plot <- temp_age_mod_brms_plot[[2]] 
	
	# brms mod checks
	pp_check(temp_age_int_mod_brms_KO)
	pp_check(temp_age_int_mod_brms_KO, type = "ecdf_overlay")

	# put together
	
	# base mod julian day effect
	
	base_mod_jday_plot <- (base_mod_jday_gamm4_plot + base_mod_brms_jday)

	base_mod_jday_plot_form <- base_mod_jday_plot +
	 ggtitle("base model (log_sc_wt ~ jday)") +
	 theme(
	 	plot.title = element_text(hjust = -28))
	
	ggsave(file = here("./output/plots/base_mod_KO_comparison.pdf"),
				 base_mod_jday_plot_form)
	