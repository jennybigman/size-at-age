	# install and load ggeffects package with sdmTMB extension
	remotes::install_github("seananderson/ggeffects", ref = "sdmTMB", force = TRUE)
	
	library(ggeffects)
	
	# read in model
	presurvey_btemp_int_pol <- readRDS( 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol.rds"))
 

# create df from ggeffects package that predicts values from model
	pol_plot_df <- ggpredict(presurvey_btemp_int_pol, 
													 terms = c("presurvey_btemp [all]", "age_f [all]"))
	
	cols <- colorRampPalette(c("#000073", "#e5e5f2"))
	pal <- cols(10)
	
	# plot directly from ggpredict
	p1 <- plot(pol_plot_df, facet = TRUE) + scale_color_manual(values = pal)
	# only 9 colors built in so need to define colors
	
	ggsave(p1, file = here("./output/plots/ggpredict_ex_direct.png"))

	# plot df using ggplot geom_line()
	p2 <- ggplot(pol_plot_df, aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#ecedee") +
		geom_line() +
		facet_wrap( ~ group) +
		theme_sleek() +
		ylab("log scaled weight-at-age") +
		xlab("Bottom temperature averaged April - June")
	
	ggsave(p2, file = here("./output/plots/ggpredict_ex.png"))
	
	# plot df using ggplot geom_smooth()
	p3 <- ggplot(pol_plot_df, aes(x = x, y = predicted)) +
		#geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#ecedee") +
		geom_smooth(method = "loess") +
		facet_wrap( ~ group) +
		theme_sleek() +
		ylab("log scaled weight-at-age") +
		xlab("Bottom temperature averaged April - June")
	
	ggsave(p3, file = here("./output/plots/ggeom_smooth_w_ggpredict_df_ex.png"))

	
	# try geom_smooth with data not ggpredict df (so not actual model formula or output)
	p3 <- ggplot(pollock_dat, aes(y = log_wt_std, x = presurvey_btemp)) +
		geom_smooth(method = "loess") +
		facet_wrap( ~ age_f) +
		theme_sleek()
	
	ggsave(p3, file = here("./output/plots/ggeom_smooth_noform_ex.png"))

	# try example from https://github.com/pbs-assess/sdmTMB/issues/101
	
	ggpredict(presurvey_btemp_int_pol, c("presurvey_btemp", "age_f")) %>% 
		plot(facet = TRUE) + see::scale_color_flat()
	#same as above
	
	# predict from scratch ######### WORK IN PROGRESS #######
	
	new_dat <- expand.grid(
		age_f = unique(pollock_dat$age_f), 
		presurvey_btemp = seq(from = min(pollock_dat$presurvey_btemp),
													to = max(pollock_dat$presurvey_btemp),
													length.out = 100),
		jday_std = mean(pollock_dat$jday_std))

	
	fits <- predict(presurvey_btemp_int_pol,
									newdata = new_dat,
									re_form = NA,
									se_fit = TRUE)
	
	
	fits$low <- fits$est + (qnorm(0.025) * fits$est_se)
	fits$high <- fits$est + (qnorm(0.975) * fits$est_se)

	
	p6 <- ggplot(fits, aes(x = presurvey_btemp, y = est)) +
		geom_line() +
		geom_ribbon(aes(ymin = low, ymax = high), 
										fill = "lightgrey", alpha = 0.4) +
		facet_wrap( ~ age_f, ncol = 5) +
		ylab("log scaled weight-at-age") +
		xlab("Bottom temperature averaged April - June") +
		theme_sleek()
	
	ggsave(p6, file = here("./output/plots/sdmTMB_scratch.png"))

	##################
	