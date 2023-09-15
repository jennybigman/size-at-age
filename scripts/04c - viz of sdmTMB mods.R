# 04c - visualizations of sdmTMB mods

	# plot pollock model with factor-smooth interaction, spatial & spatiotemporal effects, and no jday
	
	# read in model
	presurvey_btemp_int_pol_nj <- readRDS( 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_pol_nj.rds"))
 
	# from scratch
	
	new_dat <- expand.grid(
		age_f = unique(pollock_dat$age_f), 
		presurvey_btemp = seq(from = min(pollock_dat$presurvey_btemp),
													to = max(pollock_dat$presurvey_btemp),
													length.out = 100),
		year = 1993) # what year do I want to show?
	
	
	pred <- predict(presurvey_btemp_int_pol_nj,
									newdata = new_dat,
									se_fit = TRUE,
									re_form = NA)
	
	ggplot(pred, aes(presurvey_btemp, est)) +
		geom_line() +
		facet_wrap(~ age_f)
	
	# for all years
	
	plot_df_func <- function(year){
	
		# create new data to estimate fitted values
		new_dat <- expand.grid(
			age_f = unique(pollock_dat$age_f), 
			presurvey_btemp = 
				seq(from = min(pollock_dat$presurvey_btemp),
						to = max(pollock_dat$presurvey_btemp),
						length.out = 100),
			year = year) 
	
		# data frame of fitted vals with SE
		fits <- predict(presurvey_btemp_int_pol_nj,
										newdata = new_dat,
										se_fit = TRUE,
										re_form = NA)
		
		# high and low CIs
		fits$low <- fits$est + (qnorm(0.025) * fits$est_se)
		fits$high <- fits$est + (qnorm(0.975) * fits$est_se)
		
		fits

	}
	
	# vector of years to plot
	years_plot <- sort(unique(pollock_dat$year))

	# list of plots for each year in vector above
	pol_plot_dfs <- lapply(years_plot, plot_df_func)	
	
	pol_plot_df <- bind_rows(pol_plot_dfs)
	
	# plot by year
	
		fit_plot <-
			ggplot(pol_plot_df, aes(presurvey_btemp, est)) +
			geom_line() +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f, ncol = 5) +
			ylab("log scaled weight-at-age") +
			xlab("Bottom temperature (averaged April - June)") +
			theme_sleek()

			ggsave(fit_plot, file = here("./output/plots/fit_plot.png"))
