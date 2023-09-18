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
		
		fit_plot <-
			ggplot(fits, aes(presurvey_btemp, est)) +
			geom_line() +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f, ncol = 5) +
			ylab("log scaled weight-at-age") +
			xlab("Bottom temperature (averaged April - June)") +
			labs(title = year) +
			theme_sleek()

		fit_plot
		
	}
	
	# vector of years to plot
	years_plot <- sort(unique(pollock_dat$year))

	# list of plots for each year in vector above
	pol_plot_dfs <- lapply(years_plot, plot_df_func)	
	
	names(pol_plot_dfs) <- years_plot
	
	# save all plots
	
  plot_name <- function(year){
  	paste0(here(), "/output/plots/", year, ".png")
  }
   
  plot_names <- sapply(years_plot, plot_name)
  
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y))
    }
  
	mapply(ggsave_func, x = pol_plot_dfs, y = plot_names)

	
	#### spatial predictions ####
	
	# create frid for mapping
	
	bs_grid <- pollock_dat %>%
		select(X, Y, presurvey_btemp, age_f) %>%
		mutate(year = 1993)
	
	space_preds <- predict(presurvey_btemp_int_pol_nj,
												 newdata = bs_grid)
	
	plot_map <- function(dat, column){
		ggplot(dat, aes(X, Y, fill = {{column}})) +
			geom_raster() +
			coord_fixed()
	}
	
	# predictions of all fixed and random effects
	plot_map(space_preds, est) +
		scale_color_viridis_c(
			na.value = "yellow",
			limits = c(0, quantile(space_preds$est), 0.95)) +
		facet_wrap( ~age_f, ncol = 5) +
		ggtitle("Prediction (fixed and random effects)")
	
	# predictions of just fixed effects
	plot_map(predictions, est_non_rf) +
		scale_fill_viridis_c() +
		ggtitle("Prediction (fixed effects only)")
	
	# predictions of just spatial random effects (not accounted for by fixed effects)
	plot_map(predictions, omega_s) +
		scale_fill_gradient2() +
		ggtitle("Spatial random effects only")
	
	# predictions of just spatiotemporal effects
	plot_map(predictions, epsilon_st) +
		scale_fill_gradient2() +
		facet_wrap( ~year) +
		ggtitle("Spatiotemporal random effects only")