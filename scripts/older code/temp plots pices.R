# pices plots best mods

	black_facet_theme <- function(x = 12, y = 14){
			theme_bw() +
 			theme(
 				legend.position = "none", 
 				strip.text = element_text(size = 14, face = "bold", color = "white"),
 				strip.background = element_blank(),
 				panel.grid.major = element_blank(),
  			panel.grid.minor = element_blank(),
  			panel.border = element_rect(fill = NA, color = "grey50"),
 				axis.text=element_text(size = x, colour = "white"),
  			axis.title= element_text(size = y, color = "white"),
  			axis.line = element_line(color = "white"),
  			axis.ticks = element_line(colour = "white"),
 				panel.background = element_rect(fill = "black"),
				panel.grid = element_blank(),
  			plot.background = element_rect(fill = "black", color = "black"))
	}

	# set up file paths
	file_path <- "/output/model output/sdmTMB output/no year RE/"

	# data wrangling

	## order age class
  levels_to_ord <- sort(unique(pcod_dat_trim_10$age_f))
  pcod_dat_trim_10$age_f_ord <- ordered(pcod_dat_trim_10$age_f, levels = c(levels_to_ord))	

  levels_to_ord <- sort(unique(pollock_dat$age_f))
  pollock_dat$age_f_ord <- ordered(pollock_dat$age_f, levels = c(levels_to_ord))	

  levels_to_ord <- sort(unique(yfinsole_dat$age_f))
  yfinsole_dat$age_f_ord <- ordered(yfinsole_dat$age_f, levels = c(levels_to_ord))	

	## temperature

	# pollock

	# read in top temp model
	yrprior_btemp_int_mod_pollock <- readRDS(
		paste0(here(), file_path, "yrprior_btemp_int_mod_pollock", ".rds"))
	
	
	new_dat <- expand_grid(
		age_f_ord = unique(pollock_dat$age_f_ord),
		yrprior_btemp = seq(from = min(pollock_dat$yrprior_btemp),
										 to = max(pollock_dat$yrprior_btemp),
										 length.out = 100))
	
	
	pol_preds <- predict(yrprior_btemp_int_mod_pollock,
									 newdata = new_dat,
									 se_fit = TRUE,
									 re_form = NA)
	
		# high and low CIs
		pol_preds$low <- pol_preds$est + (qnorm(0.025) * pol_preds$est_se)
		pol_preds$high <- pol_preds$est + (qnorm(0.975) * pol_preds$est_se)
		
		pollock_temp_plot <-
			ggplot(pol_preds, aes(yrprior_btemp, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 5) +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			#labs(title = year) +
			black_facet_theme(x = 16, y = 18)

	ggsave(filename = paste0(here(), "/output/plots/pollock_temp_plot_PICES.png"),
				 pollock_temp_plot, width = 10, height = 6)

	# pcod

	# read in model
	presurvey_btemp_no_int_mod_pcod <- readRDS(paste0(here(), 
																							 file_path, 
																							 "presurvey_btemp_no_int_mod_pcod", ".rds"))
	
	new_dat <- expand_grid(
		presurvey_btemp = seq(from = min(pcod_dat$presurvey_btemp),
										 to = max(pcod_dat$presurvey_btemp),
										 length.out = 100))
	
	preds <- predict(presurvey_btemp_no_int_mod_pcod,
									 newdata = new_dat,
									 se_fit = TRUE,
									 re_form = NA)
	
		# high and low CIs
		preds$low <- preds$est + (qnorm(0.025) * preds$est_se)
		preds$high <- preds$est + (qnorm(0.975) * preds$est_se)
		
		pcod_temp_plot <-
			ggplot(preds, aes(presurvey_btemp, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			#facet_wrap(~ age_f_ord, ncol = 5) +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged April - June)") +
			black_theme(x = 16, y = 20)

	ggsave(filename = paste0(here(), "/output/plots/pcod_temp_plot_PICES.png"),
				 pcod_temp_plot, width = 8, height = 6)


	# yfin

	# read in model
	yrprior_btemp_int_mod_yfsole <- readRDS(
		paste0(here(), file_path, "yrprior_btemp_int_mod_yfsole", ".rds"))
	
	new_dat <- expand_grid(
		age_f_ord = unique(yfinsole_dat$age_f_ord),
		yrprior_btemp = seq(from = min(yfinsole_dat$yrprior_btemp),
										 to = max(yfinsole_dat$yrprior_btemp),
										 length.out = 100))
	
	preds <- predict(yrprior_btemp_int_mod_yfsole,
									 newdata = new_dat,
									 se_fit = TRUE,
									 re_form = NA)
	
		# high and low CIs
		preds$low <- preds$est + (qnorm(0.025) * preds$est_se)
		preds$high <- preds$est + (qnorm(0.975) * preds$est_se)
		
	yfin_temp_plot <-
			ggplot(preds, aes(yrprior_btemp, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 5) +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			#labs(title = year) +
			black_facet_theme(x = 16, y = 18)

	ggsave(filename = paste0(here(), "/output/plots/yfin_temp_plot_PICES.png"),
				 yfin_temp_plot, width = 10, height = 6)


## oxygen

	# pollock

	# read in model
	presurvey_boxy_no_int_mod_pollock <- readRDS(
		paste0(here(), file_path, "presurvey_boxy_no_int_mod_pollock", ".rds"))

	new_dat <- expand_grid(
		presurvey_boxy = seq(from = min(yfinsole_dat$presurvey_boxy),
										 to = max(yfinsole_dat$presurvey_boxy),
										 length.out = 100))
	
	preds <- predict(presurvey_boxy_no_int_mod_pollock,
									 newdata = new_dat,
									 se_fit = TRUE,
									 re_form = NA)
	
		# high and low CIs
		preds$low <- preds$est + (qnorm(0.025) * preds$est_se)
		preds$high <- preds$est + (qnorm(0.975) * preds$est_se)
		
	pollock_oxy_plot <-
			ggplot(preds, aes(presurvey_boxy, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			ylab("log scaled weight") +
			xlab("bottom oxygen (averaged June - June)") +
			black_theme(x = 16, y = 20)

	ggsave(filename = paste0(here(), "/output/plots/pollock_oxy_plot_PICES.png"),
				 pollock_oxy_plot, width = 8, height = 6)




	# pcod

	# read in model
	presurvey_boxy_no_int_mod_pcod <- readRDS(paste0(here(), 
																							 file_path, 
																							 "presurvey_boxy_no_int_mod_pcod", ".rds"))
	
	new_dat <- expand_grid(
		presurvey_boxy = seq(from = min(pcod_dat$presurvey_boxy),
										 to = max(pcod_dat$presurvey_boxy),
										 length.out = 100))
	
	preds <- predict(presurvey_boxy_no_int_mod_pcod,
									 newdata = new_dat,
									 se_fit = TRUE,
									 re_form = NA)
	
		# high and low CIs
		preds$low <- preds$est + (qnorm(0.025) * preds$est_se)
		preds$high <- preds$est + (qnorm(0.975) * preds$est_se)
		
		pcod_oxy_plot <-
			ggplot(preds, aes(presurvey_boxy, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			#facet_wrap(~ age_f_ord, ncol = 5) +
			ylab("log scaled weight") +
			xlab("bottom oxygen (averaged April - June)") +
			black_theme(x = 16, y = 20)

	ggsave(filename = paste0(here(), "/output/plots/pcod_oxy_plot_PICES.png"),
				 pcod_oxy_plot, width = 8, height = 6)

	# yfin

	# read in model
	yrprior_boxy_int_mod_yfsole <- readRDS(
		paste0(here(), file_path, "yrprior_boxy_int_mod_yfsole", ".rds"))

	new_dat <- expand_grid(
		age_f_ord = unique(yfinsole_dat$age_f_ord),
		yrprior_boxy = seq(from = min(yfinsole_dat$yrprior_boxy),
										 to = max(yfinsole_dat$yrprior_boxy),
										 length.out = 100))
	
	preds <- predict(yrprior_boxy_int_mod_yfsole,
									 newdata = new_dat,
									 se_fit = TRUE,
									 re_form = NA)
	
		# high and low CIs
		preds$low <- preds$est + (qnorm(0.025) * preds$est_se)
		preds$high <- preds$est + (qnorm(0.975) * preds$est_se)
		
	yfin_oxy_plot <-
			ggplot(preds, aes(yrprior_boxy, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 5) +
			ylab("log scaled weight") +
			xlab("bottom oxygen (averaged June - June)") +
			black_facet_theme(x = 16, y = 18)

	ggsave(filename = paste0(here(), "/output/plots/yfin_oxy_plot_PICES.png"),
				 yfin_oxy_plot, width = 10, height = 6)
