# models with k - 3, no year as RE

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
	file_path_all <- "/output/model output/sdmTMB output/no year RE and k/"

	# check residual plots
	
	#### pollock ####
	
	 # 1. age0_boxy_int_mod_pollock
	
	age0_boxy_int_mod_pollock <- read_rds(
		file = paste0(here(), file_path_all, "age0_boxy_int_mod_pollock", ".rds"))
		
	sims <- simulate(age0_boxy_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(age0_boxy_int_mod_pollock)
	
	# 2. yrprior_boxy_int_mod_pollock
	
	yrprior_boxy_int_mod_pollock <- read_rds(
		file = paste0(here(), file_path_all, "yrprior_boxy_int_mod_pollock", ".rds"))
		
	sims <- simulate(yrprior_boxy_int_mod_pollock, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_boxy_int_mod_pollock)

	#### pcod ####
	
	
	#### yfin ####
	yrprior_btemp_no_int_mod_yfsole.rds <- readRDS(
		paste0(here(), file_path_all, "yrprior_btemp_no_int_mod_yfsole", ".rds"))

	sims <- simulate(yrprior_btemp_no_int_mod_yfsole.rds, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(yrprior_btemp_no_int_mod_yfsole.rds)
 
# plots
	
		# pollock

	# read in top temp model
	yrprior_btemp_no_int_mod_pollock <- readRDS(
		paste0(here(), file_path_all, "yrprior_btemp_no_int_mod_pollock", ".rds"))
	
	
	new_dat <- expand_grid(
		#age_f_ord = unique(pollock_dat$age_f_ord),
		yrprior_btemp = seq(from = min(pollock_dat$yrprior_btemp),
										 to = max(pollock_dat$yrprior_btemp),
										 length.out = 100))
	
	
	pol_preds <- predict(yrprior_btemp_no_int_mod_pollock,
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
			#facet_wrap(~ age_f_ord, ncol = 5) +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			#labs(title = year) +
			black_facet_theme(x = 16, y = 18)

	ggsave(filename = paste0(here(), "/output/plots/pollock_temp_plot_PICES.png"),
				 pollock_temp_plot, width = 10, height = 6)

	
	
	#### yfin temp plots ####
	
	# read in top temp model
	yrprior_btemp_int_mod_yfsole <- readRDS(
		paste0(here(), file_path_all, "yrprior_btemp_int_mod_yfsole", ".rds"))
	
	
	new_dat <- expand_grid(
		age_f_ord = unique(yfinsole_dat$age_f_ord),
		yrprior_btemp = seq(from = min(yfinsole_dat$yrprior_btemp),
										 to = max(yfinsole_dat$yrprior_btemp),
										 length.out = 100))
	
	
	yfin_preds <- predict(yrprior_btemp_int_mod_yfsole,
									 newdata = new_dat,
									 se_fit = TRUE,
									 re_form = NA)
	
		# high and low CIs
		yfin_preds$low <- yfin_preds$est + (qnorm(0.025) * yfin_preds$est_se)
		yfin_preds$high <- yfin_preds$est + (qnorm(0.975) * yfin_preds$est_se)
		
		yfin_temp_plot <-
			ggplot(yfin_preds, aes(yrprior_btemp, est)) +
			geom_line(color = "white") +
			geom_ribbon(aes(ymin = low, ymax = high), 
									fill = "lightgrey", alpha = 0.4) +
			facet_wrap(~ age_f_ord, ncol = 5) +
			ylab("log scaled weight") +
			xlab("bottom temperature (averaged June - June)") +
			#labs(title = year) +
			black_facet_theme(x = 16, y = 18)

	ggsave(filename = paste0(here(), "/output/plots/yfin_temp_k.png"),
				 yfin_temp_plot, width = 10, height = 6)
