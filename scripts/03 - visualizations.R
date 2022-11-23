# 03 - visualizations

	# PCOD #
	load(file = "./output/model output/pocod_temp_int_age_glm.rda")

	# plot predictions
	newdata <- data.frame(
		julian_day = pcod_dat$julian_day,
		haul = pcod_dat$haul,
		year = pcod_dat$year,
		latitude = pcod_dat$latitude,
		longitude = pcod_dat$longitude,
		mean_sum_temp = pcod_dat$mean_sum_temp,
  	age_f = pcod_dat$age_f)
	
	pcod_fits <- predict(pcod_temp_int_age_gam, newdata = newdata, reformula = NA) %>% 
		as_tibble()

	pcod_fits <- bind_cols(pcod_fits, newdata) %>%
		rename(lowCI = "Q2.5",
					 highCI = "Q97.5")
	
	# prelim
	pcod_fits %>%
		ggplot() +
		geom_smooth(aes(mean_sum_temp, Estimate)) +
		facet_wrap(~ age_f)

	# formatted
	pcod_age_wt_fig <- 
		ggplot(pcod_fits) +
		geom_ribbon(aes(x = mean_sum_temp, ymin = lowCI, ymax = highCI)) +
		geom_smooth(aes(mean_sum_temp, Estimate), se = TRUE, color = "white", size = 1) +
		facet_wrap(~ age_f, ncol = 5) +
		scale_x_continuous(
			name = "Summer temperature (˚C)",
			breaks = c(-0.5, 0.5, 1.5),
			labels = c(-0.5, 0.5, 1.5)) +
		ylab(expression(paste("Weight (", log[10], " g)"))) +
		theme(
			strip.text = element_text(color = "lightgrey", size = 10),
			strip.background = element_blank(),
  		axis.text = element_text(size = 8, colour = "lightgrey"),
  		axis.title = element_text(size = 12, color = "lightgrey"),
  		axis.line = element_line(color = "white"),
			panel.border = element_rect(fill = NA, color = "grey50"),
  		axis.ticks = element_line(colour = "white"),
  		panel.background = element_rect(fill = "black"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "black", color = "black"))
	
		ggsave(pcod_age_wt_fig, filename = "./output/plots/pcod_age_wt_fig.png",
				 height = 5, width = 10, units = "in")
	
	ggplot(p, aes(x2, fit)) + geom_smooth_ci(fac)
	
	ggplot(pcod_fits, aes(mean_sum_temp, Estimate)) + geom_smooth_ci(age_f)
	
	# try other way
	pcod_ms <- conditional_smooths(pcod_temp_int_age_gam)
	plot(pcod_ms)
	
	# customize
	pcod_fits <-  conditional_smooths(pcod_temp_int_age_gam)[[1]]
	
	pcod_age_wt_fig <- 
		ggplot(pcod_fits) +
		geom_smooth(aes(x = mean_sum_temp, y = estimate__), color = "white", size = 1) +
		facet_wrap(~ age_f, ncol = 5) +
			scale_x_continuous(
			name = "Summer temperature (˚C)",
			breaks = c(-0.5, 0.5, 1.5),
			labels = c(-0.5, 0.5, 1.5)) +
		ylab(expression(paste("Weight (", log[10], " g)"))) +
		theme(
			strip.text = element_text(color = "lightgrey", size = 14, face = "bold"),
			strip.background = element_blank(),
  		axis.text = element_text(size = 10, colour = "lightgrey"),
  		axis.title = element_text(size = 14, color = "lightgrey"),
  		axis.line = element_line(color = "white"),
			panel.border = element_rect(fill = NA, color = "grey50"),
  		axis.ticks = element_line(colour = "white"),
  		panel.background = element_rect(fill = "black"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "black", color = "black"))
	
		ggsave(pcod_age_wt_fig, filename = "./output/plots/pcod_age_wt_fig.png",
				 height = 5, width = 10, units = "in")


	#### pollock ####
	poll_ms <- conditional_smooths(poll_temp_int_age_gam)
	plot(poll_ms)
	
	# customize
	poll_fits <-  conditional_smooths(poll_temp_int_age_gam)[[1]]
	
	poll_age_wt_fig <- 
		ggplot(poll_fits) +
		geom_smooth(aes(x = mean_sum_temp, y = estimate__), color = "white", size = 1) +
		facet_wrap(~ age_f, ncol = 5) +
			scale_x_continuous(
			name = "Summer temperature (˚C)",
			breaks = c(-0.5, 0.5, 1.5),
			labels = c(-0.5, 0.5, 1.5)) +
		ylab(expression(paste("Weight (", log[10], " g)"))) +
		theme(
			strip.text = element_text(color = "lightgrey", size = 14, face = "bold"),
			strip.background = element_blank(),
  		axis.text = element_text(size = 10, colour = "lightgrey"),
  		axis.title = element_text(size = 14, color = "lightgrey"),
  		axis.line = element_line(color = "white"),
			panel.border = element_rect(fill = NA, color = "grey50"),
  		axis.ticks = element_line(colour = "white"),
  		panel.background = element_rect(fill = "black"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "black", color = "black"))
	
		ggsave(poll_age_wt_fig, filename = "./output/plots/poll_age_wt_fig.png",
				 height = 5, width = 10, units = "in")

	#### yellowfin sole ###
	yfin_ms <- conditional_smooths(yfin_temp_int_age_gam)
	plot(yfin_ms)
	
	# customize
	yfin_fits <-  conditional_smooths(yfin_temp_int_age_gam)[[1]]
	
	yfin_age_wt_fig <- 
		ggplot(yfin_fits) +
		geom_smooth(aes(x = mean_sum_temp, y = estimate__), color = "white", size = 1) +
		facet_wrap(~ age_f, ncol = 5) +
			scale_x_continuous(
			name = "Summer temperature (˚C)",
			breaks = c(-0.5, 0.5, 1.5),
			labels = c(-0.5, 0.5, 1.5)) +
		ylab(expression(paste("Weight (", log[10], " g)"))) +
		theme(
			strip.text = element_text(color = "lightgrey", size = 14, face = "bold"),
			strip.background = element_blank(),
  		axis.text = element_text(size = 10, colour = "lightgrey"),
  		axis.title = element_text(size = 14, color = "lightgrey"),
  		axis.line = element_line(color = "white"),
			panel.border = element_rect(fill = NA, color = "grey50"),
  		axis.ticks = element_line(colour = "white"),
  		panel.background = element_rect(fill = "black"),
			panel.grid = element_blank(),
  		plot.background = element_rect(fill = "black", color = "black"))
	
		ggsave(yfin_age_wt_fig, filename = "./output/plots/yfin_age_wt_fig.png",
				 height = 5, width = 10, units = "in")


		
