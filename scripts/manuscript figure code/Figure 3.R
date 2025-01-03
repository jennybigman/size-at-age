 # figure for MS

	library(cowplot)
	library(magick)
	
	# load svg's of silhouettes
	 pol <- image_read_svg("https://images.phylopic.org/images/dab8c606-129e-482a-8a57-100fe652a6c8/vector.svg")
	 pcod <- image_read_svg("https://images.phylopic.org/images/bba1800a-dd86-451d-a79b-c5944cfe5231/vector.svg")
	 yfin <- image_read_svg("https://images.phylopic.org/images/4db55c0c-77b6-48f8-b7b6-1e5ff9c73787/vector.svg")
	 atooth <- image_read_svg("https://images.phylopic.org/images/4c2220b9-68a8-4594-8dd8-229643e11c84/vector.svg")
	 
	# load predictions for plotting
	
	# load predictions
	preds_all <- fread(file = paste0(here(), "/data/temp_preds_all.csv"))
	
	# set up colors
	cols <- c("independent fields" = "orange", 
						"shared spatial field" = "#01cdfe",
						"shared spatial and spatiotemporal fields" = "#b967ff")
		
	preds_all$'Model type' <- factor(preds_all$type_name,
																levels=c('independent fields', 
																				 'shared spatial field', 
																				 'shared spatial and spatiotemporal fields'))

	
	# atooth ####
	atooth_preds <- preds_all |> filter(sp == "atooth")	

	a_scales <- list(
		scale_y_continuous(
			breaks = c(1.6, 2.1),
			limits = c(1.22, 2.65)), # age 2
		scale_y_continuous(
			breaks = c(2.4, 3.0),
			limits = c(1.93, 3.3)),
		scale_y_continuous(
			breaks = c(2.5, 2.9),
			limits = c(2.23, 3.1)),
		scale_y_continuous(
			breaks = c(2.65, 2.75),
			limits = c(2.62, 2.77)), # age 5
		scale_y_continuous(
			breaks = c(2.8, 2.85),
			limits = c(2.75, 2.9)),
		scale_y_continuous(
			breaks = c(2.9, 3),
			limits = c(2.83, 3.03)),
		scale_y_continuous(
			breaks = c(3, 3.1),
			limits = c(2.97, 3.16)),
		scale_y_continuous(
			breaks = c(3.10, 3.15),
			limits = c(3.03, 3.21)),
		scale_y_continuous(
			breaks = c(3.1, 3.5),
			limits = c(2.78, 3.75)), # age 10
		scale_y_continuous(
			breaks = c(3.5, 4.5),
			limits = c(2.68, 5.1)),
		scale_y_continuous(
			breaks = c(3.3, 3.4),
			limits = c(3.19, 3.45)))
	
	
	atooth_all_plot  <-
	 ggplot(atooth_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p, color = type_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 6) +
	 scale_color_manual(values = cols) +
	 geom_text(aes(label = age_label), size = 2, 
 	 				x = -Inf, y = Inf, color = "black",
    			hjust = -0.1, vjust = 1.1, check_overlap = T) +
 	 ylab("Predicted mean\n(log) weight (g)") +					
	 xlab("Temperature (˚C)") +
		theme(
			axis.text.y = element_text(margin = margin(0,-.5,0,.5, unit = 'cm')),
   		legend.position = "none",
   		axis.title.x = element_blank(),
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA),
			plot.margin = margin(0.5, 0.2, 0, 0.2, "in")) +
		facetted_pos_scales(y = a_scales) +
	 	coord_cartesian(clip = "off") 
	


	# pcod ####
	pcod_preds <- preds_all |> filter(sp == "pcod")
	
	# set up scales and range
	pc_scales <- list(
		scale_y_continuous(
			limits = c(1.51, 2.1),
			breaks = c(1.7, 2.0)), # age 1
		scale_y_continuous(
			limits = c(2.21, 2.91),
			breaks = c(2.4, 2.7)),
		scale_y_continuous(
			limits = c(2.74, 3.25),
			breaks = c(2.9, 3.1)),
		scale_y_continuous(
			limits = c(3.03, 3.47),
			breaks = c(3.1, 3.4)),
		scale_y_continuous(
			limits = c(3.26, 3.52),
			breaks = c(3.3, 3.5)), # age 5
		scale_y_continuous(
			limits = c(3.38, 3.66),
			breaks = c(3.4, 3.6)),
		scale_y_continuous(
			limits = c(3.45, 3.74),
			breaks = c(3.5, 3.7)),
				scale_y_continuous(
			limits = c(3.63, 3.87),
			breaks = c(3.7, 3.8)),
		scale_y_continuous(
			limits = c(3.56, 3.92),
			breaks = c(3.6, 3.8)),
		scale_y_continuous(
			limits = c(3.54, 3.98),
			breaks = c(3.6, 3.8))) # age 10

	pcod_all_plot  <-
	 ggplot(pcod_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p, color = type_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 6) +
	 scale_color_manual(values = cols) +
	 geom_text(aes(label = age_label), size = 4, 
 	 				x = -Inf, y = Inf, color = "black",
    			hjust = -0.1, vjust = 1.1, check_overlap = T) +
 	 ylab("Predicted mean\n(log) weight (g)") +					
	 xlab("Temperature (˚C)") +
		theme(
   		 		legend.position = "none",
   		axis.title.x = element_blank(),
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA),
			plot.margin = margin(0.5, 0.2, 0, 0.2, "in")) +
		facetted_pos_scales(y = pc_scales) +
	 	coord_cartesian(clip = "off") 
	

	#### pollock ####
	pol_preds <- preds_all |> filter(sp == "pollock")

	pl_scales <- list(
		scale_y_continuous(
			breaks = c(1.0, 1.20),
			limits = c(0.926, 1.3)), # age 1
		scale_y_continuous(
			breaks = c(1.6, 2.0),
			limits = c(1.47, 2.34)),
		scale_y_continuous(
			breaks = c(2.2, 2.4),
			limits = c(2.1, 2.54)),
		scale_y_continuous(
			breaks = c(2.65, 2.75),
			limits = c(2.6, 2.80)),
		scale_y_continuous(
			breaks = c(2.8, 2.9),
			limits = c(2.7, 2.97)), # age 5
		scale_y_continuous(
			breaks = c(2.85, 2.95),
			limits = c(2.82, 2.98)),
		scale_y_continuous(
			breaks = c(2.9, 3.0),
			limits = c(2.88, 3.05)),
		scale_y_continuous(
			breaks = c(2.95, 3.05),
			limits = c(2.92, 3.09)),
		scale_y_continuous(
			breaks = c(3, 3.1),
			limits = c(2.97, 3.15)),
		scale_y_continuous(
			breaks = c(3.05, 3.15),
			limits = c(3.01, 3.23)), # age 10
		scale_y_continuous(
			breaks = c(3.1 ,3.2),
			limits = c(2.99, 3.27)),
		scale_y_continuous(
			breaks = c(3.1 ,3.2),
			limits = c(3.05, 3.27)),
		scale_y_continuous(
			breaks = c(3.1, 3.3),
			limits = c(2.97, 3.42)),
		scale_y_continuous(
			breaks = c(3.25, 3.35),
			limits = c(3.17, 3.4)),
		scale_y_continuous(
			breaks = c(3.3, 3.4),
			limits = c(3.18, 3.48)), # age 15
		scale_y_continuous(
			breaks = c(3.35, 3.45),
			limits = c(3.24, 3.58)),
		scale_y_continuous(
		  breaks = c(3.3, 3.4),
			limits = c(3.17, 3.48)),
		scale_y_continuous(
			breaks = c(3.3, 3.5), 
			limits = c(3.25, 3.54)),
		scale_y_continuous(
			breaks = c(3.3, 3.4),
			limits = c(3.2, 3.5)),
		scale_y_continuous(
		  breaks = c(3.3 ,3.4),
			limits = c(3.14, 3.53))
		)
	
	pol_all_plot  <-
	 ggplot(pol_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p, color = type_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 6) +
	 scale_color_manual(values = cols) +
	 geom_text(aes(label = age_label), size = 4, 
 	 				x = -Inf, y = Inf, color = "black",
    			hjust = -0.1, vjust = 1.1, check_overlap = T) +
 	 ylab("Predicted mean\n(log) weight (g)") +					
	 xlab("Temperature (˚C)") +
		theme(
   		 		legend.position = "none",
   		axis.title.x = element_blank(),
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA),
			plot.margin = margin(0.5, 0.2, 0, 0.2, "in")) +
		facetted_pos_scales(y = pl_scales) +
	 	coord_cartesian(clip = "off") 
	
	# yfin ####

	yfin_preds <- preds_all |> filter(sp == "yfin")

	y_scales <- list(
		scale_y_continuous(
			breaks = c(1.3, 1.6),
			limits = c(1.16, 1.8)),
		scale_y_continuous(
			breaks = c(1.6, 1.9),
			limits = c(1.51, 2.04)),
		scale_y_continuous(
			breaks = c(1.6, 2.1),
			limits = c(1.29, 2.48)), # age 5
		scale_y_continuous(
			breaks = c(1.9, 2.2),
			limits = c(1.7, 2.37)),
		scale_y_continuous(
			breaks = c(2, 2.2),
			limits = c(1.91, 2.28)),
		scale_y_continuous(
			breaks = c(2.0, 2.3),
			limits = c(1.92, 2.5)),
		scale_y_continuous(
			breaks = c(2.15, 2.35),
			limits = c(2.07, 2.45)),
		scale_y_continuous(
			breaks = c(2.25, 2.45),
			limits = c(2.18, 2.6)), # age 10
		scale_y_continuous(
			breaks = c(2.25, 2.45),
			limits = c(2.16, 2.63)),
		scale_y_continuous(
			breaks = c(2.45, 2.55),
			limits = c(2.38, 2.64)),
		scale_y_continuous(
			breaks = c(2.4, 2.6),
			limits = c(2.29, 2.69)),
		scale_y_continuous(
			breaks = c(2.35, 2.65),
			limits = c(2.27, 2.9)),
		scale_y_continuous(
			breaks = c(2.4, 2.7),
			limits = c(2.24, 3)), # age 15 
		scale_y_continuous(
			breaks = c(2.4, 2.7),
			limits = c(2.2, 3)),
		scale_y_continuous(
			breaks = c(2.5, 2.8),
			limits = c(2.35, 3)),
		scale_y_continuous(
			breaks = c(2.5, 2.8),
			limits = c(2.27, 3.1)),
		scale_y_continuous(
			breaks = c(2.4, 2.8),
			limits = c(2.21, 3.1)),
		scale_y_continuous(
			breaks = c(2.5, 2.8),
			limits = c(2.38, 3)), # age 20
		scale_y_continuous(
			breaks = c(2.6, 2.9),
			limits = c(2.43, 3.1)),
		scale_y_continuous(
			breaks = c(2.5, 2.8),
			limits = c(2.21, 3)),
		scale_y_continuous(
			breaks = c(2.5, 2.9),
			limits = c(2.23, 3.2)),
		scale_y_continuous(
			breaks = c(2.6, 2.8),
			limits = c(2.52, 3)),
		scale_y_continuous(
			breaks = c(2.6, 3),
			limits = c(2.41, 3.2)), # age 25
		scale_y_continuous(
			breaks = c(2.6, 2.9),
			limits = c(2.51, 3.1)),
		scale_y_continuous(
			breaks = c(2.6, 2.9),
			limits = c(2.48, 3.05)),
		scale_y_continuous(
			breaks = c(2.7, 3),
			limits = c(2.53, 3.1)),
		scale_y_continuous(
			breaks = c(2.85, 2.95),
			limits = c(2.77, 3.05)),
		scale_y_continuous(
			breaks = c(2.4, 2.9),
			limits = c(2.12, 3.2))
		)
	
	yfin_all_plot  <-
	 ggplot(yfin_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p, color = type_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 5) +
	 scale_color_manual(values = cols) +
	 geom_text(aes(label = age_label), size = 4, 
 	 				x = -Inf, y = Inf, color = "black",
    			hjust = -0.1, vjust = 1.1, check_overlap = T) +
 	 ylab("Predicted mean\n(log) weight (g)") +					
	 xlab("Temperature (˚C)") +
		theme(
   		#legend.text = element_text(size = 12),
   		legend.position = "bottom",
   		legend.background = element_rect(fill = "white", color = NA),
   		legend.title = element_blank(),
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA),
			plot.margin = margin(0.5, 0.2, 0, 0.2, "in")) +
		facetted_pos_scales(y = y_scales) +
	 	coord_cartesian(clip = "off") 
	
	# add silhouettes and labels and put together
	
	atooth_form <-
		ggdraw() +
		draw_plot(atooth_all_plot) +
		draw_text("a)", x = 0.02, y = 0.96, size = 10) + 
	 	draw_image(atooth, x = 0.04, y = 0.905,
	 						 width = 0.1, height = 0.1) +
	 	draw_text("arrowtooth flounder", x = 0.21, y = 0.96, size = 10) 
	
	pcod_form <-
		ggdraw() +
		draw_plot(pcod_all_plot) +
		draw_text("b)", x = 0.02, y = 0.96, size = 10) + 
	 	draw_image(pcod, x = 0.04, y = 0.905,
	 						 width = 0.1, height = 0.1) +
	 	draw_text("Pacific cod", x = 0.21, y = 0.96, size = 10) 
	
	pol_form <-
		ggdraw() +
		draw_plot(pol_all_plot) +
		draw_text("c)", x = 0.02, y = 0.96, size = 10) + 
	 	draw_image(atooth, x = 0.04, y = 0.905,
	 						 width = 0.1, height = 0.1) +
	 	draw_text("walleye pollock", x = 0.21, y = 0.96, size = 10) 
	
	yfin_form <-
		ggdraw() +
		draw_plot(yfin_all_plot) +
		draw_text("a)", x = 0.02, y = 0.96, size = 10) + 
	 	draw_image(atooth, x = 0.04, y = 0.905,
	 						 width = 0.1, height = 0.1) +
	 	draw_text("yellowfin sole", x = 0.21, y = 0.96, size = 10) 
	
	test <- plot_grid(
		atooth_form + theme(legend.position = "none"),
		pcod_form + theme(legend.position = "none"),
		pol_form + theme(legend.position = "none"),
		yfin_form,
		ncol = 1
	)
	
		ggsave(filename = paste0(here(), "/output/plots for paper/TEST.png"),
					 test, height = 11, width = 8.5, units = "in")
	