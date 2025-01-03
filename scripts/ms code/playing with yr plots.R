  plots <- purrr::map(unique(dat_all$short_name), \(sp){

  	
   df <- year_preds %>% filter(short_name == sp)
   name <- unique(df$common_name)

   df <- df |> group_by(year, age_f, age) |> summarise(mean_est = mean(est),
   																				  mean_est_se = mean(est_se))
   num <- length(unique(df$age_f))/3
 	 num <- floor(num)
 	
   p <- 
   	ggplot(df, aes(x = year, y = mean_est)) +
   	geom_pointrange(aes(x = year, y = mean_est,
	 											ymin = mean_est - mean_est_se,
	 											ymax = mean_est + mean_est_se),
	 									size = 0.05) +
   	facet_wrap(~ reorder(age_f, age), scales = "free_y", nrow = num) +
  	#geom_text(x = Inf, y = Inf, aes(label = label), data = f_labels) +
   	ylab("Predicted mean (log) weight (g)") +					
		xlab("Year") +
   	#scale_x_discrete(guide = guide_axis(angle = 90)) +
   	ggtitle(name) +
		theme_sleek() +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 7),
	 		strip.text.x = element_blank(),
   	)
   
   p

   #ggsave(filename = paste0(here(), "/output/year_plots_", sp, ".png"),
	#				 p,
	#				 height = 7, width = 10, units = "in")
	
  
   	
  })
 
  ages <- year_preds |>
  	group_by(short_name, age, age_f) %>%
  	summarise(ages = n())
 

	a_scales <- list(
		scale_y_continuous(
			breaks = c(1.6, 1.8),
			limits = c(1.45, 1.95)), # age 2
		scale_y_continuous(
			breaks = c(2.1, 2.3),
			limits = c(1.95, 2.45)), # age 3
		scale_y_continuous(
			breaks = c(2.35, 2.55),
			limits = c(2.2, 2.7)), # age 4
		scale_y_continuous(
			breaks = c(2.65, 2.85),
			limits = c(2.5, 3)),# age 5
		scale_y_continuous(
			breaks = c(2.8, 3),
			limits = c(2.65, 3.15)),# age 6
		scale_y_continuous(
			breaks = c(2.9, 3.1),
			limits = c(2.8, 3.2))# age 8
		)
	
	a_preds <- year_preds |> filter(short_name == "atooth")
	
	labels <- paste0("Age", " ", unique(a_preds$age_f))
	
	sum <- a_preds |>
		group_by(age_f) |>
		summarise(y_pos = max(est))
	
	f_labels <- data.frame(
	 	age_f = unique(a_preds$age_f), 
	 	label = labels,
	 	x_pos = min(a_preds$year) + 0.5,
	 	y_pos = sum$y_pos - 0.05)
	
		
  a_plot <- plots[[1]]

	a_plot <- a_plot + 
		facetted_pos_scales(y = a_scales) +
  	geom_text(aes(x = x_pos, y = y_pos, label = label), data = f_labels)

	a_plot + 
		facetted_pos_scales(y = a_scales) +
  	geom_text(data = f_labels, aes(x = x_pos, y = y_pos, label = "text"))

	
	pc_plot <- plots[[2]]
	pl_plot <- plots[[3]]
	y_plot <- plots[[4]]
	
	
	# customize axes
	
	
 
	year_plots <- plot1/plot2/plot3/plot4
	
	
	 num <- length(unique(df$age_f))/3
 	 num <- floor(num)
 	 
 	 x_pos <- unique(df$year)[[2]]
 	 y_pos <- max(df$mean_est)
 	 	 
 	 labels <- paste0("Age", " ", unique(df$age_f))

	 f_labels <- data.frame(
	 	age_f = unique(df$age_f), 
	 	label = labels)