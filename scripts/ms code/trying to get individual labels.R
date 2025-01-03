		# plotting Figure 1 - changes in size-at-age per year
		year_preds <- read.csv(file = paste0(here(), "/data/year_model_preds_Sept2024.csv"))
	
 
		# atooth
		df <- year_preds %>% filter(short_name == "atooth")
  	name <- unique(df$common_name)

  	df <- df |> group_by(year, age_f, age) |> summarise(mean_est = mean(est),
   																				  mean_est_se = mean(est_se))
  	num <- length(unique(df$age_f))/3
 		num <- floor(num)
 		
 		high_num <- c(1.95, 2.45, 2.7, 3, 3.15, 3.2)
 	
 	 a_scales <- list(
		scale_y_continuous(
			breaks = c(1.6, 1.8),
			limits = c(1.45, high_num[1])), # age 2
		scale_y_continuous(
			breaks = c(2.1, 2.3),
			limits = c(1.95, high_num[2])), # age 3
		scale_y_continuous(
			breaks = c(2.35, 2.55),
			limits = c(2.2, high_num[3])), # age 4
		scale_y_continuous(
			breaks = c(2.65, 2.85),
			limits = c(2.5, high_num[4])),# age 5
		scale_y_continuous(
			breaks = c(2.8, 3),
			limits = c(2.65, high_num[5])),# age 6
		scale_y_continuous(
			breaks = c(2.9, 3.1),
			limits = c(2.8, high_num[6]))# age 8
		)
	
	a_preds <- year_preds |> filter(short_name == "atooth")
	
	labels <- paste0("Age", " ", unique(a_preds$age_f))
	
	sum <- a_preds |>
		group_by(age_f) |>
		summarise(y_pos = max(est))
	
	f_labels <- data.frame(
	 	age = unique(a_preds$age), 
	 	label = labels,
	 	year = min(a_preds$year) + 0.5,
	 	mean_est = high_num)
	
		
   a_plot <- 
   	ggplot(df, aes(x = year, y = mean_est)) +
   	geom_pointrange(aes(x = year, y = mean_est,
	 											ymin = mean_est - mean_est_se,
	 											ymax = mean_est + mean_est_se),
	 									size = 0.05) +
   	facet_wrap(~ age, scales = "free_y", ncol = 3) +
   	#facet_wrap(~ reorder(age_f, age), scales = "free_y", nrow = num) +
   	scale_x_continuous(
			breaks = c(2000, 2015)) +
   	ylab("Predicted mean (log) weight (g)") +					
		xlab("Year") +
   	ggtitle(name) +
		theme_sleek() +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 7),
	 		strip.text.x = element_blank()
   	) +
  	facetted_pos_scales(y = a_scales) +
  	geom_text(data = f_labels, aes(label = label))
	
  	
   # pcod ####
   
	 df <- year_preds %>% filter(short_name == "pcod")
   name <- unique(df$common_name)

   df <- df |> group_by(year, age_f, age) |> summarise(mean_est = mean(est),
   																				  mean_est_se = mean(est_se))
   num <- length(unique(df$age_f))/3
 	 num <- floor(num)
 	
 	 pc_scales <- list(
		scale_y_continuous(
			breaks = c(1.65, 1.95),
			limits = c(1.4, 2.2)), # age 1
		scale_y_continuous(
			breaks = c(2.3, 2.6),
			limits = c(2.1, 2.9)), # age 2
		scale_y_continuous(
			breaks = c(2.7, 3.0),
			limits = c(2.5, 3.3)), # age 3
		scale_y_continuous(
			breaks = c(3.0, 3.3),
			limits = c(2.8, 3.6)),# age 4
		scale_y_continuous(
			breaks = c(3.3, 3.6),
			limits = c(3.1, 3.9)), # age 5
		scale_y_continuous(
			breaks = c(3.55, 3.85),
			limits = c(3.3, 4.1)), # age 6
		scale_y_continuous(
			breaks = c(3.6, 3.9),
			limits = c(3.4, 4.2)), # age 7
		scale_y_continuous(
			breaks = c(3.7, 4.0),
			limits = c(3.5, 4.3)) # age 8
		)
	
	pc_preds <- year_preds |> filter(short_name == "pcod")
	
	labels <- paste0("Age", " ", unique(pc_preds$age_f))
	
	sum <- pc_preds |>
		group_by(age_f) |>
		summarise(y_pos = max(est))
	
	f_labels <- data.frame(
	 	age = unique(pc_preds$age), 
	 	label = labels,
	 	year = min(pc_preds$year) + 0.5,
	 	mean_est = c(2.2, 2.9, 3.3, 3.6, 3.9, 4.1, 4.2, 4.3))
	
		
   pc_plot <- 
   	ggplot(df, aes(x = year, y = mean_est)) +
   	geom_pointrange(aes(x = year, y = mean_est,
	 											ymin = mean_est - mean_est_se,
	 											ymax = mean_est + mean_est_se),
	 									size = 0.05) +
   	facet_wrap(~ age, scales = "free_y", ncol = 3) +
   	#facet_wrap(~ reorder(age_f, age), scales = "free_y", nrow = num) +
   	ylab("Predicted mean (log) weight (g)") +					
		xlab("Year") +
   	scale_x_continuous(
			breaks = c(2000, 2015)) +
   	ggtitle(name) +
		theme_sleek() +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 7),
	 		strip.text.x = element_blank(),
   	) +
  	facetted_pos_scales(y = pc_scales) +
  	geom_text(data = f_labels, aes(label = label))

   
  # pollock
   
   df <- year_preds %>% filter(short_name == "pollock")
   name <- unique(df$common_name)

   df <- df |> group_by(year, age_f, age) |> summarise(mean_est = mean(est),
   																				  mean_est_se = mean(est_se))
   num <- length(unique(df$age_f))/3
 	 num <- floor(num)
 	
 	 pl_scales <- list(
		scale_y_continuous(
			breaks = c(1.05, 1.25),
			limits = c(0.85, 1.45)), # age 1
		scale_y_continuous(
			breaks = c(1.8, 2.0),
			limits = c(1.6, 2.2)), # age 2
		scale_y_continuous(
			breaks = c(2.2, 2.4),
			limits = c(2, 2.6)), # age 3
		scale_y_continuous(
			breaks = c(2.6, 2.8),
			limits = c(2.4, 3)),# age 4
		scale_y_continuous(
			breaks = c(2.8, 3),
			limits = c(2.6, 3.2)), # age 5
		scale_y_continuous(
			breaks = c(2.85, 3.05),
			limits = c(2.65, 3.25)), # age 6
		scale_y_continuous(
			breaks = c(2.95, 3.15),
			limits = c(2.75, 3.35)), # age 7
		scale_y_continuous(
			breaks = c(2.95, 3.15),
			limits = c(2.75, 3.35)), # age 8
		scale_y_continuous(
			breaks = c(3.1, 3.3),
			limits = c(2.9, 3.5)), # age 9
		scale_y_continuous(
			breaks = c(3.1, 3.3),
			limits = c(2.9, 3.5)), # age 10
		scale_y_continuous(
			breaks = c(3.1, 3.3),
			limits = c(2.9, 3.5)), # age 11
		scale_y_continuous(
			breaks = c(3.05, 3.25),
			limits = c(2.85, 3.45)), # age 12
		scale_y_continuous(
			breaks = c(3.05, 3.25),
			limits = c(2.85, 3.45)), # age 13
		scale_y_continuous(
			breaks = c(3.1, 3.3),
			limits = c(2.9, 3.5)) # age 14
		)
	
	pl_preds <- year_preds |> filter(short_name == "pollock")
	
	labels <- paste0("Age", " ", unique(pl_preds$age_f))
	
	sum <- pl_preds |>
		group_by(age_f) |>
		summarise(y_pos = max(est))
	
	f_labels <- data.frame(
	 	age = unique(pl_preds$age), 
	 	label = labels,
	 	year = min(pl_preds$year) + 0.5,
	 	mean_est = c(1.45, 2.2, 2.6, 3, 3.2, 3.25, 3.35, 3.35, 3.5, 3.5, 3.5, 3.45, 3.45, 3.5))
	
		
   pl_plot <- 
   	ggplot(df, aes(x = year, y = mean_est)) +
   	geom_pointrange(aes(x = year, y = mean_est,
	 											ymin = mean_est - mean_est_se,
	 											ymax = mean_est + mean_est_se),
	 									size = 0.05) +
   	facet_wrap(~ age, scales = "free_y", ncol = 3) +
   	#facet_wrap(~ reorder(age_f, age), scales = "free_y", nrow = num) +
   	ylab("Predicted mean (log) weight (g)") +					
		xlab("Year") +
   	scale_x_continuous(
			breaks = c(2000, 2015)) +
   	ggtitle(name) +
		theme_sleek() +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 7),
	 		strip.text.x = element_blank(),
   	) +
  	facetted_pos_scales(y = pl_scales) +
  	geom_text(data = f_labels, aes(label = label))

   #### yfin ####
   
   df <- year_preds %>% filter(short_name == "yfin")
   name <- unique(df$common_name)

   df <- df |> group_by(year, age_f, age) |> summarise(mean_est = mean(est),
   																				  mean_est_se = mean(est_se))
   num <- length(unique(df$age_f))/3
 	 num <- floor(num)
 	
 	 y_scales <- list(
		scale_y_continuous(
			breaks = c(0.95, 1.35),
			limits = c(0.75, 1.55)), # age 3
		scale_y_continuous(
			breaks = c(1.3, 1.6),
			limits = c(1.1, 1.8)), # age 4
		scale_y_continuous(
			breaks = c(1.6, 1.9),
			limits = c(1.4, 2.1)), # age 5
		scale_y_continuous(
			breaks = c(1.85, 2.15),
			limits = c(1.65, 2.35)),# age 6
		scale_y_continuous(
			breaks = c(2.05, 2.35),
			limits = c(1.85, 2.55)), # age 7
		scale_y_continuous(
			breaks = c(2.15, 2.45),
			limits = c(1.95, 2.65)), # age 8
		scale_y_continuous(
			breaks = c(2.4, 2.7),
			limits = c(2.2, 2.9)), # age 9
		scale_y_continuous(
			breaks = c(2.4, 2.7),
			limits = c(2.2, 2.9)), # age 10
		scale_y_continuous(
			breaks = c(2.4, 2.7),
			limits = c(2.25, 2.85)), # age 11
		scale_y_continuous(
			breaks = c(2.75, 3.05),
			limits = c(2.55, 3.25)) # age 28
		)
	
	y_preds <- year_preds |> filter(short_name == "yfin")
	
	labels <- paste0("Age", " ", unique(y_preds$age_f))
	
	sum <- y_preds |>
		group_by(age_f) |>
		summarise(y_pos = max(est))
	
	f_labels <- data.frame(
	 	age = unique(y_preds$age), 
	 	label = labels,
	 	year = min(y_preds$year) + 0.5,
	 	mean_est = c(1.55, 1.8, 2.1, 2.35, 2.55, 2.65, 2.9, 2.9, 2.85, 3.25))
	
	y_plot <- 
   	ggplot(df, aes(x = year, y = mean_est)) +
   	geom_pointrange(aes(x = year, y = mean_est,
	 											ymin = mean_est - mean_est_se,
	 											ymax = mean_est + mean_est_se),
	 									size = 0.05) +
   	facet_wrap(~ age, scales = "free_y", ncol = 3) +
   	#facet_wrap(~ reorder(age_f, age), scales = "free_y", nrow = num) +
   	ylab("Predicted mean (log) weight (g)") +					
		xlab("Year") +
   	ggtitle(name) +
		theme_sleek() +
		scale_x_continuous(
			breaks = c(2000, 2015)) +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 7),
	 		strip.text.x = element_blank(),
   	) +
  	facetted_pos_scales(y = y_scales) +
  	geom_text(data = f_labels, aes(label = label))

   
	#### plot together ####
	
	year_plot <- 
		a_plot/pc_plot/pl_plot/y_plot
	
	ggsave(file = "output/plots for paper/year_plot.png", year_plot,
				 height = 8, width = 12, units = "in")
	
	# try with the egg package
	p <- egg::ggarrange(a_plot, pc_plot, pl_plot, y_plot)
	
	ggsave(file = "output/plots for paper/WAA_yr.png", p,
				 height = 8, width = 10, units = "in")
