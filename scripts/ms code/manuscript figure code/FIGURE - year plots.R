# changes in weight-at-age over year

	year_preds <- read.csv(file = paste0(here(), "/data/year_model_preds_Sept2024.csv"))
 
	#s <- year_preds |>
	#	group_by(short_name) |> 
	#	summarise(groups = length(unique(age_f)))
	
	year_preds$age_f <- as.factor(year_preds$age_f)		
	year_preds$year_f <- as.factor(year_preds$year)	
	year_preds$age_label <- paste0("Age ", year_preds$age_f)

	min_yr_all <- min(year_preds$year)
	max_yr_all <- max(year_preds$year)
	
	year_preds <- year_preds |> 
			group_by(short_name, year, year_f, age_f, age) |> 
			summarise(mean_est = mean(est),
   		mean_est_se = mean(est_se))
	
	yrs <- unique(year_preds$year_f)

	yr_len <- length(levels(yrs))
	
	#### arrowtooth flounder ####
	a_df <- year_preds |> filter(short_name == "atooth")
	
	a_yr_len <- length(unique(a_df$year_f))
	
	max_yr <- max(a_df$year)
	
	a_ys <- a_df |>
		ungroup() |>
		filter(year == max_yr) |>
		select(age_f, age, mean_est)
	
	a_plot <- 
		ggplot(a_df, aes(x = year_f, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Year") 
	
	a_max_y <- ggplot_build(a_plot)$layout$panel_scales_y[[1]]$range$range[2]
	
	a_plot <- a_plot +
		annotate("text", x = 2.15, y = a_max_y + 0.05, 
					 label = "Arrowtooth flounder", 
					 color = "black",
					 size = 5) +
		coord_cartesian(xlim = c(1, a_yr_len + 1))
	
	ap_df <- ggplot_build(a_plot)$data[[1]]
	
	cols <- ap_df |>
		select(colour) |>
		distinct_all()

	a_text_df <- tibble(
		year_f = rep(a_yr_len + 0.75, nrow(a_ys)),
		mean_est = a_ys$mean_est,
		age_f = a_ys$age_f,
		lab = paste0("Age ", a_ys$age_f),
		cols = cols)

	a_plot <- 
		a_plot +
		geom_text(data = a_text_df,
							label = a_text_df$lab,
							size = 4) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_text(size = 14),
			axis.title.x = element_text(size = 14),
			#axis.ticks.length.x = unit(.3, "cm"),
			axis.text.x = element_text(size = 12, angle = 90))
							
	#### pacific cod ####
	pc_df <- year_preds |> filter(short_name == "pcod")
	
	pc_yr_len <- length(unique(pc_df$year_f))
	
	max_yr <- max(pc_df$year)
	
	pc_ys <- pc_df |>
		ungroup() |>
		filter(year == max_yr) |>
		select(age_f, age, mean_est)
	
	pc_ys <- pc_ys |>
   mutate(mean_est = 
  	case_when(
  		age == 8 ~ mean_est + 0.05,
  		age < 8 ~ mean_est
))
	
	pc_plot <- 
		ggplot(pc_df, aes(x = year_f, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Year") 

	pc_max_y <- ggplot_build(pc_plot)$layout$panel_scales_y[[1]]$range$range[2]

	pc_plot <- pc_plot +
		annotate("text", x = 2.75, y = pc_max_y, 
			 label = "Pacific cod", 
			 color = "black",
			 size = 5) + 
		coord_cartesian(xlim = c(1, pc_yr_len + 2.5))
	
	pc_df <- ggplot_build(pc_plot)$data[[1]]
	
	cols <- pc_df |>
		select(colour) |>
		distinct_all()
	
	pc_text_df <- tibble(
		year_f = rep(pc_yr_len + 1.75, nrow(pc_ys)),
		mean_est = pc_ys$mean_est,
		age_f = pc_ys$age_f,
		lab = paste0("Age ", pc_ys$age_f),
		cols = cols)

	pc_plot <- 
		pc_plot +
		geom_text(data = pc_text_df,
							label = pc_text_df$lab,
							size = 4) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_blank(),
			axis.title.x = element_text(size = 14),
			#axis.ticks.length.x = unit(.3, "cm"),
			axis.text.x = element_text(size = 12, angle = 90))
							
	
	#### walleye pollock ####
	pl_df <- year_preds |> filter(short_name == "pollock")
	
	pl_yr_len <- length(unique(pl_df$year_f))
	
	max_yr <- max(pl_df$year)
	
	pl_ys <- pl_df |>
		ungroup() |>
		filter(year == max_yr) |>
		select(age_f, age, mean_est)
	
	pl_ys <- pl_ys |>
   mutate(mean_est_adj = 
  	case_when(
  		age >= 12 ~ mean_est + 0.05,
  		age < 3 ~ mean_est + 0.05,
  		age >= 3 & age <= 11 ~ mean_est
))

#  	age 11 - 3 move down
#  		age < 12 & age > 3 ~ mean_est - 0.05,
#  	 	age < 3 ~ mean_est))
#  		#age < 3 ~ mean_est
#
#  		
 # 		,
 # 		age >= 3 & age <= 12 ~ mean_est - 0.05,
 # 		age < 3 ~ mean_est
 # 	))
	
	pl_plot <- 
		ggplot(pl_df, aes(x = year_f, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Year") 
	
	pl_max_y <- ggplot_build(pl_plot)$layout$panel_scales_y[[1]]$range$range[2]
	
	pl_plot <- pl_plot +
		annotate("text", x = 3.5, y = pl_max_y, 
				 label = "Walleye pollock", 
				 color = "black",
				 size = 5) +
		coord_cartesian(xlim = c(1, pl_yr_len + 2))
	
	pl_df <- ggplot_build(pl_plot)$data[[1]]
	
	cols <- pl_df |>
		select(colour) |>
		distinct_all()

	pl_text_df <- tibble(
		year_f = rep(pl_yr_len + 2, nrow(pl_ys)),
		mean_est = pl_ys$mean_est_adj,
		age_f = pl_ys$age_f,
		lab = paste0("Age ", pl_ys$age_f),
		cols = cols)

	pl_plot <- 
		pl_plot +
		geom_text(data = pl_text_df,
							label = pl_text_df$lab,
							size = 5) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.x = element_text(size = 14),
			#axis.ticks.length.x = unit(.3, "cm"),
			axis.text.x = element_text(size = 12, angle = 90))
							
	
	#### yellowfin sole ####
	y_df <- year_preds |> filter(short_name == "yfin")
	
	y_yr_len <- length(unique(y_df$year_f))
	
	max_yr <- max(y_df$year)
	
	y_ys <- y_df |>
		ungroup() |>
		filter(year == max_yr) |>
		select(age_f, age, mean_est)
	
	y_plot <- 
		ggplot(y_df, aes(x = year_f, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Year") 
	
	y_max_y <- ggplot_build(y_plot)$layout$panel_scales_y[[1]]$range$range[2]
	
	y_plot <- y_plot +
		annotate("text", x = 3, y = y_max_y, 
					 label = "Yellowfin sole", 
					 color = "black",
					 size = 5) +
		coord_cartesian(xlim = c(1, y_yr_len + 2))
	
	y_df <- ggplot_build(y_plot)$data[[1]]
	
	cols <- y_df |>
		select(colour) |>
		distinct_all()

	y_text_df <- tibble(
		year_f = rep(y_yr_len + 1.5, nrow(y_ys)),
		mean_est = y_ys$mean_est,
		age_f = y_ys$age_f,
		lab = paste0("Age ", y_ys$age_f),
		cols = cols)

	y_plot <- 
		y_plot +
		geom_text(data = y_text_df,
							label = y_text_df$lab,
							size = 4) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_blank(),
			axis.title.x = element_text(size = 14),
			#axis.ticks.length.x = unit(.3, "cm"),
			axis.text.x = element_text(size = 12, angle = 90))
							
	
			
	
	# put together
	top <- a_plot + pc_plot
	bottom <- pl_plot + y_plot

	Fig1 <- top/bottom
	
	ggsave(file = paste0(here(), "/output/plots for paper/Fig1.png"), 
				 Fig1,
				 height = 10, width = 15)
	
	
	
	
	
	
	
	
	