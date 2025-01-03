# plot predictions from year models

	year_preds <- read.csv(file = paste0(here(), "/data/year_model_preds_Sept2024.csv"))

	year_preds <- year_preds |> 
			group_by(short_name, year, year_f, age_f, age) |> 
			summarise(mean_est = mean(est),
   		mean_est_se = mean(est_se))

	year_preds$age_label <- paste0("Age ", year_preds$age_f)
	year_preds$age_f <- as.factor(year_preds$age)

	#### arrowtooth flounder ####
	a_df <- year_preds |> filter(short_name == "atooth")
	
	a_df$year_f <- as.factor(a_df$year)
	
	a_max_y <- max(a_df$mean_est)

	a_min_yr <- min(a_df$year)
	a_max_yr <- max(a_df$year)
	
	a_len <- length(levels(a_df$year_f))

	a_repel_dat <- a_df |>
		filter(year == a_min_yr) |>
		mutate(age_label = paste0("Age ", age_f))
					
	a_plot <- 
		ggplot(a_df, aes(x = year_f, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = a_repel_dat,
      aes(x = a_len + 1, y = mean_est, label = age_label, color = age_f), size = 7,
			vjust = 0.5,
			segment.color = 'transparent') +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Year") +
		annotate("text", x = 3, y = 2, 
						 label = "Arrowtooth flounder", 
						 color = "black",
						 size = 5) +
		theme_sleek() +
		theme(
			legend.position = "none",
		) 
	
	#### Pacific cod ####
	p_df <- year_preds |> filter(short_name == "pcod")
	
	p_max_y <- max(p_df$mean_est)
	
	p_df$year_f <- as.factor(p_df$year)

	p_len <- length(levels(p_df$year_f))

	p_repel_dat <- p_df |>
		filter(year == p_min_yr) |>
		mutate(age_label = paste0("Age ", age_f))
					
	p_plot <- 
		ggplot(p_df, aes(x = year_f, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
			ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = p_repel_dat,
      aes(x = yr_len + 1, y = mean_est, label = age_label, color = age_f),
			size = 7,
			vjust = 0.5,
			segment.color = 'transparent') +
		ylab("Predicted mean (log) weight (g)") +
		xlab('Year') +
		annotate("text", x = 1, y = p_max_y + 0.1, 
						 label = "Pacific cod", color = "black", size = 5) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_blank()
		) 
	
	#### walleye pollock ####
	pl_df <- year_preds |> filter(short_name == "pollock")
	
	pl_df$year_f <- as.factor(pl_df$year)
	
	pl_max_y <- max(pl_df$mean_est)

	pl_len <- length(levels(pl_df$year_f))

	pl_repel_dat <- pl_df |>
		filter(year == 2000) |>
		mutate(age_label = paste0("Age ", age_f))
					
	pl_plot <- 
		ggplot(pl_df, aes(x = year_f, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = pl_repel_dat,
      aes(x = pl_len + 1, y = mean_est, label = age_label, color = age_f), 
			size = 7,
			vjust = 0.5,
			segment.color = 'transparent') +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Year") +
		annotate("text", x = 1, y = a_max_y + 0.05, 
						 label = "Walleye pollock", 
						 color = "black",
						 size = 5) +
		theme_sleek() +
		theme_sleek() +
		theme(
			legend.position = "none") 
	
	# Yellowfin sole
	y_df <- year_preds |> filter(short_name == "yfin")
	
	y_df$year_f <- as.factor(y_df$year)
	
	y_max_y <- max(y_df$mean_est)

	y_len <- length(levels(y_df$year_f))

	y_repel_dat <- y_df |>
		filter(year == 2000) |>
		mutate(age_label = paste0("Age ", age_f))
			
	y_plot <- 
		ggplot(y_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1.5) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
			ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = y_repel_dat,
      aes(x = y_len + 1, y = mean_est, label = age_label, color = age_f), size = 7,
			vjust = 0.5,
			segment.color = 'transparent') +
		annotate("text", x = 1, y = y_max_y + 0.05, 
						 label = "Yellowfin sole", color = "black", size = 5) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_blank()
		) 
	
	# put together
	top <- a_plot + p_plot
	bottom <- pl_plot + y_plot

	Fig1 <- top/bottom
	
	ggsave(file = paste0(here(), "/output/plots for paper/Fig1.png"), 
				 Fig1,
				 height = 10, width = 15)
	
	
	
	
	
	
	
	
	
	
	################################
	
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
		annotate("text", x = 3, y = a_max_y, 
					 label = "Arrowtooth flounder", 
					 color = "black",
					 size = 5) +
		coord_cartesian(xlim = c(1, a_yr_len + 2))
	
	ap_df <- ggplot_build(a_plot)$data[[1]]
	
	cols <- ap_df |>
		select(colour) |>
		distinct_all()

	a_text_df <- tibble(
		year_f = rep(a_yr_len + 1.5, nrow(a_ys)),
		mean_est = a_ys$mean_est,
		age_f = a_ys$age_f,
		lab = paste0("Age ", a_ys$age_f),
		cols = cols)

	a_plot <- 
		a_plot +
		geom_text(data = a_text_df,
							label = a_text_df$lab,
							size = 5) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_text(size = 14),
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
	
	
	