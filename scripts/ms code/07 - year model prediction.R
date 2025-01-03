# 07 - plotting size-at-age year 

	file_path <- paste0(here(), "/output/model output/sdmTMB output/year models/")

	# load mods
	file_list <- list.files(path = file_path)[-42]
	file_list <- file_list[-42]
	
	mod_names <- str_remove(file_list, ".rds")

	mod_file_list <- paste0(file_path, file_list)
	
	mods <- purrr::map(mod_file_list, read_rds)

  s <- purrr::map(mods, \(mod){
		
		sanity(mod)
		
	})
  
  
  # remove those that didn't converge
	s_df <- purrr::map2_dfr(mods, mod_names, \(mod, name) {
		
		s <- sanity(mod)
		
			hess    <- s$hessian_ok 
			eigen   <- s$eigen_values_ok            
			nl      <- s$nlminb_ok       		         
			range 	<- s$range_ok       
			#se_na 	<- s$se_na_ok         
			sigma 	<- s$sigmas_ok        
		
	
			#df <- tibble(name, hess, eigen, nl, range, se_na, sigma)
			df <- tibble(name, hess, eigen, nl, range, sigma)

			df
		
	})

	drop <- s_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- drop$name
	
	mod_list_keep <- s_df %>% 
		filter(name %!in% drop)
	
	mod_list_keep <- mod_list_keep$name

  # add .rds
	mod_keep <- paste0(mod_list_keep, ".rds")
	
	mods <- purrr::map(paste0(file_path, mod_keep), readRDS)

  # predictions
  year_preds <- purrr::map_dfr(mods, \(mod){
  	
  	dat <- mod$data
  	sp <- unique(dat$short_name)
  	age <- unique(dat$age_f)
  	
  	p <- predict(mod, se_fit = TRUE)
  	
  	p
  	
  })
  
  
  write.csv(year_preds, file = paste0(here(), "/data/year_model_preds_Sept2024.csv"))
  
	year_preds <- read.csv(file = paste0(here(), "/data/year_model_preds_Sept2024.csv"))
 
	#s <- year_preds |>
	#	group_by(short_name) |> 
	#	summarise(groups = length(unique(age_f)))
	
	year_preds$age_f <- as.factor(year_preds$age_f)		
	year_preds$year_f <- as.factor(year_preds$year)	
	a_df$age_label <- paste0("Age ", a_df$age_f)

	min_yr_all <- min(year_preds$year)
	max_yr_all <- max(year_preds$year)
	
	year_preds <- year_preds |> 
			group_by(short_name, year, year_f, age_f, age) |> 
			summarise(mean_est = mean(est),
   		mean_est_se = mean(est_se))

	
	#year_preds2 <- year_preds |> 
	#	mutate(bt_est = 10^est) |>
	#	group_by(short_name, year, age_f, age) |> 
	#	summarise(
	#		mean_est = mean(est),
	#		mean_est_bt = mean(bt_est),
	#		mean_se = sd(est)/sqrt(n()),
	#		mean_se_bt = sd(bt_est)/sqrt(n())
	#	)
	#

	#### arrowtooth flounder ####
	a_df <- year_preds |> filter(short_name == "atooth")
	
	a_max_y <- max(a_df$mean_est)
	
	a_min_yr <- min(a_df$year)
	a_max_yr <- max(a_df$year)

	a_repel_dat <- a_df |>
		filter(year == a_max_yr) |>
		mutate(age_label = paste0("Age ", age_f))
					
	a_plot <- 
		ggplot(a_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = a_repel_dat,
      aes(x = year, y = mean_est, label = age_label, color = age_f),
			vjust = 0.5, xlim = c(2023, 2023)) +
		xlim(min_yr_all, max_yr_all + 4) +
			ylab("Predicted mean (log) weight (g)") +
		annotate("text", x = 1987 + 4, y = a_max_y + 0.2, label = "Arrowtooth flounder", color = "black") +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.x = element_blank(),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank()
		) 
	
	#### Pacific cod ####
	p_df <- year_preds |> filter(short_name == "pcod")
	
	p_max_y <- max(p_df$mean_est)
	
	p_min_yr <- min(p_df$year)
	p_max_yr <- max(p_df$year)

	p_repel_dat <- p_df |>
		filter(year == p_max_yr) |>
		mutate(age_label = paste0("Age ", age_f))
					
	p_plot <- 
		ggplot(p_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = p_repel_dat,
      aes(x = year, y = mean_est, label = age_label, color = age_f),
			vjust = 0.5, hjust = 0, xlim = c(2024, 2024),
			max.overlaps = Inf) +
		xlim(min_yr_all, max_yr_all + 4) +
			ylab("Predicted mean (log) weight (g)") +
		annotate("text", x = 1987 + 1.5, y = p_max_y + 0.2, 
						 label = "Pacific cod", color = "black") +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title = element_blank(),
			axis.text = element_blank(),
			axis.ticks = element_blank()
		) 
	
	#### walleye pollock ####
	pl_df <- year_preds |> filter(short_name == "pollock")
	
	pl_max_y <- max(pl_df$mean_est)
	
	pl_min_yr <- min(pl_df$year)
	pl_max_yr <- max(pl_df$year)

	pl_repel_dat <- pl_df |>
		filter(year == pl_min_yr) |>
		mutate(age_label = paste0("Age ", age_f))
				
	pl_plot <- 
		ggplot(pl_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = pl_repel_dat,
      aes(x = year, y = mean_est, label = age_label, color = age_f),
			vjust = 0.5, xlim = c(1991, 1991),
			max.overlaps = Inf) +
		xlim(min_yr_all, max_yr_all + 4) +
			ylab("Predicted mean (log) weight (g)") +
		annotate("text", x = 1987 + 4, y = pl_max_y + 0.2, 
						 label = "Walleye pollock", color = "black") +
		theme_sleek() +
		theme(
			legend.position = "none"
		) 
	
	# Yellowfin sole
	y_df <- year_preds |> filter(short_name == "yfin")
	
	y_max_y <- max(y_df$mean_est)
	
	y_max_yr <- max(y_df$year)
	y_min_yr <- min(y_df$year)
	
	y_repel_dat <- y_df |>
		filter(year == y_max_yr) |>
		mutate(age_label = paste0("Age ", age_f))
					
	y_plot <- 
		ggplot(y_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(alpha = 0.6, size = 1) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se),
   										 alpha = 0.6) +
		scale_color_viridis_d(option = "turbo") +
		ggrepel::geom_text_repel(
			inherit.aes = FALSE, 
      data = y_repel_dat,
      aes(x = year, y = mean_est, label = age_label, color = age_f),
			vjust = 0.5, xlim = c(2023, 2023),
			max.overlaps = Inf) +
		xlim(min_yr_all, max_yr_all + 4) +
			ylab("Predicted mean (log) weight (g)") +
		annotate("text", x = 1987 + 4, y = y_max_y + 0.2, 
						 label = "Yellowfin sole", color = "black") +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank()
		) 
	
	# put together
	top <- a_plot + p_plot
	bottom <- pl_plot + y_plot

	Fig1 <- top/bottom
	
	ggsave(file = paste0(here(), "/output/plots for paper/Fig1.png"), 
				 Fig1,
				 height = 6, width = 12)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	#### Pacific cod ####
	
	pc_df <- year_preds |> filter(short_name == "pcod")
	
	pc_df <- pc_df |> 
			group_by(year, age_f, age) |> 
			summarise(mean_est = mean(est),
   		mean_est_se = mean(est_se))
		
	pc_df$year_f <- as.factor(pc_df$year)

	max_yr <- max(pc_df$year)
	min_yr <- min(pc_df$year)
		
	pc_p <- 
		ggplot(pc_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
    geom_point(aes(alpha = 0.6), size = 1) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se,
   										 alpha = 0.6)) +
		scale_color_viridis_d(option = "turbo") +
		xlim(min_yr_all, max_yr_all) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title = element_blank(),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank()
		)
	
	pcp_df <- ggplot_build(pc_p)$data[[1]]
	
	cols <- pcp_df |>
		select(colour) |>
		distinct_all()
	
	max_y <- max(pcp_df$y)
	
	pc_p <- 
		pc_p +
		ggtitle("Pacific cod") +
		#annotate("text", x = min_yr + 7, y = max_y, label = "Pacific cod", color = "black") +
		annotate("text", x = 1987 + 1, y = 1.8, label = "Age 1", color = cols[1,1]) +
		annotate("text", x = 1987 + 1, y = 1.8, label = "Age 2", color = cols[2,1]) +
		annotate("text", x = 1987 + 1, y = 2.0, label = "Age 3", color = cols[3,1]) +
		annotate("text", x = 1987 + 1, y = 2.4, label = "Age 4", color = cols[4,1]) +
		annotate("text", x = 1987 + 1, y = 2.6, label = "Age 5", color = cols[5,1]) +
		annotate("text", x = 1987 + 1, y = 2.8, label = "Age 6", color = cols[6,1]) +
		annotate("text", x = 1987 + 1, y = 1.8, label = "Age 7", color = cols[7,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 8", color = cols[8,1]) +
		theme()
	
	top <- a_p + pc_p

	#### walleye pollock ####
	
	pl_df <- year_preds |> filter(short_name == "pollock")
	
	pl_df <- pl_df |> 
			group_by(year, age_f, age) |> 
			summarise(mean_est = mean(est),
   		mean_est_se = mean(est_se))
		
	pl_df$year_f <- as.factor(pl_df$year)

	max_yr <- max(pl_df$year)
	min_yr <- min(pl_df$year)
		
	pl_p <- 
		ggplot(pl_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(aes(alpha = 0.6), size = 1) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se,
   										 alpha = 0.6)) +
		scale_color_viridis_d(option = "turbo") +
		xlim(min_yr_all, max_yr_all) +
		theme_sleek() +
		theme(
			legend.position = "none"
		)
	
	pl_df <- ggplot_build(pl_p)$data[[1]]
	
	cols <- pl_df |>
		select(colour) |>
		distinct_all()
	
	max_y <- max(pl_df$y)
	
	pl_p <- 
		pl_p +
		ggtitle("Walleye pollock") + 
		ylab("Predicted mean (log) weight (g)") +
		#annotate("text", x = min_yr + 7, y = max_y, label = "Walleye pollock", color = "black") +
		annotate("text", x = 1987 + 1, y = 1.8, label = "Age 1", color = cols[1,1]) +
		annotate("text", x = 1987 + 1, y = 1.8, label = "Age 2", color = cols[2,1]) +
		annotate("text", x = 1987 + 1, y = 2.0, label = "Age 3", color = cols[3,1]) +
		annotate("text", x = 1987 + 1, y = 2.4, label = "Age 4", color = cols[4,1]) +
		annotate("text", x = 1987 + 1, y = 2.6, label = "Age 5", color = cols[5,1]) +
		annotate("text", x = 1987 + 1, y = 2.8, label = "Age 6", color = cols[6,1]) +
		annotate("text", x = 1987 + 1, y = 1.8, label = "Age 7", color = cols[7,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 8", color = cols[8,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 9", color = cols[9,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 10", color = cols[10,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 11", color = cols[11,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 12", color = cols[12,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 13", color = cols[13,1]) +
		annotate("text", x = 1987 + 1, y = 3.2, label = "Age 14", color = cols[14,1]) 
	
	#### yellowfin sole ####
	
	y_df <- year_preds |> filter(short_name == "yfin")
	
	y_df <- y_df |> 
			group_by(year, age_f, age) |> 
			summarise(mean_est = mean(est),
   		mean_est_se = mean(est_se))
		
	y_df$year_f <- as.factor(y_df$year)

	max_yr <- max(y_df$year)
	min_yr <- min(y_df$year)
		
	y_p <- 
		ggplot(y_df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point(aes(alpha = 0.6), size = 1) +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se,
   										 alpha = 0.6)) +
		scale_color_viridis_d(option = "turbo") +
		xlim(min_yr_all, max_yr_all) +
		theme_sleek() +
		ggtitle("Yellowfin sole") +
		theme(
			legend.position = "none",
			axis.title.y = element_blank()
		)
	
	y_df <- ggplot_build(y_p)$data[[1]]
	
	cols <- y_df |>
		select(colour) |>
		distinct_all()
	
	max_y <- max(y_df$y)
	
	y_p <- 
		y_p +
		#annotate("text", x = min_yr + 7, y = max_y, label = "Yellowfin sole", color = "black") +
		annotate("text", x = 1990, y = 1.8, label = "Age 3", color = cols[1,1]) +
		annotate("text", x = 1990, y = 1.8, label = "Age 4", color = cols[2,1]) +
		annotate("text", x = 1990, y = 2.0, label = "Age 5", color = cols[3,1]) +
		annotate("text", x = 1990, y = 2.4, label = "Age 6", color = cols[4,1]) +
		annotate("text", x = 1990, y = 2.6, label = "Age 7", color = cols[5,1]) +
		annotate("text", x = 1990, y = 2.8, label = "Age 8", color = cols[6,1]) +
		annotate("text", x = 1990, y = 1.8, label = "Age 9", color = cols[7,1]) +
		annotate("text", x = 1990, y = 3.2, label = "Age 10", color = cols[8,1]) +
		annotate("text", x = 1990, y = 3.2, label = "Age 11", color = cols[9,1]) +
		annotate("text", x = 1990, y = 3.2, label = "Age 28", color = cols[20,1]) 

	# put together
	top <- a_p + pc_p
	bottom <- pl_p + y_p

	Fig1 <- top/bottom
	
	ggsave(file = paste0(here(), "/output/plots for paper/Fig1.png"), 
				 Fig1,
				 height = 6, width = 12)
	
	
	
	 year_preds <- read.csv(file = paste0(here(), "/data/year_model_preds_Sept2024.csv"))
 
 plots <- purrr::map(unique(dat_all$short_name), \(sp){

  	
   df <- year_preds %>% filter(short_name == sp)
   
   name <- unique(df$common_name)
   
   df <- df |> group_by(year, age_f, age) |> summarise(mean_est = mean(est),
   																				  mean_est_se = mean(est_se))

  	
   p <- 
   	ggplot(df, aes(x = year, y = mean_est)) +
   	geom_point() +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se)) +
   	facet_wrap(~ reorder(age_f, age), scales = "free_y") +
   	ylab("Predicted mean (log) weight (g)") +					
		xlab("Year") +
   	ggtitle(name) +
		theme_sleek() +
   	theme(
   		panel.spacing = unit(0, "lines")
   	)
   
   p

   #ggsave(filename = paste0(here(), "/output/year_plots_", sp, ".png"),
	#				 p,
	#				 height = 7, width = 10, units = "in")
	
  
   	
  })
 
 
	plot1 <- plots[[1]]
	plot2 <- plots[[2]]
	plot3 <- plots[[3]]
	plot4 <- plots[[4]]
 
	year_plots <- plot1/plot2/plot3/plot4
	
	# plot all ages on same plot
	
	shared_plots <- purrr::map(unique(dat_all$short_name), \(sp){
		

		df <- year_preds |> filter(short_name == sp)
	
		df <- df |> 
			group_by(year, age_f, age) |> 
			summarise(mean_est = mean(est),
   		mean_est_se = mean(est_se))
		
		df$year_f <- as.factor(df$year)

		max_yr <- max(df$year)
		min_yr <- min(df$year)
		
	p <- 
		ggplot(df, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
   	geom_point() +
   	geom_linerange(aes(ymin = mean_est - mean_est_se,
   										 ymax = mean_est + mean_est_se)) +
		scale_color_viridis_c() +
		xlab("Year") +
		xlim(1987, max_yr + 5) +
		theme_sleek() +
		theme(
			legend.position = "none",
			axis.title.y = element_blank()
		)
	
	p
	
	})
	
	
	plot1 <- shared_plots[[1]]
	
	df <- ggplot_build(plot1)$data[[1]]
	
	cols <- df |>
		select(colour) |>
		distinct_all()
	
	max_y <- max(df$y)
	
	plot1 <- 
		plot1 +
		annotate("text", x = min_yr + 7, y = max_y, label = "Arrowtooth flounder", color = "black") +
		annotate("text", x = 2025, y = 1.8, label = "Age 2", color = cols[1,1]) +
		annotate("text", x = 2025, y = 2.0, label = "Age 3", color = cols[2,1]) +
		annotate("text", x = 2025, y = 2.4, label = "Age 4", color = cols[3,1]) +
		annotate("text", x = 2025, y = 2.6, label = "Age 5", color = cols[4,1]) +
		annotate("text", x = 2025, y = 2.8, label = "Age 6", color = cols[5,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 8", color = cols[6,1])
	
	plot2 <- shared_plots[[2]]
	
	df <- ggplot_build(plot2)$data[[1]]
	
	cols <- df |>
		select(colour) |>
		distinct_all()
	
	max_y <- max(df$y)

	plot2 <- 
		plot2 +
		annotate("text", x = min_yr + 7, y = max_y, label = "Pacific cod", color = "black") +
		annotate("text", x = 2025, y = 1.8, label = "Age 1", color = cols[1,1]) +
		annotate("text", x = 2025, y = 1.8, label = "Age 2", color = cols[2,1]) +
		annotate("text", x = 2025, y = 2.0, label = "Age 3", color = cols[3,1]) +
		annotate("text", x = 2025, y = 2.4, label = "Age 4", color = cols[4,1]) +
		annotate("text", x = 2025, y = 2.6, label = "Age 5", color = cols[5,1]) +
		annotate("text", x = 2025, y = 2.8, label = "Age 6", color = cols[6,1]) +
		annotate("text", x = 2025, y = 1.8, label = "Age 7", color = cols[7,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 8", color = cols[8,1]) 
		
	
	plot3 <- shared_plots[[3]]
	
	df <- ggplot_build(plot3)$data[[1]]
	
	cols <- df |>
		select(colour) |>
		distinct_all()
	
	max_y <- max(df$y)

	plot3 <- 
		plot3 +
		annotate("text", x = min_yr + 7, y = max_y, label = "Walleye pollock", color = "black") +
		annotate("text", x = 2025, y = 1.8, label = "Age 1", color = cols[1,1]) +
		annotate("text", x = 2025, y = 1.8, label = "Age 2", color = cols[2,1]) +
		annotate("text", x = 2025, y = 2.0, label = "Age 3", color = cols[3,1]) +
		annotate("text", x = 2025, y = 2.4, label = "Age 4", color = cols[4,1]) +
		annotate("text", x = 2025, y = 2.6, label = "Age 5", color = cols[5,1]) +
		annotate("text", x = 2025, y = 2.8, label = "Age 6", color = cols[6,1]) +
		annotate("text", x = 2025, y = 1.8, label = "Age 7", color = cols[7,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 8", color = cols[8,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 9", color = cols[9,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 10", color = cols[10,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 11", color = cols[11,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 12", color = cols[12,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 13", color = cols[13,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 14", color = cols[14,1]) 
	
	
	plot4 <- shared_plots[[4]]
	
	df <- ggplot_build(plot4)$data[[1]]
	
	cols <- df |>
		select(colour) |>
		distinct_all()
	
	max_y <- max(df$y)

	plot4 <- 
		plot4 +
		annotate("text", x = min_yr + 7, y = max_y, label = "Yellowfin sole", color = "black") +
		annotate("text", x = 2025, y = 1.8, label = "Age 3", color = cols[1,1]) +
		annotate("text", x = 2025, y = 1.8, label = "Age 4", color = cols[2,1]) +
		annotate("text", x = 2025, y = 2.0, label = "Age 5", color = cols[3,1]) +
		annotate("text", x = 2025, y = 2.4, label = "Age 6", color = cols[4,1]) +
		annotate("text", x = 2025, y = 2.6, label = "Age 7", color = cols[5,1]) +
		annotate("text", x = 2025, y = 2.8, label = "Age 8", color = cols[6,1]) +
		annotate("text", x = 2025, y = 1.8, label = "Age 9", color = cols[7,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 10", color = cols[8,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 11", color = cols[9,1]) +
		annotate("text", x = 2025, y = 3.2, label = "Age 28", color = cols[20,1]) 

	top/bottom

	top <- plot1 + plot2 +
		ylab("Predicted mean (log) weight (g)") 
	
	bottom <- plot3 + plot4 
	
	(plot1 + plot2)/ (plot3 + plot4)
	
	
	
	
	
	######## plot separately for max control ############