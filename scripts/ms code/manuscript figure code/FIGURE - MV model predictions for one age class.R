# plot predictions of weight with temperature for one age class for main text

	theme_sleek2 <- function(base_size = 11, base_family = "") {
	  half_line <- base_size/2
	  theme_light(base_size = base_size, base_family = base_family) +
	    theme(
	      panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(),
	      axis.ticks.length = unit(half_line / 2.2, "pt"),
	      strip.background = element_rect(fill = NA, colour = NA),
	      axis.text = element_text(colour = "grey30"),
	      axis.title = element_text(colour = "grey30"),
	      legend.title = element_text(colour = "grey30", size = rel(0.9)),
	      panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
	      legend.key.size = unit(0.9, "lines"),
	      legend.text = element_text(size = rel(0.7), colour = "grey30"),
	      legend.key = element_rect(colour = NA, fill = NA),
	      legend.background = element_rect(colour = NA, fill = NA),
	      plot.title = element_text(colour = "grey30", size = rel(1)),
	      plot.subtitle = element_text(colour = "grey30", size = rel(.85))
	    )
	}

file_paths <- paste0(here(), "/output/model output/tinyVAST/top mods/")
 
  file_list <- list.files(file_paths)
  
  # read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	

	#### plots for paper - one age class ####
	
	MV_mods <- str_subset(file_list, "MV")
	
	names <- str_remove(MV_mods, ".rds")
	
	# read in mods
	MV_mods <- purrr::map2(file_paths, MV_mods, read_mod_fun)

	# predictions
	MV_preds <- purrr::map2_dfr(MV_mods, names, \ (mod, mod_name){ 

		dat <- mod$data
		sp <- unique(dat$short_name)

		nd <- expand.grid(
			yrprior_btemp = seq(
				from = min_temp,
				to = max_temp,
				length.out = 25),
			age_f = unique(dat$age_f),
			year = 2004, # predict for any year
			X = X_pred,
			Y = Y_pred, 
			single_age = "shared") 
			
		nd <- data.frame(nd)
    p <- predict(mod, newdata = nd, what = "palpha_g")
  
  	preds <- tibble(nd, p, sp, mod_name) 
  		
		preds
		
})
	
	# for Figure 2, just show one age class in main text since same relationship and full figure in SI
	# selecting age 10 because all species have age 10
	MV_preds_age10 <- MV_preds |>
		filter(age_f == "age_10")
	
	# wrangle for plotting
	MV_preds_age10 <- MV_preds_age10 |> select(-single_age)
	
	ages <- str_split(MV_preds_age10$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	MV_preds_age10$age <- as.numeric(ages)
	

	# separate for plotting
	MV_preds_age10_list <- MV_preds_age10 %>% group_split(sp)

	# unscale temp
	all_dat <- read.csv(paste0(here(), "/data/all_data.csv"))

	spp <- unique(all_dat$short_name)

	scale_info <- purrr::map_dfr(spp, \(sp){
	
	d <- all_dat %>% filter(short_name == sp)
	
	temp <- d$yrprior_btemp
	oxy <- d$yrprior_boxy
	
	m_t <- mean(temp)
	sd_t <- sd(temp)
	
	m_o <- mean(oxy)
	sd_o <- sd(oxy)
	
	nd <- tibble(sp, m_t, sd_t, m_o, sd_o)
	
})
	

	## unscale temp --- NOT WORKING RIGHT NOW 
	#unscale_fun <- function(df, sp){
	#	
	#	sp_scale_info <- scale_info %>% filter(sp == "pollock")
	#	m_t <- sp_scale_info$m_t
	#	sd_t <- sp_scale_info$sd_t
#
	#	df <- df %>%
	#		rowwise() |>
	#		mutate(raw_temp = ((yrprior_btemp * sd_t) + m_t))
	#	
	#	df
	#	
	#}
	
	#preds <- purrr::map2(top_mods_list, spp, unscale_fun)
	
	
	atooth_preds_age10 <-  MV_preds_age10_list[[1]]
	  pcod_preds_age10 <-    MV_preds_age10_list[[2]]
 pollock_preds_age10 <- MV_preds_age10_list[[3]]
	  yfin_preds_age10 <-    MV_preds_age10_list[[4]]
	
	
	a_m <- scale_info$m_t[1]
	a_sd <- scale_info$sd_t[1]
	
	pc_m <- scale_info$m_t[2]
	pc_sd <- scale_info$sd_t[2]
	
	pl_m <- scale_info$m_t[3]
	pl_sd <- scale_info$sd_t[3]
	
	y_m <- scale_info$m_t[4]
	y_sd <- scale_info$sd_t[4]
	
	
	atooth_preds_age10 <- atooth_preds_age10 %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * a_sd) + a_m))
		
	pcod_preds_age10 <- pcod_preds_age10 %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * pc_sd) + pc_m))

	pollock_preds_age10 <- pollock_preds_age10 %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * pl_sd) + pl_m))

	yfin_preds_age10 <- yfin_preds_age10 %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * y_sd) + y_m))


	preds_all_age10 <- bind_rows(atooth_preds_age10, pcod_preds_age10, 
															 pollock_preds_age10, yfin_preds_age10)
	preds_all_age10$age_label <- paste0("Age ", preds_all_age10$age)
	
	preds_all_age10 <- preds_all_age10 |>
		mutate(full_name = case_when(
			sp == "attoth" ~ "Arrowtooth flounder",
			sp == "pcod" ~ "Pacific cod",
			sp == "pollock" ~ "Walleye pollock",
			sp == "yfin" ~ "Yellowfin sole"
		))
	
	
	# make plots separately for max control
	
	a_df <- preds_all_age10 |> filter(sp == "atooth")

	a_plot <- 
		ggplot(a_df) +
		geom_line(aes(x = raw_temp, y = p)) +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Temperature (˚C)") 
	
	a_max_y <- ggplot_build(a_plot)$layout$panel_scales_y[[1]]$range$range[2]
	
	a_plot <- 
		a_plot +
		annotate("text", x = 0.9, y = a_max_y + 0.05, 
					 label = "arrowtooth flounder", 
					 color = "black",
					 size = 4) +
		scale_y_continuous(
			breaks = c(3.1, 3.2, 3.3),
			labels = c(3.1, 3.2, 3.3)
		) +
	#	scale_x_continuous(
	#		breaks = c(-2, 0, 2, 4, 6),
	#		limits = c(-2, 7)
	#	) +
		theme_sleek() +
		theme(axis.title = element_blank())
	
	# pcod
	
	pc_df <- preds_all_age10 |> filter(sp == "pcod")

	pc_plot <- 
		ggplot(pc_df) +
		geom_line(aes(x = raw_temp, y = p)) +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Temperature (˚C)") 
	
	pc_max_y <- ggplot_build(pc_plot)$layout$panel_scales_y[[1]]$range$range[2]
	
	pc_plot <- 
		pc_plot +
		annotate("text", x = -0.8, y = pc_max_y, 
					 label = "Pacific cod", 
					 color = "black",
					 size = 4) +
		scale_y_continuous(
			breaks = c(3.88, 3.89, 3.90),
			labels = c(3.88, 3.89, 3.90),
			limits = c(3.881318, 3.905510)) +
		theme_sleek() +
		theme(axis.title = element_blank())
	
	# pollock
	
	pl_df <- preds_all_age10 |> filter(sp == "pollock")

	pl_plot <- 
		ggplot(pl_df) +
		geom_line(aes(x = raw_temp, y = p)) +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Temperature (˚C)") 
	
	pl_max_y <- ggplot_build(pl_plot)$layout$panel_scales_y[[1]]$range$range[[2]]
	
	pl_plot <- 
		pl_plot +
		annotate("text", x = -0.25, y = 3.14, 
					 label = "walleye pollock", 
					 color = "black",
					 size = 4) +
		scale_y_continuous(
			breaks = c(3.10, 3.12, 3.14),
			labels = c(3.10, 3.12, 3.14),
			limits = c(3.098, 3.14)) +
		theme_sleek() +
		theme(axis.title = element_blank())

	
	# yfin
	
	y_df <- preds_all_age10 |> filter(sp == "yfin")

	y_plot <- 
		ggplot(y_df) +
		geom_line(aes(x = raw_temp, y = p)) +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Temperature (˚C)") 
	
	y_max_y <- ggplot_build(y_plot)$layout$panel_scales_y[[1]]$range$range[[2]]
	
	y_plot <- 
		y_plot +
		annotate("text", x = -0.65, y = y_max_y, 
					 label = "yellowfin sole", 
					 color = "black",
					 size = 4) +
		scale_x_continuous(
			breaks = c(0, 2, 4, 6),
			labels = c(0, 2, 4, 6)) + 
		theme_sleek() +
		theme(axis.title = element_blank())
	
	# plot together
	
	top <- a_plot + pc_plot
	bottom <- pl_plot + y_plot
	
	Fig2 <- top/bottom
	
	Fig2 <- Fig2 |>
		add_global_label(Xlab = "Temperature (˚C)", Ylab = "Predicted mean (log) weight (g)")

	ggsave(file = paste0(here(), "/output/plots for paper/Fig2.png"), 
				 Fig2,
				 height = 5, width = 7)
	


	#### now for all age classes for SI ####
	
	# wrangle for plotting
	MV_preds <- MV_preds |> select(-single_age)
	
	ages <- str_split(MV_preds$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	MV_preds$age <- as.numeric(ages)
	

	# separate for plotting
	MV_preds_list <- MV_preds %>% group_split(sp)

	# unscale temp
	all_dat <- read.csv(paste0(here(), "/data/all_data.csv"))

	spp <- unique(all_dat$short_name)

	scale_info <- purrr::map_dfr(spp, \(sp){
	
	d <- all_dat %>% filter(short_name == sp)
	
	temp <- d$yrprior_btemp
	oxy <- d$yrprior_boxy
	
	m_t <- mean(temp)
	sd_t <- sd(temp)
	
	m_o <- mean(oxy)
	sd_o <- sd(oxy)
	
	nd <- tibble(sp, m_t, sd_t, m_o, sd_o)
	
})
	

	## unscale temp --- NOT WORKING RIGHT NOW 
	#unscale_fun <- function(df, sp){
	#	
	#	sp_scale_info <- scale_info %>% filter(sp == "pollock")
	#	m_t <- sp_scale_info$m_t
	#	sd_t <- sp_scale_info$sd_t
#
	#	df <- df %>%
	#		rowwise() |>
	#		mutate(raw_temp = ((yrprior_btemp * sd_t) + m_t))
	#	
	#	df
	#	
	#}
	
	#preds <- purrr::map2(top_mods_list, spp, unscale_fun)
	
	
	atooth_preds <-  MV_preds_list[[1]]
	  pcod_preds <-    MV_preds_list[[2]]
 pollock_preds <- MV_preds_list[[3]]
	  yfin_preds <-    MV_preds_list[[4]]
	
	
	a_m <- scale_info$m_t[1]
	a_sd <- scale_info$sd_t[1]
	
	pc_m <- scale_info$m_t[2]
	pc_sd <- scale_info$sd_t[2]
	
	pl_m <- scale_info$m_t[3]
	pl_sd <- scale_info$sd_t[3]
	
	y_m <- scale_info$m_t[4]
	y_sd <- scale_info$sd_t[4]
	
	
	atooth_preds <- atooth_preds %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * a_sd) + a_m))
		
	pcod_preds <- pcod_preds %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * pc_sd) + pc_m))

	pollock_preds <- pollock_preds %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * pl_sd) + pl_m))

	yfin_preds <- yfin_preds %>%
			rowwise() |>
			mutate(raw_temp = ((yrprior_btemp * y_sd) + y_m))


	preds_all <- bind_rows(atooth_preds, pcod_preds, 
												 pollock_preds, yfin_preds)
	preds_all$age_label <- paste0("Age ", preds_all$age)
	
	preds_all <- preds_all |>
		mutate(full_name = case_when(
			sp == "atooth" ~ "arrowtooth flounder",
			sp == "pcod" ~ "Pacific cod",
			sp == "pollock" ~ "walleye pollock",
			sp == "yfin" ~ "yellowfin sole"
		))
	
# make plots
	
	
	atooth_preds <- preds_all |> filter(sp == "atooth")	

	atooth_all_plot  <-
	 ggplot(atooth_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p), color = "black") +
	 ggtitle(unique(atooth_preds$full_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", axes = "x") +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 theme_sleek() 

	ggsave(filename = paste0(here(), "/output/plots for paper/atooth_all_plot.png"),
				 atooth_all_plot, dpi = 500,
				 height = 6, width = 12, units = "in")

	
	
	pcod_preds <- preds_all |> filter(sp == "pcod")	

	pcod_all_plot  <-
	 ggplot(pcod_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p), color = "black") +
	 ggtitle(unique(pcod_preds$full_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", axes = "x") +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 theme_sleek() 

	ggsave(filename = paste0(here(), "/output/plots for paper/pcod_all_plot.png"),
				 pcod_all_plot, dpi = 500,
				 height = 6, width = 12, units = "in")


	# pollock
	pollock_preds <- preds_all |> filter(sp == "pollock")	

	pol_all_plot  <-
	 ggplot(pollock_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p), color = "black") +
	 ggtitle(unique(pollock_preds$full_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", axes = "x") +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 theme_sleek() 

	ggsave(filename = paste0(here(), "/output/plots for paper/pol_all_plot.png"),
				 pol_all_plot, dpi = 500,
				 height = 8, width = 12, units = "in")


	
	# yfin
	yfin_preds <- preds_all |> filter(sp == "yfin")	

	yfin_all_plot  <-
	 ggplot(yfin_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p), color = "black") +
	 ggtitle(unique(yfin_preds$full_name)) +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", axes = "x", ncol = 7) +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 theme_sleek() 

	ggsave(filename = paste0(here(), "/output/plots for paper/yfin_all_plot.png"),
				 yfin_all_plot,
				 height = 8, width = 12, units = "in", dpi = 500)

	
	
	
	
	#######
	
	atooth_all_plot  <-
	 ggplot(atooth_preds, aes(x = raw_temp, y = p)) +
	 geom_line() +
	 geom_rug() +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 4, axes = "x") +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 facetted_pos_scales(a = a_scales) 

	
	
	########### ADD SEPARATE AXES ##################
		a_df <- preds_all |> filter(sp == "atooth")

	a_plot <- 
		ggplot(a_df) +
		geom_line(aes(x = raw_temp, y = p)) +
		facet_wrap(~age_label, scales = "free_y") +
		ylab("Predicted mean (log) weight (g)") +
		xlab("Temperature (˚C)") 
	
	a_max_y <- ggplot_build(a_plot)$layout$panel_scales_y[[1]]$range$range[2]
	
	a_plot <- 
		a_plot +
		annotate("text", x = 0.9, y = a_max_y + 0.05, 
					 label = "Arrowtooth flounder", 
					 color = "black",
					 size = 4) +
		scale_y_continuous(
			breaks = c(3.1, 3.2, 3.3),
			labels = c(3.1, 3.2, 3.3)
		) +
		theme_sleek() +
		theme(axis.title = element_blank())
	
	
	