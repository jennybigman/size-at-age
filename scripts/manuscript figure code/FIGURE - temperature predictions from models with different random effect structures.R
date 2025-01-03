# plot comparing predictions from different model structure

	theme_sleek3 <- function(base_size = 11, base_family = "") {
	  half_line <- base_size/2
	  theme_light(base_size = base_size, base_family = base_family) +
	    theme(
	      panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(),
	      axis.ticks.length = unit(half_line / 2.2, "pt"),
	      strip.background = element_rect(fill = NA, colour = NA),
	      axis.text = element_text(colour = "grey30"),
	      axis.title = element_text(colour = "grey30"),
	      panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
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
	
	# read in mods
	top_temp_mods <- purrr::map2(file_paths, file_list, read_mod_fun)

	# predict 
	pred_fun <- function(mod, mod_name){
	
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
		
	}
	
	# predictions
	
	top_temp_mod_preds <- purrr::map2_dfr(top_temp_mods, file_list, pred_fun)

	top_temp_mod_preds <- top_temp_mod_preds |> select(-single_age)
	
	ages <- str_split(top_temp_mod_preds$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	top_temp_mod_preds$age <- as.numeric(ages)
#	
	# names
	type <- str_extract(top_temp_mod_preds$mod_name, "shared|MV|ssf")
	top_temp_mod_preds$type <- type
	
	# edit names for plotting
	#top_temp_mod_preds$type <- top_temp_mod_preds$type[top_temp_mod_preds$type == "MV"] <- "separate S & ST fields"
	#top_temp_mod_preds$type <- top_temp_mod_preds$type[top_temp_mod_preds$type == "shared"] <- "shared S & ST fields"
	#top_temp_mod_preds$type <- top_temp_mod_preds$type[top_temp_mod_preds$type == "ssf"] <- "shared S field only"

	
	
	# separate for plotting
	top_preds_list <- top_temp_mod_preds %>% group_split(sp)

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
	
	spp <- unique(dat_all$short_name)

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
	
	
	atooth_preds <-  top_preds_list[[1]]
	pcod_preds <-    top_preds_list[[2]]
	pollock_preds <- top_preds_list[[3]]
	yfin_preds <-    top_preds_list[[4]]
	
	
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


	preds_all <- bind_rows(atooth_preds, pcod_preds, pollock_preds, yfin_preds)
	preds_all$age_label <- paste0("age ", preds_all$age)
	
	
	# add better labels
	preds_all <- preds_all |>
		mutate(type_name = case_when(
			type == "MV" ~ "age-specific fields",
			type == "ssf" ~ "shared spatial field",
			type == "shared" ~ "shared spatial/spatiotemporal fields"
		))
	
	fwrite(preds_all, file = paste0(here(), "/data/temp_preds_all.csv"))
	
	############### LOAD PREDICTIONS FOR PLOTTING ####################
	
	# load predictions
	preds_all <- fread(file = paste0(here(), "/data/temp_preds_all.csv"))
	
	preds_all$type_name[preds_all$type_name == "independent fields"] <- "age-specific fields"
	preds_all$type_name[preds_all$type_name == "shared spatial and spatiotemporal fields"] <- "shared spatial/spatiotemporal fields"

	cols <- c("age-specific fields" = "orange", 
						"shared spatial field" = "#01cdfe",
						"shared spatial/spatiotemporal fields" = "#b967ff")
		
	preds_all$type_name <- factor(preds_all$type_name,
																levels=c('age-specific fields', 
																				 'shared spatial/spatiotemporal fields',
																				 'shared spatial field'))
	# pcod ####
	pcod_preds <- preds_all |> filter(sp == "pcod")
	
	set1 <- c("MV", "ssf")
	
	pcod_preds2 <- pcod_preds |> filter(type %in% set1)
	
	pcodMV <- pcod_preds |> filter(type == "MV")
	pcod2 <- pcod_preds |> filter(type %in% set1)
	
	# set up scales and range
	pc_scales <- list(
		scale_y_continuous(
			limits = c(1.51, 2.1),
			breaks = c(1.7, 1.9)), # age 1
		scale_y_continuous(
			limits = c(2.21, 2.91),
			breaks = c(2.4, 2.7)),
		scale_y_continuous(
			limits = c(2.74, 3.25),
			breaks = c(2.9, 3.1)),
		scale_y_continuous(
			limits = c(3.03, 3.47),
			breaks = c(3.1, 3.3)),
		scale_y_continuous(
			limits = c(3.26, 3.52),
			breaks = c(3.35, 3.45)), # age 5
		scale_y_continuous(
			limits = c(3.38, 3.66),
			breaks = c(3.45, 3.6)),
		scale_y_continuous(
			limits = c(3.45, 3.78),
			breaks = c(3.55, 3.7)),
		scale_y_continuous(
			limits = c(3.63, 3.87),
			breaks = c(3.7, 3.8)),
		scale_y_continuous(
			limits = c(3.56, 3.96),
			breaks = c(3.65, 3.85)),
		scale_y_continuous(
			limits = c(3.54, 3.98),
			breaks = c(3.65, 3.85))) # age 10

	pcod_all_plot  <-
	 ggplot(pcod_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p, color = type_name)) +
	 facet_wrap(~ reorder(age_label, age), scales = "free_y", ncol = 5) +
	 facetted_pos_scales(y = pc_scales) + 
	 scale_y_continuous(
	 	expand = expansion(add = 2)) +
	 scale_color_manual(values = cols) +
		 	 geom_text(aes(label = age_label), size = 4, 
	 				x = -Inf, y = Inf, color = "black",
	  			hjust = -0.1, vjust = 1.3, check_overlap = T) +
	 xlab("temperature (˚C)") +
	 ylab("predicted mean (log) weight (g)") +
   	theme(
   		#legend.text = element_text(size = 12),
   		legend.position = "bottom",
   		legend.background = element_rect(fill = "white", color = NA),
   		legend.title = element_blank(),
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA)
   	) +
		ggtitle("Pacific cod") 
	
		ggsave(filename = paste0(here(), "/output/plots for paper/pcod_comp_plot.png"),
					 pcod_all_plot,
					 height = 6, width = 12, units = "in")
		

	#### pollock ####
	pol_preds <- preds_all |> filter(sp == "pollock")

	pl_scales <- list(
		scale_y_continuous(
			breaks = c(1.0, 1.20),
			limits = c(0.926, 1.3)), # age 1
		scale_y_continuous(
			breaks = c(1.7, 2.0),
			limits = c(1.47, 2.35)),
		scale_y_continuous(
			breaks = c(2.2, 2.4),
			limits = c(2.1, 2.55)),
		scale_y_continuous(
			breaks = c(2.65, 2.75),
			limits = c(2.6, 2.78)),
		scale_y_continuous(
			breaks = c(2.75, 2.85),
			limits = c(2.7, 2.95)), # age 5
		scale_y_continuous(
			breaks = c(2.85, 2.95),
			limits = c(2.82, 2.96)),
		scale_y_continuous(
			breaks = c(2.9, 3.0),
			limits = c(2.88, 3.04)),
		scale_y_continuous(
			breaks = c(2.95, 3.00),
			limits = c(2.92, 3.06)),
		scale_y_continuous(
			breaks = c(3, 3.1),
			limits = c(2.97, 3.15)),
		scale_y_continuous(
			breaks = c(3.05, 3.15),
			limits = c(3.01, 3.22)), # age 10
		scale_y_continuous(
			breaks = c(3.05 ,3.2),
			limits = c(2.99, 3.25)),
		scale_y_continuous(
			breaks = c(3.1 ,3.2),
			limits = c(3.05, 3.25)),
		scale_y_continuous(
			breaks = c(3.1, 3.3),
			limits = c(2.97, 3.41)),
		scale_y_continuous(
			breaks = c(3.2, 3.3),
			limits = c(3.17, 3.37)),
		scale_y_continuous(
			breaks = c(3.25, 3.4),
			limits = c(3.18, 3.48)), # age 15
		scale_y_continuous(
			breaks = c(3.3, 3.45),
			limits = c(3.24, 3.53)),
		scale_y_continuous(
		  breaks = c(3.25, 3.4),
			limits = c(3.17, 3.46)),
		scale_y_continuous(
			breaks = c(3.3, 3.4), 
			limits = c(3.25, 3.46)),
		scale_y_continuous(
			breaks = c(3.3, 3.4),
			limits = c(3.2, 3.47)),
		scale_y_continuous(
		  breaks = c(3.3 ,3.4),
			limits = c(3.14, 3.5))
		)
	

	pol_all_plot  <-
	 ggplot(pol_preds) +
	 geom_line(
	 	aes(x = raw_temp, y = p, color = type_name)) +
	 facet_wrap(~ reorder(age_label, age), scales = "free_y", ncol = 5) +
	 facetted_pos_scales(y = pl_scales) + 
	 scale_y_continuous(
	 	expand = expansion(add = 2)) +
	 scale_color_manual(values = cols) +
		 	 geom_text(aes(label = age_label), size = 4, 
	 				x = -Inf, y = Inf, color = "black",
	  			hjust = -0.1, vjust = 1.3, check_overlap = T) +
	 xlab("temperature (˚C)") +
	 ylab("predicted mean (log) weight (g)") +
   	theme(
   		#legend.text = element_text(size = 12),
   		legend.position = "bottom",
   		legend.background = element_rect(fill = "white", color = NA),
   		legend.title = element_blank(),
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA)
   	) +
		ggtitle("walleye pollock")

		#	d <- ggplot_build(pol_all_plot)
	
		ggsave(filename = paste0(here(), "/output/plots for paper/pol_all_plot.png"),
					 pol_all_plot,
					 height = 6, width = 12, units = "in")
		
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
			limits = c(2.18, 2.65)), # age 10
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
			limits = c(2.27, 2.95)),
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
			limits = c(2.41, 3.25)), # age 25
		scale_y_continuous(
			breaks = c(2.6, 2.9),
			limits = c(2.51, 3.5)),
		scale_y_continuous(
			breaks = c(2.6, 2.9),
			limits = c(2.48, 3.05)),
		scale_y_continuous(
			breaks = c(2.7, 3),
			limits = c(2.53, 3.15)),
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
	 facet_wrap(~ reorder(age_label, age), scales = "free_y", ncol = 6) +
	 facetted_pos_scales(y = y_scales) + 
	 scale_y_continuous(
	 	expand = expansion(add = 2)) +
	 scale_color_manual(values = cols) +
		 	 geom_text(aes(label = age_label), size = 4, 
	 				x = -Inf, y = Inf, color = "black",
	  			hjust = -0.1, vjust = 1.3, check_overlap = T) +
	 xlab("temperature (˚C)") +
	 ylab("predicted mean (log) weight (g)") +
   	theme(
   		#legend.text = element_text(size = 12),
   		legend.position = "bottom",
   		legend.background = element_rect(fill = "white", color = NA),
   		legend.title = element_blank(),
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA)
   	) +
		ggtitle("yellowfin sole")
	#	d <- ggplot_build(yfin_all_plot)
	
	ggsave(filename = paste0(here(), "/output/plots for paper/yfin_all_plot.png"),
				 yfin_all_plot,
				 height = 6, width = 12, units = "in")
		
		
	# atooth ####
	atooth_preds <- preds_all |> filter(sp == "atooth")	

	a_scales <- list(
		scale_y_continuous(
			breaks = c(1.6, 2.2),
			limits = c(1.22, 2.67)), # age 2
		scale_y_continuous(
			breaks = c(2.4, 3.0),
			limits = c(1.93, 3.35)),
		scale_y_continuous(
			breaks = c(2.5, 2.9),
			limits = c(2.23, 3.15)),
		scale_y_continuous(
			breaks = c(2.65, 2.75),
			limits = c(2.62, 2.77)), # age 5
		scale_y_continuous(
			breaks = c(2.8, 2.85),
			limits = c(2.75, 2.9)),
		scale_y_continuous(
			breaks = c(2.85, 2.95),
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
			limits = c(2.68, 5.15)),
		scale_y_continuous(
			breaks = c(3.3, 3.4),
			limits = c(3.19, 3.45)))
	
	
	atooth_all_plot  <-
	 ggplot(atooth_preds) +
		 geom_line(
	 	aes(x = raw_temp, y = p, color = type_name)) +
	 facet_wrap(~ reorder(age_label, age), scales = "free_y", ncol = 6) +
	 facetted_pos_scales(y = a_scales) + 
	 scale_y_continuous(
	 	expand = expansion(add = 2)) +
	 scale_color_manual(values = cols) +
		 	 geom_text(aes(label = age_label), size = 4, 
	 				x = -Inf, y = Inf, color = "black",
	  			hjust = -0.1, vjust = 1.3, check_overlap = T) +
	 xlab("temperature (˚C)") +
	 ylab("predicted mean (log) weight (g)") +
   	theme(
   		#legend.text = element_text(size = 12),
   		legend.position = "bottom",
   		legend.background = element_rect(fill = "white", color = NA),
   		legend.title = element_blank(),
			strip.background = element_blank(),
      strip.text = element_blank(),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
			panel.background = element_rect(fill = NA)
   	) +
		ggtitle("arrowtooth flounder")

	#	d <- ggplot_build(atooth_all_plot)
	
	ggsave(filename = paste0(here(), "/output/plots for paper/atooth_all_plot.png"),
				 atooth_all_plot,
				 height = 6, width = 12, units = "in")

		
		
		
		############################ older plots
		
	# atooth MV only ####
	atooth_preds <- MV_preds_all |> filter(sp == "atooth")

	atooth_temp_preds_plot <-
	 ggplot(atooth_preds) +
	 geom_line(aes(x = raw_temp, y = p), color = "white") +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 4, axes = "x") +
	 geom_text(aes(label = age_label, size = 10), 
 	 				x = Inf, y = Inf, color = "white",
    			hjust = 1, vjust = 1, check_overlap = T) +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 black_theme() +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank()
   	) 
	
	a_scales <- list(
		scale_y_continuous(
			breaks = c(1.55, 1.65)), # age 2
		scale_y_continuous(
			breaks = c(2.1, 2.2)),
		scale_y_continuous(
			breaks = c(2.45, 2.55)),
		scale_y_continuous(
			breaks = c(2.65, 2.75)), # age 5
		scale_y_continuous(
			breaks = c(2.75, 2.85)),
		scale_y_continuous(
			breaks = c(2.9, 3.0)),
				scale_y_continuous(
			breaks = c(3, 3.1)),
		scale_y_continuous(
			breaks = c(3.1, 3.2)),
		scale_y_continuous(
			breaks = c(3.15, 3.2)), # age 10
		scale_y_continuous(
			breaks = c(3.2, 3.3)),
		scale_y_continuous(
			breaks = c(3.2, 3.3)))
	
	atooth_temp_preds_plot <- 
		atooth_temp_preds_plot +
		facetted_pos_scales(y = a_scales) 

		ggsave(filename = paste0(here(), "/output/plots for presentations/atooth_temp_preds_plot.png"),
					 atooth_temp_preds_plot,
					 height = 6, width = 12, units = "in")
		 
	# pcod ####
	pcod_preds <- MV_preds_all |> filter(sp == "pcod")

	pcod_temp_preds_plot <-
	 ggplot(pcod_preds) +
	 geom_line(aes(x = raw_temp, y = p), color = "white") +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 4, axes = "x") +
	 geom_text(aes(label = age_label, size = 14), 
 	 				x = -Inf, y = Inf, color = "white",
    			hjust = -0.1, vjust = 1.5, check_overlap = T) +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 scale_y_continuous(expand = expansion(mult = c(0.3))) +
	 black_theme() +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank()
   	) 
	
		pc_scales <- list(
		scale_y_continuous(
			breaks = c(1.68, 1.69)), # age 1
		scale_y_continuous(
			breaks = c(2.48, 2.49)),
		scale_y_continuous(
			breaks = c(2.91, 2.92)),
		scale_y_continuous(
			breaks = c(3.21, 3.22)),
		scale_y_continuous(
			breaks = c(3.39, 3.40)), # age 5
		scale_y_continuous(
			breaks = c(3.54, 3.55)),
		scale_y_continuous(
			breaks = c(3.65, 3.66)),
				scale_y_continuous(
			breaks = c(3.76, 3.77)),
		scale_y_continuous(
			breaks = c(3.82, 3.83)),
		scale_y_continuous(
			breaks = c(3.89, 3.90))) # age 10

	pcod_temp_preds_plot <- 
		pcod_temp_preds_plot +
		facetted_pos_scales(y = pc_scales)

		
		ggsave(filename = paste0(here(), "/output/plots for presentations/pcod_temp_preds_plot.png"),
					 pcod_temp_preds_plot,
					 height = 6, width = 12, units = "in")

	# pollock ####
	pol_preds <- MV_preds_all |> filter(sp == "pollock")
	
	pl_scales <- list(
		scale_y_continuous(
			breaks = c(1.18, 1.20)), # age 1
		scale_y_continuous(
			breaks = c(1.89, 1.91)),
		scale_y_continuous(
			breaks = c(2.33, 2.35)),
		scale_y_continuous(
			breaks = c(2.65, 2.67)),
		scale_y_continuous(
			breaks = c(2.80, 2.82)), # age 5
		scale_y_continuous(
			breaks = c(2.90, 2.92)),
		scale_y_continuous(
			breaks = c(2.97, 2.99)),
		scale_y_continuous(
			breaks = c(3.01, 3.03)),
		scale_y_continuous(
			breaks = c(3.08, 3.1)),
		scale_y_continuous(
			breaks = c(3.11, 3.13)), # age 10
		scale_y_continuous(
			breaks = c(3.15, 3.17)),
		scale_y_continuous(
			breaks = c(3.17, 3.19)),
		scale_y_continuous(
			breaks = c(3.2, 3.22)),
		scale_y_continuous(
			breaks = c(3.24, 3.26)),
		scale_y_continuous(
			breaks = c(3.24, 3.26)), # age 15
		scale_y_continuous(
			breaks = c(3.27, 3.29)),
			scale_y_continuous(
			breaks = c(3.30, 3.32)),
			scale_y_continuous(
			breaks = c(3.31, 3.33)),
			scale_y_continuous(
			breaks = c(3.33, 3.35)),
			scale_y_continuous(
			breaks = c(3.34, 3.36))
		)
	

	pol_temp_preds_plot <-
	 ggplot(pol_preds) +
	 geom_line(aes(x = raw_temp, y = p), color = "white") +
	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 4, axes = "x") +
	 geom_text(aes(label = age_label, size = 4), 
 	 				x = Inf, y = Inf, color = "white", ,
    			hjust = 1, vjust = 1.5, check_overlap = T) +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 #scale_y_continuous(expand = expansion(mult = c(1))) +
	 black_theme(x = 8) +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank()
   	) 
	
	pol_temp_preds_plot <- 
		pol_temp_preds_plot +
		facetted_pos_scales(y = pl_scales)

		
		ggsave(filename = paste0(here(), "/output/plots for presentations/pol_temp_preds_plot.png"),
					 pol_temp_preds_plot,
					 height = 6, width = 12, units = "in")
		
	# yfin #####
	yfin_preds <- MV_preds_all |> filter(sp == "yfin")
	
	y_scales <- list(
		scale_y_continuous(
			breaks = c(1.21, 1.22)),
		scale_y_continuous(
			breaks = c(1.54, 1.55)),
		scale_y_continuous(
			breaks = c(1.76, 1.77)), # age 5
		scale_y_continuous(
			breaks = c(1.94, 1.95)),
		scale_y_continuous(
			breaks = c(2.08, 2.09)),
		scale_y_continuous(
			breaks = c(2.22, 2.23)),
		scale_y_continuous(
			breaks = c(2.23, 2.24)),
		scale_y_continuous(
			breaks = c(2.45, 2.46)), # age 10
		scale_y_continuous(
			breaks = c(2.51, 2.52)),
		scale_y_continuous(
			breaks = c(2.54, 2.55)),
		scale_y_continuous(
			breaks = c(2.58, 2.59)),
		scale_y_continuous(
			breaks = c(2.64, 2.65)),
		scale_y_continuous(
			breaks = c(2.67, 2.68)), # age 15 
		scale_y_continuous(
			breaks = c(2.71, 2.72)),
		scale_y_continuous(
			breaks = c(2.74, 2.75)),
		scale_y_continuous(
			breaks = c(2.76, 2.77)),
		scale_y_continuous(
			breaks = c(2.75, 2.76)),
		scale_y_continuous(
			breaks = c(2.75, 2.76)), # age 20
		scale_y_continuous(
			breaks = c(2.78, 2.79)),
		scale_y_continuous(
			breaks = c(2.79, 2.8)),
		scale_y_continuous(
			breaks = c(2.78, 2.79)),
		scale_y_continuous(
			breaks = c(2.77, 2.78)),
		scale_y_continuous(
			breaks = c(2.81, 2.82)), # age 25
		scale_y_continuous(
			breaks = c(2.82, 2.83)),
		scale_y_continuous(
			breaks = c(2.82, 2.83)),
		scale_y_continuous(
			breaks = c(2.81, 2.82)),
		scale_y_continuous(
			breaks = c(2.84, 2.85)),
		scale_y_continuous(
			breaks = c(2.81, 2.82))
		)
	
	yfin_temp_preds_plot <-
	 ggplot(yfin_preds) +
	 geom_line(aes(x = raw_temp, y = p), color = "white") +
	 	 facet_wrap2(~ reorder(age_label, age), scales = "free_y", ncol = 5, axes = "x") +
	 geom_text(aes(label = age_label, size = 1), 
 	 				x = -Inf, y = Inf, color = "white",
    			hjust = -0.1, vjust = 1.5, check_overlap = T) +
 	 ylab("Predicted mean (log) weight (g)") +					
	 xlab("Temperature (˚C)") +
	 #scale_y_continuous(expand = expansion(mult = c(5, 5))) +
	 black_theme(x = 10) +
   	theme(
   		panel.spacing = unit(0.5, "lines"),
   		#strip.text = element_text(size = 14)
			strip.background = element_blank(),
      strip.text = element_blank()
   	) 
	
	yfin_temp_preds_plot <- 
		yfin_temp_preds_plot +
		facetted_pos_scales(y = y_scales)

		
		ggsave(filename = paste0(here(), "/output/plots for presentations/yfin_temp_preds_plot.png"),
					 yfin_temp_preds_plot,
					 height = 6, width = 12, units = "in")

