	# file list for mods

	#remotes::install_github("vast-lib/tinyVAST", ref = "dev", force = TRUE)

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

	# check temps
	#all_preds <- bind_rows(atooth_preds,
	#											 pcod_preds,
	#											 pollock_preds,
	#											 yfin_preds)
	#
	#ggplot(all_preds) +
	#	geom_line(aes(x = yrprior_btemp, y = raw_temp)) +
	#	facet_wrap(~ sp)
	
	preds_list <- list(atooth_preds, pcod_preds, pollock_preds, yfin_preds)
	
	#### plotting ####
	plots <- purrr::map(preds_list, \ (df){
		
		sp <- unique(df$sp)
		
		cols <- c(MV = "#f98404", 
							ssf = "#01cdfe",
							shared = "#b967ff")
						
	
		p <- 
			ggplot(df) +
					geom_line(aes(x = raw_temp, y = p, color = type)) +
					facet_wrap(~ reorder(age_f, age), scales = "free_y", ncol = 5) +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					ggtitle(sp) +
					theme_sleek() +
					scale_color_manual(values = cols) +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
		
		ggsave(filename = paste0(here(), "/output/prediction_comp_", sp, ".png"),
					 p,
					 height = 8, width = 15, units = "in")
	
		
	})
	
	
	## same y axis
	#purrr::map(top_mods_list, \ (df){
	#	
	#	sp <- unique(df$sp)
	#	
	#	cols <- c(separate = "#f98404", 
	#						"shared S field" = "#01cdfe",
	#						"shared S & ST fields" = "#b967ff")
	#					
	#
	#	p <- 
	#		ggplot(df) +
	#				geom_line(aes(x = yrprior_btemp, y = (10^p), color = type)) +
	#				facet_wrap(~ reorder(age_f, age)) +
 	#				ylab("Predicted mean (log) weight (g)") +					
	#				xlab("Temperature (˚C)") +
	#				ggtitle(sp) +
	#				theme_sleek() +
	#				scale_color_manual(values = cols) +
	#				theme(
	#			 		panel.spacing.y = unit(0, "lines"))
	#	
	#	ggsave(filename = paste0(here(), "/output/prediction_comp_same_axis_", sp, ".png"),
	#				 p,
	#				 height = 8, width = 20, units = "in")
	#
	#	
	#})
	
	

