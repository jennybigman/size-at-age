	# file list for mods
	
	file_path_MV <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	shared_file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	ssf_file_path <- paste0(here(), "/output/model output/tinyVAST/spatial field only/")
	
	MV_top_mods <- readRDS(file = paste0(here(), "/output/tables/MV_top_mods.rds"))
	shared_top_mods_temp <- readRDS(file = paste0(here(), "/output/tables/shared_top_mods_temp.rds"))
  ssf_top_mods_temp <- readRDS(file = paste0(here(), "/output/tables/ssf_top_mods_temp.rds"))
  
  # read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	# read in mods
	top_mods_MV <- purrr::map2(file_path_MV, MV_top_mods, read_mod_fun)
	top_mods_shared <- purrr::map2(shared_file_path, shared_top_mods_temp, read_mod_fun)
	top_mods_ssf <- purrr::map2(ssf_file_path, ssf_top_mods_temp, read_mod_fun)

	# unscale temp
	unscale_fun <- function(df, sp){
		
		sp_scale_info <- scale_info %>% filter(sp == sp)
		m_t <- sp_scale_info$m_t
		sd_t <- sp_scale_info$sd_t

		df <- df %>%
			mutate(raw_temp = ((yrprior_btemp * sd_t) + m_t))
		
		df
		
	}
	
	dat_list <- dat_all %>% group_split(short_name)
	
	d <- purrr::map2_dfr(dat_list, spp, unscale_fun)

	
	# predict 
	pred_fun <- function(mod, spp){
	
		obs_d <- d %>% filter(short_name == spp)
		
		min_temp <- min(obs_d$raw_temp, na.rm = TRUE)
		max_temp <- max(obs_d$raw_temp, na.rm = TRUE)
		X_pred <- obs_d$X[1]
		Y_pred <- obs_d$Y[1]

		dat <- mod$data

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
    p <- predict(mod, newdata = nd)
  
  	preds <- tibble(nd, p, spp) 
  		
		preds
		
	}
	
	# predictions
	spp <- unique(d$short_name)
	
	# fully MV
	top_mods_MV_preds <- purrr::map2_dfr(top_mods_MV, spp, pred_fun)
	top_mods_MV_preds$type <- "separate"

	top_mods_shared_preds <- purrr::map2_dfr(top_mods_shared, spp, pred_fun)
	top_mods_shared_preds$type <- "shared S & ST fields"
	
	top_mods_ssf_preds <- purrr::map2_dfr(top_mods_ssf, spp, pred_fun)
	top_mods_ssf_preds$type <- "shared S field"
	
	top_mods_preds <- bind_rows(top_mods_MV_preds, top_mods_shared_preds, top_mods_ssf_preds)
	
	top_mods_preds <- top_mods_preds |> select(-single_age)
	
	ages <- str_split(top_mods_preds$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	
	top_mods_preds$age <- as.numeric(ages)
	
	# separate for plotting
	top_mods_list <- top_mods_preds %>% group_split(spp)


	
	#### plotting ####
	plots <- purrr::map(top_mods_list, \ (df){
		
		sp <- unique(df$spp)
		
		cols <- c(separate = "#f98404", 
							"shared S field" = "#01cdfe",
							"shared S & ST fields" = "#b967ff")
						
	
		p <- 
			ggplot(df) +
					geom_line(aes(x = yrprior_btemp, y = 10^p, color = type)) +
					facet_wrap(~ reorder(age_f, age), scales = "free_y", ncol = 5) +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					ggtitle(sp) +
					theme_sleek() +
					scale_color_manual(values = cols) +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
		
		#ggsave(filename = paste0(here(), "/output/prediction_comp_", sp, ".png"),
		#			 p,
		#			 height = 8, width = 15, units = "in")
	
		
	})
	
	
	# same y axis
	purrr::map(top_mods_list, \ (df){
		
		sp <- unique(df$sp)
		
		cols <- c(separate = "#f98404", 
							"shared S field" = "#01cdfe",
							"shared S & ST fields" = "#b967ff")
						
	
		p <- 
			ggplot(df) +
					geom_line(aes(x = yrprior_btemp, y = (10^p), color = type)) +
					facet_wrap(~ reorder(age_f, age)) +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					ggtitle(sp) +
					theme_sleek() +
					scale_color_manual(values = cols) +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
		
		ggsave(filename = paste0(here(), "/output/prediction_comp_same_axis_", sp, ".png"),
					 p,
					 height = 8, width = 20, units = "in")
	
		
	})
	
	atooth_oxy_preds <- top_mods_preds |>
		filter(sp == "atooth", type == "shared S & ST fields") |>
		mutate(type  = "shared S & ST fields (oxygen)")
	

		
	

