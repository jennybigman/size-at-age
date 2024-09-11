# turn into a function?

# plot predictions from both models


	file_path_sARsES <- paste0(here(), "/output/model output/tinyVAST/shared AR shared ES/")
	file_list_sARsES <- list.files(path = paste0(file_path_sARsES))
	
	file_path_MV <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	file_list_MV <- list.files(path = paste0(file_path_MV))

	
	# just read the rds file
	read_mod_fun <- function(mod, file_path){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	

	# predict function
	est_temp_pred_fun <- function(mod){
		
		nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(mod$data$yrprior_btemp, na.rm = TRUE),
					to = max(mod$data$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				age_f = unique(mod$data$age_f),
				year = 2004,
				X = mod$data$X[1],
				Y = mod$data$Y[1]) 
  
  	nd <- data.frame(nd)
  
  	p <- predict(mod, newdata = nd)
  	
  	preds <- tibble(nd, p)

	}
	
	oxy_pred_fun <- function(mod){
		
		nd <- expand.grid(
				yrprior_boxy = seq(
					from = min(mod$data$yrprior_boxy, na.rm = TRUE),
					to = max(mod$data$yrprior_boxy, na.rm = TRUE),
					length.out = 25),
				age_f = unique(mod$data$age_f),
				year = 2004,
				X = mod$data$X[1],
				Y = mod$data$Y[1]) 
  
  	nd <- data.frame(nd)
  
  	p <- predict(mod, newdata = nd, se.fit = FALSE)
  	
  	return(p)

	}
	
	
	#### no cov ####
	
	preds_no_cov_fun <- function(sp){
	
		dat <- dat_all %>% filter(short_name == sp) %>%
			select(log_wt, short_name, weight, age, age_f, year, yrprior_btemp, yrprior_boxy)

		names_sARsER <- str_subset(file_list_sARsES, sp)
		names_MV <- str_subset(file_list_MV, sp)

		# predictions
	
		# shared AR term, shared residual variance
		names_sARsER_no_cov <- str_subset(names_sARsER, "no_cov")
		no_cov_sARsER_mod <- read_mod_fun(names_sARsER_no_cov, file_path_sARsES)
		no_cov_preds_sARsER <- predict(no_cov_sARsER_mod)

		# fully MV 
		names_no_cov_MV_mod <- str_subset(names_MV, "no_cov")
		no_cov_MV_mod <- read_mod_fun(names_no_cov_MV_mod, file_path_MV)
		no_cov_preds_MV <- predict(no_cov_MV_mod)

		# join together
		no_cov_preds <- tibble(dat, no_cov_preds_sARsER, no_cov_preds_MV) 
		no_cov_preds$short_name <- sp
		
		no_cov_preds

	}
	
	#pcod_no_cov_preds <- purrr::map("pcod", preds_no_cov_fun)
	sp <- c("atooth", "pcod")
	
	no_cov_preds <- purrr::map_dfr(sp, preds_no_cov_fun)
	
	# pull in preds from stacked analysis
	no_cov_preds <- left_join(no_cov_preds, no_cov_st_preds)
	
	
	#### plotting ####
	plot_no_cov_fun <- function(df){
		
		# plot
		colors <- c("no_cov_preds_sARsER" = "yellow", "no_cov_preds_MV" = "red")
	
		sp <- unique(df$short_name)
		
		p <- 
			ggplot(df, aes(x = age_f)) +
			stat_summary(fun.y = mean,
									 fun.min = function(x) mean(x) - sd(x),
									 fun.max = function(x) mean(x) + sd(x),
									 geom = "pointrange",
									 aes(y = no_cov_preds_sARsER, color = "no_cov_preds_sARsER"), alpha = 0.5) +
			stat_summary(fun.y = mean,
									 fun.min = function(x) mean(x) - sd(x),
									 fun.max = function(x) mean(x) + sd(x),
									 geom = "pointrange",
									 aes(y = no_cov_preds_MV, color = "no_cov_preds_MV"), alpha = 0.5) +	
			scale_color_manual(values = colors) +
			ylab("prediction log weight") +
			ggtitle(sp) + 
			theme_sleek()
		
		p 
		
	}
	
	plots <- purrr::map(no_cov_preds, plot_no_cov_fun)

	
	#### temp ####
	temp_pred_fun <- function(sp){
		
		names_sARsER <- str_subset(file_list_sARsES, sp)
		names_MV <- str_subset(file_list_MV, sp)

		names_sARsER <- str_subset(names_sARsER, "temp")
		names_MV <- str_subset(names_MV, "temp")
	
		sARsER_temp_mods <- purrr::map2(names_sARsER, file_path_sARsES, read_mod_fun)	
		MV_temp_mods <- purrr::map2(names_MV, file_path_MV, read_mod_fun)	

		sARsER_temp_mod_preds <- purrr::map(sARsER_temp_mods, est_temp_pred_fun)
	
		mod_names <- str_replace_all(names_sARsER,  '.rds', '')
	
		
		preds_sARsER <- purrr::map2_dfr(sARsER_temp_mod_preds, mod_names, \(df, mod_name){
		
		df <- df %>%
			mutate(sp = sp,
						 mod_name = mod_name)
		
		})
	
		preds_sARsER$type <- "sARsER"
		preds_sARsER$mod_name_type <- paste0(preds_sARsER$type, "_", preds_sARsER$mod_name)
	
		#MV
		MV_temp_mod_preds <- map(MV_temp_mods, est_temp_pred_fun)
	
		mod_names <- str_replace_all(names_MV,  '.rds', '')

		preds_MV <- purrr::map2_dfr(MV_temp_mod_preds, mod_names, \(df, mod_name){
		
			df <- df %>%
				mutate(sp = sp,
							 mod_name = mod_name)
		
		})
	
		preds_MV$type <- "MV"
		preds_MV$mod_name_type <- paste0(preds_MV$type, "_", preds_MV$mod_name)
	
		preds <- bind_rows(preds_sARsER, preds_MV)
		
		ages <- str_split(preds$age_f, "_")
		ages <- sapply(ages, "[[", 2)
	
		preds <- preds %>%
			mutate(age = ages,
						 age = as.numeric(age))
		
		preds$short_name <- sp

		preds
		
		}
	
	sp <- c("atooth", "pcod")
	
	temp_preds <- purrr::map(sp, temp_pred_fun)
		
	# plots
	
	temp_plot_fun <- function(df){
	
		sp <- unique(df$short_name)
		
		p <- ggplot(pcod_preds) +
					geom_line(aes(x = yrprior_btemp, y = p, color = type)) +
					facet_grid(reorder(age_f, age) ~ mod_name, scales = "free_y") +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					ggtitle(sp) +
					theme_sleek() +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
		
		p
		
	}
	
	temp_plots <- purrr::map(temp_preds, temp_plot_fun)
	
	