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
	temp_pred_fun <- function(mod){
		
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
	
	
	#### pcod ####
	
	pcod_dat <- dat_all %>% filter(short_name == "pcod") %>%
		select(log_wt, short_name, weight, age, age_f, year, yrprior_btemp, yrprior_boxy)

	pcod_names_sARsER <- str_subset(file_list_sARsES, "pcod")
	pcod_names_MV <- str_subset(file_list_MV, "pcod")

	# predictions
	
	# no_cov
	pcod_no_cov_sARsER_mod <- str_subset(pcod_names_sARsER, "no_cov")
	pcod_no_cov_sARsER_mod <- read_mod_fun(pcod_no_cov_sARsER_mod, file_path_sARsES)
	no_cov_preds_sARsER <- predict(pcod_no_cov_sARsER_mod)

	pcod_no_cov_MV_mod <- str_subset(pcod_names_MV, "no_cov")
	pcod_no_cov_MV_mod <- read_mod_fun(pcod_no_cov_MV_mod, file_path_MV)
	no_cov_preds_MV <- predict(pcod_no_cov_MV_mod)

	pcod_no_cov_preds <- tibble(pcod_dat, no_cov_preds_sARsER, no_cov_preds_MV) 
	
	sum <- pcod_no_cov_preds %>% group_by(age_f) %>%
		summarise(mean_pred_sARsER = mean(no_cov_preds_sARsER),
							mean_pred_MV = mean(no_cov_preds_MV))
	
	colors <- c("no_cov_preds_sARsER" = "yellow", "no_cov_preds_MV" = "red")
	
	#ggplot(pcod_no_cov_preds, aes(x = age_f)) +
	#	geom_point(aes(y = no_cov_preds_sARsER, color = "no_cov_preds_sARsER"), alpha = 0.5) +
	#	geom_point(aes(y = no_cov_preds_MV, color = "no_cov_preds_MV"), alpha = 0.5) +
	#	labs(color = "Legend") +
	#	scale_color_manual(values = colors)
	
	ggplot(pcod_no_cov_preds, aes(x = age_f)) +
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
		ylab("prediction log weght")
	
		geom_point(aes(y = no_cov_preds_sARsER, color = "no_cov_preds_sARsER"), alpha = 0.5) +
		geom_point(aes(y = no_cov_preds_MV, color = "no_cov_preds_MV"), alpha = 0.5) +
		labs(color = "Legend") +
		scale_color_manual(values = colors)
	
	# temp
	pcod_sARsER_temp_mods_names <- str_subset(pcod_names_sARsER, "temp")
	pcod_MV_temp_mods_names <- str_subset(pcod_names_MV, "temp")
	
	pcod_sARsER_temp_mods <- purrr::map2(pcod_sARsER_temp_mods_names, file_path_sARsES, read_mod_fun)	
	pcod_MV_temp_mods <- purrr::map2(pcod_MV_temp_mods_names, file_path_MV, read_mod_fun)	

	pcod_sARsER_temp_mod_preds <- map(pcod_sARsER_temp_mods, temp_pred_fun)
	
	mod_names <- str_replace_all(pcod_sARsER_temp_mods_names,  '.rds', '')
	sp <- "pcod"
	
	pcod_preds_sARsER <- purrr::map2_dfr(pcod_sARsER_temp_mod_preds, mod_names, \(df, mod_name){
		
		df <- df %>%
			mutate(sp = sp,
						 mod_name = mod_name)
		
	})
	
	pcod_preds_sARsER$type <- "sARsER"
	pcod_preds_sARsER$mod_name_type <- paste0(pcod_preds_sARsER$type, "_", pcod_preds_sARsER$mod_name)
	
	#MV
	pcod_MV_temp_mod_preds <- map(pcod_MV_temp_mods, temp_pred_fun)
	
	mod_names <- str_replace_all(pcod_MV_temp_mods_names,  '.rds', '')
	sp <- "pcod"
	
	pcod_preds_MV <- purrr::map2_dfr(pcod_MV_temp_mod_preds, mod_names, \(df, mod_name){
		
		df <- df %>%
			mutate(sp = sp,
						 mod_name = mod_name)
		
	})
	
	pcod_preds_MV$type <- "MV"
	pcod_preds_MV$mod_name_type <- paste0(pcod_preds_MV$type, "_", pcod_preds_MV$mod_name)
	
	ages <- str_split(pcod_preds_MV$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	
	pcod_preds_MV <- pcod_preds_MV %>%
			mutate(age = ages,
						 age = as.numeric(age))
		
	ggplot(pcod_preds_MV) +
					geom_line(aes(x = yrprior_btemp, y = p)) +
					facet_grid(reorder(age_f, age) ~ mod_name, scales = "free_y") +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					#ggtitle(sp) +
					theme_sleek() +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
	
	# try plotting both on same plot
	
	pcod_preds <- bind_rows(pcod_preds_sARsER, pcod_preds_MV)
	
	ages <- str_split(pcod_preds$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	
	pcod_preds <- pcod_preds %>%
			mutate(age = ages,
						 age = as.numeric(age))

	ggplot(pcod_preds) +
					geom_line(aes(x = yrprior_btemp, y = p, color = type)) +
					facet_grid(reorder(age_f, age) ~ mod_name, scales = "free_y") +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					#ggtitle(sp) +
					theme_sleek() +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
	
		#ggplot(pcod_preds) +
		#			geom_line(aes(x = yrprior_btemp, y = (10^p), color = type)) +
		#			facet_grid(reorder(age_f, age) ~ mod_name, scales = "free_y") +
 		#			ylab("Predicted mean (log) weight (g)") +					
		#			xlab("Temperature (˚C)") +
		#			#ggtitle(sp) +
		#			theme_sleek() +
		#			theme(
		#		 		panel.spacing.y = unit(0, "lines"))
	
	