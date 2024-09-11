# just read the rds file
	read_mod_fun <- function(mod, file_path){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	

	#file_path_sARsES <- paste0(here(), "/output/model output/tinyVAST/shared AR shared ES/")
	#file_list_sARsES <- list.files(path = paste0(file_path_sARsES))

	file_path_MV_AR <- paste0(here(), "/output/model output/tinyVAST/fully MV_AR/")
	file_list_MV_AR <- list.files(path = paste0(file_path_MV_AR))

	file_path_MV <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	file_list_MV <- list.files(path = paste0(file_path_MV))

	file_path_st <- paste0(here(), "/output/model output/tinyVAST/sep age models/")
	file_list_st <- list.files(path = paste0(file_path_st))

	file_path_stAR <- paste0(here(), "/output/model output/tinyVAST/sep age models/AR term/")
	file_list_stAR <- list.files(path = paste0(file_path_stAR))

	#### temp ####
	
	# read in mods
	#names_temp_mods_sARsES <- stringr::str_subset(file_list_sARsES, 'temp')
	#names_temp_mods_sARsES <- stringr::str_subset(names_temp_mods_sARsES, 'int')
#
	#temp_mods_sARsES <- purrr::map2(names_temp_mods_sARsES, file_path_sARsES, read_mod_fun)
	
	names_temp_mods_MV <- stringr::str_subset(file_list_MV, 'temp')
	names_temp_mods_MV <- stringr::str_subset(names_temp_mods_MV, 'int')

	temp_mods_MV <- purrr::map2(names_temp_mods_MV, file_path_MV, read_mod_fun)

	names_temp_mods_MV_AR <- stringr::str_subset(file_list_MV_AR, 'temp')
	names_temp_mods_MV_AR <- stringr::str_subset(names_temp_mods_MV_AR, 'int')

	temp_mods_MV_AR <- purrr::map2(names_temp_mods_MV_AR, file_path_MV_AR, read_mod_fun)

	names_temp_mods_st <- stringr::str_subset(file_list_st, 'temp')
	temp_mods_st <- purrr::map2(names_temp_mods_st, file_path_st, read_mod_fun)

	names_temp_mods_stAR <- stringr::str_subset(file_list_stAR, 'temp')
	temp_mods_stAR <- purrr::map2(names_temp_mods_stAR, file_path_stAR, read_mod_fun)

	
	temp_pred_fun <- function(mod){
	
		dat <- mod$data
		sp <- unique(dat$short_name)
		
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
  	
  	preds <- tibble(nd, p, sp)
		
	}
	
	add_mod_name <- function(df, mod_name) {
		df <- df %>%
			mutate(mod_name = mod_name)
		
	}
	
	# predictions
	
	## sARsER
	#temp_preds_sARsER <- purrr::map(temp_mods_sARsES, temp_pred_fun)
	#
	## add mod names and type of model
	#names_temp_mods_sARsES <- str_replace_all(names_temp_mods_sARsES, '.rds', '')
	#names_temp_mods_sARsES <- str_replace_all(names_temp_mods_sARsES, '_tvt', '')
	#names_temp_mods_sARsES <- str_replace_all(names_temp_mods_sARsES, '_yrprior_btemp_int', '')
	#names_temp_mods_sARsES <- gsub(".*_","", names_temp_mods_sARsES)

	#temp_preds_sARsER <- purrr::map2_dfr(temp_preds_sARsER, names_temp_mods_sARsES, add_mod_name)
	#temp_preds_sARsER$type <- "sARsER"
	#saveRDS(temp_preds_sARsER, file = paste0(here(), "/output/temp_preds_sARsER.rds"))
	#temp_preds_sARsER <- readRDS(file = paste0(here(), "/output/temp_preds_sARsER.rds"))
	
	# fully MV
	temp_preds_MV <- purrr::map(temp_mods_MV, temp_pred_fun)
	
	# add mod names and type of model
	names_temp_mods_MV <- str_replace_all(names_temp_mods_MV, 'yrprior_btemp_int', '')
	names_temp_mods_MV <- str_replace_all(names_temp_mods_MV, '.rds', '')
	names_temp_mods_MV <- str_replace_all(names_temp_mods_MV, '_tvt', '')
	names_temp_mods_MV <- gsub(".*_","", names_temp_mods_MV)

	temp_preds_MV <- purrr::map2_dfr(temp_preds_MV, names_temp_mods_MV, add_mod_name)
	temp_preds_MV$type <- "MV"
	#saveRDS(temp_preds_MV, file = paste0(here(), "/output/temp_preds_MV.rds"))
	#temp_preds_MV <- readRDS(file = paste0(here(), "/output/temp_preds_MV.rds"))
	
	# fully MV_AR
	temp_preds_MV_AR <- purrr::map(temp_mods_MV_AR, temp_pred_fun)
	
	# add mod names and type of model
	names_temp_mods_MV_AR <- str_replace_all(names_temp_mods_MV_AR, 'yrprior_btemp_int', '')
	names_temp_mods_MV_AR <- str_replace_all(names_temp_mods_MV_AR, '.rds', '')
	names_temp_mods_MV_AR <- str_replace_all(names_temp_mods_MV_AR, '_tvt', '')
	names_temp_mods_MV_AR <- gsub(".*_","",  names_temp_mods_MV_AR)

	temp_preds_MV_AR <- purrr::map2_dfr(temp_preds_MV_AR, names_temp_mods_MV_AR, add_mod_name)
	temp_preds_MV_AR$type <- "MV_AR"
	#saveRDS(temp_preds_MV_AR, file = paste0(here(), "/output/temp_preds_MV_AR.rds"))
	#temp_preds_MV_AR <- readRDS(file = paste0(here(), "/output/temp_preds_MV_AR.rds"))
	
	
	# stacked model predictions
	temp_preds_st <- purrr::map(temp_mods_st, \ (mod){
		
		dat <- mod$data
		sp <- unique(dat$short_name)
		age_f <- as.factor(unique(dat$age))
		
		nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(dat$yrprior_btemp, na.rm = TRUE),
					to = max(dat$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				year = dat$year[1],
				X = dat$X[1],
				Y = dat$Y[1],
				var = 1) # what is this? value doesn't seem to matter?
  
  	nd <- data.frame(nd)
 
  	p <- predict(mod, newdata = nd)
  	
  	preds <- tibble(p, sp, age_f, nd)
		
	})
	
	names_temp_mods_st <- str_replace_all(names_temp_mods_st, 'yrprior_btemp', '')
	names_temp_mods_st <- str_replace_all(names_temp_mods_st, '.rds', '')
	names_temp_mods_st <- str_replace_all(names_temp_mods_st, '_tvt', '')
	names_temp_mods_st <- unlist(qdapRegex::ex_between(names_temp_mods_st, "__", "_age"))

	temp_preds_st <- purrr::map2_dfr(temp_preds_st, names_temp_mods_st, add_mod_name)
	temp_preds_st$type <- "stacked"
	temp_preds_st <- temp_preds_st %>% select(-var)
	temp_preds_st$age_f <- paste0("age_", temp_preds_st$age_f)
	
	# stacked with AR
	temp_preds_stAR <- purrr::map(temp_mods_stAR, \ (mod){
		
		dat <- mod$data
		sp <- unique(dat$short_name)
		age_f <- as.factor(unique(dat$age))
		
		nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(dat$yrprior_btemp, na.rm = TRUE),
					to = max(dat$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				year = dat$year[1],
				X = dat$X[1],
				Y = dat$Y[1],
				var = 1) # what is this? value doesn't seem to matter?
  
  	nd <- data.frame(nd)
 
  	p <- predict(mod, newdata = nd)
  	
  	preds <- tibble(p, sp, age_f, nd)
		
	})
	
	names_temp_mods_stAR <- str_replace_all(names_temp_mods_stAR, 'yrprior_btemp', '')
	names_temp_mods_stAR <- str_replace_all(names_temp_mods_stAR, '.rds', '')
	names_temp_mods_stAR <- str_replace_all(names_temp_mods_stAR, '_tvt', '')
	names_temp_mods_stAR <- unlist(qdapRegex::ex_between(names_temp_mods_stAR, "__", "_age"))

	temp_preds_stAR <- purrr::map2_dfr(temp_preds_stAR, names_temp_mods_stAR, add_mod_name)
	temp_preds_stAR$type <- "stacked_AR"
	temp_preds_stAR <- temp_preds_stAR %>% select(-var)
	temp_preds_stAR$age_f <- paste0("age_", temp_preds_stAR$age_f)


	# bind together
	#temp_preds <- bind_rows(temp_preds_sARsER, temp_preds_MV, temp_preds_MV_AR, temp_preds_st, temp_preds_stAR)
	#temp_preds <- bind_rows(temp_preds_MV, temp_preds_MV_AR, temp_preds_st, temp_preds_stAR)
	temp_preds <- bind_rows(temp_preds_MV, temp_preds_st)

	ages <- str_split(temp_preds$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	
	temp_preds$age <- as.numeric(ages)
	
	temp_preds_list <- temp_preds %>% 
		group_split(sp)
	
	
	#### plotting ####
	plots <- purrr::map(temp_preds_list, \ (df){
		
		sp <- unique(df$sp)
		
		#cols <- c(MV = "#FF4500", stacked = "#ffc7b2",
		#					MV_AR = "#962FBF", stacked_AR = "#d5abe5")
		
		cols <- c(MV = "#FF4500", stacked = "#ffc7b2")
						
	
		p <- 
			ggplot(df) +
					geom_line(aes(x = yrprior_btemp, y = (10^p), color = type)) +
					facet_grid(reorder(age_f, age) ~ mod_name) +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					ggtitle(sp) +
					theme_sleek() +
					scale_color_manual(values = cols) +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
		
		p
		
		#ggsave(filename = paste0(here(), "/output/prediction_comp_", sp, ".png"),
		#			 p)
	#
		
	})
		
	

