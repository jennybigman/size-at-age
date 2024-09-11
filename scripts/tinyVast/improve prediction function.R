# just read the rds file
	read_mod_fun <- function(mod, file_path){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	

	file_path_sARsES <- paste0(here(), "/output/model output/tinyVAST/shared AR shared ES/")
	file_list_sARsES <- list.files(path = paste0(file_path_sARsES))

	file_path_MV <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	file_list_MV <- list.files(path = paste0(file_path_MV))


	file_path_st <- paste0(here(), "/output/model output/tinyVAST/sep age models/")
	file_list_st <- list.files(path = paste0(file_path_st))

	
	#### temp ####
	
	# read in mods
	names_temp_mods_sARsES <- stringr::str_subset(file_list_sARsES, 'temp')
	names_temp_mods_sARsES <- stringr::str_subset(names_temp_mods_sARsES, 'int')

	temp_mods_sARsES <- purrr::map2(names_temp_mods_sARsES, file_path_sARsES, read_mod_fun)
	
	names_temp_mods_MV <- stringr::str_subset(file_list_MV, 'temp')
	names_temp_mods_MV <- stringr::str_subset(names_temp_mods_MV, 'int')

	temp_mods_MV <- purrr::map2(names_temp_mods_MV, file_path_MV, read_mod_fun)

	names_temp_mods_st <- stringr::str_subset(file_list_st, 'temp')
	temp_mods_st <- purrr::map2(names_temp_mods_st, file_path_st, read_mod_fun)

	
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
	
	temp_preds_sARsER <- purrr::map_dfr(temp_mods_sARsES, temp_pred_fun)
	temp_preds_sARsER$type <- "sARsER"
	#saveRDS(temp_preds_sARsER, file = paste0(here(), "/output/temp_preds_sARsER.rds"))
	#temp_preds_sARsER <- readRDS(file = paste0(here(), "/output/temp_preds_sARsER.rds"))
	
	temp_preds_MV <- purrr::map_dfr(temp_mods_MV, temp_pred_fun)
	temp_preds_MV$type <- "MW"
	#saveRDS(temp_preds_MV, file = paste0(here(), "/output/temp_preds_MV.rds"))
	#temp_preds_MV <- readRDS(file = paste0(here(), "/output/temp_preds_MV.rds"))
	
	# stacked model predictions
	temp_preds_st <- purrr::map_dfr(temp_mods_st, \ (mod){
		
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
	
	temp_preds_st$type <- "stacked"
	
	temp_preds_st <- temp_preds_st %>% select(-var)

	# bind together
	temp_preds <- bind_rows(temp_preds_sARsER, temp_preds_MV, temp_preds_st)
	
	temp_preds_list <- temp_preds %>% 
		group_split(sp)
	
	
	#### plotting ####
	plots <- purrr::map(temp_preds_list, \ (df){
		
		p <- 
			ggplot(df) +
					geom_line(aes(x = yrprior_btemp, y = p, color = type)) +
					facet_grid(age_f ~ mod_name, scales = "free_y") +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					#ggtitle(sp) +
					theme_sleek() +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
		
		p
		
	})
		
	
	reorder(age_f, age)

