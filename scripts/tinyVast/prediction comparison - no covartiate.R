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

	
	#### no cov ####
	
	# read in mods
	names_no_cov_mods_sARsES <- stringr::str_subset(file_list_sARsES, 'no')
	no_cov_mods_sARsES <- purrr::map2(names_no_cov_mods_sARsES, file_path_sARsES, read_mod_fun)
	
	names_no_cov_mods_MV <- stringr::str_subset(file_list_MV, 'no')
	no_cov_mods_MV <- purrr::map2(names_no_cov_mods_MV, file_path_MV, read_mod_fun)

	names_no_cov_mods_st <- stringr::str_subset(file_list_st, 'no')
	no_cov_mods_st <- purrr::map2(names_no_cov_mods_st, file_path_st, read_mod_fun)

	
	no_cov_pred_fun <- function(mod){
	
		dat <- mod$data
		sp <- unique(dat$short_name)
		
		dat <- dat %>% 
			select(log_wt, short_name, weight,
						 age, age_f, year, 
						 yrprior_btemp, yrprior_boxy)
		
		preds <- predict(mod)
		preds <- tibble(preds, dat)
		preds
		
	}
	
	no_cov_preds_sARsER <- purrr::map_dfr(no_cov_mods_sARsES, no_cov_pred_fun)
	no_cov_preds_sARsER$type <- "sARsER"
	
	no_cov_preds_MV <- purrr::map_dfr(no_cov_mods_MV, no_cov_pred_fun)
	no_cov_preds_MV$type <- "MW"

	no_cov_preds_st <- purrr::map_dfr(no_cov_mods_st, no_cov_pred_fun)
	no_cov_preds_st$type <- "stacked"

	no_cov_preds <- bind_rows(no_cov_preds_sARsER, no_cov_preds_MV, no_cov_preds_st)
	
	no_cov_preds_list <- no_cov_preds %>% 
		group_split(short_name)
	
	# plotting ####
	p <- 
			ggplot(no_cov_preds, aes(x = age)) +
			stat_summary(fun.y = mean,
									 fun.min = function(x) mean(x) - sd(x),
									 fun.max = function(x) mean(x) + sd(x),
									 geom = "pointrange",
									 aes(y = preds, color = type), alpha = 0.4) +
			facet_wrap(~ short_name, scales = "free") +
			ylab("prediction log weight") +
			theme_sleek()
		


	