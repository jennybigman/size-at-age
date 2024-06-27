
return_mod_fun <- function(num) {
		
	mod <- yr_mod_list[[num]]
		
		s <- sanity(mod)
		
		hess    <- s$hessian_ok 
		eigen   <- s$eigen_values_ok            
		nl      <- s$nlminb_ok       		         
		range 	<- s$range_ok       
		se_na 	<- s$se_na_ok         
		sigma 	<- s$sigmas_ok        
	
		name <- names(yr_mod_list)[[num]]

		df <- tibble(name, hess, eigen, nl, range, se_na, sigma)
		
		df
		
}

	num = 1:length(yr_mod_list)

	mod_df <- lapply(num, return_mod_fun) %>% bind_rows()	 	
	
	mod_df_drop <- mod_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- mod_df_drop$name

	mod_list_keep <- mod_df %>% 
		filter(name %!in% drop)
	
	# read in models
	mod_list_keep <- mod_list_keep$name
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/")))

	file_list <- intersect(mod_list_keep, file_list)
	
	prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
	yr_mod_list_trim <- lapply(mod_names_list, readRDS)
 
  # set up names
	nums <- qdapRegex::ex_between(names(yr_mod_list_trim), "yr_mod_", ".rds")
	nums <- unlist(nums)

	ages <- str_sub(nums, start= -2)
	ages <- str_remove(ages, pattern = "_")

	# predictions 
	
	preds_fun <- function(mod){
		
		preds <- predict(mod, se_fit = TRUE)
	
	}
	
	preds <- purrr::map(yr_mod_list_trim, preds_fun) %>% 
			bind_rows() 


	
	mod <- readRDS(paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/"), "pcod_yr_f_mod_.rds"))

	
	
	visreg(mod, xvar = "year_f", gg = TRUE)
	
	plot(preds$year_f, preds$est)

		ggplot(preds) +
		geom_line(aes(x = year_f, y = est)) 
							 
	pollock_dat_sum <- pollock_dat %>%
		group_by(age_f, year) %>%
		summarise(mean_log_wt = mean(log_wt))

	ggplot(pollock_dat_sum, aes(x = year, y = mean_log_wt)) +
		geom_point() +
		facet_wrap(~ age_f, scales = "free") +
		stat_smooth(method = "lm", col = "red")