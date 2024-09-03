	file_path <- "/output/model output/sdmTMB output/May 2024/sep age classes/time-varying/"
	
	# load mods where temp explains size-at-age
	temp_top_mods <- readRDS(file = paste0(here(), file_path, "temp_top_mods.rda"))

	## read in models ####
		
	# names of all models in folder
	file_list <- list.files(path = paste0(here(), file_path))

	# subset that list to only include models with .rds 
	file_list <- stringr::str_subset(file_list, '.rds')
	
	# subset that list to only include those with 'temp' in the name
	temp_file_list <- stringr::str_subset(file_list, 'temp')
	
	# remove '.rds' from name
	temp_file_list <- str_replace_all(temp_file_list, '.rds', '')

	# extract species, age, model_type
	species <-  str_extract(temp_file_list, "atooth|pcod|yfin|pollock")
	model_type <- str_extract(temp_file_list, "poly3|poly2|gam|lin|no_cov")
	age <- sub(".*age_", "", temp_file_list)
  age <- str_remove(age, pattern = "\\_yrprior.*")

  temp_mods_all <- tibble(
  	temp_file_list, species, age, model_type
  )

  temp_mods_all$name <- paste(temp_mods_all$species, 
  														temp_mods_all$age, 
  														temp_mods_all$model_type, 
  														sep = "_")		

  temp_mods_top <- temp_mods_all %>%
  	filter(name %in% temp_top_mods)

  # mods to read in
  temp_mod_list <- temp_mods_top$temp_file_list

  # add back .rds
  temp_mod_list <- paste0(temp_mod_list, ".rds")
  
  mod_names_list <- list()

  for(i in temp_mod_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  temp_mod_list <- lapply(mod_names_list, readRDS)
  
  # predictions
  temp_preds <- purrr::map(temp_mod_list, \(mod){
	 	
		try(
			
		{
		dat <- mod$data
		yrs <- unique(dat$year)
		age <- unique(dat$age)
		sp <- unique(dat$short_name)
		
		max_temp <- max(dat$yrprior_btemp)
		min_temp <- min(dat$yrprior_btemp)
		
		yr <- yrs[1]
		
		yrprior_btemp = seq(from = min_temp, to = max_temp, length.out = 200)
		
		nd <- expand_grid(
			yrprior_btemp = yrprior_btemp,
			year = yr,
			age = age) 

		psim <- predict(mod, newdata = nd, re_form = NA, nsim = 200)

		m <- apply(psim, 1, mean)
		se <- apply(psim, 1, sd)
		u <- m + (1.96*se)
		l <- m - (1.96*se)

		out <- bind_cols(nd, m, u, l, sp)
		
		names_out <- c("yrprior_btemp", "year", "age", "est", "upr", "lwr", "sp")
		
		names(out) <- names_out
			
		out
		
		})
})
  
  # plot
  
  temp_preds_all <- temp_preds %>% bind_rows()
  
  plot_fun <- function(sp){
  	
  	df <- temp_preds_all %>% filter(sp == "atooth")
  	
  	df$age_label = paste("Age", df$age, " ")
  	
  	df_age <- as.numeric(df$age)
  	
  	#p <-
			ggplot(df, aes(yrprior_btemp, est)) +
					geom_ribbon(aes(ymin = lwr, ymax = upr), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ reorder(age_label, age), scales = "fixed") +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					#ggtitle(sp) +
					theme_sleek() +
					theme(
				 		panel.spacing.y = unit(0, "lines"))

  }

  plots <- map(temp_preds, plot_fun)
  