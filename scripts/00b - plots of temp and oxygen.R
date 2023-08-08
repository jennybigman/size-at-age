# plots of temp and oxygen

	#### wrangle temp and oxy metrics for plotting
	presurvey_vars <- presurvey_hind_var %>%
		mutate(value = presurvey_mean_val)
	
	yr_prior_vars <- yr_prior %>%
		mutate(value = mean_yr)

	# tidy dat of age 0 temp and oxy
	trim_func <- function(x){
	
		df %>%
		ungroup() %>%
		select(cohort, year, age0_boxy, age0_btemp) %>%
		distinct(cohort, .keep_all = TRUE)
		
	}
	
	age0_dat <- lapply(list(pollock_dat, pcod_dat, yfinsole_dat),
											trim_func) %>%
							bind_rows() %>%
							distinct(cohort, .keep_all = TRUE)
	
	age0_dat_long <- age0_dat %>%
			rename(oxygen_bottom5m = age0_boxy,
						 temp_bottom5m = age0_btemp) %>%
		  pivot_longer(cols = contains("bottom"),
                   names_to = "var",
                   values_to = "value")

	vars_yr <- 
		ggplot() +
		geom_line(data = presurvey_vars, 
							aes(x = year, y = value),
							color = "red") +
		geom_line(data = yr_prior_vars, 
							aes(x = year, y = value),
							color = "blue") +
		geom_line(data = age0_dat_long,
						 aes(x = year, y = value),
							color = "black") +
		facet_wrap( ~ var, scales = "free")
	
 
	#######################################
	
	trim_func <- function(x){
		x %>% 
			ungroup() %>%
			select(year, haul_id, age0_boxy, age0_btemp, 
						 species, presurvey_boxy, presurvey_btemp,
						 yrprior_boxy, yrprior_btemp, log_wt, age_f)
	}
	
	dat_var <- lapply(specimen_dat, trim_func) %>%
		bind_rows() %>%
		pivot_longer(cols = contains(c("btemp", "boxy")),
                   names_to = "var",
                   values_to = "value")
	
	
	vars_id <- 
		ggplot() +
		geom_line(data = dat_var, 
							aes(x = log_wt, y = value)) +
		facet_grid(species ~ var, scales = "free")

	plot_func <- function(x){
		
		new_dat <- dat_var %>% 
			filter(grepl(x, var)) %>%
			group_by(age_f, species, var, log_wt) %>%
			summarise(mean_val = mean(value))
		
		ggplot() +
		geom_line(data = new_dat, 
							aes(x = log_wt, y = mean_val)) +
		facet_grid(species + age_f ~ var, scales = "free")
	}
	
	vars <- c("temp", "oxy")
	
	plot_list <- lapply(vars, plot_func)	
	
	names(plot_list) <- vars

	file_path_func <- function(x){
  	paste0("/Users/jenniferbigman/Library/CloudStorage/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/plots/", x)
  }
   
  file_paths <- sapply(names(plot_list), file_path_func)
	
  mapply(ggsave_func, x = plot_list, y = file_paths)
