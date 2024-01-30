# older code for setting up data

#	## code random effect haul and year ##
#	year_sort <- sort(unique(specimen_dat$year))
#	ID <- letters[1:length(year_sort)]
#	
#	year_ID <- data.frame(year_sort, ID) %>%
#		rename(year = year_sort)
#	
#	specimen_dat <- left_join(specimen_dat, year_ID, by = "year")
#
#	specimen_dat <- specimen_dat %>%
#		mutate(haul_id = paste(ID, haul, sep = "_"))
#
#	specimen_dat <- specimen_dat %>%
#	mutate(ID_f = as.factor(ID),
#				 haul_id_f = as.factor(haul_id))
#	

#	#### 3. temp during first year of life ####
 # 
#	age0_func <- function(x){
#	
#		df <- x %>%
#		mutate(year_age0 = cohort,
#					 year_age0_f = as.factor(year_age0))
#		
#		yr_age0_dat <- df %>%
#			ungroup() %>%
#			dplyr::select(year_age0_f, year_age0) %>%
#			rename(year = year_age0) %>%
#			distinct()
#		
#		yr_age0_vars <- left_join(yr_age0_dat, yr_prior_short) %>%
#			ungroup() %>%
#			mutate(cohort = year) %>%
#			rename(age0_btemp = yrprior_btemp,
#						 age0_boxy = yrprior_boxy) %>%
#			select(cohort, age0_btemp, age0_boxy) %>%
#			distinct(cohort, .keep_all = T)
#	
#		dat <- left_join(df, yr_age0_vars)
#	
#	}	
#	
#	specimen_dat <- lapply(specimen_dat, age0_func)
#	
#	# plot
#	age0_dat_trim_func <- function(x){
#		
#		new_dat <- x %>% 
#			ungroup() %>%
#			select(species_name, year, age0_btemp, age0_boxy)
#	}
#
#	age0_dat <- lapply(specimen_dat, age0_dat_trim_func) %>% bind_rows()
		
 # age0_long <- age0_dat %>%
 # 	pivot_longer(
 # 		cols = starts_with("age0"),
 # 		names_to = "var",
 # 		values_to = "val")
 # 
#	ggplot(data = age0_long) +
#		geom_line(aes(x = year, y = val)) +
#		facet_grid(species_name ~ var, scales = "free")
	
	pollock_dat <- specimen_dat_list[[1]]
	pcod_dat <- specimen_dat_list[[2]]
	yfinsole_dat <- specimen_dat_list[[3]]

	
 ID_keep <- sort(unique(survey_grid$roms_ID))

 roms_temps <- temp_hind_dat %>%
 	filter(roms_ID %in% ID_keep)