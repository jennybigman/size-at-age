# 03 - visualizations

	
	#### raw data -- weight over temp by age class ####

	# pcod
	ggplot(pcod_dat, aes(x = mean_sum_temp, y = log_wt)) +
		geom_point() +
		facet_wrap(~ age_f)
	
	# pollock
	ggplot(pollock_dat_new, aes(x = mean_sum_temp, y = log_wt)) +
		geom_point() +
		facet_wrap(~ age_f)
	
	# yellowfin sole
	ggplot(yfinsole_dat, aes(x = mean_sum_temp, y = log_wt)) +
		geom_point() +
		facet_wrap(~ age_f)
	
	
	#### plot output of gams ####
	
	# pcod (top model: temp_age_int_mod_pcod)
	