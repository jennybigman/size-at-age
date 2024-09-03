
tv_oxy <- oxy_mod_comp %>% 
	pivot_longer(cols = c("lin", "poly2", "poly3", "no_cov"),
							 names_to = "type",
							 values_to = "AIC")

tv_oxy_top <- tv_oxy %>% 
	group_by(species, age) %>% 
	slice_min(AIC) %>%
	rename(tv_AIC = AIC)

s_oxy <- oxy_mod_comp_s %>% 
	pivot_longer(cols = c("lin", "poly2", "poly3", "no_cov"),
							 names_to = "type",
							 values_to = "AIC")

s_oxy_top <- s_oxy %>% 
	group_by(species, age) %>% 
	slice_min(AIC) %>%
	rename(s_AIC = AIC)

oxy_AIC <- left_join(tv_oxy_top, s_oxy_top)


#### temp


tv_temp <- temp_mod_comp %>% 
	pivot_longer(cols = c("lin", "poly2", "poly3", "no_cov"),
							 names_to = "type",
							 values_to = "AIC")

tv_temp_top <- tv_temp %>% 
	group_by(species, age) %>% 
	slice_min(AIC) %>%
	rename(tv_AIC = AIC)

s_temp <- temp_mod_comp_s %>% 
	pivot_longer(cols = c("lin", "poly2", "poly3", "no_cov"),
							 names_to = "type",
							 values_to = "AIC")

s_temp_top <- s_temp %>% 
	group_by(species, age) %>% 
	slice_min(AIC) %>%
	rename(s_AIC = AIC)

temp_AIC <- left_join(tv_temp_top, s_temp_top)
