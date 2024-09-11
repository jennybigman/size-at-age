# plots of size-at-age and envir vars

	######################################################################### 
	# size-at-age  ####
	######################################################################### 
	
	size_age_sum <- dat_all %>%
		select(length, weight, age_f, year, log_wt, log_length, short_name) %>%
		group_by(short_name, year, age_f) %>%
		summarise(mean_wt = mean(weight),
							mean_ln = mean(length)) %>%
		mutate(log_wt = log10(mean_wt),
					 log_ln = log10(mean_ln))
	
	
	# file path for plots
	file_path_plots <- paste0(here(), "/output/plots/May 2024/")

	size_age_plot_fun <- function(sp){
		
		new_dat <- size_age_sum %>% filter(short_name == sp)
		
		p <- ggplot(new_dat) +
					geom_line(aes(x = year, y = log_wt)) +
					facet_wrap(~age_f, scales = "free") +
					theme_sleek() +
					ggtitle(sp)
		
		plot_name <- paste0(sp, "_change_SAA.png")
	
		ggsave(p, file = paste0(file_path_plots, plot_name),
					 width = 10, height = 7, units = "in")
	
	}
	
	sp <- unique(size_age_sum$short_name)
	
	purrr::map(sp, size_age_plot_fun)
	
	######################################################################### 
	# envr vars ####
	######################################################################### 
	
	temp_hind_dat <- read_csv(file = here("./data/hindcast_temp_K20P19.csv"))
	oxy_hind_dat <- read_csv(file = here("./data/hindcast_oxy_K20P19.csv"))
	
	temp_yr_sum <- temp_hind_dat %>%
		dplyr::group_by(year) %>%
		dplyr::summarise(mean_val = mean(temp, na.rm = TRUE)) %>%
		mutate(var = "temp",
					 mean_val_std = as.vector(scale(mean_val)))

	yrs <- 1987:2023
	
	temp_yr_sum <- temp_yr_sum %>%
		filter(year %in% yrs)
	
	# oxygen
	oxy_yr_sum <- oxy_hind_dat %>%
		dplyr::group_by(year) %>%
		dplyr::summarise(mean_val = mean(oxygen, na.rm = TRUE)) %>%
		mutate(var = "oxygen",
					 mean_val_std = as.vector(scale(mean_val)))

	yrs <- 1987:2023
	
	oxy_yr_sum <- oxy_yr_sum %>%
		filter(year %in% yrs)
	
	envr_vars <- bind_rows(temp_yr_sum, oxy_yr_sum)
	
		
	breaks = c(-2, 0, 2, 4)
	labels = c(-2, 0, 2, 4)
	lims = c(-2.5, 4.25)

	
	p1 <- 
		ggplot(envr_vars) +
		geom_line(aes(x = year, y = mean_val_std, group = var, color = var)) +
			scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) +
		theme_sleek()
	
		ggsave(paste0(here(), "/output/plots/May 2024/envr_time_series_ROMS.png"), p1,
				 height = 4, width = 7.5)

	################ plot envr vars for each species #####################
	envr_var_sum <- dat_all %>%
		select(short_name, contains(c("temp", "oxy")), year) %>%
		pivot_longer(
			cols = contains(c("temp", "oxy")),
			names_to = "var",
			values_to = "value"
		) %>%
		group_by(short_name, year, var) %>%
		summarise(mean_val = mean(value))
		
	
	sp_specific_vars <-
		ggplot(envr_var_sum) + 
		geom_line(aes(x = year, y = mean_val, color = var, group = var)) +
		facet_wrap(~ short_name) +
		theme_sleek()
	
	ggsave(here("./output/plots/May 2024/envr_time_series_ROMS_sp.png"), sp_specific_vars,
				 width = 7.5, height = 5, units = "in")

	#####################################################################################
	## over survey grid ####
		
	EBS_survey_grid <- st_read("./data/Kelly shapefiles/EBS_NBS_2019.shp")	

	plot(EBS_survey_grid)	
	
	EBS_survey_grid <- st_transform(EBS_survey_grid, 4326)
	
	plot(EBS_survey_grid)

	# trim temp & oxygen datasets to SEBS grid ####
	
	# unify polygon
	BS_poly <- st_union(EBS_survey_grid$geometry)

	plot(BS_poly)	
	
	temp_sf <- temp_hind_dat %>%
		mutate(lon = case_when(
			longitude >= 180 ~ longitude - 360,
			longitude < 180 ~ longitude))  %>% 
		rename(lat = latitude) %>%
  	st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
	oxy_sf <- oxy_hind_dat %>%
		mutate(lon = case_when(
			longitude >= 180 ~ longitude - 360,
			longitude < 180 ~ longitude))  %>% 
		rename(lat = latitude) %>%
  	st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
	
	temp_survey_grid <- temp_sf %>%
  	st_intersection(BS_poly) 
	
	temp_survey_grid <- temp_survey_grid %>%
  	st_transform(crs = 4326) 
	
	temp_survey_grid <- temp_survey_grid %>%
  	mutate(longitude = st_coordinates(.)[,1],
  				 latitude = st_coordinates(.)[,2]) %>%
  	st_drop_geometry()
  
	# do the same for this 
	oxy_survey_grid <- oxy_sf %>%
  	st_intersection(BS_poly) 
	
	oxy_survey_grid <- oxy_survey_grid %>%
  	st_transform(crs = 4326) %>%
  	mutate(longitude = st_coordinates(.)[,1],
  				 latitude = st_coordinates(.)[,2]) %>%
  	st_drop_geometry()

	# write_csv(temp_survey_grid, file = here("./data/temp_survey_grid_hind.csv"))
	# write_csv(oxy_survey_grid, file = here("./data/oxy_survey_grid_hind.csv"))
  
	# summarize by year
	
	# how many NAs
	
	# temp
	temp_cc <- temp_survey_grid %>% filter(is.na(temp))
	unique(temp_cc$year)
	
	NA_sum <- temp_cc %>% group_by(year, month) %>%
		summarise(count = n())
	
	temp_cc_sum <- temp_cc %>%
		group_by(lat, lon) %>%
		summarise(mean_temp = mean(temp))
	
	# map these by year
	ggplot(temp_cc_sum) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  							 fill = "darkgrey", color = NA) +
		coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) + 
		geom_point(aes(x = lon, y = lat))
	
	# oxygen
	oxy_cc <- oxy_survey_grid %>% filter(is.na(oxygen))
	unique(oxy_cc$year)
	
	NA_sum <- oxy_cc %>% group_by(year, month) %>%
		summarise(count = n())
	
	oxy_cc_sum <- oxy_cc %>%
		group_by(lat, lon) %>%
		summarise(oxygen = mean(oxygen))
	
	# map these by year
	ggplot(oxy_cc_sum) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  							 fill = "darkgrey", color = NA) +
		coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) + 
		geom_point(aes(x = lon, y = lat))

	
	# can probably drop them - on land?
		
	temp_grid_sum <- temp_survey_grid %>%
		drop_na(temp) %>%
 		group_by(year) %>%
		summarise(mean_val = mean(temp)) %>%
		mutate(var = "temp",
					 mean_val_std = as.vector(scale(mean_val)))

	oxy_grid_sum <- oxy_survey_grid %>%
		drop_na() %>%
		group_by(year) %>%
		summarise(mean_val = mean(oxygen)) %>%
		mutate(var = "oxygen",
					 mean_val_std = as.vector(scale(mean_val)))

	envr_var_grid <- bind_rows(temp_grid_sum, oxy_grid_sum) %>%
		filter(year %in% yrs)
	
	p2 <- 
		ggplot(envr_var_grid) +
		geom_line(aes(x = year, y = mean_val_std, 
									group = var, color = var)) +
		scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) +
		theme_sleek()
	
	ggsave(here("./plots/envr_time_series_ROMS_surveygrid.png"), p2,
				 				 height = 4, width = 7.5)


	# calculating presurvey temp ####
	presurvey_mo <- 4:6 # (April to June)
  
  presurvey_temp <- temp_survey_grid %>%
  	drop_na(temp) %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(year) %>%
  	summarise(pre_val = mean(temp)) %>%
  	filter(year %in% yrs) %>%
  	mutate(var = "pre_temp",
  				 pre_val_std = as.vector(scale(pre_val))) 

	 presurvey_oxy <- oxy_survey_grid %>%
  	drop_na(oxygen) %>%
  	filter(month %in% presurvey_mo) %>%
  	group_by(year) %>%
  	summarise(pre_val = mean(oxygen)) %>%
	 	filter(year %in% yrs) %>%
	 	mutate(var = "pre_oxygen",
  				 pre_val_std = as.vector(scale(pre_val))) 
	 
	 pre_survey_vars <- bind_rows(presurvey_temp, presurvey_oxy)
	 
	p3 <- 
		ggplot(pre_survey_vars) +
		geom_line(aes(x = year, y = pre_val_std, 
									group = var, color = var)) +
		scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) + 
		theme_sleek()
	
	ggsave(here("./plots/envr_time_series_ROMS_surveygrid_presurvey.png"), p3,
				 				 height = 4, width = 7.5)

	 # calculating year prior temp ####
	 
	  yr_prior_temp_func <- function(x){
   	
   	# temp same year as collected, Jan - June
   	current <- temp_survey_grid %>%
   		drop_na(temp) %>%
   		filter(year == x & month <= 6)
 
   	# temp previous year July - Dec
		previous <- temp_survey_grid %>%
			drop_na() %>%
  		filter(year == x - 1 & month > 6)
 
		# combine and take a mean temp July - June
  	var_yr <- bind_rows(current, previous) %>%
  		summarise(yr_val = mean(temp)) %>%
  		mutate(var = "yr_temp",
  					 year = x)
   
   }
   
  yr_prior_temp <- lapply(1970:2023, yr_prior_temp_func) %>% bind_rows()
  
  yr_prior_temp_trim <- yr_prior_temp %>% 
  	filter(year %in% yrs) %>%
  	 	mutate(yr_val_std = as.vector(scale(yr_val))) 
	
  # calculating year prior temp ####
	 
	 yr_prior_oxy_func <- function(x){
   	
   	# temp same year as collected, Jan - June
   	current <- oxy_survey_grid %>%
   		drop_na() %>%
   		filter(year == x & month <= 6)
 
   	# temp previous year July - Dec
		previous <- oxy_survey_grid %>%
			drop_na() %>%
  		filter(year == x - 1 & month > 6)
 
		# combine and take a mean temp July - June
  	var_yr <- bind_rows(current, previous) %>%
  		summarise(yr_val = mean(oxygen)) %>%
  		mutate(var = "yr_oxy",
  					 year = x)
   
   }
   
  yr_prior_oxy <- lapply(1970:2023, yr_prior_oxy_func) %>% bind_rows()
  
  yr_prior_oxy_trim <- yr_prior_oxy %>% 
  	filter(year %in% yrs) %>%
  	 	mutate(yr_val_std = as.vector(scale(yr_val))) 
	
  yr_prior_vars <- bind_rows(yr_prior_temp_trim, yr_prior_oxy_trim)
  
	p4 <- 
		ggplot(yr_prior_vars) +
		geom_line(aes(x = year, y = yr_val_std, 
									group = var, color = var)) +
		scale_y_continuous(
			breaks = breaks,
			labels = labels,
			limits = lims
		) +
		theme_sleek()
	
	
	ggsave(here("./plots/envr_time_series_ROMS_surveygrid_yrprior.png"), p4,
				 				 height = 4, width = 7.5)

	
	