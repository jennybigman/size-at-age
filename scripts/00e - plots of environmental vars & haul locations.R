# plots of temp and oxygen

	#### wrangle temp and oxy metrics for plotting
	presurvey_vars <- presurvey_hind_var %>%
		mutate(value = presurvey_mean_val)
	
	yr_prior_vars <- yr_prior %>%
		mutate(value = mean_yr) %>%
		rename()

	# tidy dat of age 0 temp and oxy
	trim_func <- function(x){
	
		df <- x %>%
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
	
	temp_dat <- dat_var %>% filter(str_detect(var, 'temp'))
	
	temp_plots <- 
		ggplot() +
		geom_line(data = temp_dat, 
							aes(x = year, y = value)) +
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

  #######################################################
  
  presurvey_vars <- presurvey_hind_var_short %>%
  		pivot_longer(cols = contains(c("btemp", "boxy")),
                   names_to = "var",
                   values_to = "value")
  
  yrprior_vars <- yr_prior_short %>%
  		pivot_longer(cols = contains(c("btemp", "boxy")),
                   names_to = "var",
                   values_to = "value")
  
  age0_vars <- age0_dat %>%
  	#select(-cohort) %>%
  		pivot_longer(cols = contains(c("btemp", "boxy")),
                   names_to = "var",
                   values_to = "value")
  
  all_vars <- bind_rows(presurvey_vars, yrprior_vars)

  # plot
  
  ggplot() +
		geom_line(data = all_vars %>% filter(str_detect(var, "temp")), 
							aes(x = year, y = value)) +
		facet_wrap( ~ var, scales = "free")

  ggplot() +
		geom_line(data = all_vars %>% filter(str_detect(var, "oxy")), 
							aes(x = year, y = value)) +
		facet_wrap( ~ var, scales = "free")
  	  	
  ggplot() +
		geom_line(data = age0_dat_long %>% filter(str_detect(var, "temp")), 
							aes(x = year, y = value, color = cohort)) +
		facet_wrap( ~ var, scales = "free")
  
  
  cohort1993 <- age0_dat_long %>% filter(cohort == 1999)

  
  # plot temp and oxygen from level 2 data
  
  # read in bottom temps
  bot_temp_hind_dat <- fread(file = here("./data/hindcast_bot_temp_K20P19.csv"))

  # trim to survey grid 
  survey_grid <- pollock_dat %>%
		select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	
	# find temp value closest to survey haul locations
	roms_grid <- bot_temp_hind_dat %>%
		distinct_at(vars(latitude, longitude)) %>%
		mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	roms_grid$roms_ID <- 1:nrow(roms_grid)
	
	# match lat/longs of survey grid to nearest neighbor from ROMS grid	
	
	# use nn2() to calculate min distance to nearest ROMS lat/long
	survey_grid[, c(4, 5)] <- as.data.frame(RANN::nn2(roms_grid[, c('latitude', 'longitude')],
                                                  survey_grid[, c('latitude', 'longitude')],
                                                  k = 1))
	
	# Match nearest lat/long from ROMS
	survey_grid$roms_ID <- pull(roms_grid[c(survey_grid$nn.idx), 'roms_ID'])
	
	# any NAs in matching?
	which(is.na(survey_grid$roms_ID), )
	
	# drop cols from nn2
	survey_grid <- survey_grid %>%
		select(-nn.idx, -nn.dists)

	# remove unecessary cols and convert lat/long
	bot_temp_hind_dat <- bot_temp_hind_dat %>%
			select(-Xi, -Eta, -DateTime, -Time) %>%
			mutate(longitude = case_when(
					 longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude * -1))
	
	# add in col denoting grid ID based on lat/long
	bot_temp_hind_dat <- left_join(bot_temp_hind_dat, roms_grid)
	
	# filter out grid cells/points that aren't in survey data
	ID_keep <- sort(unique(survey_grid$roms_ID))

	roms_bot_temps <- bot_temp_hind_dat %>%
 		filter(roms_ID %in% ID_keep)
	
	# summarise temps by month and year for each grid cell/point
	roms_temp_sum <- roms_temps %>%
		group_by(roms_ID, month, year) %>%
		summarise(temp = mean(temp)) 

	
	#### mapping haul locations ####
	
# map by lat/long
	all_sp_dat_loc <- df_list_wrangled_names %>%
		#distinct(latitude, longitude, .keep_all = TRUE) %>%
		na.omit() %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>% 
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
  
	# plot locations of hauls
	
	library(rnaturalearth)
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 

	breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)
	
	breaks_y <- c(55, 60)
	limits_y <- c(470000, 1900000)

	haul_loc_map <- 
		ggplot() +
		geom_sf(data = all_sp_dat_loc, aes(color = age))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			limits = c(470000, 1900000),
 			name = "Latitude") +
		theme_bw() +
		theme(legend.position = "none",
					plot.title = element_text(hjust = 0.5),
					plot.tag.position = c(0.2, 0.87),
					axis.text = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks.x = element_line(colour = "grey50"),
  	  		axis.line = element_blank(),
  	  		axis.title.x = element_text(size=14, color = "grey50"),
  	  		panel.border = element_rect(fill = NA, color = "grey50"),
					plot.margin = margin(0, 0, 0, 0, "cm"))

	ggsave(haul_loc_map,
				 file = here("./output/plots/map_haul_locations.png"))

# map by station id
	# plot locations of hauls
	stations <- df_list_wrangled_names %>%
		distinct(stationid, .keep_all =  TRUE)
	
	just_stations <- sort(unique(stations$stationid))
	
	haul_loc_map <- 
		ggplot(data = stations, 
					 aes(x = longitude, y = latitude,
					 		label = stationid)) +
		geom_text() +
		theme_sleek()
	
	long_stations <- stations %>%
		filter(str_length(stationid) > 4)
	
	haul_loc_map <- 
		ggplot(data = long_stations, 
					 aes(x = longitude, y = latitude,
					 		label = stationid)) +
		geom_text() +
		theme_sleek()
	