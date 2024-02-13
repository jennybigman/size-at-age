# plots of temp and oxygen
	
	## plot environmental variable timeseries
	envr_dat <- dat_all %>%
		dplyr::select(contains(c("temp", "oxy")), 
					 latitude, longitude, stationid, 
					 date, year, roms_ID) %>%
		group_by(year) %>%
		summarise(mean_ytemp = mean(yrprior_btemp),
							mean_ptemp = mean(presurvey_btemp),
							mean_yoxy = mean(yrprior_boxy),
							mean_poxy = mean(presurvey_boxy),
							#sd_ytemp = sd(yrprior_btemp),
							#sd_ptemp = sd(presurvey_btemp),
							#sd_yoxy = sd(yrprior_boxy),
							#sd_poxy = sd(presurvey_boxy)) 
		)
	
	# timeseries
	envr_dat_long <- envr_dat %>%
		pivot_longer(
			col = contains(c("temp", "oxy")),
			names_to = "var",
			values_to = "value")
	
	vars_yr <- 
		ggplot(envr_dat_long, aes(x = year, y = value)) +
		geom_line() +
		facet_wrap( ~ var, scales = "free")
	
	############### ADD IN hindcast temp!
	
	
	
	
 
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
	all_sp_dat_loc <- dat_all %>%
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
	
	
	# for each species ####
	library(mapdata)
	
	reg = map_data("world2Hires")
	reg = subset(reg, region %in% c('USSR', 'USA'))
	
	# convert lat longs
	reg$long = (360 - reg$long)*-1
	
	yr_dat <- survey_grid_full %>% filter(year == 2000)
	
	# set map limits
	lons = c(-181, -160)
	lats = c(52, 65)

	haul_loc_func <- function(sp){
		
		sp_dat <- dat_all %>% filter(short_name == sp)
		
		sp_dat <- sp_dat %>% 
			distinct_at(vars(latitude, longitude))
		
		ggplot(sp_dat, aes(longitude, latitude)) +
		  geom_point() +
			geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	            fill = "darkgrey", color = NA) +
			scale_colour_viridis_c(direction = -1) +
		  coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
			ggtitle(sp) +
			theme_sleek()
		
	}
			
	sp <- unique(dat_all$short_name)
	
	plots <- purrr::map(sp, haul_loc_func)
		

	station_filter <- function(sp){
		
		sp_dat <- dat_all %>% filter(short_name == sp)
		
		sp_dat <- left_join(sp_dat, haul_dat)
	
		
	}
	
	sp <- unique(dat_all$short_name)

	sp_dfs <- purrr::map(sp, station_filter)
	
	all_dfs <- sp_dfs %>% bind_rows()

	stratums <- unique(all_dfs$STRATUM)	

	
		#### plot temps in models
	
	pollock_grid <- grids_all %>%
		filter(species == "pollock")
	
	pollock_envr_vars <- dat_all %>%
		dplyr::select(year_f, log_wt, age_f,
									short_name, X, Y, 
									roms_ID, latitude, longitude,
									contains(c("temp", "oxy")))
	
	fill_vars_fun <- function(yr){
		
		var_dat_yr <- pollock_envr_vars %>%
			filter(year_f == yr)
		
		pollock_grid$year <- yr
	
		pollock_grid[, c(8, 9)] <- as.data.frame(
			RANN::nn2(var_dat_yr[, c("longitude", "latitude")],
								pollock_grid[, c("lon", "lat")], k = 1))
	
		pollock_grid$presurvey_btemp <- 
			pull(pollock_envr_vars[c(pollock_grid$nn.idx), 'presurvey_btemp'])
	
		pollock_grid$yrprior_btemp <- 
			pull(pollock_envr_vars[c(pollock_grid$nn.idx), 'yrprior_btemp'])
		
		pollock_grid$presurvey_boxy <- 
			pull(pollock_envr_vars[c(pollock_grid$nn.idx), 'presurvey_boxy'])
		
		pollock_grid$yrprior_boxy <- 
			pull(pollock_envr_vars[c(pollock_grid$nn.idx), 'yrprior_boxy'])
		
	
		pollock_grid_long <- pollock_grid %>%
			pivot_longer(col = starts_with(c("pre", "yr")),
									 names_to = "variable",
									 values_to = "value") %>%
			dplyr::select(-contains("nn"))
		
		pollock_grid_long
		
	}
	
	yrs <- sort(unique(pollock_grid_long$year))
	
	pollock_grid_all <- purrr::map(yrs, fill_vars_fun) %>% bind_rows()

	
	plot_yr_fun <- function(yr){
		
		yr_dat <- pollock_grid_all %>% filter(year == yr)
		
		species <- unique(yr_dat$species)
		
		p <- ggplot(yr_dat, aes(lon, lat, fill = value)) +
		  geom_raster() +
			geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	            fill = "darkgrey", color = NA) +
			scale_colour_viridis_c(direction = -1) +
		  coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
			facet_wrap(~ variable) +
			#scale_fill_continuous(limits = c(-1.4, 5.2), breaks = seq(-1, 5, by = 1)) +
			ggtitle(paste0(yr, ":", species)) +
			theme_sleek()
		
		plot_name <- paste0(species, "_", yr, "_vars.png")
		
		ggsave(p, file = paste0(file_path_plots, plot_name),
					 height = 10, width = 10, units = "in")
		
	}
	
	yrs <- sort(unique(pollock_grid_long$year))
	
	plots <- purrr::map(yrs, plot_yr_fun)
	
	}
	