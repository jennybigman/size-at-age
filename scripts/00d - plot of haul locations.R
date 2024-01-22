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
	