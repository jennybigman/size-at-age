# 03 - visualizations

	#### mapping haul locations ####
	
	all_sp_dat <- bind_rows(spec_temp_dat)
	
	all_sp_dat_loc <- all_sp_dat %>%
		distinct(latitude, longitude, .keep_all = TRUE) %>%
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

	haul_loc_map <- ggplot() +
		geom_sf(data = all_sp_dat_loc, aes(color = age_f))  +
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


	#### plotting model output ####
	
	# pollock ####
	
	pol_temp_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pol_temp_int_age_bam_ACLIM_SEBS.rds"))

	pol_plot <- visreg(pol_temp_int_age_bam_ACLIM_SEBS, "SEBS_mean_sum_temp", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("mean temperature April - June (˚C)") +
		facet_wrap(~ age_f, ncol = 5) +
		theme_classic() 

	ggsave(file = here("./output/plots/pol_plot2.png"),
			 pol_plot)

		
	# pcod ####
	
	pcod_temp1_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/pcod_temp1_int_age_bam_ACLIM_SEBS.rds"))

	pcod_plot <- visreg(pcod_temp1_int_age_bam_ACLIM_SEBS_ML, "temp_firstyr", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temperature (˚C) during\nfirst year of life") +
		facet_wrap(~ age_f, ncol = 6) 

	ggsave(file = here("./output/plots/pcod_plot.png"),
			 pcod_plot)
	
	# yfin sole ####
	
	yfin_temp1_int_age_bam_ACLIM_SEBS <- readRDS(file = here("./output/model output/ACLIM temps/yfin_temp1_int_age_bam_ACLIM_SEBS.rds"))

	yfin_plot <- visreg(yfin_temp1_int_age_bam_ACLIM_SEBS_ML, "temp_firstyr", by = "age_f",
										 gg = TRUE, partial = FALSE, rug = FALSE) +
		ylab("partial effect log\nscaled weight-at-age") +
		xlab("temperature (˚C) during\nfirst year of life") +
		facet_wrap(~ age_f, ncol = 9) 

	ggsave(file = here("./output/plots/yfin_plot.png"),
			 yfin_plot)
