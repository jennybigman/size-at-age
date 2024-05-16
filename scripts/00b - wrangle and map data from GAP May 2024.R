# wrangle updated GAP data May 14 2024

	GAP_dat_fixed <- GAP_dat_fixed %>%
		drop_na(weight, length, age) %>%
		filter(weight > 0,
					 length > 0,
					 age > 0)
	
	GAP_dat_fixed <- GAP_dat_fixed %>%
		dplyr::select(species_code, length, weight, age, start_time,
									start_latitude, start_longitude, stationid, 
									common_name)
	
	GAP_dat_fixed <- GAP_dat_fixed %>%
		mutate(date =  as_date(start_time),
					 month = lubridate::month(date),
					 year =  lubridate::year(date),
					 jday =  lubridate::yday(date),
					 cohort = year - age)
		
	GAP_dat_fixed <- GAP_dat_fixed %>%
			rename(latitude = start_latitude,
						 longitude = start_longitude)
	
	sum <- GAP_dat_fixed %>% 
		group_by(common_name, age) %>%
		summarise(count = n())
	
	GAP_dat_fixed$age_f <- as.factor(GAP_dat_fixed$age)
	
	GAP_dat_fixed <- GAP_dat_fixed %>%
		group_by(common_name, age_f) %>%
		filter(n() > 100) %>%
		drop_na(age, stationid)
	
	sum2 <- GAP_dat_fixed %>% 
		group_by(common_name, age) %>%
		summarise(count = n())

	#### map sample location ####
	
	# alaska map for plot
	reg = map_data("world2Hires")
	reg = subset(reg, region %in% c('USSR', 'USA'))
	
	# convert lat longs
	reg$long = (360 - reg$long)*-1

	# file path for plots
	file_path_plots <- paste0(here(), "/plots/May 2024/")

	# mapping function - no 0 catch	####
	survey_map_fun <- function(sp){
		
	new_dat <- GAP_dat_fixed %>%
		dplyr::filter(common_name == sp) 

	p <- 
		ggplot(new_dat, aes(x = longitude, y = latitude)) +
		geom_hex(bins = 150, alpha = 0.9) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  		fill = "darkgrey", color = NA) +
		scale_fill_viridis_c() +
		coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) + 
		facet_wrap(~ year) +
		theme_sleek() +
		theme(panel.grid.major = element_line(color = "grey")) +
		ggtitle(sp)
	
	plot_name <- paste0(sp, "_all_dat_map_May2024.png")
	
	ggsave(p, file = paste0(file_path_plots, plot_name),
				 width = 10.5, height = 5.5, units = "in")
	
	}
	
	sp <- unique(GAP_dat_fixed$common_name)

	#purrr::map(sp, survey_map_fun)
	

	