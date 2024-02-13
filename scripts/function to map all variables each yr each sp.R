	#### plot temps in models
	
	plot_vars_fun <- function(sp){
		
	grid <- grids_all %>%
		filter(species == sp)
	
	envr_vars <- dat_all %>%
		dplyr::select(year_f, log_wt, age_f,
									short_name, X, Y, 
									roms_ID, latitude, longitude,
									contains(c("temp", "oxy")))
	
		fill_vars_fun <- function(yr){
		
			var_dat_yr <- envr_vars %>%
				filter(year_f == yr)
		
			grid$year <- yr
	
			grid[, c(8, 9)] <- as.data.frame(
				RANN::nn2(var_dat_yr[, c("longitude", "latitude")],
									grid[, c("lon", "lat")], k = 1))
	
			grid$presurvey_btemp <- 
				pull(var_dat_yr[c(grid$nn.idx), 'presurvey_btemp'])
	
			grid$yrprior_btemp <- 
				pull(var_dat_yr[c(grid$nn.idx), 'yrprior_btemp'])
			
			grid$presurvey_boxy <- 
				pull(var_dat_yr[c(grid$nn.idx), 'presurvey_boxy'])
			
			grid$yrprior_boxy <- 
				pull(var_dat_yr[c(grid$nn.idx), 'yrprior_boxy'])
		
	
		grid_long <- grid %>%
			pivot_longer(col = starts_with(c("pre", "yr")),
									 names_to = "variable",
									 values_to = "value") %>%
			dplyr::select(-contains("nn"))
		
		grid_long
		
	}
	
	yrs <- sort(unique(grid$year))
	
	grid_all_yrs <- purrr::map(yrs, fill_vars_fun) %>% bind_rows()

	
	plot_yr_fun <- function(yr){
		
		yr_dat <- grid_all_yrs %>% filter(year == yr)
		
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
	
	yrs <- sort(unique(grid_all_yrs$year))
	
	purrr::map(yrs, plot_yr_fun)
	
	}
	
	sp_names <- unique(dat_all$short_name)
	
	purrr::map(sp_names, plot_vars_fun)
	
	