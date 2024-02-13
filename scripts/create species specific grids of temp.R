# plotting temp grid for each species

	#### function to create grid of temps for each species ####
	
	sp_grid_func <- function(sp){
		
			sp_dfs <- all_dfs %>% filter(short_name == sp)
			
			sp_strat <- unique(sp_dfs$STRATUM)

			EBS_survey_grid_trim <- EBS_survey_grid %>%
				filter(STRATUM %in% stratums)

			survey_polygon <- EBS_survey_grid_trim %>%
				st_union() 

			# create a grid using the raster package
			resolution <- 0.5
			r <- raster::raster(as(survey_polygon, "Spatial"), resolution = resolution)
			rr <- raster::rasterize(as(survey_polygon, "Spatial"), r, getCover = TRUE)
			
			grid <- as.data.frame(raster::rasterToPoints(rr))
			grid$area <- grid$layer * resolution * resolution
	
			grid <- dplyr::filter(grid, area > 0) %>%
	  		dplyr::select(-layer) 

			grid <- grid %>%
				dplyr::select(-area) %>%
				rename(lat = y,
						 lon = x)
	
			grid <- 
				sdmTMB::add_utm_columns(
					grid,
					c("lon", "lat"))
			
			grid$species <- sp
			
			# function to match temps by year
			fill_temps <- function(x){
		
				# summarize to year
				roms_temp_yr <- roms_temps %>%
					filter(year == x)
			
				grid <- grid %>%
					mutate(year = x)
				
				grid[, c(7, 8)] <- as.data.frame(
					RANN::nn2(roms_temp_yr[, c('Y', 'X')],
  			            grid[, c('Y', 'X')], k = 1))
				
				# Match nearest lat/long from ROMS
				grid$temp <- pull(roms_temp_yr[c(grid$nn.idx), 'temp'])
			
				# remove unecessary cols
				grid <- grid %>%
					dplyr::select(-contains("nn"))
				
	
		}
	
		yrs <- sort(unique(dat_all$year))
		
		grid_full <- lapply(yrs, fill_temps) %>% bind_rows()

		#file_name <- paste0(sp, "_grid.csv")
		
		#fwrite(grid_full, file = paste0(here(), "/data/", file_name))

	
	}
	
	sp <- unique(dat_all$short_name)
	
	#purrr::map(sp, sp_grid_func)
	
	grids <- purrr::map(sp, sp_grid_func)

	grids_all <- grids %>% bind_rows()
	
	fwrite(grids_all, file = paste0(here(), "/data/grids_all.csv"))

	
	#### plot all temps for all species ####
	
	# plot temps
	file_path_plots <- paste0(here(), "/plots/")
	
	plot_fun <- function(sp){
		
		sp_dat <- grids_all %>% filter(species == sp)
		
			p <- ggplot(sp_dat, aes(lon, lat, fill = temp)) +
			  geom_raster() +
				geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  		          fill = "darkgrey", color = NA) +
				scale_colour_viridis_c(direction = -1) +
				scale_fill_continuous(limits = c(-1.4, 5.2), breaks = seq(-1, 5, by = 1)) +
				coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) +
				facet_wrap(~ year) +
				theme_sleek() +
				ggtitle(sp)
			
			plot_name <- paste0(sp, "_temp_", ".png")
			
			ggsave(p, file = paste0(file_path_plots, plot_name))
		
	}

	purrr:::map(sp_names, plot_fun)
	
