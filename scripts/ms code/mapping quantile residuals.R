# mapping quantile residuals

	# set up AK polygon
	poly = map_data("world2Hires")
	poly = subset(poly, region %in% c('USSR', 'USA'))

	# convert lat longs
	poly$long = (360 - poly$long)*-1

	# set map limits
	lons = c(-179, -158)
	lats = c(54, 63)

	# degree symbol
	degree <- "\u00B0"


	# load models
	file_path_MV <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	shared_file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	ssf_file_path <- paste0(here(), "/output/model output/tinyVAST/spatial field only/")
	
	MV_top_mods <- readRDS(file = paste0(here(), "/output/tables/MV_top_mods.rds"))
	shared_top_mods_temp <- readRDS(file = paste0(here(), "/output/tables/shared_top_mods_temp.rds"))
  ssf_top_mods_temp <- readRDS(file = paste0(here(), "/output/tables/ssf_top_mods_temp.rds"))
  
  shared_top_mods <- readRDS(file = paste0(here(), "/output/tables/shared_top_mods.rds"))
  ssf_top_mods <- readRDS(file = paste0(here(), "/output/tables/ssf_top_mods.rds"))
  
  
  shared_top_mods <- unique(c(shared_top_mods, shared_top_mods_temp))

  # read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	# read in mods
	top_mods_MV <- purrr::map2(file_path_MV, MV_top_mods, read_mod_fun)
	top_mods_shared <- purrr::map2(shared_file_path, shared_top_mods, read_mod_fun)
	top_mods_ssf <- purrr::map2(ssf_file_path, ssf_top_mods_temp, read_mod_fun)

	
	# function to extract and map residuals
	resid_fun <- function(mod, mod_name, type){
		
		dat <- mod$data
		dat$resid <- residuals(mod, type = "deviance")
		dat$mod_name <- mod_name
		dat$age_label = paste0("Age", " ", dat$age)

		p <- 
			ggplot(dat) +
				#geom_polygon(data = poly, aes(x = long, y = lat, group = group), 
			#							 fill = "darkgrey", color = NA) +
				geom_point(aes(x = longitude, y = latitude, color = resid), alpha = 0.8) +
				scale_color_gradient2() +
				facet_grid(reorder(age_label, age) ~ year) +
				ggtitle(paste0(type, ":", mod_name)) +
				theme_light() +
				theme(panel.spacing = unit(0, 'lines')) +
				coord_fixed()
	
		ggsave(paste0(here(), "/output/tinyVAST resid maps/", mod_name, "_", type, ".png"),
				 p,
				 height = 10, width = 15, units = "in")
		
}

	purrr::pmap(list(top_mods_MV, MV_top_mods, "MV"), resid_fun)	
	purrr::pmap(list(top_mods_shared, shared_top_mods, "shared"), resid_fun)	
	purrr::pmap(list(top_mods_ssf, ssf_top_mods, "ssf"), resid_fun)	

	
	
	
