# map quantile residuals of the same model

# mapping quantile residuals

	# set up AK polygon
	#poly = mapdata::map_data("world2Hires")
	#poly = subset(poly, region %in% c('USSR', 'USA'))

	# convert lat longs
	#poly$long = (360 - poly$long)*-1

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
	
	n <- MV_top_mods[2]
	n <- str_extract(n, "yrprior_btemp_poly2_tvt.rds")
	n <- paste0("pcod_", n)
	
	a <- MV_top_mods[1]
	pl <- MV_top_mods[3]
	y <- MV_top_mods[4]
	
	top_mods <- c(a, n, pl, y)
	
  # read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	
	mod <- str_subset(MV_top_mods, "pollock")

	pol_MV <- purrr::map2(file_path_MV, mod, read_mod_fun)
	pol_shared <- purrr::map2(shared_file_path, mod, read_mod_fun)
	pol_ssf <- purrr::map2(ssf_file_path, mod, read_mod_fun)
	
	#all_mods <- list(MV, shared, ssf)
	names <- c('age-specific fields', 'shared spatial/spatiotemporal fields', 'shared spatial field')
	
	# plot age 1 and 11 pollock residuals for years 2009 and 2019

	
#	resids <- purrr::map2_dfr(pol_mods, names, \ (mod, mod_name){
		
	resid_comp_fun <- function(mod, mod_name){

		# extract residuals
		dat <- mod$data
		dat$resid <- residuals(mod, type = "deviance")
		dat$mod_name <- mod_name

		dat
		
	}
	
	pol_MV_resids <- purrr::map2_dfr(pol_MV, "age-specific fields", resid_comp_fun)	
	pol_shared_resids <- purrr::map2_dfr(pol_shared, "shared spatial/spatiotemporal fields", resid_comp_fun)	
	pol_ssf_resids <- purrr::map2_dfr(pol_ssf, "shared spatial field", resid_comp_fun)	
	
	pol_resids <- bind_rows(pol_MV_resids, pol_shared_resids, pol_ssf_resids)
	
	pol_resids$age_label <- paste0("Age ", pol_resids$age)
	
	# filter rows and years out
	years1 <- c(2009, 2019)
	years2 <- c(2009, 2019)
		
	pol_resids_trim1 <- pol_resids |>
		filter(age == 1 & year %in% years1) 
	
	pol_resids_trim2 <- pol_resids |>
		filter(age == 12 & year %in% years2) 
	
	pol_resids_trim <- bind_rows(pol_resids_trim1, pol_resids_trim2)
	
	pol_resids_trim$residual <- pol_resids_trim$resid

	
	p <- 
		ggplot(pol_resids_trim) +
		geom_point(aes(x = longitude, y = latitude, color = residual), alpha = 1) +
		scale_color_gradient2() +
		facet_grid(age_label + year ~ mod_name) +
		theme_sleek() +
		theme(
			panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 0.5),
   		panel.spacing = unit(0, "lines"),
			panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
		)
	
		ggsave(file = paste0(here(), "/output/plots for paper/Fig4.png"), 
				 p,
				 height = 5, width = 7)
	

	# for all species and ages
	
	# read in mods
	top_mods_MV <- purrr::map2(file_path_MV, MV_top_mods, read_mod_fun)
	mods_shared <- purrr::map2(shared_file_path, top_mods, read_mod_fun)
	mods_ssf <- purrr::map2(ssf_file_path, top_mods, read_mod_fun)

	resids <- function(mod, mod_name) {
		
		dat <- mod$data
		dat$resid <- residuals(mod, type = "deviance")
		dat$mod_name <- mod_name
		dat$age_label = paste0("Age", " ", dat$age)
		
		dat

	}
	
	MV_resids <- purrr::map2_dfr(top_mods_MV, "independent fields", resids)
	shared_resids <- purrr::map2_dfr(mods_shared, "shared spatial and spatiotemporal fields", resids)
	ssf_resids <- purrr::map2_dfr(mods_ssf, "spatial field only", resids)

	resids <- bind_rows(MV_resids, shared_resids, ssf_resids)
	
	resids$residual <- resids$resid
	
	resids <- resids |>
		mutate(proper_name = case_when(
			common_name == "arrowtooth flounder" ~ "Arrowtooth flounder",
			common_name == "walleye pollock" ~ "Walleye pollock",
			common_name == "yellowfin sole" ~ "Yellowfin sole",
			common_name == "Pacific cod" ~ "Pacific cod",
		))

	resids_list <- resids |>
		group_split(proper_name)

	map_resid <- function(dat, name){
		
		sp <- unique(dat$proper_name)

		d <- dat |> 
			filter(mod_name == name)
		
		p <- 
			ggplot(d) +
				geom_point(aes(x = longitude, y = latitude, color = residual), 
									 alpha = 1, size = 1) +
				scale_color_gradient2(limits = c(-1, 0.82)) +
				scale_x_continuous(
					breaks = c(-175, -165)) +
				scale_y_continuous(
					breaks = c(57, 60)) +
				facet_grid(age ~ year) +
				ggtitle(paste0(sp, ": " , name)) +
			theme_sleek() + 
			theme(
				strip.text = element_text(size = 10),
				legend.position = "bottom",
				panel.border = element_rect(color = "grey", fill = "NA", linewidth = 0.5),
				panel.spacing = unit(0, "lines"),
				panel.grid.major = element_blank(),
	    	panel.grid.minor = element_blank(),
				axis.text = element_text(size = 8))
		
		ggsave(file = paste0(here(), "/output/plots for paper/", sp, "_", name, ".png"), 
				 p,
				 height = 7, width = 15)
		
		
	}
	
	fdf <- crossing(
		dat = resids_list,
		name = c("independent fields", 
						 "shared spatial and spatiotemporal fields", 
						 "spatial field only")
		#mod_name = "independent fields"
	)
	
	purrr::map2(fdf$dat, fdf$name, map_resid)

	

	
# function to extract and map residuals
	resid_fun <- function(mod, mod_name, type){
		
		dat <- mod$data
		dat$resid <- residuals(mod, type = "deviance")
		dat$mod_name <- mod_name
		dat$age_label = paste0("Age", " ", dat$age)

		p <- 
			ggplot(dat) +
			geom_point(aes(x = longitude, y = latitude, color = resid), alpha = 0.8) +
			scale_color_gradient2() +
			facet_grid(reorder(age_label, age) ~ year) +
			ggtitle(paste0(mod_name, type)) +
			coord_cartesian()
	
		p
	#	ggsave(paste0(here(), "/output/plots for paper/", mod_name, "_", ".png"),
	#			 p)
		
}

	p <- purrr::map2(top_mods_MV, MV_top_mods, resid_fun)	
	p <- purrr::pmap(list(mods_shared, top_mods, "shared"), resid_fun)	
	
	p <- purrr::pmap(list(mods_ssf, top_mods, "ssf"), resid_fun)	

	
	
	
