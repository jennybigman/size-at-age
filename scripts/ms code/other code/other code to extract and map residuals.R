	resid_fun <- function(mod, mod_name){
		
		dat <- mod$data
		dat$resid <- residuals(mod, type = "deviance")
		dat$mod_name <- mod_name
		
		return(dat)
		
	}
	
	MV_resids <- purrr::map2_dfr(top_mods_MV, MV_top_mods, resid_fun)
	MV_resids$type = "MV"
	MV_resids$age_label = paste0("Age", " ", MV_resids$age)

	shared_resids <- purrr::map2_dfr(top_mods_shared, shared_top_mods, resid_fun)
	shared_resids$type = "shared"
	shared_resids$age_label = paste0("Age", " ", shared_resids$age)

	ssf_resids <- purrr::map2_dfr(top_mods_ssf, ssf_top_mods, resid_fun)
	ssf_resids$type = "ssf"
	ssf_resids$age_label = paste0("Age", " ", ssf_resids$age)

		
	#resids <- bind_rows(MV_resids, shared_resids, ssf_resids)
	#
	#resids$age_label = paste0("Age", " ", resids$age)
	#
	#MV_names <- tibble(name = MV_top_mods, type = "MV")
	#shared_names <- tibble(name = shared_top_mods, type = "shared")
	#ssf_names <- tibble(name = ssf_top_mods, type = "ssf")
	
	
	resid_plot_fun <- function(df, mod_name){
		
		df <- df %>% filter(mod_name == name, type == type)
		type <- unique(df$type)
		#sp <- unique(df$sp)
		
		#if (sp == "atooth")

		
		p <- 
			ggplot(df) +
				#geom_polygon(data = poly, aes(x = long, y = lat, group = group), 
			#							 fill = "darkgrey", color = NA) +
				geom_point(aes(x = longitude, y = latitude, color = resid), alpha = 0.8) +
				scale_color_gradient2() +
				facet_grid(reorder(age_label, age) ~ year) +
				ggtitle(paste0(type, ":", name)) +
				theme_light() +
				theme(panel.spacing = unit(0, 'lines'))
			
				#coord_map(xlim = lons, ylim = lats) +
				#scale_x_continuous(
			 	#	name = (label = paste("Longitude (", degree, "W)", sep = "")),
				#	breaks = c(-180, -170, -160),
				#	labels = c("180", "-170", "-160")
				#	) +
				#scale_y_continuous(
			 	#	name = (label = paste("Latitude (", degree, "N)", sep = "")),
				#	breaks = c(55, 60, 65),
				#	labels = c("55", "60", "65")
				#) +
			  #theme_bw() +
			  #theme(
				#	axis.text = element_text(size = 6, colour = "grey50"),
				#	axis.title = element_text(size = 8, color = "grey50"),
				#	panel.border = element_rect(fill = NA, color = "grey50"),
				#	axis.line = element_blank())
	
		ggsave(paste0(here(), "/output/tinyVAST resid maps/", name, "_", type, ".png"),
					 p,
					 height = 10, width = 15, units = "in")
		
}

	purrr::map2(MV_names$name, MV_names$type, resid_plot_fun)
	purrr::map2(shared_names$name, shared_names$type, resid_plot_fun)
	purrr::map2(ssf_names$name, ssf_names$type, resid_plot_fun)
	

mod2 <- top_mods_ssf[[1]]

r <- residuals(mod2)

d2 <- mod2$data

d2$residual <- r

ggplot(d2) +
	geom_point(aes(x = X, y = Y, color = residual)) +
	scale_color_gradient2() +
	facet_grid(age_f ~ year) +
	ggtitle("spatial only")


mod3 <- top_mods_shared[[1]]

r <- residuals(mod3)

d3 <- mod3$data

d3$residual <- r

ggplot(d3) +
	geom_point(aes(x = X, y = Y, color = residual)) +
	scale_color_gradient2() +
	facet_grid(age_f ~ year) +
	ggtitle("shared s and st")

