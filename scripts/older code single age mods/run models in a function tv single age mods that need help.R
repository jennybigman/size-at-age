# try to fit models for ages that are missing

	file_path <- "/output/model output/sdmTMB output/May 2024/sep age classes/time-varying/"

	vars <- names(dat_all %>% select(contains("yrprior")))
	
	mod_types <- c("poly2", "poly3", "lin", "gam")
	
	#### arrowtooth ######## arrowgam()tooth ####
	
	atooth_dat <- dat_all %>% filter(short_name == "atooth")
	
	# temp
	# ages 
		# 7 for gam, both polys
		# 9 for all
		# 11 for all but poly 2
		# 12 for all but linear
	
	# age 9 
	
	atooth_dat_9 <- atooth_dat %>% filter(age_f == 9)
	
	mesh <- make_mesh(
			atooth_dat_9, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				max.edge = 80,
				offset = c(60, 70)
			)
	
	plot(mesh)
	
	mesh$mesh$n

	#form <- paste0("log_wt ~ poly(yrprior_btemp, 2, raw = TRUE)")
	
	fdf <- crossing(
		mod_type = mod_types,
		y = vars
	)
	
	purrr::map(fdf$mod_type, fdf$y, \(mod_type, y){
	
		if (mod_type == "poly2") {
						
				form <- paste0("log_wt ~ poly(", y, ", 1, raw = TRUE)")
					
		} else if (mod_type == "poly3") {
							
				form <- paste0("log_wt ~ poly(", y, ", 3, raw = TRUE)")
					
		} else if (mod_type == "lin") {
						
				form <- paste0("log_wt ~", y)
						
		} else if (mod_type == "gam") {
						
			 form <- paste0("log_wt ~ s(" , y, ", k = 3)")
					
			}
	
	mod <- 
		sdmTMB(
			formula = as.formula(form),
			data = atooth_dat_9,
			mesh = mesh,
			time_varying = ~ 1,
			time_varying_type = "ar1",
			control = sdmTMBcontrol(profile = "b_j"),
			spatial = "off",
			spatiotemporal = "IID",
		  time = "year",
		  share_range = FALSE,
		  silent = FALSE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
		mod_name <- paste0("atooth_9_", form, "_", y)
		
		write_rds(mod, 
			file = paste0(here(), file_path, mod_name, ".rds"))
		 		
	})
	
	
	sanity(mod_poly2)
	