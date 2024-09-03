# try to fit models for ages that are missing

	file_path <- "/output/model output/sdmTMB output/May 2024/sep age classes/time-varying/"

	#vars <- names(dat_all %>% select(contains("yrprior_boxy")))
	
	mod_types <- c("poly2", "poly3", "lin", "gam", "no_cov")
	
	poly2  <- log_wt ~ poly(yrprior_boxy, 2, raw = TRUE)
	poly3 <-  log_wt ~ poly(yrprior_boxy, 3, raw = TRUE)
	lin <-  	log_wt ~ yrprior_boxy
	gam <-		log_wt ~ s(yrprior_boxy, k = 3)
	no_cov <- log_wt ~ 1

	
	### run mod
	
	yfin_dat <- dat_all %>% 
		filter(short_name == "yfin") %>% 
		filter(age_f == 29)
	
	# mesh
	mesh <- make_mesh(
			yfin_dat, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 80,
				offset = c(60, 70)
			)
	
	plot(mesh)
	
	mesh$mesh$n


	mod <- 
		sdmTMB(
			formula = as.formula(no_cov),
			data =  yfin_dat,
			mesh = mesh,
			#time_varying = ~ 1,
			#time_varying_type = "ar1",
			control = sdmTMBcontrol(profile = "b_j"),
			spatial = "on",
			spatiotemporal = "iid",
		  time = "year",
		  share_range = FALSE,
		  silent = FALSE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 400, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 600, sigma_lt = 2)))
	
	sanity(mod)
	
	write_rds(mod, 
		file = paste0(here(), file_path, "yfin_dat_age_29_yrprior_btemp_no_cov.rds"))
		 		
##### run models for other species' ages that wouldn't run
		### possibly run all atooth mods without spatial effects
		### re compare temp mods and oxy mods
	
