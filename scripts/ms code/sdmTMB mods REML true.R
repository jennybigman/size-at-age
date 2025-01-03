
	file_path <- paste0(here(), "/output/model output/sdmTMB output/reml/")
	
		sp <- unique(dat_all$short_name)
		y <- dat_all %>% 
			ungroup %>% 
			select(contains(c("yrprior_btemp", "yrprior_boxy"))) %>%
			names() 
		
		fdf <- crossing(
			sp = sp,
			var = var
		)

	
		# no covariate model
		
		purrr::map(sp, \(sp){ 
	
			# filter data by species
			d <- dat_all |> filter(short_name == sp)

			# mesh
			mesh <- make_mesh(d, c("X", "Y"),
				fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 50,
				offset = c(70, 60))
	
			no_cov_fit <- 
			 try(
				sdmTMB(
					formula = log_wt ~ 0 + age_f,
					data = d,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "IID",
					time = "year",
					share_range = FALSE,
					silent = FALSE,
					reml = TRUE,
					priors = sdmTMBpriors(
						matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
						matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			 )
		
			mod_name <- "_no_cov"
	
			try(write_rds(no_cov_fit, 
						file = paste0(file_path, sp, mod_name, ".rds")))

		})
		
		
		#### linear ####
		
		purrr::map2(fdf$sp, fdf$var, \(sp, y){ 
			
			# filter data by species
			d <- dat_all |> filter(short_name == sp)

			# mesh
			mesh <- make_mesh(d, c("X", "Y"),
				fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 50,
				offset = c(70, 60))
	
	
			fit_lin <- 
				try(
					sdmTMB(
					formula = as.formula(paste0("log_wt ~ 0 + age_f +",  y)), 
					data = d,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "IID",
					time = "year",
					share_range = FALSE,
					silent = FALSE,
					reml = TRUE,
					priors = sdmTMBpriors(
						matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
						matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			)
	
			mod_name <- paste0("_", y, "_lin")
			
			try(write_rds(fit_lin, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
			fit_lin_int <- 
				try(
					sdmTMB(
					formula = as.formula(paste0("log_wt ~ 0 + age_f + age_f:",  y)), 
					data = d,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "IID",
					time = "year",
					share_range = FALSE,
					silent = FALSE,
					reml = TRUE,
					priors = sdmTMBpriors(
						matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
						matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			)
	
			mod_name <- paste0("_", y, "_int_lin")

			try(write_rds(fit_lin_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
			
		})
	
		
		
	#### poly 2 ####
	
	purrr::map2(fdf$sp, fdf$var, \(sp, y){ 
			
			# filter data by species
			d <- dat_all |> filter(short_name == sp)

			# mesh
			mesh <- make_mesh(d, c("X", "Y"),
				fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 50,
				offset = c(70, 60))
	
	
			fit <- 
				try(
					sdmTMB(
  				formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")), 
					data = d,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "IID",
					time = "year",
					share_range = FALSE,
					silent = FALSE,
					reml = TRUE,
					priors = sdmTMBpriors(
						matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
						matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			)
	
			mod_name <- paste0("_", y, "_poly2")
			
			try(write_rds(fit, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
			fit_int <- 
				try(
					sdmTMB(
  				formula = as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 2, raw = TRUE)")), 
					data = d,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "IID",
					time = "year",
					share_range = FALSE,
					silent = FALSE,
					reml = TRUE,
					priors = sdmTMBpriors(
						matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
						matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			)
	
			mod_name <- paste0("_", y, "_int_poly2")

			try(write_rds(fit_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
			
		})
	
	
		
	#### poly 3 ####
		
		
	purrr::map2(fdf$sp, fdf$var, \(sp, y){ 
			
			# filter data by species
			d <- dat_all |> filter(short_name == sp)

			# mesh
			mesh <- make_mesh(d, c("X", "Y"),
				fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 50,
				offset = c(70, 60))
	
	
			fit <- 
				try(
					sdmTMB(
  				formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), 
					data = d,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "IID",
					time = "year",
					share_range = FALSE,
					silent = FALSE,
					reml = TRUE,
					priors = sdmTMBpriors(
						matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
						matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			)
	
			mod_name <- paste0("_", y, "_poly3")
			
			try(write_rds(fit, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
			fit_int <- 
				try(
					sdmTMB(
  				formula = as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 3, raw = TRUE)")), 
					data = d,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "IID",
					time = "year",
					share_range = FALSE,
					silent = FALSE,
					reml = TRUE,
					priors = sdmTMBpriors(
						matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
						matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			)
	
			mod_name <- paste0("_", y, "_int_poly3")

			try(write_rds(fit_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
			
		})