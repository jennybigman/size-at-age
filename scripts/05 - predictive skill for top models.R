# run each model in sdmTMB

	future::plan(multisession)
	# set up data and meshes
	
	## pollock mesh ####
	pollock_dat <- dat_all %>% 
		dplyr::filter(short_name == "pollock")

	pol_mesh <- make_mesh(
		pollock_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
		#	max.edge = 85#,
			offset = c(70, 60)
		)
	
	plot(pol_mesh)
	
	## pcod mesh ####
	pcod_dat <- dat_all %>% 
		dplyr::filter(short_name == "pcod")
	
	pcod_mesh <- make_mesh(
		pcod_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 30,
			offset = c(60, 70)
		)
	
	plot(pcod_mesh)
	
	## yellowfin mesh ####
	yfin_dat <- dat_all %>% 
		dplyr::filter(short_name == "yfin")

	yfin_mesh <- make_mesh(
		yfin_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 60,
			offset = c(60, 70)
		)
	
	plot(yfin_mesh)
	
	# atooth
	atooth_dat <- dat_all %>% 
		filter(short_name == "atooth")
	
	atooth_mesh <- make_mesh(
		atooth_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
		#	max.edge = 85#,
			offset = c(70, 60)
		)
	
	plot(atooth_mesh)


	# arrowtooth - pre survey boxy ####
	
	# number of years of data
	length(sort(unique(atooth_dat$year)))

	atooth_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(presurvey_boxy, 3),
			data = atooth_dat,
			mesh = atooth_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = 5,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	# pcod - pre survey temp ####
	
	# number of years of data
	length(sort(unique(pcod_dat$year)))

	pcod_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(presurvey_btemp, 3),
			data = pcod_dat,
			mesh = pcod_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = 10,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	# pollock - pre survey temp ####
	
	# number of years of data
	length(sort(unique(pollock_dat$year)))

	pollock_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(presurvey_btemp, 3),
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = 10,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	## yfin - yr prior temp ####
	
	# number of years of data
	length(sort(unique(yfin_dat$year)))

	yfin_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 3),
			data = yfin_dat,
			mesh = yfin_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = 10,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	# save models
			
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/May 2024/poly models/cv mods/"
	
	write_rds(atooth_top_mod_cv, file = paste0(here(), file_path_all, "atooth_top_mod_cv.rds"))
	write_rds(pcod_top_mod_cv, file = paste0(here(), file_path_all, "pcod_top_mod_cv.rds"))
	write_rds(pollock_top_mod_cv, file = paste0(here(), file_path_all, "pollock_top_mod_cv.rds"))
	write_rds(yfin_top_mod_cv, file = paste0(here(), file_path_all, "yfin_top_mod_cv.rds"))
	
	# run models for forecasting - find missing years
	max_yr <- max(atooth_dat$year)
	min_yr <- min(atooth_dat$year)
	yrs <- sort(unique(atooth_dat$year))
	all_yrs <- min_yr:max_yr
	yrs_missing <- setdiff(all_yrs, yrs)