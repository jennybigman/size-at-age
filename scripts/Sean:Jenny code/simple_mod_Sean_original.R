# simple example for Pacific cod

	# load data
	load(here("./data/pcod_dat.Rdata"))

	# make meh
	pcod_mesh <- make_mesh(
		pcod_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 30,
			offset = c(60, 70)
		)
	
	plot(pcod_mesh)
	
	# fit model
	mod_poly3_int <- 
			sdmTMB(
				formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 3, raw = TRUE),
				data = pcod_dat,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
			  time = "year",
			  share_range = FALSE,
			  silent = FALSE,
				priors = sdmTMBpriors(
					matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
					matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	sanity(mod_poly3_int, gradient_thresh = 0.05)
	
	