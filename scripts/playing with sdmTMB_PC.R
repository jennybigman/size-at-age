	
	
	dat <- droplevels(pollock_dat)
	
	mesh <- make_mesh(dat, xy_cols = c("X", "Y"), cutoff = 20)
	#mesh2 <- make_mesh(dat, c("X", "Y"), n_knots = 800, type = "kmeans")
	#mesh$mesh$n
	#plot(mesh)
	
	# playing with parameterizations

	#plan(multisession)

	fit1 <- sdmTMB(
		data = dat,
		formula = log_wt ~ age_f + s(presurvey_btemp),
		mesh = mesh, 
		family = gaussian(),
		spatial = "off",	
		spatiotemporal = "off",
		control = sdmTMBcontrol(newton_loops = 1))
	# no error, sanity() perfect
	
	
	fit2 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f),
		data = dat,
		mesh = mesh,
		spatial = "off",
		spatiotemporal = "off",
		control = sdmTMBcontrol(newton_loops = 1))
	# no error, sanity() says ln_smooth_sigma SE may be too large
	
	fit2b <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f),
		data = dat,
		mesh = mesh2,
		spatial = "off",
		spatiotemporal = "off",
		control = sdmTMBcontrol(newton_loops = 1))
	# no error, sanity() says ln_smooth_sigma SE may be too large
	
	fit2c <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f),
		data = dat,
		priors = sdmTMBpriors(
			b = normal(location = 0, scale = 5)
		),
		mesh = mesh2,
		spatial = "off",
		spatiotemporal = "off",
		control = sdmTMBcontrol(newton_loops = 1))
	# no error, sanity() says ln_smooth_sigma SE may be too large
	
	fit2d <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f, k = 4),
		data = dat,
		priors = sdmTMBpriors(
			b = normal(location = 0, scale = 5),
			phi = normal(location = 0, scale = 5)
		),
		family = student(),
		mesh = mesh2,
		spatial = "off",
		spatiotemporal = "off",
		control = sdmTMBcontrol(newton_loops = 1))
	
	fit3 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) +
			s(jday),
		data = dat,
		mesh = mesh,
		spatial = "off",
		spatiotemporal = "off",
		control = sdmTMBcontrol(nlminb_loops = 2)
		)


	fit4 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) +
			s(jday),
		data = dat,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "off",
		control = sdmTMBcontrol(nlminb_loops = 2)
		)

	fit6 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		control = sdmTMBcontrol(nlminb_loops = 2)
	)

	fit7 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		anisotropy = TRUE,
		control = sdmTMBcontrol(nlminb_loops = 2)
	)


	saveRDS(fit7, file = here("output/model output PC/fit7.rds"))
	
	fit7 <- readRDS(file = here("output/model output/sdmTMB output/fit7.rds"))
	
	#### with priors ####
	
	fit7_p <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		anisotropy = TRUE,
		priors = sdmTMBpriors(
			b = normal(location = c(0,0) scale = c(20)),
		control = sdmTMBcontrol(nlminb_loops = 2)
	)
	
	head(fit7_p$tmb_data$X_ij[[1]])
	#### sanity checks ####
	
	sanity(fit7)
	
	#### plotting ####
	
	
	
	
	
	
	
	visreg::visreg(fit4, xvar = "presurvey_btemp", by = "age_f")
	
	visreg::visreg(fit4, xvar = "presurvey_btemp", by = "age_f", scale = "response",
								 gg = TRUE, rug = FALSE, partial = FALSE)
	

	## compare two meshes
	
	fit4 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) +
			(1|cohort_f) + (1|jday_f) + (1|ID_f) + (1|haul_id_f),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		reml = TRUE
	)
	
	fit4.1 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) +
			(1|cohort_f) + (1|jday_f) + (1|ID_f) + (1|haul_id_f),
		data = dat,
		mesh = mesh2,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		reml = TRUE
	)