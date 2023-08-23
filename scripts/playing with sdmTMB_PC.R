	
	
	dat <- droplevels(pollock_dat)
	
	mesh <- make_mesh(dat, xy_cols = c("X", "Y"), cutoff = 20)
	#mesh2 <- make_mesh(dat, c("X", "Y"), n_knots = 800, type = "kmeans")
	#mesh$mesh$n
	#plot(mesh)
	
	# playing with parameterizations

	#plan(multisession)
	
	fit1 <- sdmTMB(
  	data = dat,
  	formula = log_wt ~ age_f,
  	mesh = mesh, 
  	family = gaussian(),
  	spatial = "off",	
  	spatiotemporal = "off")

	fit2 <- sdmTMB(
		data = dat,
		formula = log_wt ~ age_f + s(presurvey_btemp),
		mesh = mesh, 
		family = gaussian(),
		spatial = "off",	
		spatiotemporal = "off")
		
	fit3 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f),
		data = dat,
		mesh = mesh,
		spatial = "off",
		spatiotemporal = "off")

	fit4 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) +
			s(jday),
		data = dat,
		mesh = mesh,
		spatial = "off",
		spatiotemporal = "off",
		control = sdmTMBcontrol(nlminb_loops = 2)
		)


	fit5 <- sdmTMB(
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
			b = normal(location = 0, scale = 5)),
		control = sdmTMBcontrol(nlminb_loops = 2)
	)
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