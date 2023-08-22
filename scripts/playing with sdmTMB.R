	
	
	dat <- pollock_dat %>% droplevels()
	
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
  	formula = log_wt ~ s(presurvey_btemp),
  	mesh = mesh, 
  	family = gaussian(),
  	spatial = "off",
  	spatiotemporal = "off")	
	

	fit2 <- sdmTMB(
		data = dat,
		forumla = log_wt_std ~ age_f + s(presurvey_btemp),
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "off")
	# no error

	fit2_m2 <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp_std),
		data = dat,
		mesh = mesh2,
		spatial = "on",
		spatiotemporal = "off")
	# no error, mesh 2 better fit
		
	fit3 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f),
		data = dat,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "off")
	# no error
	
	fit3_an <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp_std, by = age_f),
		data = dat,
		mesh = mesh2,
		anisotropy = TRUE,
		spatial = "on",
		spatiotemporal = "off",
		control = sdmTMBcontrol(nlminb_loops = 2)
		)

	fit4 <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp_std, by = age_f) +
			s(jday) + (1|cohort_f),
		data = dat,
		mesh = mesh2,
		spatial = "off",
		spatiotemporal = "off",
		control = sdmTMBcontrol(nlminb_loops = 2)
		)
	# no error

	fit5 <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp_std, by = age_f) +
			s(jday) + (1|cohort_f),
		data = dat,
		mesh = mesh2,
		spatial = "on",
		spatiotemporal = "off",
		control = sdmTMBcontrol(nlminb_loops = 4, newton_loops = 4)
		)
	# no error
	
	fit6 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		control = sdmTMBcontrol(nlminb_loops = 4, newton_loops = 4)
	)
	# no error
	
	fit7 <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp_std, by = age_f) + s(jday_std),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		anisotropy = TRUE,
		control = sdmTMBcontrol(nlminb_loops = 5, newton_loops = 5)
	)
	# no error

	
	fit8 <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp_std) + s(jday_std),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		anisotropy = TRUE #,
		#control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 2)
	)
	
	
	fit6 <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f) +
			(1|cohort_f) + (1|jday_f) + (1|ID_f) + (1|haul_id_f),
		spatial_varying = ~ 0 + presurvey_btemp,
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		reml = TRUE
	)
		
	AIC(fit)
	AIC(fit2)
	AIC(fit3)
	AIC(fit4)
	AIC(fit5)
	AIC(fit6)

	tidy(fit3, conf.int = TRUE)	
	sanity(fit3)	
	
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