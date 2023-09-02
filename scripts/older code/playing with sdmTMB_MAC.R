	
	
	dat <- droplevels(pollock_dat)
	
	mesh <- make_mesh(dat, xy_cols = c("X", "Y"), cutoff = 20)
	#mesh2 <- make_mesh(dat, c("X", "Y"), n_knots = 800, type = "kmeans")
	#mesh$mesh$n
	#plot(mesh)
	
	dat$fjday <- as.factor(dat$jday)
	
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

	###### works below!!!!
	
	fit7 <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + jday,
		dispformula = ~ 0,
		data = dat,
		mesh = mesh,
		#priors = sdmTMBpriors(
		#	b = normal(location = 0, scale = 5)
		#),
		spatial = "on",
		time = "year",
		spatiotemporal = "off",
		#anisotropy = TRUE,
		control = sdmTMBcontrol(newton_loops = 1)
	)


	saveRDS(fit7, file = here("output/model output PC/fit7.rds"))
	
	fit7 <- readRDS(file = here("output/model output/sdmTMB output/fit7.rds"))
	
	
	######## play around ####
	
	
	#### works and better fit
	
	fit_test <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
		dispformula = ~ 0,
		data = dat,
		mesh = mesh,
		#priors = sdmTMBpriors(
		#	b = normal(location = 0, scale = 5)
		#),
		spatial = "on",
		time = "year",
		spatiotemporal = "off",
		#anisotropy = TRUE,
		control = sdmTMBcontrol(newton_loops = 1)
	)
	
		sanity(fit_test)
	
	# another - works, prob same as above
		
		fit_test <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
		dispformula = ~ 0,
		data = dat,
		mesh = mesh,
		#priors = sdmTMBpriors(
		#	b = normal(location = 0, scale = 5)
		#),
		spatial = "on",
		#time = "year",
		spatiotemporal = "off",
		#anisotropy = TRUE,
		control = sdmTMBcontrol(newton_loops = 1)
	)
	
		sanity(fit_test)
	
	## another -- works with good convergence
	
	fit_test <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
		dispformula = ~ 0,
		data = dat,
		mesh = mesh,
		#priors = sdmTMBpriors(
		#	b = normal(location = 0, scale = 5)
		#),
		spatial = "on",
		#time = "year",
		spatiotemporal = "off",
		#anisotropy = TRUE,
		control = sdmTMBcontrol(newton_loops = 1)
	)
	
		sanity(fit_test)
	
	##############
	#### sanity checks ####
	
	sanity(fit7)
	
	# need to try adding priors
	
	
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
	
	
	
	######################## PCOD ###########################
	
	 
	
	priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = 10, sigma_lt = 5),
    b = normal(c(0, 0), c(1, 10)),
    phi = halfnormal(0, 15)
  )
)
	pcod_dat_trim <- na.omit(pcod_dat_trim)
	pcod_dat_trim <- pcod_dat_trim %>% filter(age_f != "1")
	pcod_mesh <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), cutoff = 20)

	yfin_mod <- sdmTMB(
		log_wt ~ age_f + s(presurvey_btemp, by = age_f),
		#dispformula = ~ 0,
		data = yfinsole_dat_trim,
		mesh = yfin_mesh,
		priors = sdmTMBpriors(
			matern_s = pc_matern(range_gt = 10, sigma_lt = 5),
			b = normal(location = 0, scale = 10)
		),
		spatial = "on",
		#time = "year",
		spatiotemporal = "off",
		#anisotropy = TRUE,
		control = sdmTMBcontrol(newton_loops = 1)
	)
	
		sanity(fit_test)
