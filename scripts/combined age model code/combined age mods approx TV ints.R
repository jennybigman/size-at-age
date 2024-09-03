
d <- dat_all %>% filter(short_name == "yfin")
d <- d %>% filter(age > 4, age < 26)

mesh <- make_mesh(
	d, c("X", "Y"),
	fmesher_func = fmesher::fm_mesh_2d_inla,
	cutoff = 50,
	offset = c(60, 70)
)

d$fake_time <- factor(paste0(d$year, d$age))


fit <- 
	sdmTMB(
		formula = log_wt ~ age_f * year_f + age_f + poly(yrprior_boxy, 3, raw = TRUE),
		data = d,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "iid",
		control = sdmTMBcontrol(profile = "b_j"),
		time = "fake_time",
		share_range = FALSE,
		silent = FALSE,
		priors = sdmTMBpriors(
			matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
			matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
		)
	)

sanity(fit)

	write_rds(fit, 
					file = paste0(file_path, "yfin_poly3_noint_oxy.rds"))
	

# will fit for pcod ages 1-10, will fit for pollock 1-13, will for atooth 2-12 bot both temp & oxy
	
# can I get these to work for all species AND does the same model without temp/oxy do better or worse

fit_no_cov <- 
	sdmTMB(
		formula = log_wt ~ age_f * year_f,
		data = d,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "iid",
		control = sdmTMBcontrol(profile = "b_j"),
		time = "fake_time",
		share_range = FALSE,
		silent = FALSE,
		priors = sdmTMBpriors(
			matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
			matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
		)
	)

sanity(fit_no_cov)


	write_rds(fit_no_cov, 
					file = paste0(file_path, "yfin_no_cov.rds"))
