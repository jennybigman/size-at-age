mesh_a <- make_mesh(
  a_d, c("X", "Y"),
  fmesher_func = fmesher::fm_mesh_2d_inla,
  cutoff = 8, # minimum triangle edge length
  #max.edge = c(20, 20), # inner and outer max triangle lengths
  offset = c(5, 20) # inner and outer border widths
)
plot(mesh_a)

n_year_age <- length(unique(a_d$age)) * length(unique(a_d$year))


fit <-
			sdmTMB(
				formula = log_wt ~ 0 + age_f + yrprior_btemp,
				data = a_d,
				mesh = mesh_a,
				spatial = "on",
				spatiotemporal = "off",
				spatial_varying = ~ 0 + year_age,
				groups = "age_f",
				control = sdmTMBcontrol(
					profile = "b_j",
					map = list(ln_tau_Z = factor(rep(1, n_year_age))),
					multiphase = FALSE
				),
				time = "year",
				silent = FALSE
			)
	
	sanity(fit)
	
	mod_name <- paste0("yrprior_btemp", "_lin_crw")
	
	write_rds(fit, 
					file = paste0(file_path, "atooth_", mod_name, ".rds"))
