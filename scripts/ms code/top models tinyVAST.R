# rerunning top models under new branch

	##### shared spatial & spatiotemporal fields (shared mods) ####

		file_path <- paste0(here(), "/output/model output/tinyVAST/top mods/")

		# filter data by species
		d <- dat_all |> filter(short_name == "atooth")

		# create a dummy column for spatial and spatiotemporal fields
		d$single_age <- "shared"

		# mesh
		mesh = fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 37)
		
		d$age_f <- paste0("age_", d$age_f)

		# set up spatial and spatiotemporal field SDs
		sem = "
  		shared <-> shared, shared_s_SD
  	"
	
		dsem = "
			shared <-> shared, 0, shared_st_SD
		"


	fit <- 
		tinyVAST(
						data = d,
  					formula =  log_wt ~ 0 + age_f + age_f:yrprior_btemp,
  					family = gaussian(),
  					sem = sem, 
  					dsem = dsem,
  					space_column = c("X", "Y"), 
  					spatial_graph = mesh,
  					time_column = "year",
  					times = min(d$year):max(d$year),
  					variable_column = "single_age",
  					control = tinyVASTcontrol(profile = "alpha_j"))
		
	write_rds(fit, 
						file = paste0(file_path, "shared_atooth_yrprior_btemp_lin_int_tvt.rds"))
	
	
	#### spatial field only (ssf mods) ####
	
	file_path <- paste0(here(), "/output/model output/tinyVAST/top mods/")

		# filter data by species
		d <- dat_all |> filter(short_name == "pollock") # change this to fit diff sp

		# create a dummy column for spatial and spatiotemporal fields
		d$single_age <- "shared"
		
		d$age_f <- paste0("age_", d$age_f)

		# mesh
		mesh = fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 37)

		# set up spatial and spatiotemporal field SDs
		sem = "
  		shared <-> shared, shared_s_SD
  	"
	
	fit <- 
		tinyVAST(
						data = d,
  					formula =  log_wt ~ 0 + age_f + age_f:poly(yrprior_btemp, 3, raw = TRUE),
  					family = gaussian(),
  					sem = sem, 
  					dsem = NULL,
  					space_column = c("X", "Y"), 
  					spatial_graph = mesh,
  					#time_column = "year",
  					#times = min(d$year):max(d$year),
  					variable_column = "single_age",
  					control = tinyVASTcontrol(profile = "alpha_j"))
		
	write_rds(fit, 
						file = paste0(file_path, "ssf_pollock_yrprior_btemp_poly3_int_tvt.rds"))
	
	
	
	#### fully MV models ####
	
	atooth_family = list(
		age_2 = gaussian(), 
		age_3 = gaussian(), 
		age_4 = gaussian(), 
		age_5 = gaussian(), 
		age_6 = gaussian(), 
		age_7 = gaussian(), 
		age_8 = gaussian(), 
		age_9 = gaussian(), 
		age_10 = gaussian(),
		age_11 = gaussian(),
		age_12 = gaussian())
	
	pcod_family = list(age_1 = gaussian(), 
								age_2 = gaussian(), 
								age_3 = gaussian(), 
								age_4 = gaussian(), 
								age_5 = gaussian(), 
								age_6 = gaussian(), 
								age_7 = gaussian(), 
								age_8 = gaussian(), 
								age_9 = gaussian(), 
								age_10 = gaussian())
	
		
	pollock_family = list(
		age_1 = gaussian(),
		age_2 = gaussian(), 
		age_3 = gaussian(), 
		age_4 = gaussian(), 
		age_5 = gaussian(), 
		age_6 = gaussian(), 
		age_7 = gaussian(), 
		age_8 = gaussian(), 
		age_9 = gaussian(), 
		age_10 = gaussian(),
		age_11 = gaussian(),
		age_12 = gaussian(),
		age_13 = gaussian(),
		age_14 = gaussian(),
		age_15 = gaussian(),
		age_16 = gaussian(),
		age_17 = gaussian(),
		age_18 = gaussian(),
		age_19 = gaussian(),
		age_20 = gaussian())
	
	yfin_family = list(
		age_3 = gaussian(),
		age_4 = gaussian(),
		age_5 = gaussian(),
		age_6 = gaussian(),
		age_7 = gaussian(),
		age_8 = gaussian(),
		age_9 = gaussian(),
		age_10 = gaussian(),
		age_11 = gaussian(),
		age_12 = gaussian(),
		age_13 = gaussian(),
		age_14 = gaussian(),
		age_15 = gaussian(),
		age_16 = gaussian(),
		age_17 = gaussian(),
		age_18 = gaussian(),
		age_19 = gaussian(),
		age_20 = gaussian(),
		age_21 = gaussian(),
		age_22 = gaussian(),
		age_23 = gaussian(),
		age_24 = gaussian(),
		age_25 = gaussian(),
		age_26 = gaussian(),
		age_27 = gaussian(),
		age_28 = gaussian(),
		age_29 = gaussian(),
		age_30 = gaussian())
		
	
	
	d <- dat_all %>% filter(short_name == "atooth") # change for diff sp
	
	d$age_f = factor(paste0("age_", d$age))
	
	# mesh
	mesh = fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 37)

	
	fit <- 
		tinyVAST(
				data = d,
  			formula =  log_wt ~ 0 + age_f + poly(yrprior_btemp, 3, raw = TRUE), # change 2 poly(2) for all but atooth
  			family = atooth_family,
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh,
  			time_column = "year",
  			times = min(d$year):max(d$year),
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j"))
	
		write_rds(fit, 
						file = paste0(file_path, "MV_atooth_yrprior_btemp_poly2_tvt.rds"))
	
	