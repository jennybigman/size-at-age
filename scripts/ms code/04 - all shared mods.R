	
#remotes::install_github("vast-lib/tinyVAST@dev", dependencies = TRUE)
# tinyVAST

	library(tinyVAST)
	library(fmesher)

	
	file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	
	# ML 
	
	all_shared_fun <- function(sp, y) {
	
		# filter data by species
		d <- dat_all |> filter(short_name == sp)

		# create a dummy column for spatial and spatiotemporal fields
		d$single_age <- "shared"

		# mesh
		mesh = fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 37)

		# set up spatial and spatiotemporal field SDs
		sem = "
  		shared <-> shared, shared_s_SD
  	"
	
		dsem = "
			shared <-> shared, 0, shared_st_SD
		"

		no_cov_fit <- 
			try(tinyVAST(
				data = d,
  			formula = log_wt ~ 0 + age_f,
  			family = gaussian(), # one for all ages, like sdmTMB
  			sem = sem, 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh,
  			time_column = "year",
  			times = min(d$year):max(d$year),
  			variable_column = "single_age",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- "_no_cov"

		try(write_rds(no_cov_fit, 
					file = paste0(file_path, sp, mod_name, ".rds")))

		
		#linear
		fit_lin <- 
			try(tinyVAST(
						data = d,
  					formula =  as.formula(paste0("log_wt ~ 0 + age_f +",  y)), 
  					family = gaussian(),
  					sem = sem, 
  					dsem = dsem,
  					space_column = c("X", "Y"), 
  					spatial_graph = mesh,
  					time_column = "year",
  					times = min(d$year):max(d$year),
  					variable_column = "single_age",
  					control = tinyVASTcontrol(profile = "alpha_j")))
	
			mod_name <- paste0("_", y, "_lin_tvt")

			try(write_rds(fit_lin, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
			fit_lin_int <- 
				try(tinyVAST(
							data = d,
  						formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:",  y)), 
  						family = gaussian(),
  						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  						control = tinyVASTcontrol(profile = "alpha_j")))
	
			mod_name <- paste0("_", y, "_int_lin_tvt")

			try(write_rds(fit_lin_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
			
		# poly2
			fit_poly2 <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  						control = tinyVASTcontrol(profile = "alpha_j")))
	
			mod_name <- paste0("_", y, "_poly2_tvt")

			try(write_rds(fit_poly2, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
		fit_poly2_int <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 2, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  						control = tinyVASTcontrol(profile = "alpha_j")))
	
			mod_name <- paste0("_", y, "_poly2_int_tvt")

			try(write_rds(fit_poly2_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
	
		# poly3
			fit_poly3 <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  						control = tinyVASTcontrol(profile = "alpha_j")))
	
			mod_name <- paste0("_", y, "_poly3_tvt")

			try(write_rds(fit_poly3, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
		fit_poly3_int <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 3, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  						control = tinyVASTcontrol(profile = "alpha_j")))
	
			mod_name <- paste0("_", y, "_poly3_int_tvt")

			try(write_rds(fit_poly3_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
			
	}
	
	
	y <- c("yrprior_btemp", "yrprior_boxy")
	
	sp <- unique(dat_all$short_name)
	
	fdf <- crossing(
		y = y,
		sp = sp
	)
	
	purrr::map2(fdf$sp, fdf$y, all_shared_fun)
	
	
	
###########################
#### REML ####
##########################
		
	file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/reml/")
  if(!dir.exists(file_path)) dir.create(file_path)
		
	
	all_shared_fun <- function(sp, y) {
	
		# filter data by species
		d <- dat_all |> filter(short_name == sp)

		# create a dummy column for spatial and spatiotemporal fields
		d$single_age <- "shared"
		
		d <- droplevels(d)

		# mesh
		mesh = fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 37)

		# set up spatial and spatiotemporal field SDs
		sem = "
  		shared <-> shared, shared_s_SD
  	"
	
		dsem = "
			shared <-> shared, 0, shared_st_SD
		"

		no_cov_fit <- 
			try(tinyVAST(
				data = d,
  			formula = log_wt ~ 0 + age_f,
  			family = gaussian(), # one for all ages, like sdmTMB
  			sem = sem, 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh,
  			time_column = "year",
  			times = min(d$year):max(d$year),
  			variable_column = "single_age",
  			control = tinyVASTcontrol(reml = TRUE)))
	
		mod_name <- "_no_cov"

		try(write_rds(no_cov_fit, 
					file = paste0(file_path, sp, mod_name, ".rds")))

		
		#linear
		fit_lin <- 
			try(tinyVAST(
						data = d,
  					formula =  as.formula(paste0("log_wt ~ 0 + age_f +",  y)), 
  					family = gaussian(),
  					sem = sem, 
  					dsem = dsem,
  					space_column = c("X", "Y"), 
  					spatial_graph = mesh,
  					time_column = "year",
  					times = min(d$year):max(d$year),
  					variable_column = "single_age",
  					control = tinyVASTcontrol(reml = TRUE)))
	
			mod_name <- paste0("_", y, "_lin_tvt")

			try(write_rds(fit_lin, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
			fit_lin_int <- 
				try(tinyVAST(
							data = d,
  						formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:",  y)), 
  						family = gaussian(),
  						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  					control = tinyVASTcontrol(reml = TRUE)))
	
			mod_name <- paste0("_", y, "_int_lin_tvt")

			try(write_rds(fit_lin_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
			
		# poly2
			fit_poly2 <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  					control = tinyVASTcontrol(reml = TRUE)))
	
			mod_name <- paste0("_", y, "_poly2_tvt")

			try(write_rds(fit_poly2, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
		fit_poly2_int <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 2, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  					control = tinyVASTcontrol(reml = TRUE)))
	
			mod_name <- paste0("_", y, "_poly2_int_tvt")

			try(write_rds(fit_poly2_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
	
		# poly3
			fit_poly3 <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  					control = tinyVASTcontrol(reml = TRUE)))
	
			mod_name <- paste0("_", y, "_poly3_tvt")

			try(write_rds(fit_poly3, 
						file = paste0(file_path, sp, mod_name, ".rds")))
	
		
		fit_poly3_int <- 
				try(tinyVAST(
							data = d,
  						formula = as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 3, raw = TRUE)")), 
  						family = gaussian(),
   						sem = sem, 
  						dsem = dsem,
  						space_column = c("X", "Y"), 
  						spatial_graph = mesh,
  						time_column = "year",
  						times = min(d$year):max(d$year),
  						variable_column = "single_age",
  					control = tinyVASTcontrol(reml = TRUE)))
	
			mod_name <- paste0("_", y, "_poly3_int_tvt")

			try(write_rds(fit_poly3_int, 
						file = paste0(file_path, sp, mod_name, ".rds")))
			
	}
	
	
	y <- c("yrprior_btemp", "yrprior_boxy")
	
	sp <- unique(dat_all$short_name)
	
	fdf <- crossing(
		y = y,
		sp = sp
	)
	
	purrr::map2(fdf$sp, fdf$y, all_shared_fun)
	