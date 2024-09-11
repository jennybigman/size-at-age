	
# tinyVAST

	# yfin ####
	file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")

	yfin_dat <- dat_all %>% filter(short_name == "yfin")
	
	yfin_dat$age_f = factor(paste0("age_", yfin_dat$age))
	
	sp <- unique(yfin_dat$short_name)
	

	# mesh
	mesh_y = fm_mesh_2d(loc = yfin_dat[,c("X","Y")],
                            cutoff = 37)


	family = list(
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
		
	
	yfin_no_cov_vast <- 
		tinyVAST(
		data = yfin_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
		times = min(yfin_dat$year):max(yfin_dat$year),  	
		variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "_no_cov"

		write_rds(yfin_no_cov_vast, 
					file = paste0(file_path, sp, mod_name, ".rds"))

		
	#linear
	lin_tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = yfin_dat,
  			formula =  as.formula(paste0("log_wt ~ 0 + age_f +",  y)), 
  			family = family,
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_y,
  			time_column = "year",
		times = min(yfin_dat$year):max(yfin_dat$year),  	
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0("_", y, "_lin_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = yfin_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:",  y)), 
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
		times = min(yfin_dat$year):max(yfin_dat$year),  	
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0("_", y, "_int_lin_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, lin_tinyv_mod_fun)
	
	# poly2
	poly2_tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = yfin_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_y,
  			time_column = "year",
		times = min(yfin_dat$year):max(yfin_dat$year),  	
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0("_", y, "_poly2_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = yfin_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 2, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
		times = min(yfin_dat$year):max(yfin_dat$year),  	
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0("_", y, "_int_poly2_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, poly2_tinyv_mod_fun)
	
	
	# poly3
	poly3_tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = yfin_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_y,
  			time_column = "year",
		times = min(yfin_dat$year):max(yfin_dat$year),  	
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0("_", y, "_poly3_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = yfin_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 3, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
		times = min(yfin_dat$year):max(yfin_dat$year),  	
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0("_", y, "_int_poly3_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, poly3_tinyv_mod_fun)
	
	
	
	
	