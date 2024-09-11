	
# tinyVAST

library(tinyVAST)
library(fmesher)

	file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	
	pcod_dat <- dat_all |> filter(short_name == "pcod")
	#table(pcod_dat$age, pcod_dat$year)
	
	pcod_dat$age_f <- paste0("age_", pcod_dat$age)
	
	sp <- unique(pcod_dat$short_name)

	# mesh
	mesh_p = fm_mesh_2d(loc = pcod_dat[,c("X","Y")],
                            cutoff = 37)

	family = list(age_1 = gaussian(), 
								age_2 = gaussian(), 
								age_3 = gaussian(), 
								age_4 = gaussian(), 
								age_5 = gaussian(), 
								age_6 = gaussian(), 
								age_7 = gaussian(), 
								age_8 = gaussian(), 
								age_9 = gaussian(), 
								age_10 = gaussian())
		
	pcod_no_cov_vast <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "no_cov"

		write_rds(pcod_no_cov_vast, 
					file = paste0(file_path, sp, mod_name, ".rds"))

		
	#linear
	lin_tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = pcod_dat,
  			formula =  as.formula(paste0("log_wt ~ 0 + age_f +",  y)), 
  			family = family,
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_p,
  			time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_lin_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pcod_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:",  y)), 
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_lin_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, lin_tinyv_mod_fun)
	
	# poly2
	poly2_tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = pcod_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  	dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_p,
  			time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly2_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pcod_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 2, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_poly2_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, poly2_tinyv_mod_fun)
	
	
	# poly3
	poly3_tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = pcod_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  	dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_p,
  			time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly3_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pcod_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 3, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_poly3_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, poly3_tinyv_mod_fun)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

		
	

	

	