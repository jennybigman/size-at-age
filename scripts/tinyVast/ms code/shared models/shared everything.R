	
# tinyVAST

library(tinyVAST)
library(fmesher)

	file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	
	pcod_dat <- dat_all |> filter(short_name == "pcod")
	#table(pcod_dat$age, pcod_dat$year)
	
	pcod_dat$age_f <- paste0("age_", pcod_dat$age)
	
	sp <- unique(pcod_dat$short_name)

	# mesh
	mesh_p = fm_mesh_2d(loc = pcod_dat[,c("X","Y")],
                            cutoff = 37)

	sem = "
  age_1 <-> age_1, spatial_sd
  age_2 <-> age_2, spatial_sd
  age_3 <-> age_3, spatial_sd
  age_4 <-> age_4, spatial_sd
  age_5 <-> age_5, spatial_sd
  age_6 <-> age_6, spatial_sd
  age_7 <-> age_7, spatial_sd
  age_8 <-> age_8, spatial_sd
  age_9 <-> age_9, spatial_sd
  age_10 <-> age_10, spatial_sd
 "
	
	
	dsem = "
	age_1 <-> age_1, 0, st_sd
	age_2 <-> age_2, 0, st_sd 
	age_3 <-> age_3, 0, st_sd 
	age_4 <-> age_4, 0, st_sd 
	age_5 <-> age_5, 0, st_sd 
	age_6 <-> age_6, 0, st_sd 
	age_7 <-> age_7, 0, st_sd 
	age_8 <-> age_8, 0, st_sd 
	age_9 <-> age_9, 0, st_sd 
	age_10 <-> age_10, 0, st_sd
	"


	pcod_no_cov_vast <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  	variable_column = "age_f",
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
  			sem = sem, 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_p,
  			time_column = "year",
  			times = 1993:2022,
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
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = 1993:2022,
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
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_p,
  			time_column = "year",
  			times = 1993:2022,
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
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = 1993:2022,
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
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_p,
  			time_column = "year",
  			times = 1993:2022,
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
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_poly3_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, poly3_tinyv_mod_fun)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

		
	

	

	