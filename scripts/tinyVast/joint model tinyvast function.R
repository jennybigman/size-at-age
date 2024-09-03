	
# tinyVAST

library(tinyVAST)
library(fmesher)

	file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
	pcod_dat <- dat_all |> filter(short_name == "pcod")
	table(pcod_dat$age, pcod_dat$year)

	pcod_dat <- pcod_dat |>
		filter(age <= 9) 
	
	# mesh
	mesh_p = fm_mesh_2d(loc = pcod_dat[,c("X","Y")],
                            cutoff = 37)

	dsem = "
	age_1 -> age_1, 1, lag1,
	age_2 -> age_2, 1, lag1,
	age_3 -> age_3, 1, lag1,
	age_4 -> age_4, 1, lag1,
	age_5 -> age_5, 1, lag1,
	age_6 -> age_6, 1, lag1,
	age_7 -> age_7, 1, lag1,
	age_8 -> age_8, 1, lag1,
	age_9 -> age_9, 1, lag1,
	"
	
	pcod_no_cov_vast <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "no_cov"

		write_rds(pcod_no_cov_vast, 
					file = paste0(file_path, "pcod_", mod_name, ".rds"))

		
	
	tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = pcod_dat,
  			formula =  as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), # change this to poly 3, linear and re run function
  			family = gaussian(),
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_v,
  			time_column = "year",
  			times = 1993:2022,
  			variable_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly3_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, "pcod_", mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pcod_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f * poly(", y,", 3, raw = TRUE)")), # change this to poly 3, linear and re run function
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_poly3_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, "pcod_", mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, tinyv_mod_fun)
	
	
	# atooth ####
	
	atooth_dat <- dat_all %>% filter(short_name == "atooth")
	
	atooth_dat$age_f = factor(paste0("age_", atooth_dat$age))
	
	# mesh
	mesh_v = fm_mesh_2d(loc = atooth_dat[,c("X","Y")],
                            cutoff = 37)

	dsem = "
	age_2 -> age_2, 1, lag1,
	age_3 -> age_3, 1, lag1,
	age_4 -> age_4, 1, lag1,
	age_5 -> age_5, 1, lag1,
	age_6 -> age_6, 1, lag1,
	age_7 -> age_7, 1, lag1,
	age_8 -> age_8, 1, lag1,
	age_9 -> age_9, 1, lag1,
	age_10 -> age_10, 1, lag1
	age_11 -> age_11, 1, lag1,
	age_12 -> age_12, 1, lag1
	"

	
	atooth_no_cov_vast <- 
		tinyVAST(
		data = atooth_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1996:2021,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "no_cov"

		write_rds(atooth_no_cov_vast, 
					file = paste0(file_path, "atooth_", mod_name, ".rds"))

		
	
	tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = atooth_dat,
  			formula =  as.formula(paste0("log_wt ~ 0 + age_f +",  y)), # change this to poly 3, poly 2 and re run function
  			family = gaussian(),
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_v,
  			time_column = "year",
  			times = 1996:2021,
  			variable_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_lin_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, "atooth_", mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = atooth_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f *",  y)),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1996:2021,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
	# distribution_col = age_f and feed a list of families per age class 
	
	# 2 ages, 10 yrs, sdm TMB and tinyVAST 
	
		mod_name <- paste0(y, "_int_lin_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, "atooth_", mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, tinyv_mod_fun)
	
	
	# pollock ####
	
	pol_dat <- dat_all %>% filter(short_name == "pollock")
	
	pol_dat$age_f = factor(paste0("age_", pol_dat$age))
	

	# mesh
	mesh_v = fm_mesh_2d(loc = pol_dat[,c("X","Y")],
                            cutoff = 37)

	dsem = "
	age_1 -> age_1, 1, lag1,
	age_2 -> age_2, 1, lag1,
	age_3 -> age_3, 1, lag1,
	age_4 -> age_4, 1, lag1,
	age_5 -> age_5, 1, lag1,
	age_6 -> age_6, 1, lag1,
	age_7 -> age_7, 1, lag1,
	age_8 -> age_8, 1, lag1,
	age_9 -> age_9, 1, lag1,
	age_10 -> age_10, 1, lag1
	age_11 -> age_11, 1, lag1,
	age_12 -> age_12, 1, lag1
	age_13 -> age_13, 1, lag1
	age_14 -> age_14, 1, lag1
	age_15 -> age_15, 1, lag1
	age_16 -> age_16, 1, lag1
	age_17 -> age_17, 1, lag1
	age_18 -> age_18, 1, lag1
	age_19 -> age_19, 1, lag1
	age_20 -> age_20, 1, lag1
	"

	
	pol_no_cov_vast <- 
		tinyVAST(
		data = pol_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j", getsd = FALSE))
	
	mod_name <- "no_cov"

		write_rds(pol_no_cov_vast, 
					file = paste0(file_path, "pol_", mod_name, ".rds"))

		
	
	tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = pol_dat,
  			formula =  as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), # change this to poly 2, lin and re run function
  			family = gaussian(),
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_v,
  			time_column = "year",
  			times = 1991:2023,
  			variable_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j", getsd = FALSE)))
	
		mod_name <- paste0(y, "_poly3_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, "pol_", mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pol_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f * poly(", y,", 3, raw = TRUE)")), # change this to poly 2, lin and re run function
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j", getsd = FALSE)))
	
		mod_name <- paste0(y, "_int_poly3_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, "pol_", mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, tinyv_mod_fun)
	
	
	# yfin ####
	

	yfin_dat <- dat_all %>% filter(short_name == "yfin")
	
	yfin_dat$age_f = factor(paste0("age_", yfin_dat$age))
	

	# mesh
	mesh_y = fm_mesh_2d(loc = yfin_dat[,c("X","Y")],
                            cutoff = 37)

	dsem = "
	age_3 -> age_3, 1, lag1,
	age_4 -> age_4, 1, lag1,
	age_5 -> age_5, 1, lag1,
	age_6 -> age_6, 1, lag1,
	age_7 -> age_7, 1, lag1,
	age_8 -> age_8, 1, lag1,
	age_9 -> age_9, 1, lag1,
	age_10 -> age_10, 1, lag1,
	age_11 -> age_11, 1, lag1,
	age_12 -> age_12, 1, lag1
	age_13 -> age_13, 1, lag1,
	age_14 -> age_14, 1, lag1
	age_15 -> age_15, 1, lag1
	age_16 -> age_16, 1, lag1
	age_17 -> age_17, 1, lag1
	age_18 -> age_18, 1, lag1
	age_19 -> age_19, 1, lag1
	age_20 -> age_20, 1, lag1
	age_21 -> age_21, 1, lag1
	age_22 -> age_22, 1, lag1
	age_23 -> age_23, 1, lag1
	age_24 -> age_24, 1, lag1
	age_25 -> age_25, 1, lag1
	age_26 -> age_26, 1, lag1
	age_27 -> age_27, 1, lag1
	age_28 -> age_28, 1, lag1
	age_29 -> age_29, 1, lag1
	age_30 -> age_30, 1, lag1
	"

	
	yfin_no_cov_vast <- 
		tinyVAST(
		data = yfin_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
  	times = 1987:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "no_cov"

		write_rds(yfin_no_cov_vast, 
					file = paste0(file_path, "yfin_", mod_name, ".rds"))

		
	
	tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = yfin_dat,
  			formula =  as.formula(paste0("log_wt ~ 0 + age_f +",  y)), # change this to poly 2, poly 3 and re run function
  			family = gaussian(),
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_y,
  			time_column = "year",
  	times = 1987:2022,
  			variable_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_lin_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, "yfin_", mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = yfin_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f *",  y)), # change this to poly 2, poly 3 and re run function
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
  	times = 1987:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_lin_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, "yfin_", mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, tinyv_mod_fun)
	
	
