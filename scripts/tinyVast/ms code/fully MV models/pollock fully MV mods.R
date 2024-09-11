	
# tinyVAST

	# pollock ####
	file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	
	pol_dat <- dat_all %>% filter(short_name == "pollock")
	
	pol_dat$age_f = factor(paste0("age_", pol_dat$age))
	
	sp <- unique(pol_dat$short_name)
	

	# mesh
	mesh_pl = fm_mesh_2d(loc = pol_dat[,c("X","Y")],
                            cutoff = 37)

	dsem = "
	age_1 -> age_1, 1, lag1
	age_2 -> age_2, 1, lag2
	age_3 -> age_3, 1, lag3
	age_4 -> age_4, 1, lag4
	age_5 -> age_5, 1, lag5
	age_6 -> age_6, 1, lag6
	age_7 -> age_7, 1, lag7
	age_8 -> age_8, 1, lag8
	age_9 -> age_9, 1, lag9
	age_10 -> age_10, 1, lag10
	age_11 -> age_11, 1, lag11
	age_12 -> age_12, 1, lag12
	age_13 -> age_13, 1, lag13
	age_14 -> age_14, 1, lag14
	age_15 -> age_15, 1, lag15
	age_16 -> age_16, 1, lag16
	age_17 -> age_17, 1, lag17
	age_18 -> age_18, 1, lag18
	age_19 -> age_19, 1, lag19
	age_20 -> age_20, 1, lag20
	"
	
	family = list(
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
	
	
		
	pol_no_cov_vast <- 
		tinyVAST(
		data = pol_dat,
  	formula = log_wt ~ 0 + age_f,
  	family = family,
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pl,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "no_cov"

		write_rds(pol_no_cov_vast, 
					file = paste0(file_path, sp, mod_name, ".rds"))

		
	#linear
	lin_tinyv_mod_fun <- function(y){
		
		fit_no_int <- 
			try(tinyVAST(
				data = pol_dat,
  			formula =  as.formula(paste0("log_wt ~ 0 + age_f +",  y)), 
  			family = family,
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_pl,
  			time_column = "year",
  			times = 1991:2023,
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_lin_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pol_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:",  y)), 
  	family = family,
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pl,
  	time_column = "year",
  	times = 1991:2023,
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
				data = pol_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_pl,
  			time_column = "year",
  	times = 1991:2023,
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly2_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pol_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 2, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pl,
  	time_column = "year",
  	times = 1991:2023,
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
				data = pol_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_pl,
  			time_column = "year",
  	times = 1991:2023,
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly3_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = pol_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 3, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pl,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_poly3_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, poly3_tinyv_mod_fun)
	
	
	
	
	