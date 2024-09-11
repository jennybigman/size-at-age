	
# tinyVAST

	# yfin ####
	file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV_AR/")

	yfin_dat <- dat_all %>% filter(short_name == "yfin")
	
	yfin_dat$age_f = factor(paste0("age_", yfin_dat$age))
	
	sp <- unique(yfin_dat$short_name)
	

	# mesh
	mesh_y = fm_mesh_2d(loc = yfin_dat[,c("X","Y")],
                            cutoff = 37)

	dsem = "
	age_3 -> age_3, 1, lag1
	age_4 -> age_4, 1, lag2
	age_5 -> age_5, 1, lag3
	age_6 -> age_6, 1, lag4
	age_7 -> age_7, 1, lag5
	age_8 -> age_8, 1, lag6
	age_9 -> age_9, 1, lag7
	age_10 -> age_10, 1, lag8
	age_11 -> age_11, 1, lag9
	age_12 -> age_12, 1, lag10
	age_13 -> age_13, 1, lag11
	age_14 -> age_14, 1, lag12
	age_15 -> age_15, 1, lag13
	age_16 -> age_16, 1, lag14
	age_17 -> age_17, 1, lag15
	age_18 -> age_18, 1, lag16
	age_19 -> age_19, 1, lag17
	age_20 -> age_20, 1, lag18
	age_21 -> age_21, 1, lag19
	age_22 -> age_22, 1, lag20
	age_23 -> age_23, 1, lag21
	age_24 -> age_24, 1, lag22
	age_25 -> age_25, 1, lag23
	age_26 -> age_26, 1, lag24
	age_27 -> age_27, 1, lag25
	age_28 -> age_28, 1, lag26
	age_29 -> age_29, 1, lag27
	age_30 -> age_30, 1, lag28
	"
	
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
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
  	times = 1987:2022,
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "no_cov"

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
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_y,
  			time_column = "year",
  			times = 1987:2022,
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_lin_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = yfin_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:",  y)), 
  	family = family,
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
  	times = 1987:2022,
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
				data = yfin_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 2, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_y,
  			time_column = "year",
		  	times = 1987:2022,
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly2_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = yfin_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 2, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
  	times = 1987:2022,
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
				data = yfin_dat,
  			formula = as.formula(paste0("log_wt ~ 0 + age_f + poly(", y,", 3, raw = TRUE)")), 
  			family = family,
  			sem = "", 
  			dsem = dsem,
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh_y,
  			time_column = "year",
  			times = 1987:2022,
  			variable_column = "age_f",
				distribution_column = "age_f",
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly3_tvt")

		try(write_rds(fit_no_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	
		
	fit_int <- 
		try(tinyVAST(
		data = yfin_dat,
  	formula =  as.formula(paste0("log_wt ~ 0 + age_f + age_f:poly(", y,", 3, raw = TRUE)")),
  	family = family,
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
  	times = 1987:2022,
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_int_poly3_tvt")

		try(write_rds(fit_int, 
					file = paste0(file_path, sp, mod_name, ".rds")))
	

	}
	

	y <- c("yrprior_btemp", "yrprior_boxy")
	
	purrr::map(y, poly3_tinyv_mod_fun)
	
	
	
	
	