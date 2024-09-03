# fit top models outside function so can predict

	library(tinyVAST)
	library(fmesher)

	file_path <- paste0(here(), "/output/model output/tinyVAST/top_mods/")
	
	# pcod top mod
	pcod_dat <- dat_all |> filter(short_name == "pcod")
	pcod_dat$age_f <- droplevels(pcod_dat$age_f)
	pcod_dat$age_f <- as.factor(paste0("age_", pcod_dat$age))

	# mesh
	mesh_pc = fm_mesh_2d(loc = pcod_dat[,c("X","Y")],
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
	"
	
	pcod_top_mod <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pc,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
		write_rds(pcod_top_mod, 
					file = paste0(file_path, "pcod_top_mod.rds"))

		
	# atooth mod
		
	atooth_dat <- dat_all |> filter(short_name == "atooth")
	atooth_dat$age_f <- droplevels(atooth_dat$age_f)
	atooth_dat$age_f <- as.factor(paste0("age_", atooth_dat$age))

	# mesh
	mesh_a = fm_mesh_2d(loc = atooth_dat[,c("X","Y")],
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

	atooth_top_mod <- 
		tinyVAST(
		data = atooth_dat,
  	formula = log_wt ~ 0 + age_f + poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_a,
  	time_column = "year",
  	times = 1996:2021,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
		write_rds(atooth_top_mod, 
					file = paste0(file_path, "atooth_top_mod.rds"))

		atooth_top_mod <- readRDS(paste0(file_path, "atooth_top_mod.rds"))

	# pollock
		
	pollock_dat <- dat_all |> filter(short_name == "pollock")
	pollock_dat$age_f <- droplevels(pollock_dat$age_f)
	pollock_dat$age_f <- as.factor(paste0("age_", pollock_dat$age))

	# mesh
	mesh_pol = fm_mesh_2d(loc = pollock_dat[,c("X","Y")],
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

	pol_top_mod <- 
		tinyVAST(
		data = pollock_dat,
  	formula = log_wt ~ 0 + age_f + poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pol,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
		write_rds(pol_top_mod, 
					file = paste0(file_path, "pol_top_mod.rds"))

	
	# yfin ####
	

	yfin_dat <- dat_all |> filter(short_name == "yfin")
	yfin_dat$age_f <- droplevels(yfin_dat$age_f)
	yfin_dat$age_f <- as.factor(paste0("age_", yfin_dat$age))

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
	
	yfin_top_mod <- 
		tinyVAST(
		data = yfin_dat,
  	formula = log_wt ~ 0 + age_f + yrprior_boxy,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_y,
  	time_column = "year",
  	times = 1987:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
		write_rds(yfin_top_mod, 
					file = paste0(file_path, "yfin_top_mod.rds"))

		
	