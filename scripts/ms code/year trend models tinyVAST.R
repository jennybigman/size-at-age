# year trend models	

	file_path <- paste0(here(), "/output/model output/tinyVAST/year MV/")
	#dir.create(file.path(file_path), showWarnings = FALSE)

	# atooth ####
	atooth_dat <- dat_all %>% filter(short_name == "atooth")
	
	atooth_dat$age_f = factor(paste0("age_", atooth_dat$age))
	
	sp <- unique(atooth_dat$short_name)
	
	# mesh
	mesh_a = fm_mesh_2d(loc = atooth_dat[,c("X","Y")],
                            cutoff = 37)

	family = list(
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
	
		
	atooth_yr <- 
		tinyVAST(
		data = atooth_dat,
  	formula = log_wt ~ 0 + age_f + age_f:year,
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_a,
  	time_column = "year",
  	times = min(atooth_dat$year):max(atooth_dat$year),
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	write_rds(atooth_yr, 
				file = paste0(file_path, "atooth", "_year_trend", ".rds"))

	
	#### pcod ####
	
	pcod_dat <- dat_all |> filter(short_name == "pcod")

	pcod_dat$age_f <- paste0("age_", pcod_dat$age)
	
	sp <- unique(pcod_dat$short_name)

	# mesh
	mesh_pc = fm_mesh_2d(loc = pcod_dat[,c("X","Y")],
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

	pcod_yr <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f + age_f:year,
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pc,
  	time_column = "year",
  	times = min(pcod_dat$year):max(pcod_dat$year),
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	write_rds(pcod_yr, 
				file = paste0(file_path, "pcod", "_year_trend", ".rds"))

	#### pollock ####
	pol_dat <- dat_all %>% filter(short_name == "pollock")
	
	pol_dat$age_f = factor(paste0("age_", pol_dat$age))
	
	sp <- unique(pol_dat$short_name)
	

	# mesh
	mesh_pl = fm_mesh_2d(loc = pol_dat[,c("X","Y")],
                            cutoff = 37)
	
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
	
	
	pol_yr <- 
		tinyVAST(
		data = pol_dat,
  	formula = log_wt ~ 0 + age_f + age_f:year,
  	family = family,
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_pl,
  	time_column = "year",
  	times = min(pol_dat$year):max(pol_dat$year),
  	variable_column = "age_f",
		distribution_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	write_rds(pol_yr, 
				file = paste0(file_path, "pollock", "_year_trend", ".rds"))

	#### yfin ####
	
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
		
	
	yfin_yr <- 
		tinyVAST(
		data = yfin_dat,
  	formula = log_wt ~ 0 + age_f + age_f:year,
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
	
	write_rds(yfin_yr, 
				file = paste0(file_path, "yfin", "_year_trend", ".rds"))
