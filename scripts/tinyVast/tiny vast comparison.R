# models with separate ST fields each age class

# now with separate ST fields for each age class
	
	pcod_dat_trim <- mutate(pcod_dat, age = ifelse(age >= 9, 9, age))
	pcod_dat_trim <- droplevels(pcod_dat_trim)
	pcod_dat_trim$age_f = factor(paste0("age_", pcod_dat_trim$age))

	
	# mesh
	mesh_v = fm_mesh_2d(loc = pcod_dat_trim[,c("X","Y")],
                            cutoff = 10)

	sem = ""
	
	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a2,
	age_3 <-> age_3, 0, a3,
	age_4 <-> age_4, 0, a4,
	age_5 <-> age_5, 0, a5,
	age_6 <-> age_6, 0, a6,
	age_7 <-> age_7, 0, a7,
	age_8 <-> age_8, 0, a8,
	age_9 <-> age_9, 0, a9	"
		
	pcod_no_cov = tinyVAST(
  	data = pcod_dat_trim,
  	formula = log_wt ~ 0 + age_f,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f")
	
	write(pcod_no_cov, file = paste0(file_path, "pcod", "_no_cov", ".rds"))
	
	pcod_temp = tinyVAST(
  	data = pcod_dat_trim,
  	formula = log_wt ~ 0 + age_f + poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f")

		write(pcod_temp, file = paste0(file_path, "pcod", "_temp", ".rds"))

	pcod_int_temp = tinyVAST(
  	data = pcod_dat_trim,
  	formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f")
	
		write(pcod_int_temp, file = paste0(file_path, "pcod", "_int_temp", ".rds"))

	
	pcod_oxy = tinyVAST(
  	data = pcod_dat_trim,
  	formula = log_wt ~ 0 + age_f + poly(yrprior_boxy, 2, raw = TRUE),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f")

	pcod_int_oxy3 = tinyVAST(
  	data = pcod_dat_trim,
  	formula = log_wt ~ 0 + age_f * poly(yrprior_boxy, 3, raw = TRUE),
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f")

	############
	
		# mesh
	mesh_s <- make_mesh(pcod_dat, c("X", "Y"), cutoff = 10)

	
	fit_s_st <- 
		sdmTMB(
			formula = log_wt ~ 0 + age_f,
			family = gaussian(),
			data = pcod_dat,
			mesh = mesh_s,
			spatial = "on",
			spatiotemporal = "ar1",
			time = "year",
			extra_time = unique(pcod_dat$year),
		  silent = TRUE)