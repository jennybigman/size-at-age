	# now with separate ST fields for each age class

	file_path <- paste0(here(), "/output/model output/tinyVAST/")
	
	pcod_dat$age_f = factor(paste0("age_", pcod_dat$age))
	
	#pcod_dat_trim <- pcod_dat %>% filter(age < 10)

	
	# mesh
	mesh_v = fm_mesh_2d(loc = pcod_dat[,c("X","Y")],
                            cutoff = 37)

#	sem = ""
#	
#	dsem = "
#	age_1 <-> age_1, 0, a1,
#	age_2 <-> age_2, 0, a2,
#	age_3 <-> age_3, 0, a3,
#	age_4 <-> age_4, 0, a4,
#	age_5 <-> age_5, 0, a5,
#	age_6 <-> age_6, 0, a6,
#	age_7 <-> age_7, 0, a7,
#	age_8 <-> age_8, 0, a8,
#	age_9 <-> age_9, 0, a9,
#	age_10 <-> age_10, 0, a10
#	"
#		
#	fit_v_st_a = tinyVAST(
#  	data = pcod_dat,
#  	formula = log_wt ~ 0 + age_f,
#  	family = gaussian(),
#  	sem = "", 
#  	dsem = dsem,
#  	space_column = c("X", "Y"), 
#  	spatial_graph = mesh_v,
#  	time_column = "year",
#  	times = 1993:2022,
#  	variable_column = "age_f",
#  	control = tinyVASTcontrol(profile = "alpha_j"))
#
	
	# add an AR1 term
	
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
	
	mod_name <- "no_cov_"

		write_rds(pcod_no_cov_vast, 
					file = paste0(file_path, "pcod_", mod_name, ".rds"))
	
		# no interaction
		
	pcod_lin_temp_vast <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f + yrprior_btemp,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "temp_lin"

		write_rds(pcod_no_cov_vast, 
					file = paste0(file_path, "pcod_", mod_name, ".rds"))
	
		
	pcod_lin_int_temp_vast <- 
		tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f * yrprior_btemp,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	mod_name <- "temp_int_lin"

		write_rds(pcod_no_cov_vast, 
					file = paste0(file_path, "pcod_", mod_name, ".rds"))
	

	

	