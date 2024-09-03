library(tinyVAST)
library(fmesher)

	file_path <- paste0(here(), "/output/model output/tinyVAST/age trim")
	
	pcod_dat <- dat_all |> filter(short_name == "pcod")
	
	pcod_dat <- pcod_dat |>
		filter(age <= 9) 
		
	table(pcod_dat$age, pcod_dat$year)
	
	pcod_dat$age_f <- paste0("age_", pcod_dat$age)

	# mesh
	mesh_p = fm_mesh_2d(loc = pcod_dat[,c("X","Y")],
                            cutoff = 37)

	dsem = "
	age_1 <-> age_1, 0, a1_sd,
	age_2 <-> age_2, 0, a2_sd,
	age_3 <-> age_3, 0, a3_sd,
	age_4 <-> age_4, 0, a4_sd,
	age_5 <-> age_5, 0, a5_sd,
	age_6 <-> age_6, 0, a6_sd,
	age_7 <-> age_7, 0, a7_sd,
	age_8 <-> age_8, 0, a8_sd,
	age_9 <-> age_9, 0, a8_sd,
	
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
  	spatial_graph = mesh_p,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j", getsd = FALSE))
	
	mod_name <- "no_cov"

		write_rds(pcod_no_cov_vast, 
					file = paste0(file_path, "pcod_", mod_name, ".rds"))
