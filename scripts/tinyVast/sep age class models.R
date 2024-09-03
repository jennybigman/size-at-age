# tinyVAST separate models for each age class

	
library(tinyVAST)
library(fmesher)

	file_path <- paste0(here(), "/output/model output/tinyVAST/sep age models/")
	
	#### no covariate model ####
	
	no_cov_mod_fun <- function(d){
	
		mod_name <- "no_cov"
		
		age <- unique(d$age)
		
		mesh <- fm_mesh_2d(loc = d[,c("X","Y")],
                       cutoff = 50)

		fit <- 
			tinyVAST(
				data = d,
  			formula = log_wt ~ 1,
  			family = gaussian(),
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh,
  			time_column = "year",
  			times = 1996:2021,
  			control = tinyVASTcontrol(profile = "alpha_j"))
		
		write_rds(fit, 
					file = paste0(file_path, "atooth_age_", age, "_", mod_name, ".rds"))
		
	}

		
	purrr::map(atooth_dat_list, no_cov_mod_fun)
		
		
	
	tinyv_mod_fun <- function(d, y){
		
		age <- unique(d$age)
		
		mesh <- fm_mesh_2d(loc = d[,c("X","Y")],
                       cutoff = 50)
		
		fit <- 
			try(tinyVAST(
				data = d,
  			formula =  as.formula(paste0("log_wt ~ poly(", y, ", 2, raw = TRUE)")),
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = mesh,
  			time_column = "year",
  			times = 1996:2021,
  			control = tinyVASTcontrol(profile = "alpha_j")))
	
		mod_name <- paste0(y, "_poly2_", "age_", age)

		try(write_rds(fit, 
					file = paste0(file_path, "pollock_", mod_name, ".rds")))
	

	}
	
	fdf <- crossing(
		d = pol_dat_list,
		y = c("yrprior_btemp", "yrprior_boxy")
	)


	purrr::map2(fdf$d, fdf$y, tinyv_mod_fun)
	
