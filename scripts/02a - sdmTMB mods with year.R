# sdmTMB function - year as factor 
	
# fit separate models for each age class for each species due to data-poor factor levels (age/year combos)

	

file_path_all <- "/output/model output/sdmTMB output/May 2024/year models/"

	# function to run each model by age class
	age_split_fun <- function(sp){
		
		new_dat <- dat_all %>% 
			filter(short_name == sp) %>%
			group_by(age_f) %>%
			group_split()
		
		new_dat
	}
	
	sp <- unique(dat_all$short_name)
	
	sp_age_dat_list <- purrr::map(sp, age_split_fun)
	sp_age_dat_list <- purrr::flatten(sp_age_dat_list)
	
	# fit model
	sdmTMB_yr_mod_fun <- function(df){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# model without interaction 
		mod_yr <- 
			sdmTMB(log_wt ~ 0 + year_f,
				data = df,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
			  time = "year",
			  silent = FALSE)
					
		s <- sanity(mod_yr, gradient_thresh = 0.05)
	
		mod_name <- "_yr_mod_age_"
		
		age <- unique(df$age_f)
		
		sp <- unique(df$short_name)
		
		write_rds(mod_yr, 
			file = paste0(here(), file_path_all, sp, mod_name, age, ".rds"))
		 					
		
	}
	
	purrr::map(sp_age_dat_list, sdmTMB_yr_mod_fun)
	

	
	
	
	
	
	
	