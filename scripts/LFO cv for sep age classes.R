# LFO CV for sep age classes

# fit separate models for each age class for each species due to data-poor factor levels (age/year combos)

###### MAY NEED TO CHANGE MODEL TYPE (poly 2,3) and temp/oxy metric based on non lfo cv mods ########
	future::plan(multisession)
	
	# file path
	file_path_all <- "/output/model output/sdmTMB output/May 2024/sep age classes/"

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

	### fit models 
	
	LFO_cov_mods_fun <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# model without temp/oxy 
		
		# for mod name
		mod_name <- "sep_age_lfo_"
		
		# extract info from df
		sp <- unique(df$short_name)
		age <- unique(df$age)
		
		len <- length(sort(unique(df$year)))
		forecast_yrs <- len - 10

		form <- paste0("log_wt ~ poly(", y, ", 3)")

		mod_cov_cv <- 
			try(sdmTMB_cv(
				formula = as.formula(form),
				data = df,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
				time = "year",
				lfo = TRUE,
				lfo_forecast = 1,
				lfo_validation = forecast_yrs,
				share_range = FALSE,
				silent = FALSE,
				parallel = TRUE,
				priors = sdmTMBpriors(
					matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
					matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
	
			write_rds(mod_cov_cv, 
				file = paste0(here(), file_path_all, mod_name, sp, "_age_", age, "_", y, ".rds"))
									
					print(paste("model for", sp, "with", y, "complete"))
					
	}
		
	vars <- dat_all %>%
		ungroup() %>%
		select(contains("yr")) %>%
		names() 

	df_func <- expand_grid(
		df = sp_age_dat_list,
		y = vars
	)
	
	purrr::map2(df_func$df, df_func$y, LFO_cov_mods_fun)
	
