# compare this to model with no interaction between age_f & temp
# what is the no covariate mod? age_f * year)f?

# figure out how many individuals per year for a given age class
	sum <- dat_all %>%
		group_by(short_name, age_f, year_f) |>
		summarise(count = n())
	
	sum_to_drop <- sum |> filter(count < 10)
	
	sum_to_drop$age_yr <- paste0(sum_to_drop$short_name, "_", sum_to_drop$age_f, "_", sum_to_drop$year_f)

	drop <- sum_to_drop$age_yr
	
	dat_all <- dat_all %>%
		mutate(sp_age_yr = paste(short_name, age_f, year_f, sep = "_"))
	
	dat_all <- dat_all %>%
		filter(sp_age_yr %!in% drop)
	
	sum2 <- dat_all %>%
		group_by(short_name, age_f, year_f) |>
		summarise(count = n())
	
	
	file_path_all <- "/output/model output/sdmTMB output/Aug 2024/TVA/"
	
	# fit models
	com_mod_fun <- function(sp, y){
		
		df <- dat_all %>% filter(short_name == sp)
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# mod name
		mod_name <- paste0(sp, "_no_int_poly3_TVA_")
		
		# formula
		form <- paste0("log_wt ~ 0 + age_f * year_f + age_f * poly(", y, ", 3, raw = TRUE)")

			
		mod_com <- 
			try(
			sdmTMB(
		  	data = df,
		  	formula = as.formula(form),
		  	#control = sdmTMBcontrol(profile = "b_j"),
		  	mesh = mesh,
		  	family = gaussian(),
		  	spatial = "on",
		  	spatiotemporal = "IID",
		  	time = "year",
		  	share_range = FALSE,
      	silent = FALSE,
      	priors = sdmTMBpriors(
        	matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
        	matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
		
		s <- sanity(mod_com)
	
    # deal with warnings and issues
					
		# if error, tell me
		if (class(mod_com) == "try-error"){
			print(paste("error!"))
		 			
		} else if 
			(s$hessian_ok ==      "TRUE" &
			s$eigen_values_ok == "TRUE" &
			s$nlminb_ok ==       "TRUE" &
			s$range_ok ==        "TRUE" &
			s$se_na_ok ==        "TRUE" &
			s$sigmas_ok ==       "TRUE") {
	
			write_rds(mod_com, 
				file = paste0(here(), file_path_all, y,sp, mod_name, ".rds"))
												
  }}

	# run function

	sp <- unique(dat_all$short_name)
	
	vars <- dat_all %>%
		ungroup() %>%
		select(contains("yrprior")) %>%
		names() 

	#df_func <- expand_grid(
	#	sp = sp,
	#	y = vars
	#)
	
	df_func <- expand_grid(
		sp = "pollock",
		y = "yrprior_btemp",
	)
	

	map2(df_func$sp, df_func$y, com_mod_fun)
