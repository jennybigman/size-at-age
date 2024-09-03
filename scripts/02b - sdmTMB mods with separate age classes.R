# sdmTMB function 
	
# fit separate models for each age class for each species due to data-poor factor levels (age/year combos)

	# file path
	file_path <- "/output/model output/sdmTMB output/May 2024/sep age classes/time-varying/Aug 2024/"

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
	
	
	##################################################
	# FIT MODELS ####
	##################################################
	
	#### no covariate models ####
	
	no_cov_sep_fun <- function(df){
		
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
		mod_name <- "_sep_age_no_cov_"
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
					
		# run models	
		print(paste('running no cov model for', sp))
		
	
 		# model without interaction 
		mod_no_cov <- 
						try(
							sdmTMB(
								formula = log_wt ~ 1,
								data = df,
								mesh = mesh,
								time_varying = ~ 1,
								time_varying_type = "ar1",
								#control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
  
		 #refit without ST fields if not needed
			if (class(mod_no_cov) != "try-error"){   
				
				b <- tidy(mod_no_cov, "ran_pars")
					
				if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  		mod_no_cov <- update(mod_no_cov, spatiotemporal = "off")
  		  		
				}}
			
			if (class(mod_no_cov) != "try-error"){   
				
			s <- sanity(mod_no_cov, gradient_thresh = 0.05)
	
			write_rds(mod_no_cov, 
				file = paste0(here(), file_path, sp, mod_name, "_age_", age, ".rds"))
									
	}}
	
	no_cov_list <- purrr::map(sp_age_dat_list, no_cov_sep_fun)
	
	
	#### with temp/oxy ####
	
	file_path_all <- "/output/model output/sdmTMB output/May 2024/sep age classes/time-varying/"


	#### 2nd order polynomial ####
	
	cov_sep_fun_poly2 <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- "_sep_age_cov_poly2_tv_"
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
					
		
 		# model 
		
		form <- paste0("log_wt ~ poly(", y, ", 2, raw = TRUE)")
		
		mod_poly2 <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								time_varying = ~ 1,
								time_varying_type = "ar1",
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
			# refit without ST fields if not needed
			try(b <- tidy(mod_poly2, "ran_pars"))
  			
			try(if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  mod_poly2 <- update(mod_poly2, spatiotemporal = "off")
  		})
					  
			s <- sanity(mod_poly2, gradient_thresh = 0.05)
	
			write_rds(mod_poly2, 
				file = paste0(here(), file_path_all, sp, mod_name, y, "_age_", age, ".rds"))
									
					print(paste("poly2 model for", sp, "complete"))
		
	}
	
	
	#vars <- dat_all %>%
	#	ungroup() %>%
	#	select(contains(c("btemp", "boxy"))) %>%
	#	names() 
	
	vars <- dat_all %>%
		ungroup() %>%
		select(contains("yrprior")) %>%
		names() 

	df_func <- expand_grid(
		df = sp_age_dat_list,
		y = vars
	)
	
	purrr::map2(df_func$df, df_func$y, cov_sep_fun_poly2)
	
	#### 3nd order polynomial ####
	
	cov_sep_fun_poly3 <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- "_sep_age_cov_poly3_tv_"
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
					
		
 		# model 
		
		form <- paste0("log_wt ~ poly(", y, ", 3, raw = TRUE)")
		
		mod_poly3 <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								time_varying = ~ 1,
								time_varying_type = "ar1",
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
			# refit without ST fields if not needed
			try(b <- tidy(mod_poly3, "ran_pars"))
  			
			try(if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  mod_poly2 <- update(mod_poly3, spatiotemporal = "off")
  		})
					  
			s <- sanity(mod_poly3, gradient_thresh = 0.05)
	
			write_rds(mod_poly3, 
				file = paste0(here(), file_path_all, sp, mod_name, y, "_age_", age, ".rds"))
									
					print(paste("poly3 model for", sp, "complete"))
		
	}
	
	
	#vars <- dat_all %>%
	#	ungroup() %>%
	#	select(contains(c("btemp", "boxy"))) %>%
	#	names() 
	
	vars <- dat_all %>%
		ungroup() %>%
		select(contains("yrprior")) %>%
		names() 

	df_func <- expand_grid(
		df = sp_age_dat_list,
		y = vars
	)
	
	purrr::map2(df_func$df, df_func$y, cov_sep_fun_poly3)
	
	
	#### GAMS #### 
	
	cov_sep_fun_gam <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- "_sep_age_cov_gam_tv_"
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
					
		
 		# model 
		
		form <- paste0("log_wt ~ s(" , y, ", k = 3)")
		
		mod_gam <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								time_varying = ~ 1,
								time_varying_type = "ar1",
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
			# refit without ST fields if not needed
			try(b <- tidy(mod_gam, "ran_pars"))
  			
			try(if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  mod_gam <- update(mod_gam, spatiotemporal = "off")
  		})
					  
			s <- sanity(mod_gam, gradient_thresh = 0.05)
	
			write_rds(mod_gam, 
				file = paste0(here(), file_path_all, sp, mod_name, y, "_age_", age, ".rds"))
									
			print(paste("mod_gam model for", sp, "complete"))
		
	}
	
	
	#vars <- dat_all %>%
	#	ungroup() %>%
	#	select(contains(c("btemp", "boxy"))) %>%
	#	names() 
	
	vars <- dat_all %>%
		ungroup() %>%
		select(contains("yrprior")) %>%
		names() 

	df_func <- expand_grid(
		df = sp_age_dat_list,
		y = vars
	)
	
	purrr::map2(df_func$df, df_func$y, cov_sep_fun_gam)
	
	#### linear ####
	
	cov_sep_fun_lin <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- "_sep_age_cov_lin_tv_"
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
					
		
 		# model 
		
		form <- paste0("log_wt ~", y)
		
		mod_lin <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								time_varying = ~ 1,
								time_varying_type = "ar1",
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
			# refit without ST fields if not needed
			try(b <- tidy(mod_lin, "ran_pars"))
  			
			try(if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  mod_lin <- update(mod_lin, spatiotemporal = "off")
  		})
					  
			s <- sanity(mod_lin, gradient_thresh = 0.05)
	
			write_rds(mod_lin, 
				file = paste0(here(), file_path_all, sp, mod_name, y, "_age_", age, ".rds"))
									
			print(paste("mod_lin model for", sp, "complete"))
		
	}
	
	
	#vars <- dat_all %>%
	#	ungroup() %>%
	#	select(contains(c("btemp", "boxy"))) %>%
	#	names() 
	
	vars <- dat_all %>%
		ungroup() %>%
		select(contains("yrprior")) %>%
		names() 

	df_func <- expand_grid(
		df = sp_age_dat_list,
		y = vars
	)
	
	purrr::map2(df_func$df, df_func$y, cov_sep_fun_lin)
	
	
	#############################################################################
	# For models that wouldn't fit in function - play around with code below
	#############################################################################
	

	# try to fit models for ages that are missing

	file_path <- "/output/model output/sdmTMB output/May 2024/sep age classes/time-varying/"

	#vars <- names(dat_all %>% select(contains("yrprior_boxy")))
	
	mod_types <- c("poly2", "poly3", "lin", "gam", "no_cov")
	
	poly2  <- log_wt ~ poly(yrprior_btemp, 2, raw = TRUE)
	poly3 <-  log_wt ~ poly(yrprior_btemp, 3, raw = TRUE)
	lin <-  	log_wt ~ yrprior_btemp
	gam <-		log_wt ~ s(yrprior_btemp, k = 3)
	no_cov <- log_wt ~ 1

	
	### run mod
	
	pollock_dat <- dat_all %>% 
		filter(short_name == "pollock") %>% 
		filter(age_f == 16)
	
	# mesh
	mesh <- make_mesh(
			pollock_dat, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 80,
				offset = c(60, 70)
			)
	
	plot(mesh)
	
	mesh$mesh$n


	mod <- 
		sdmTMB(
			formula = as.formula(poly2),
			data =  pollock_dat,
			mesh = mesh,
			time_varying = ~ 1,
			time_varying_type = "ar1",
			control = sdmTMBcontrol(profile = "b_j"),
			spatial = "off",
			spatiotemporal = "iid",
		  time = "year",
		  share_range = FALSE,
		  silent = FALSE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 400, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 600, sigma_lt = 2)))
	
	sanity(mod)
	
	write_rds(mod, 
		file = paste0(here(), file_path, "pollock_sep_age_cov_poly2_tv_yrprior_btemp_age_16.rds"))
		 		
##### run models for other species' ages that wouldn't run
		### possibly run all atooth mods without spatial effects
		### re compare temp mods and oxy mods
	
