# without tv

# sdmTMB function 
	
# fit separate models for each age class for each species due to data-poor factor levels (age/year combos)

	# file path
	file_path <- "/output/model output/sdmTMB output/August 2024 single age models/"

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
	
	no_cov_fun <- function(df){
		
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
		mod_name <- "_no_cov_"
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
		
		yrs <- unique(df$year)
					
		# run models	
		print(paste('running no cov model for', sp))
		
	
 		# model without interaction 
		mod_no_cov <- 
						try(
							sdmTMB(
								formula = log_wt ~ 1,
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								#extra_time = yrs,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
		 #refit without ST fields if not needed
		 b <- tidy(mod_no_cov, "ran_pars")
					
				if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  		mod_no_cov <- update(mod_no_cov, spatiotemporal = "off")}
  		  		
  		  s <- sanity(mod_no_cov, gradient_thresh = 0.05)

		 if
		 
			 (s$hessian_ok == "TRUE" &
				s$eigen_values_ok == "TRUE" &
				s$nlminb_ok ==       "TRUE" &
				s$range_ok ==        "TRUE" &
				s$sigmas_ok ==       "TRUE") {
	
	
				write_rds(mod_no_cov, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
									
		 } else {
		 	
		 	mod_no_cov <- update(mod_no_cov, spatial = "off")
		 	
  		s <- sanity(mod_no_cov, gradient_thresh = 0.05)
		 
		 }
		 
		 
		 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_no_cov, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
				
				} else {
				
				mod_no_cov <- 
						try(
							sdmTMB(
								formula = log_wt ~ 1,
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "off",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
				
  		  s <- sanity(mod_no_cov, gradient_thresh = 0.05)
	
				}
		 
		 	 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_no_cov, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
			
				} else {
					
					print("Error!")
			
				}
		 
	}
	
	
	no_cov_list <- purrr::map(sp_age_dat_list, no_cov_fun)
	
	
#########################################
# Models with temperature ####
#########################################

	#### 2nd order polynomial	####
	
	poly2_fun <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- paste0("_poly2_", y, "_")
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
		
		yrs <- unique(df$year)
	
 		# model 
		
		form <-  paste0("log_wt ~ poly(", y, ", 2, raw = TRUE)")

		mod_poly2 <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								#extra_time = yrs,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
		
		if  (class(mod_poly2) != "try-error"){	
		 #refit without ST fields if not needed
		 b <- tidy(mod_poly2, "ran_pars")
					
				if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  		mod_poly2 <- update(mod_poly2, spatiotemporal = "off")
  		  		}
		}
  		  s <- sanity(mod_poly2, gradient_thresh = 0.05)

		 if
		 
			 (s$hessian_ok == "TRUE" &
				s$eigen_values_ok == "TRUE" &
				s$nlminb_ok ==       "TRUE" &
				s$range_ok ==        "TRUE" &
				s$sigmas_ok ==       "TRUE") {
	
	
				write_rds(mod_poly2, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
									
		 } else {
		 	
		 	mod_poly2 <- update(mod_poly2, spatial = "off")
		 	
  		s <- sanity(mod_poly2, gradient_thresh = 0.05)
		 
		 }
		 
		 
		 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_poly2, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
				
				} else {
				
				mod_poly2 <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "off",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
				
  		  s <- sanity(mod_poly2, gradient_thresh = 0.05)
	
				}
		 
		 	 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_poly2, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
			
				} else {
					
					
					print(paste0("error for", sp, "_", age, "mod_name"))
				}
		 
	}
	
	vars <- names(dat_all %>% select(contains("yrprior")))
	
	fdf <- crossing(
		y = vars,
		df = sp_age_dat_list
	)
	
	poly2_mod_list <- purrr::map2(fdf$df, fdf$y, poly2_fun)
	
	
	#### 3rd order polynomial ####
	
	poly3_fun <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- paste0("_poly3_", y, "_")
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
		
		yrs <- unique(df$year)
	
 		# model 
		
		form <-  paste0("log_wt ~ poly(", y, ", 3, raw = TRUE)")

		mod_poly3 <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
		
		if  (class(mod_poly3) != "try-error"){	
		
		 #refit without ST fields if not needed
		 b <- tidy(mod_poly3, "ran_pars")
					
				if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  		mod_poly3 <- update(mod_poly3, spatiotemporal = "off")
  		  		}}
  		  		
  		  s <- sanity(mod_poly3, gradient_thresh = 0.05)

		 if
		 
			 (s$hessian_ok == "TRUE" &
				s$eigen_values_ok == "TRUE" &
				s$nlminb_ok ==       "TRUE" &
				s$range_ok ==        "TRUE" &
				s$sigmas_ok ==       "TRUE") {
	
	
				write_rds(mod_poly3, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
									
		 } else {
		 	
		 	mod_poly3 <- update(mod_poly3, spatial = "off")
		 	
  		s <- sanity(mod_poly3, gradient_thresh = 0.05)
		 
		 }
		 
		 
		 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_poly3, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
				
				} else {
					
					print(paste0("error for", sp, "_", age, "mod_name"))
				}
		 
	}
	
	vars <- names(dat_all %>% select(contains("yrprior")))
	
	fdf <- crossing(
		y = vars,
		df = sp_age_dat_list
	)
	
	poly3_mod_list <- purrr::map2(fdf$df, fdf$y, poly3_fun)
	
	
	#### GAM ####
	
	gam_fun <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- "_gam_"
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
		
		yrs <- unique(df$year)
	
 		# model 
		
		form <-  paste0("log_wt ~ s(", y, ", k = 3)")

		mod_gam <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
		 #refit without ST fields if not needed
  		  		
  		  s <- sanity(mod_gam, gradient_thresh = 0.05)

			if
		 
			 (s$hessian_ok == "TRUE" &
				s$eigen_values_ok == "TRUE" &
				s$nlminb_ok ==       "TRUE" &
				s$range_ok ==        "TRUE" &
				s$sigmas_ok ==       "TRUE") {
	
	
				write_rds(mod_gam, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
			
									
		 } else {
		 	
		 	mod_gam <- update(mod_gam, spatial = "off")
		 	
  		s <- sanity(mod_gam, gradient_thresh = 0.05)
		 
		 }
		 
		 
		 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_gam, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
				
				} else {
				
				mod_gam <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
				
  		  s <- sanity(mod_gam, gradient_thresh = 0.05)
	
				}
		 
		 	 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_gam, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
			
				} else {
					
						mod_gam <- 
						try(
							sdmTMB(
								formula = as.formula(form),
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "off",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
						
				}
  		  
  		   if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_gam, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
				} else {
					
					print(paste0("error for", sp, "_", age, "mod_name"))
				}
		 
	}
	
	vars <- names(dat_all %>% select(contains("yrprior")))
	
	fdf <- crossing(
		y = vars,
		df = sp_age_dat_list
	)
	
	gam_mod_list <- purrr::map2(fdf$df, fdf$y, gam_fun)
	
	
	#### linear ####
	
	lin_fun <- function(df, y){
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# for mod name
		mod_name <- paste0("_lin_", y, "_")
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
		
		yrs <- unique(df$year)
	
 		# model 
		
		form <-  paste0("log_wt ~",  y)

		mod_lin <- 
						try(
							sdmTMB(
								formula = as.formula(paste0("log_wt ~",  y)),
								data = df,
								mesh = mesh,
								control = sdmTMBcontrol(profile = "b_j"),
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
		if  (class(mod_lin) != "try-error"){	
		
		 #refit without ST fields if not needed
		 b <- tidy(mod_lin, "ran_pars")
					
				if (b$estimate[b$term == "sigma_E"] < 0.01) {
  		  		mod_lin <- update(mod_lin, spatiotemporal = "off")
  		  		}}
  		  		
  		  s <- sanity(mod_lin, gradient_thresh = 0.05)

		 if
		 
			 (s$hessian_ok == "TRUE" &
				s$eigen_values_ok == "TRUE" &
				s$nlminb_ok ==       "TRUE" &
				s$range_ok ==        "TRUE" &
				s$sigmas_ok ==       "TRUE") {
	
	
				write_rds(mod_lin, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
									
		 } else {
		 	
		 	mod_lin <- update(mod_lin, spatial = "off")
		 	
  		s <- sanity(mod_lin, gradient_thresh = 0.05)
		 
		 }
		 
		 
		 if
		 
				(s$hessian_ok ==      "TRUE" &
				 s$eigen_values_ok == "TRUE" &
				 s$nlminb_ok ==       "TRUE" &
				 s$range_ok ==        "TRUE" &
				 #s$se_na_ok ==       "TRUE" &
				 s$sigmas_ok ==       "TRUE") {
	
				write_rds(mod_lin, 
										file = paste0(here(), file_path, sp, mod_name, "age_", age, ".rds"))
				
				} else {
				
		
					print(paste0("error for", sp, "_", age, "mod_name"))
				}
		 
	}
	
	vars <- names(dat_all %>% select(contains("yrprior")))
	
	fdf <- crossing(
		y = vars,
		df = sp_age_dat_list
	)
	
	lin_mod_list <- purrr::map2(fdf$df, fdf$y, lin_fun)
	