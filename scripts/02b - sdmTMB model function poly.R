# sdmTMB function - without CV, polynomial
	
	# mesh for each species - best to do this visually for model convergence

	## pollock mesh ####
	pollock_dat <- dat_all %>% 
		dplyr::filter(short_name == "pollock")
	
	pollock_dat$age_f <- droplevels(pollock_dat$age_f)
		
	pol_mesh <- make_mesh(
		pollock_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
		#	max.edge = 85#,
			offset = c(70, 60)
		)
	
	plot(pol_mesh)
	
	## pcod mesh ####
	pcod_dat <- dat_all %>% 
		dplyr::filter(short_name == "pcod")
	
	pcod_dat$age_f <- droplevels(pcod_dat$age_f)
		
	pcod_mesh <- make_mesh(
		pcod_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 30,
			offset = c(60, 70)
		)
	
	plot(pcod_mesh)
	
	## yellowfin mesh ####
	yfin_dat <- dat_all %>% 
		dplyr::filter(short_name == "yfin")

	yfin_dat$age_f <- droplevels(yfin_dat$age_f)
		
	yfin_mesh <- make_mesh(
		yfin_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 30,
			#max.edge = 60,
			offset = c(60, 70)
		)
	
	plot(yfin_mesh)
	
	# atooth
	atooth_dat <- dat_all %>% 
		filter(short_name == "atooth")
	
	atooth_dat$age_f <- droplevels(atooth_dat$age_f)
		
	atooth_mesh <- make_mesh(
		atooth_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
		#	max.edge = 85#,
			offset = c(70, 60)
		)
	
	plot(atooth_mesh)

	dat_all <- list(pollock_dat, pcod_dat, yfin_dat, atooth_dat) %>% bind_rows()

		
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/APR 2024 NEW/poly models/"
	
	#### fit models without interaction ####
	
	sdmTMB_no_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp)
  			
					# assign mesh
					if (sp == "pcod") {
						
						mesh <- pcod_mesh
					
					} else if (sp == "pollock") {
							
						mesh <- pol_mesh
					
					} else if (sp == "atooth") {
						
						mesh <- atooth_mesh
						
					} else {
						
						mesh <- yfin_mesh
					
					}
  				
					# for mod name
					mod_name <- "_no_int_mod_"
					
		# run models	
		
					print(paste('running no int model for', sp, "with", y))
		
					# set up formulas
					form_no_int <- paste0("log_wt ~ 0 + age_f + poly(", y, ", 3)")
		

 					# model without interaction 
					mod_no_int <- 
						try(
							sdmTMB(
								formula = as.formula(form_no_int),
								data = new_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2022:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
					s <- sanity(mod_no_int, gradient_thresh = 0.05)
	
	
		 						write_rds(mod_no_int, 
									file = paste0(here(), file_path_all, y, mod_name, sp, "_poly.rds"))
		 						
		 						print(paste("no int model for", sp, "with", y, "complete"))
		 	
	}
	
	
	# run function

	sp <- unique(dat_all$short_name)
	
	vars <- dat_all %>%
		ungroup %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 


	df_func <- expand_grid(
		sp = sp,
		y = vars
	)
	

	map2(df_func$sp, df_func$y, sdmTMB_no_int_func)

	
	#### fit models with an interaction ####
	
	sdmTMB_int_func <- function(sp, y){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp)
  			
					# assign mesh
					if (sp == "pcod") {
						
						mesh <- pcod_mesh
					
					} else if (sp == "pollock") {
							
						mesh <- pol_mesh
					
					} else if (sp == "atooth") {
						
						mesh <- atooth_mesh
						
					} else {
						
						mesh <- yfin_mesh
					
					}
  				
					# for mod name
					mod_name <- "_int_mod_"
					
					
		# run models	
		
					print(paste('running int model for', sp, "with", y))
		
					# set up formulas
					form_int <- paste0("log_wt ~ 0 + age_f * poly(", y, ", 3)")
		
 					# model without interaction 
					mod_int <- 
						try(
							sdmTMB(
								formula = as.formula(form_int),
								data = new_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
								extra_time = 2020:2099,
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
					
					s <- sanity(mod_int, gradient_thresh = 0.05)
	
	
							write_rds(mod_int, 
										file = paste0(here(), file_path_all, y, mod_name, sp, "_poly.rds"))
									
							print(paste("int model for", sp, "with", y, "complete"))

	}
	
	
	# run function

	sp <- unique(dat_all$short_name)
	
	vars <- dat_all %>%
		ungroup() %>%
		select(contains(c("btemp", "boxy"))) %>%
		names() 

	df_func <- expand_grid(
		sp = sp,
		y = vars
	)
	


	map2(df_func$sp, df_func$y, sdmTMB_int_func)
