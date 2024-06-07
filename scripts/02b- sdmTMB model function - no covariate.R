# sdmTMB function - without environmental covariates
	
	# mesh for each species - best to do this visually for model convergence

	## pollock mesh ####
	pollock_dat <- dat_all %>% 
		dplyr::filter(short_name == "pollock")

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
	
	atooth_mesh <- make_mesh(
		atooth_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 50,
		#	max.edge = 85#,
			offset = c(70, 60)
		)
	
	plot(atooth_mesh)

	dat_all <- list(pollock_dat, pcod_dat, yfin_dat, atooth_dat) %>% bind_rows()

		
	#### fit models with no envr covariate ####
	
	file_path_all <- "/output/model output/sdmTMB output/May 2024/no covariate models/"

	sdmTMB_no_cov_func <- function(sp){
		
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
					mod_name <- "no_cov_"
					
		# run models	
		
					print(paste('running no covariate model for', sp))
		
					# set up formulas
					form_no_cov <- paste0("log_wt ~ 0 + age_f")
		

 					# model without interaction 
					mod_no_cov <- 
						try(
							sdmTMB(
								formula = as.formula(form_no_cov),
								data = new_dat,
								mesh = mesh,
								spatial = "on",
								spatiotemporal = "IID",
							  time = "year",
							  share_range = FALSE,
							  silent = FALSE,
								priors = sdmTMBpriors(
									matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
									matern_s = pc_matern(range_gt = 300, sigma_lt = 2))))
					
					s <- sanity(mod_no_cov, gradient_thresh = 0.05)
	
	
		 						write_rds(mod_no_cov, 
									file = paste0(here(), file_path_all, mod_name, sp, ".rds"))
		 						
		 						print(paste("no cov model for", sp, "complete"))
		 	
	}
	
	# run function

	sp <- unique(dat_all$short_name)
	
	purrr::map(sp, sdmTMB_no_cov_func)
	
	