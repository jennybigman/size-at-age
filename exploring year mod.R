# sdmTMB function - year as linear effect
	
	# there are some year/age combinations with few observations so need to remove those with fewer than 
	dat_all_trim <- dat_all %>%
		filter(short_name != "pcod") %>%
		filter(short_name != "atooth")

	dat_list <- dat_all_trim %>% 
		mutate(year_f = as.factor(year)) %>%
		group_by(short_name, age_f, year_f) %>%
		group_split()

	# file path
	file_path_all <- "/output/model output/sdmTMB output/May 2024/year models/"

	# function to run each model by age class
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
			sdmTMB(log_wt ~ year_f,
				data = df,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
			  time = "year",
			  silent = FALSE)
					
		s <- sanity(mod_yr, gradient_thresh = 0.05)
	
		mod_name <- "yr_mod_age_"
		
		age <- unique(df$age_f)
		
		sp <- unique(df$short_name)
		
		write_rds(mod_yr, 
			file = paste0(here(), file_path_all, mod_name, age, sp, ".rds"))
		 					
		
	}
	
	purrr::map(dat_list, sdmTMB_yr_mod_fun)
	
	##### model output #####
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/")))

	file_list <- stringr::str_subset(file_list, 'age_pol')
	
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  yr_mod_list <- lapply(mod_names_list, readRDS)
  
	
  # sanity
  
  sanity_fun <- function(mod){
  	
  	s <- sanity(mod)
  	
  }
  
  s <- purrr::map(yr_mod_list, sanity_fun)
	
	
	resid_fun <- function(mod){
		
		sims <- simulate(mod, nsim = 500, type = 'mle-mvn') 
		plot_df <- dharma_residuals(sims, mod, plot = FALSE)

	}
	
  pol_yr_resids  <- purrr::map(yr_mod_list, resid_fun) 

	# save these plots
	file_path_plots <- paste0(here(), "/output/plots/May 2024/residual plots for year models/")
				 
	ggsave_fun <- function(df, name){
		
		p <- 
				ggplot(df) +
				geom_point(aes(x = expected, y = observed)) +
				geom_abline(intercept = 0, slope = 1, color = "red") +
				ggtitle(name)
			
		ggsave(filename = paste0(file_path_plots, name, ".png"),
					 plot = p,
					 width = 5, height = 5, units = "in")
	
	}
	
	purrr::map2(pol_yr_resids, names(pol_yr_resids), ggsave_fun)

	# remove models not passing sanity checks
	
	names(pol_yr_resids)
	
	pol_mods_trim <- pol_yr_resids[c(1:6, 12:20)] # age 1 - 14
	
	names(pol_mods_trim)
	
	pol_mods_trim <- pol_mods_trim[c(1:7, 9:15)] # age 1 - 14

	###
	
	mod <- readRDS(file = here("./output/model output/sdmTMB output/May 2024/year models/yr_mod_age_pol_1.rds"))
	mod2 <- readRDS(file = here("./output/model output/sdmTMB output/May 2024/year models/yr_mod_age_pol_2.rds"))

	mods <- list(mod, mod2)
	
	test_list <- list()
	
	#return_mod_list_fun <- function(mod){
		
	for (i in yr_mod_list){
			
		s <- sanity(i)
		
		if (s$hessian_ok ==      "TRUE" &
				s$eigen_values_ok == "TRUE" &
		  	s$nlminb_ok ==       "TRUE" &
				s$range_ok ==        "TRUE" &
				s$se_na_ok ==        "TRUE" &
				s$sigmas_ok ==       "TRUE") {
	
		name <- names(yr_mod_list)

		}}

	return_mod_fun <- function(mod) {
			
		s <- sanity(mod)
		
		if (s$hessian_ok ==      "TRUE" &
				s$eigen_values_ok == "TRUE" &
		  	s$nlminb_ok ==       "TRUE" &
				s$range_ok ==        "TRUE" &
				s$se_na_ok ==        "TRUE" &
				s$sigmas_ok ==       "TRUE") {
	
		name <- names(mod)

		}}

	mod_list <- purrr::map(yr_mod_list, return_mods_fun)
		 	 	
	
	s$yr_mod_age_pol_1.rds$hessian_ok == TRU
	
	
	# predictions
	
	preds_fun <- function(mod, x){
		
		nsims = 100
		
		preds <- predict(mod, se_fit = FALSE, nsim = nsims)
	
		preds_df <- preds %>%
			as_tibble()
			
		preds_mean <- preds_df %>%
			rowwise() %>%
			summarise(mean_est = mean(c_across(1:ncol(preds))))
		
		preds_sd <- preds_df %>%
			rowwise() %>%
			summarise(mean_sd = sd(c_across(1:ncol(preds))))
		
		preds_se <- preds_sd %>%
			mutate(mean_se = mean_sd/(sqrt(nsims)))
						 
		preds_sum <- bind_cols(preds_mean, preds_se) 
		
		preds_sum
		
	}
	
 preds <-  purrr::map(pol_mods_trim, preds_fun) 
			bind_rows() 

	}
	
	
	
	#######################################
					###############
					
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
	
	file_path_all <- "/output/model output/sdmTMB output/May 2024/year models/"

	sdmTMB_yr_func <- function(sp){
		
		# wrangling and making a mesh 
		
					# filter df by species
					new_dat <- dat_all %>% filter(short_name == sp)
					
					new_dat$year_f <- as.factor(new_dat$year)
  			
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
					mod_name <- "yr_mod_"
					
		# run models	
		
					print(paste('running year model for', sp))
		
					# set up formulas
					form_yr <- paste0("log_wt ~ 0 + age_f * year_f")
		

 					# model without interaction 
					mod_yr <- 
						try(
							sdmTMB(
								formula = as.formula(form_yr),
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
					
					s <- sanity(mod_yr, gradient_thresh = 0.05)
	
	
		 						write_rds(mod_yr, 
									file = paste0(here(), file_path_all, mod_name, sp, ".rds"))
		 						
		 						print(paste("year model for", sp, "complete"))
		 	
	}
	
	# run function

	sp <- unique(dat_all$short_name)
	
	purrr::map(sp, sdmTMB_yr_func)
	
	
		
	# read files in 

	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/")))

	file_list <- stringr::str_subset(file_list, '.rds')
	
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  yr_mod_list <- lapply(mod_names_list, readRDS)
  
  yr_mod_list[[1]]
  
  
  ##############
  