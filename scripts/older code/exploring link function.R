# explore link function

	## pcod mesh ####
	pcod_dat <- dat_all %>% 
		filter(short_name == "pcod")
	
	pcod_dat$age_f <- droplevels(pcod_dat$age_f)
		
	pcod_mesh <- make_mesh(
		pcod_dat, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
			cutoff = 60,
			max.edge = 60,
			offset = c(60, 70)
		)
	
	plot(pcod_mesh)
	
	## center response ####
	pcod_dat <- pcod_dat %>%
		mutate(log_wt_c = as.vector(scale(log_wt, scale = FALSE)),
					 wt_c = as.vector(scale(weight, scale = FALSE)))

 	
	## fit model without interaction ####
	pcod_mod_no_int <- 
		sdmTMB(
			formula = log_wt_c ~ age_f + s(presurvey_btemp, k = 3),
			data = pcod_dat,
			family = student(),
			mesh = pcod_mesh,
			spatial = "on",
			spatiotemporal = "IID",
		  time = "year",
			#extra_time = 2020:2099,
		  share_range = FALSE,
		  silent = FALSE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))w
					
		sanity(pcod_mod_no_int, gradient_thresh = 0.05)
	
	## predictions ####
		
			mod_temp <- temp_mods[grep(sp, names(temp_mods))] ## change back to sp
			
			mod_temp_obj <- pluck(mod_temp, 1)
			
			mod_temp_data <- pluck(mod_temp_obj, 'data')
			
			var_form <- mod_temp_obj$formula
			
			var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
			
			temp_df <- mod_temp_data %>%
				ungroup %>%
				select(var)

			# new dat for prediction
	 		new_dat_temp_hind <- expand_grid(
				var = seq(
					from = min(temp_df[, 1]),
					to = max(temp_df[ ,1]),
					length.out = 25),
				age_f = sort(unique(mod_temp_data$age_f)),
				year = 2000) # need all years for plotting weight at age vs year

	 	new_dat_temp_hind <- new_dat_temp_hind %>%
	 		rename("{var}" := var)
	 		
	 		# predict
	 		temp_hind_preds <- predict(
	 			mod_temp_obj,
				newdata = new_dat_temp_hind,
				se_fit = TRUE,
				re_form = NA,
				return_tmb_object = FALSE)
	 		
	 		# add se estimates
	 		temp_hind_preds <- temp_hind_preds %>% 
	 			mutate(low = est - est_se,
	 						 high = est + est_se,
	 						 species = sp)
	 		