# 02d -

	# drop unused factor levels
	pcod_dat <- droplevels(pcod_dat)
	pollock_dat <- droplevels(pollock_dat)
	yfinsole_dat <- droplevels(yfinsole_dat)

	# make mesh
	pcod_mesh <- make_mesh(pcod_dat, xy_cols = c("X", "Y"), cutoff = 20)
	pol_mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)
	yfin_mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), cutoff = 20)

	
	# presurvey bottom temp ####
	

	# pollock ####
 		presurvey_btemp_pol <- 
			sdmTMB(	
 					log_wt ~ age_f + s(presurvey_btemp) + s(jday),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					anisotropy = TRUE,
					control = sdmTMBcontrol(nlminb_loops = 2))
 		
 		presurvey_btemp_int_pol <- 
 			sdmTMB(	
 				log_wt ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
 				data = pollock_dat,
 				mesh = pol_mesh,
 				spatial = "on",
 				time = "year",
 				spatiotemporal = "IID",
 				anisotropy = TRUE,
 				control = sdmTMBcontrol(nlminb_loops = 2))
 	
  saveRDS(presurvey_btemp_pol, 
  				file = here("./output/model output PC/presurvey_btemp_pol.rds"))
  
  saveRDS(presurvey_btemp_int_pol, 
  				file = here("./output/model output PC/presurvey_btemp_int_pol.rds"))
 
  ## pcod ####
  presurvey_btemp_pcod <- 
  	sdmTMB(	
  		log_wt ~ age_f + s(presurvey_btemp) + s(jday),
  		data = pcod_dat,
  		mesh = pcod_mesh,
  		spatial = "on",
  		time = "year",
  		spatiotemporal = "IID",
  		anisotropy = TRUE,
  		control = sdmTMBcontrol(nlminb_loops = 2))
  
  head(presurvey_btemp_pcod$tmb_data$X_ij)
  
  
  presurvey_btemp_int_pcod <- 
  	sdmTMB(	
  		log_wt ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
  		data = pcod_dat,
  		mesh = pcod_mesh,
  		spatial = "on",
  		time = "year",
  		spatiotemporal = "IID",
  		anisotropy = TRUE,
  		control = sdmTMBcontrol(nlminb_loops = 2))
  
 	
  saveRDS(presurvey_btemp_pcod, 
  				file = here("./output/model output PC/presurvey_btemp_pcod.rds"))
  
  saveRDS(presurvey_btemp_int_pcod, 
  				file = here("./output/model output PC/presurvey_btemp_int_pcod.rds"))
  
  ## yfin sole ###
  
 	presurvey_btemp_yfin <- 
		sdmTMB(	
 				log_wt ~ age_f + s(presurvey_btemp) + s(jday),
				data = yfinsole_dat,
				mesh = yfin_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				anisotropy = TRUE,
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1,
																step.min = 0.01, step.max = 1))
 		
 		presurvey_btemp_int_yfin <- sdmTMB(
			log_wt ~ age_f + s(presurvey_btemp, by = age_f) + s(jday),
			data = yfinsole_dat,
			mesh = yfin_mesh,
			spatial = "on",
			time = "year",
			spatiotemporal = "IID",
			anisotropy = TRUE,
			control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 3,
															step.min = 0.01, step.max = 1)
		)
 	
  saveRDS(presurvey_btemp_yfin, 
  				file = here("./output/model output/sdmTMB output/pol_pretemp_yfin.rds"))
  
  saveRDS(presurvey_btemp_int_yfin, 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_int_yfin.rds"))
 