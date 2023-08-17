# 02d - gams with sdmTMB

 sdmTMB_func <- function(x){
 	
 	fit <- sdmTMB(	
 		log_wt_std ~ age_f + s(presurvey_btemp_std) + s(jday_std),
		data = dat,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		anisotropy = TRUE
 	)
 	
 	fit_int <- sdmTMB(
		log_wt_std ~ age_f + s(presurvey_btemp_std, by = age_f) + s(jday_std),
		data = x,
		mesh = mesh,
		spatial = "on",
		time = "year",
		spatiotemporal = "IID",
		anisotropy = TRUE,
		control = sdmTMBcontrol(nlminb_loops = 5, newton_loops = 5)
	)
 	
 }
 
 mod_list <- lapply(dat_list, sdmTMB_func)