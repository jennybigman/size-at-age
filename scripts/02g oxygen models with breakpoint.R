# compare to breakpoint (threshold in oxygen
	
		presurvey_boxy_bp_pol_nj <- 
			sdmTMB(	
 					log_wt_std ~ age_f + breakpt(presurvey_boxy) + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
									time = "year",
				spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_boxy_bp_pol)
	
	# linear
	
	presurvey_boxy_lin_pol <- 
			sdmTMB(	
 					log_wt_std ~ age_f + presurvey_boxy + s(jday_std),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
									time = "year",
				spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))
	
	sanity(presurvey_boxy_lin_pol)
	
	AIC(presurvey_boxy_pol)
	AIC(presurvey_boxy_bp_pol)
	AIC(presurvey_boxy_lin_pol)
	