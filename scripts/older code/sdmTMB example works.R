# sdmTMB function - with CV, knots specified, and year as RE

	lat_quantiles <- function(df){
		
		first_q <- quantile(df$latitude, 0)
		sec_q <- quantile(df$latitude, 0.25)
		third_q <- quantile(df$latitude, 0.5)
		fourth_q <- quantile(df$latitude, 0.75)
		fifth_q <- 	quantile(df$latitude, 1)

		df <- df %>%
			mutate(fold = case_when(
				between(latitude, first_q, sec_q) ~ 1,
				between(latitude, sec_q, third_q) ~ 2,
				between(latitude, third_q, fourth_q) ~ 3,
				between(latitude, fourth_q, fifth_q) ~ 4))
	}
	
	dat_list <- list(pollock_dat, pcod_dat_trim, yfinsole_dat)
	
	dat_all <- lapply(dat_list, lat_quantiles) 
	
	pollock_dat <- dat_all[[1]]

	# order age class
  levels_to_ord <- sort(unique(pollock_dat$age_f))
  pollock_dat$age_f_ord <- ordered(pollock_dat$age_f, levels = c(levels_to_ord))	
  				
  # drop unused factor levels of year (error when do this outside function)
  pollock_dat$year_f <- droplevels(pollock_dat$year_f)
  				
	# make mesh
	mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), cutoff = 20)
				
	# set up formulas
	form1 <- paste0("log_wt_std ~ s(" , "presurvey_btemp", ", k = 4) + (1|year_f)")
			
 	# model without interaction 
	mod_cv <- 
			try(sdmTMB_cv(
					formula = as.formula(form1),
					data = pollock_dat,
					mesh = mesh,
					spatial = "on",
					spatiotemporal = "off",
					k_folds = max(pollock_dat$fold),
        	fold_ids = pollock_dat$fold,
					control = sdmTMBcontrol(nlminb_loops = 3)))
	
	sanity(mod_cv$models[[1]], gradient_thresh = 0.05)
	