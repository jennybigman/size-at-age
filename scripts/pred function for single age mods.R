# 04 - plot predictions of models where temp/oxy more important than not having it

	# predict and plot
	
	sep_age_preds_fun <- function(mod){
	 	
		dat <- mod$data
		yrs <- unique(dat$year)
		age <- unique(dat$age)
		
		max_temp <- max(pcod_dat$presurvey_btemp)
		min_temp <- min(pcod_dat$presurvey_btemp)
		
		yr <- yrs[1]
		
		presurvey_btemp = seq(from = min_temp, to = max_temp, length.out = 200)
		
		nd <- expand_grid(
			presurvey_btemp = presurvey_btemp,
			year = yr,
			age = age) 

		psim <- predict(mod, newdata = nd, re_form = NA, nsim = 200)

		m <- apply(psim, 1, mean)
		se <- apply(psim, 1, sd)
		u <- m + (1.96*se)
		l <- m - (1.96*se)

		out <- bind_cols(nd, m, u, l)
		
		names_out <- c("presurvey_btemp", "year", "age", "est", "upr", "lwr")
		
		names(out) <- names_out
			
		out

	}
	
	preds_sep_age <- map(age_mods, sep_age_preds_fun) %>% bind_rows()