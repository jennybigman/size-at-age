# try with cross validation

	plan(multisession)

	# meshes
	pcod_mesh <- make_mesh(pcod_dat, xy_cols = c("X", "Y"), n_knots = 500, type = "kmeans")
	pol_mesh <- make_mesh(pollock_dat, xy_cols = c("X", "Y"), n_knots = 500, type = "kmeans")
	yfin_mesh <- make_mesh(yfinsole_dat, xy_cols = c("X", "Y"), n_knots = 500, type = "kmeans")
	
	# dataset for pcod has too few samples in age 1 and 2 so remove
  drop_age <- c(1, 2)
  pcod_dat_trim <- pcod_dat %>% filter(age %!in% drop_age)
  pcod_dat_trim$age_f <- droplevels(pcod_dat_trim$age_f)
  
  # adjust mesh due to errors
	pcod_mesh_trim <- make_mesh(pcod_dat_trim, xy_cols = c("X", "Y"), n_knots = 500, type = "kmeans")

	# bin temp into quantiles for cross validation
	first_q <- quantile(pollock_dat$presurvey_btemp, 0)
	sec_q <- quantile(pollock_dat$presurvey_btemp, 0.25)
	third_q <- quantile(pollock_dat$presurvey_btemp, 0.5)
	fourth_q <- quantile(pollock_dat$presurvey_btemp, 0.75)
	fifth_q <- 	quantile(pollock_dat$presurvey_btemp, 1)

	pollock_dat <- pollock_dat %>%
		mutate(fold = case_when(
			between(presurvey_btemp, first_q, sec_q) ~ 1,
			between(presurvey_btemp, sec_q, third_q) ~ 2,
			between(presurvey_btemp, third_q, fourth_q) ~ 3,
			between(presurvey_btemp, fourth_q, fifth_q) ~ 4))
			
	####################################################
	# presurvey bottom temp (averaged April - June) ####
	####################################################

	#### pollock ####
	
	presurvey_btemp_pol_test <- 
			sdmTMB_cv(	
 					log_wt_std ~ age_f + s(presurvey_btemp),
					data = pollock_dat,
					mesh = pol_mesh,
        	k_folds = max(pollock_dat$fold),
          fold_ids = pollock_dat$fold,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))

	sanity(presurvey_btemp_pol_test$models) # how to do sanity
	
	presurvey_btemp_pol_cv <- readRDS( 
  				file = here("./output/model output/sdmTMB output/presurvey_btemp_pol_cv.rds"))
 
	# total log lik and total elpd
	presurvey_btemp_pol_test$elpd
	presurvey_btemp_pol_test$sum_loglik
	
	# log lik and elpd of each fold
	presurvey_btemp_pol_test$fold_loglik
	presurvey_btemp_pol_test$fold_elpd # pretty even
	
	# no st random effect
	presurvey_btemp_pol_test2 <- 
		sdmTMB_cv(	
 				log_wt_std ~ age_f + s(presurvey_btemp),
				data = pollock_dat,
				mesh = pol_mesh,
       	k_folds = max(pollock_dat$fold),
        fold_ids = pollock_dat$fold,
				spatial = "on",
				spatiotemporal = "off",
				control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))

	presurvey_btemp_pol_test2$elpd
	presurvey_btemp_pol_test2$sum_loglik
	
	# model with spatiotemporal effect way better

	# model with interaction
	
	pc <- pc_matern(range_gt = 6, sigma_lt = 1.7) # set up prior

	presurvey_btemp_pol_int_test <- 
		sdmTMB_cv(	
 				log_wt_std ~ age_f + s(presurvey_btemp, by = age_f),
				data = pollock_dat,
				mesh = pol_mesh,
       	k_folds = max(pollock_dat$fold),
        fold_ids = pollock_dat$fold,
				priors = sdmTMBpriors(matern_s = pc),
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 3))

	presurvey_btemp_pol_int_test$elpd
	presurvey_btemp_pol_int_test$sum_loglik

# compare linear and smooths