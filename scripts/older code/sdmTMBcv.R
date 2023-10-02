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
	
	dat_list <- list(pollock_dat, pcod_dat, yfinsole_dat)
	
	dat_all <- lapply(dat_list, lat_quantiles) %>% bind_rows()
			
	####################################################
	# presurvey bottom temp (averaged April - June) ####
	####################################################

	#### pollock ####
	
	presurvey_btemp_pol_test_cv <- 
			sdmTMB_cv(	
 					log_wt_std ~ age_f + s(presurvey_btemp),
					data = pollock_dat,
					mesh = pol_mesh,
					spatial = "on",
					time = "year",
					spatiotemporal = "IID",
					control = sdmTMBcontrol(nlminb_loops = 1, newton_loops = 1))

	# total log lik and total elpd
	presurvey_btemp_pol_test$elpd
	presurvey_btemp_pol_test$sum_loglik
	
	# log lik and elpd of each fold
	presurvey_btemp_pol_test$fold_loglik
	presurvey_btemp_pol_test$fold_elpd # pretty even
	
	# no cv for residual checking and sanity checks
	presurvey_btemp_pol_test <- 
		sdmTMB(	
 				log_wt_std ~ age_f + s(presurvey_btemp),
				data = pollock_dat,
				mesh = pol_mesh,
				spatial = "on",
				time = "year",
				spatiotemporal = "IID",
				control = sdmTMBcontrol(nlminb_loops = 3, newton_loops = 1))

	sanity(presurvey_btemp_pol_test) # how to do sanity
	
	# check residuals
	sims <- simulate(presurvey_btemp_pol_test, nsim = 250)
	dharma_sims <- sims %>% dharma_residuals(presurvey_btemp_pol_test)

	
	
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