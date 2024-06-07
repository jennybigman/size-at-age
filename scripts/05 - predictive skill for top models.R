# run each model in sdmTMB

	future::plan(multisession)
	# set up data and meshes
	
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


	#### models with no environmental covariate - for comparing to those with temperature/oxygen ####
	
	# atooth ####
	# number of years of data
	atooth_len <- length(sort(unique(atooth_dat$year)))
	forecast_yrs <- atooth_len - 10

	atooth_no_cov_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f,
			data = atooth_dat,
			mesh = atooth_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	# pcod ####
	pcod_len <- length(sort(unique(pcod_dat$year)))
	forecast_yrs <- pcod_len - 10

	pcod_no_cov_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f,
			data = pcod_dat,
			mesh = pcod_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	# pollock ####
	
	pol_len <- length(sort(unique(pollock_dat$year)))
	forecast_yrs <- pol_len - 10

	pol_no_cov_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f,
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	# yfin ####
	yfin_len <- length(sort(unique(yfin_dat$year)))
	forecast_yrs <- yfin_len - 10

	yfin_no_cov_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f,
			data = yfin_dat,
			mesh = yfin_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	# save models
	
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/May 2024/no covariate models/cv mods/"
	
	#write_rds(atooth_no_cov_cv, file = paste0(here(), file_path_all,  "atooth_no_cov_cv.rds"))
	#write_rds(pcod_no_cov_cv, file = paste0(here(), file_path_all,    "pcod_no_cov_cv.rds"))
	#write_rds(pol_no_cov_cv, file = paste0(here(), file_path_all, "pol_no_cov_cv.rds"))
	#write_rds(yfin_no_cov_cv, file = paste0(here(), file_path_all,    "yfin_no_cov_cv.rds"))
	
		
	atooth_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, " atooth_no_cov_cv.rds"))
	pcod_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, "   pcod_no_cov_cv.rds"))
	pol_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, "pol_no_cov_cv.rds"))
	yfin_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, "   yfin_no_cov_cv.rds"))

	#### most supported models for each species ####
	
	# arrowtooth - pre survey boxy ####
	
	# number of years of data
	atooth_len <- length(sort(unique(atooth_dat$year)))
	forecast_yrs <- atooth_len - 10

	atooth_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(presurvey_boxy, 3),
			data = atooth_dat,
			mesh = atooth_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	# pcod - pre survey temp ####
	
	# number of years of data
	pcod_len <- length(sort(unique(pcod_dat$year)))
	forecast_yrs <- pcod_len - 10
	
	pcod_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(presurvey_btemp, 3),
			data = pcod_dat,
			mesh = pcod_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	# pollock - pre survey temp ####
	
	# number of years of data
	pol_len <- length(sort(unique(pollock_dat$year)))
	forecast_yrs <- pol_len - 10

	pollock_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(presurvey_btemp, 3),
			data = pollock_dat,
			mesh = pol_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	## yfin - yr prior temp ####
	
	# number of years of data
	yfin_len <- length(sort(unique(yfin_dat$year)))
	forecast_yrs <- yfin_len - 10

	yfin_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 3),
			data = yfin_dat,
			mesh = yfin_mesh,
			spatial = "on",
			spatiotemporal = "IID", #AR1
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = forecast_yrs,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	# save models
			
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/May 2024/poly models/cv mods/"
	
	#write_rds(atooth_top_mod_cv, file = paste0(here(), file_path_all, "atooth_top_mod_cv.rds"))
	#write_rds(pcod_top_mod_cv, file = paste0(here(), file_path_all, "pcod_top_mod_cv.rds"))
	#write_rds(pollock_top_mod_cv, file = paste0(here(), file_path_all, "pollock_top_mod_cv.rds"))
	#write_rds(yfin_top_mod_cv, file = paste0(here(), file_path_all, "yfin_top_mod_cv.rds"))
	
		
	atooth_top_mod_cv <- read_rds(file = paste0(here(), file_path_all, "atooth_top_mod_cv.rds"))
	pcod_top_mod_cv <- read_rds(file = paste0(here(), file_path_all, "pcod_top_mod_cv.rds"))
	pollock_top_mod_cv <- read_rds(file = paste0(here(), file_path_all, "pollock_top_mod_cv.rds"))
	yfin_top_mod_cv <- read_rds(file = paste0(here(), file_path_all, "yfin_top_mod_cv.rds"))

	
	# predictions generated from these models
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
						 
		preds_sum$mod_run <- as.factor(x)
		
		preds_sum
		
	}
	
	
	apply_fun <- function(mod_list, sp){
		
		mods <- mod_list$models
		num_mods <- length(mods)
		preds <- purrr::map2(mods, 1:num_mods, preds_fun) %>% 
			bind_rows() 
		
		preds$short_name <- sp
	
		preds

	}
	
	# for models with no covariate 
	mod_list_no_cov <- list(atooth_no_cov_cv, pcod_no_cov_cv, pol_no_cov_cv, yfin_no_cov_cv)
	
	species <- unique(dat_all$short_name)
	
	pred_dfs_no_cov <- purrr::map2(mod_list_no_cov, species, apply_fun)
	
	pred_df_no_cov <- pred_dfs_no_cov %>% bind_rows()
	
	write_csv(pred_df_no_cov, file = here("./data/pred_sims_no_cov.csv"))
	
	# for most supported models ####
	mod_list <- list(atooth_top_mod_cv, pcod_top_mod_cv, pollock_top_mod_cv, yfin_top_mod_cv)
	
	species <- unique(dat_all$short_name)
	
	pred_dfs <- purrr::map2(mod_list, species, apply_fun)
	
	pred_df <- pred_dfs %>% bind_rows()
	
	write_csv(pred_df, file = here("./data/pred_sims.csv"))

	
	
	
	
	
	
	
	
	
	
	
	
	

	
	#### PLOTS ####
	
	# pcod ####
	
	# what years are forecasted
	pcod_yrs <- pluck(forecast_yrs_dfs, 1) %>%
		rename(year = forecast_yrs)
	
	pcod_obs <- pcod_dat %>%
		select(age, log_wt, year, age_f)
	
	pcod_obs_rep <- replicate(length(pcod_yrs$year), pcod_obs, simplify = FALSE) %>% bind_rows()

	pcod_preds <- pred_df %>% filter(short_name == "pcod")
	
	pcod_all <- bind_cols(pcod_preds, pcod_obs_rep)
	
	pcod_all_trim <- pcod_all %>% filter(year %in% pcod_yrs$year)

	
	sep_df_fun <- function(years, ages) {
		
		
		df <- pcod_all_trim %>%
			filter(year == years) %>%
			filter(age == ages)
		
		
	}
	
	ages <- sort(unique(pcod_all_trim$age))

	fdf <- crossing(
		years = sort(unique(pcod_all_trim$year)),
		ages = ages
	)
	
	dfs <- map2(fdf$years, fdf$ages, sep_df_fun) 
	
	# remove empty lists
 dfs_trim <- dfs[-110]
 

	# compute correlation coefficient
	cor_test_fun <- function(df){
		
		cor <- cor.test(df$log_wt, df$mean_est, method = 'spearman')
		
		rho <- try(cor$estimate)
		
		new_dat <- tibble(
			"year" = df$year, 
			"age_f" = df$age,
			"rho" = rho) %>%
			distinct_all()
		
		new_dat
	}
	
	
	cor_dfs <- map(dfs_trim, cor_test_fun) 
	
	cor_df <- cor_dfs %>% bind_rows()
	
	pcod_cor_plot <- 
		ggplot(cor_df) + 
		geom_line(aes(x = year, y = rho)) +
		facet_wrap(~ age_f, nrow = 2) +
		theme_sleek()
	
	
	all_preds_sum <- dfs_trim %>% 
		bind_rows() %>%
		group_by(age_f, year) %>%
		summarise(mean_est = mean(mean_est),
							mean_se = mean(mean_se),
							mean_log_wt = mean(log_wt),
							mean_obs_se = (sd(log_wt))/(sqrt(length(log_wt))))
	
	pcod_preds <- 
		ggplot(data = all_preds_sum, aes(x = year, y = mean_est)) +
		geom_point(aes(x = year, y = mean_est), 
							 color = "black", shape = 21, alpha = 0.5) +
		geom_linerange(aes(ymin = mean_est - mean_se, ymax = mean_est + mean_se), 
									 color = "black", alpha = 0.5) +
		geom_point(aes(x = year, y = mean_log_wt), 
							 color = "black", alpha = 0.5) +
		geom_linerange(aes(x = year, y = mean_log_wt, 
											 ymin = mean_log_wt - mean_obs_se, 
											 ymax = mean_log_wt + mean_obs_se), 
									 color = "black", alpha = 0.5) +
		facet_wrap(~ age_f, scales = "free") +
		theme_sleek() 
		
	
	
	
	
	
