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


	# arrowtooth - pre survey boxy ####
	
	# number of years of data
	length(sort(unique(atooth_dat$year)))

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
			lfo_validation = 5,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
	
	# pcod - pre survey temp ####
	
	# number of years of data
	length(sort(unique(pcod_dat$year)))

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
			lfo_validation = 10,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	# pollock - pre survey temp ####
	
	# number of years of data
	length(sort(unique(pollock_dat$year)))

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
			lfo_validation = 10,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	## yfin - yr prior temp ####
	
	# number of years of data
	length(sort(unique(yfin_dat$year)))

	yfin_top_mod_cv <- 
		sdmTMB_cv(
			formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 3),
			data = yfin_dat,
			mesh = yfin_mesh,
			spatial = "on",
			spatiotemporal = "IID",
			time = "year",
			lfo = TRUE,
			lfo_forecast = 1,
			lfo_validation = 10,
			share_range = FALSE,
			silent = FALSE,
			parallel = TRUE,
			priors = sdmTMBpriors(
				matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
				matern_s = pc_matern(range_gt = 300, sigma_lt = 2)))
			
	# save models
			
	# file path to save models
	file_path_all <- "/output/model output/sdmTMB output/May 2024/poly models/cv mods/"
	
	write_rds(atooth_top_mod_cv, file = paste0(here(), file_path_all, "atooth_top_mod_cv.rds"))
	write_rds(pcod_top_mod_cv, file = paste0(here(), file_path_all, "pcod_top_mod_cv.rds"))
	write_rds(pollock_top_mod_cv, file = paste0(here(), file_path_all, "pollock_top_mod_cv.rds"))
	write_rds(yfin_top_mod_cv, file = paste0(here(), file_path_all, "yfin_top_mod_cv.rds"))
	
		
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
	
	
	apply_fun <- function(mod_list, dat){
		
		mods <- mod_list$models
		num_mods <- length(mods)
		preds <- purrr::map2(mods, 1:num_mods, preds_fun) %>% 
			bind_rows() 
	
		preds

	}
	
	mod_list <- list(atooth_top_mod_cv, pcod_top_mod_cv, pollock_top_mod_cv, yfin_top_mod_cv)
	
	pred_dfs <- purrr::map(mod_list, apply_fun)
	
	
	bind_fun <- function(pred_df, obs_df){
		
		ages <- obs_df %>%
			select(age, age_f, year, log_wt, short_name)
	
		num_mods <- length(unique(pred_df$mod_run))
		
		ages_rep <- replicate(num_mods, ages, simplify = FALSE) %>% bind_rows()
	
		all_df <- bind_cols(ages_rep, pred_df)
		
		all_df
		
	}
	
	fdf <- tibble(
		pred_df = pred_dfs,
		obs_df = list(atooth_dat, pcod_dat, pollock_dat, yfin_dat)
	)
	
	all_dfs <- map2(fdf$pred_df, fdf$obs_df, bind_fun)

	
	file_path_plots <- here("./output/plots/May 2024/forecast skill/")
		
	plot_fun <- function(df){
		
		pred_sum  <- df %>%
			group_by(year, age_f, mod_run, age) %>%
			summarise(mean_est = mean(mean_est))
		
		obs_sum <- df %>%
			group_by(year, age_f, age) %>%
			summarise(mean_log_wt = mean(log_wt))
	
		name <- unique(df$short_name)
	
		# plot
		p <- 
			ggplot(pred_sum, aes(x = year)) +
			#geom_ribbon(aes(ymin = low, ymax = high, fill = mod_run, alpha = 0.4)) +
			geom_line(aes(x = year, y = mean_est, group = mod_run, color = mod_run)) + 
			geom_line(data = obs_sum, aes(x = year, y = mean_log_wt), color = "black") +
			facet_wrap(~ reorder(age_f, age), scales = "free") +
			ggtitle(name) +
			theme_sleek()
		
		plot_name <- paste0(name, "_forecast_skill_May2024.png")

		ggsave(p, file = paste0(file_path_plots, plot_name))
	
	}
	
	purrr::map(all_dfs, plot_fun)	
	
	
	
	
	
	
	
