# assess forcast skill of climate-population relationships (top models)

	#######################################################################
	## set up data and meshes ####
	#######################################################################

	# pollock 
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
	
	# pcod 
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
	
	# yellowfin 
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


	#######################################################################
	## run models without temp/oxygen using LFO CV 1-yr moving windows ####
	#######################################################################
	
	future::plan(multisession)
	
	# atooth #
	
	# find the number of years of data
	atooth_len <- length(sort(unique(atooth_dat$year)))
	forecast_yrs <- atooth_len - 10

	# model
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
	
	
	# pcod #
	
	# number of years of data
	pcod_len <- length(sort(unique(pcod_dat$year)))
	forecast_yrs <- pcod_len - 10

	# model
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
	
	# pollock #
	
	# number of years of data
	pol_len <- length(sort(unique(pollock_dat$year)))
	forecast_yrs <- pol_len - 10

	# model
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
	
	# yfin #
	
	# number of years of data
	yfin_len <- length(sort(unique(yfin_dat$year)))
	forecast_yrs <- yfin_len - 10

	# model
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
	
		
	atooth_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, "atooth_no_cov_cv.rds"))
	pcod_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, "pcod_no_cov_cv.rds"))
	pol_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, "pol_no_cov_cv.rds"))
	yfin_no_cov_cv <- read_rds(file = paste0(here(), file_path_all, "yfin_no_cov_cv.rds"))

	
	#######################################################################
	## run models without temp/oxygen using LFO CV 1-yr moving windows ####
	#######################################################################

	# arrowtooth - pre survey boxy was the predictor with the most support #
	
	# number of years of data
	atooth_len <- length(sort(unique(atooth_dat$year)))
	forecast_yrs <- atooth_len - 10

	# model
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
	
	# pcod - pre survey temp #
	
	# number of years of data
	pcod_len <- length(sort(unique(pcod_dat$year)))
	forecast_yrs <- pcod_len - 10
	
	# model
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
			
	
	# pollock - pre survey temp #
	
	# number of years of data
	pol_len <- length(sort(unique(pollock_dat$year)))
	forecast_yrs <- pol_len - 10

	# model
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
			
	# yfin - yr prior temp #
	
	# number of years of data
	yfin_len <- length(sort(unique(yfin_dat$year)))
	forecast_yrs <- yfin_len - 10

	# model
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

	#######################################################################
	## generate predictions from these models ####
	#######################################################################

	# prediction function
	preds_fun <- function(mod, x){
		
		nsims = 100
		
		preds <- predict(mod, se_fit = FALSE, nsim = nsims) #feed prediction grid for each species
	
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
	
	#write_csv(pred_df_no_cov, file = here("./data/pred_sims_no_cov.csv"))
	
	#pred_df_no_cov <- read.csv(file = here("./data/pred_sims_no_cov.csv"))
	
	# for most supported models ####
	mod_list <- list(atooth_top_mod_cv, pcod_top_mod_cv, pollock_top_mod_cv, yfin_top_mod_cv)
	
	species <- unique(dat_all$short_name)
	
	pred_dfs <- purrr::map2(mod_list, species, apply_fun)
	
	pred_df <- pred_dfs %>% bind_rows()
	
  #write_csv(pred_df, file = here("./data/pred_sims.csv"))
	
	#pred_df <- read_csv(file = here("./data/pred_sims.csv"))
	
	
	# which years forecasting for?
		
	yrs_fun <- function(sp){

		new_dat <- dat_all %>% filter(short_name == sp)

		len <- length(unique(new_dat$year))
		yrs <- sort(unique(new_dat$year))
		forecast_n_yrs <- len - 10
		forecast_yrs <- tail(yrs, forecast_n_yrs)
	
		df <- tibble(forecast_yrs, short_name = sp)
		
		df
	
	}
	
	sp <- unique(dat_all$short_name)
	
	forecast_yrs_dfs <- purrr::map(sp, yrs_fun) %>% bind_rows()
	
	
	# create dfs of predicted 
	forecast_df_fun <- function(sp){

		dat <- dat_all %>% filter(short_name == sp)
	
		f_yrs <- forecast_yrs_dfs %>% filter(short_name == sp) %>%
			rename(year = forecast_yrs)
	
		obs <- dat %>%
			select(age, log_wt, year, age_f)
		
		obs_rep <- replicate(length(f_yrs$year), obs, simplify = FALSE) %>% bind_rows()

		preds <- pred_df %>% filter(short_name == sp)
		
		preds_no_cov <- pred_df_no_cov %>% filter(short_name == sp) %>%
			rename(mean_est_nocov = mean_est,
						 mean_est_se_nocov = mean_se) %>%
			select(-mean_sd, -mod_run, -short_name)

	preds_all <- bind_cols(preds, preds_no_cov, obs_rep)
	
	preds_all_trim <- preds_all %>% filter(year %in% f_yrs$year)
	
	}
	
	sp <- unique(dat_all$short_name)
	sp <- sp[sp != "atooth"]
	
	forecast_dfs <- purrr::map(sp, forecast_df_fun)
	
	#######################################################################
	## plot forecast skill for the 3 species with longer time series ####
	#######################################################################

	# set up observations
	
	# add observations going back in time
	
	obs_dat_fun <- function(sp){
		
		dat <- dat_all %>% filter(short_name == sp)

		dat_sum <- dat %>%
			group_by(age, age_f, year, year_f) %>%
			summarise_at(vars(log_wt),list(mean = ~mean(.), se = ~sd(./sqrt(.))))
		
		f_yrs <- forecast_yrs_dfs %>% filter(short_name == sp) %>%
			rename(year = forecast_yrs)
		
		f_yrs <- f_yrs$year

		dat_sum <- dat_sum %>% filter(year %!in% f_yrs)	
		
		dat_sum$age_label <- paste0("Age ", dat_sum$age)

		dat_sum$type_name <- "Observed"
		
		dat_sum$short_name <- sp
		
		dat_sum
		
	}
	
	sp <- unique(dat_all$short_name)[-1]
	
	obs_df <- map(sp, obs_dat_fun) %>% bind_rows()
		
		
	# function to concatenate predicted values and plot
	
	
	plot_fun <- function(df){
		
		# assign species name to object
		sp <- unique(df$short_name)

		# set up prediction df
		all_preds_sum <- df %>% 
			select(-mean_sd, mean_se, mean_est_se_nocov) %>%
			group_by(age_f, year) %>%
			 summarise_at(vars(mean_est, mean_est_nocov, log_wt),list(mean = ~mean(.), se = ~sd(./sqrt(.))))
			
		all_preds_sum <- all_preds_sum %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
		all_preds_sum <- all_preds_sum %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
		
		all_preds_sum$age_label <- paste0("Age ", all_preds_sum$age_f)
		
		all_preds_waa <- all_preds_sum %>%
			select(-contains("se")) %>%
			rename(mean_est = mean_est_mean,
						 mean_est_nocov = mean_est_nocov_mean,
						 mean_log_wt = log_wt_mean)
		
		all_preds_waa <- all_preds_waa %>%
			pivot_longer(
				cols = contains("mean"),
				names_to = "type",
				values_to = "waa_value"
			) 
		
		all_preds_waa_se <- all_preds_sum %>%
			select(age_f, year, age, age_label, contains("_se"))
	
		all_preds_waa_se <- all_preds_waa_se %>%
			pivot_longer(
				cols = contains("_se"),
				names_to = "type",
				values_to = "waa_se"
			) %>%
			mutate(type = case_when(
				type == "mean_est_se" ~ "mean_est",
				type == "mean_est_nocov_se" ~ "mean_est_nocov",
				type == "log_wt_se" ~ "mean_log_wt"
			))
		
		all_preds_waa <- left_join(all_preds_waa, all_preds_waa_se)
		
		all_preds_waa <- all_preds_waa %>%
			mutate(type_name = case_when(
				type == "mean_est" ~ "Predicted with temperature",
				type == "mean_est_nocov" ~ "Predicted without temperature",
				type == "mean_log_wt" ~ "Observed"
			))
		
		all_preds_waa$type_name <- as.factor(all_preds_waa$type_name)
		
		# assign colors
		colors <- c("#1E88E5", "#FFC107", "#004D40")
		
		names(colors) <- levels(all_preds_waa$type_name)
		
		# add observations going back in time
		obs_dat <- obs_df %>% filter(short_name == sp)

		# plot
		pcod_plot <- 
			ggplot() +
			geom_point(data = all_preds_waa, 
								 aes(x = year, y = waa_value, color = type_name, group = type_name), 
								 alpha = 0.5) +
			geom_linerange(data = all_preds_waa, 
										 aes(x = year, ymin = waa_value - waa_se, ymax = waa_value + waa_se,
										 		group = type_name, color = type_name), 
												alpha = 0.5) +
				geom_point(data = obs_dat, 
								 aes(x = year, y = mean), color = "#1E88E5", 
								 alpha = 0.5) +
			geom_linerange(data = obs_dat, 
										 aes(x = year, ymin = mean - se, ymax = mean + se),
										 		color = "#1E88E5", 
												alpha = 0.5) +
	 		facet_wrap(~ fct_reorder(age_label, age), scales = "free_y") +
			scale_color_manual(name = "type_name", values = colors) +
			ylab("Mean (log) weight (g)") +
	 		xlab("Year") +
			theme_sleek() +
			theme(
				legend.title = element_blank(),
				legend.text = element_text(size = 8, face = "bold"),
				legend.position = "right"
			) +
			ggtitle(sp) 
		
		
		plot_name <- paste0(sp, "_forecast_skill.png")

		ggsave(here("output", "plots", "May 2024", "forecast skill", plot_name), 
	 	 	height = 6, width = 10)

		
	}

		map(forecast_dfs, plot_fun)
	
	
	#######################################################################
	## plot atooth separately because only 1 year of forecast ####
	#######################################################################
	
	# create dfs of predicted 
	
		atooth_dat <- dat_all %>% filter(short_name == "atooth")
	
		f_yrs <- forecast_yrs_dfs %>% filter(short_name == "atooth") %>%
			rename(year = forecast_yrs)
	
		atooth_obs <- atooth_dat %>%
			select(age, log_wt, year, age_f)
	
		atooth_obs_rep <- replicate(length(f_yrs$year), atooth_obs, simplify = FALSE) %>% bind_rows()

		atooth_preds <- pred_df %>% filter(short_name == "atooth")
		
		atooth_preds_no_cov <- pred_df_no_cov %>% filter(short_name == "atooth") %>%
			rename(mean_est_nocov = mean_est,
						 mean_est_se_nocov = mean_se) %>%
			select(-mean_sd, -mod_run, -short_name)

	atooth_preds_all <- bind_cols(atooth_preds, atooth_preds_no_cov, atooth_obs_rep)
	
	atooth_preds_all_trim <- atooth_preds_all %>% filter(year %in% f_yrs$year)
	
	# plot
	
	all_preds_sum <- atooth_preds_all_trim %>% 
			group_by(age_f, year) %>%
			summarise(mean_est = mean(mean_est),
								mean_se = mean(mean_se),
								mean_est_nocov = mean(mean_est_nocov),
								mean_est_se_nocov = mean(mean_est_se_nocov),
								mean_log_wt = mean(log_wt),
								mean_obs_se = (sd(log_wt))/(sqrt(length(log_wt))))
		
		all_preds_sum <- all_preds_sum %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
		all_preds_sum <- all_preds_sum %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
		
		all_preds_sum$age_label <- paste0("Age ", all_preds_sum$age_f)
		
		all_preds_waa <- all_preds_sum %>%
			select(-contains("se"))
		
		all_preds_waa <- all_preds_waa %>%
			pivot_longer(
				cols = contains("mean"),
				names_to = "type",
				values_to = "waa_value"
			) 
		
		all_preds_waa_se <- all_preds_sum %>%
			select(age_f, year, age, age_label, contains("_se"))
	
		all_preds_waa_se <- all_preds_waa_se %>%
			pivot_longer(
				cols = contains("_se"),
				names_to = "type",
				values_to = "waa_se"
			) %>%
			mutate(type = case_when(
				type == "mean_se" ~ "mean_est",
				type == "mean_est_se_nocov" ~ "mean_est_nocov",
				type == "mean_obs_se" ~ "mean_log_wt"
			))
		
		all_preds_waa <- left_join(all_preds_waa, all_preds_waa_se)
		
		all_preds_waa <- all_preds_waa %>%
			mutate(type_name = case_when(
				type == "mean_est" ~ "Predicted with temperature",
				type == "mean_est_nocov" ~ "Predicted without temperature",
				type == "mean_log_wt" ~ "Observed"
			))
		
		all_preds_waa$type_name <- as.factor(all_preds_waa$type_name)
		
		colors <- c("#1E88E5", "#FFC107", "#004D40")
		
		names(colors) <- levels(all_preds_waa$type_name)
		
		atooth_dat_sum <- atooth_dat %>%
			group_by(year, year_f, age, age_f) %>%
			summarise(mean_log_wt = mean(log_wt),
								mean_log_wt_se = sqrt(sd(log_wt))/length(atooth_dat_sum))
		
	 atooth_dat_sum$age_label <- paste0("Age ", atooth_dat_sum$age_f)

		
		p <- 
			ggplot(data = all_preds_waa, aes(x = year)) +
			geom_point(data = atooth_dat_sum, aes(x = year, y = mean_log_wt), 
								 alpha = 0.5, color = "#1E88E5") +
			geom_linerange(data = atooth_dat_sum, 
										 aes(x = year, 
										 		ymin = mean_log_wt - mean_log_wt_se, ymax = mean_log_wt + mean_log_wt_se), 
										 alpha = 0.5, color = "#1E88E5") +
			geom_point(aes(x = year, y = waa_value, group = type_name, color = type_name), 
								 alpha = 0.5) +
			geom_linerange(aes(ymin = waa_value - waa_se, ymax = waa_value + waa_se,
												 group = type_name, color = type_name), 
										 group = "type", alpha = 0.5) +
	 		facet_wrap(~ fct_reorder(age_label, age), scales = "free_y") +
			scale_color_manual(name = "type_name", values = colors) +
			ylab("Mean (log) weight (g)") +
	 		xlab("Year") +
			theme_sleek() +
			#guides(color = guide_stringlegend()) +
			theme(
				legend.title = element_blank(),
				legend.text = element_text(size = 8, face = "bold"),
				legend.position = "bottom"
			) 
	
		ggsave(filename = here("output", "plots", "May 2024", "forecast skill", "atooth.png"), 
	 	 	height = 6, width = 10)
	
		
		# plot without obs
		
			p <- 
			ggplot(data = all_preds_waa, aes(x = year)) +
			geom_point(aes(x = year, y = waa_value, group = type_name, color = type_name), 
								 alpha = 0.5) +
			geom_linerange(aes(ymin = waa_value - waa_se, ymax = waa_value + waa_se,
												 group = type_name, color = type_name), 
										 group = "type", alpha = 0.5) +
	 		facet_wrap(~ fct_reorder(age_label, age), scales = "free_y") +
			scale_color_manual(name = "type_name", values = colors) +
			ylab("Mean (log) weight (g)") +
	 		xlab("Year") +
			theme_sleek() +
			scale_x_continuous(
				breaks = 2021,
				labels = 2021
			) +
			#guides(color = guide_stringlegend()) +
			theme(
				legend.title = element_blank(),
				legend.text = element_text(size = 8, face = "bold"),
				legend.position = "bottom"
			) 
	
		ggsave(filename = here("output", "plots", "May 2024", "forecast skill", "atooth_2.png"), 
	 	 	height = 6, width = 10)
	
		
	#######################################################################
	## customize axis scales ####
	#######################################################################

	pcod_scales <- list(
		scale_y_continuous(
			breaks = c(1.6, 1.8)), 
		scale_y_continuous(
			breaks = c(2.5, 2.7)), 
		scale_y_continuous(
			breaks = c(2.9, 3.1)),
		scale_y_continuous(
			breaks = c(3.15, 3.25)),
		scale_y_continuous(
			breaks = c(3.4, 3.5)),
		scale_y_continuous(
			breaks = c(3.55, 3.65)),
		scale_y_continuous(
			breaks = c(3.65, 3.75)),
		scale_y_continuous(
			breaks = c(3.75, 3.85)),
		scale_y_continuous(
			breaks = c(3.8, 3.9)),
		scale_y_continuous(
			breaks = c(3.9, 4.0))
)
	
	pcod_plot2 <- pcod_plot1 + 
		facetted_pos_scales(y = pcod_scales) +
		geom_text(data = label_df, label = label_df$lab, size = 2) 

	 
	ggsave(here("output", "plots", "May 2024", "forecast skill", paste0("pcod_custom_forecast", ".png")), 
	 	 	height = 6, width = 12)

	#######################################################################
	## correlation coefficient ####
	#######################################################################

	preds_all <- forecast_dfs %>% bind_rows()

	corr_dfs <- preds_all %>%
		group_by(short_name, age_f, year) %>%
		group_split()

	# correlation between observed and model-predicted with temp
	cor_test_mt_fun <- function(df){
		
		cor <- cor.test(df$log_wt, df$mean_est, method = 'spearman')
		
		rho <- try(cor$estimate)
		
		sp <- unique(df$short_name)
		
		new_dat <- tibble(
			"year" = df$year, 
			"age_f" = df$age,
			"rho_mt" = rho,
			"short_name" = sp) %>%
			distinct_all()
		
		new_dat
	}
	
	cor_mt_dfs <- map(corr_dfs, cor_test_mt_fun) %>% 
		bind_rows() 
	
	# correlation between observed and model-predicted without temp
	cor_test_m_fun <- function(df){
		
		cor <- cor.test(df$log_wt, df$mean_est_nocov, method = 'spearman')
		
		rho <- try(cor$estimate)
		
		sp <- unique(df$short_name)
		
		new_dat <- tibble(
			"year" = df$year, 
			"age_f" = df$age,
			"rho_m" = rho,
			"short_name" = sp) %>%
			distinct_all()
		
		new_dat
	}
	
	cor_m_dfs <- map(corr_dfs, cor_test_m_fun) %>% 
		bind_rows() 

	cor_test_df <- left_join(cor_mt_dfs, cor_m_dfs)	
	
	cor_test_df <- cor_test_df %>%
		pivot_longer(
			cols = contains("rho"),
			names_to = "type",
			values_to = "value") 
	


	# plot
	
	cor_plot_fun <- function(sp){
		
	sp_cor_df <- cor_test_df %>%
		filter(short_name == sp)
	
	sp_cor_df <- sp_cor_df %>%
		mutate(type_name = case_when(
			type == "rho_m" ~ "rho: model without temperature",
			type == "rho_mt" ~ "rho: model with temperature",
		))
	
	sp_cor_df$type_name <- as.factor(sp_cor_df$type_name)
	
	colors <- c("#5ca8a3", "#5e718b")
	
	names(colors) <- levels(sp_cor_df$type_name)
	
	sp_cor_df$age_f <- as.factor(sp_cor_df$age_f)

	sp_cor_df <- sp_cor_df %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
	sp_cor_df <- sp_cor_df %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
	
	sp_cor_df$age_label <- paste0("Age ", sp_cor_df$age_f)

	p <- 
	 ggplot(sp_cor_df) +
		geom_point(aes(x = year, y = value, group = type_name, color = type_name), alpha = 0.5) +
		facet_wrap(~ fct_reorder(age_label, age)) +
		scale_y_continuous(
			breaks = c(-0.2, 0.2, 0.6),
			labels = c(-0.2, 0.2, 0.6)
		) +
	 	ylab("Rho") +
		scale_color_manual(values = colors) +
		theme_sleek() +
		theme(
			legend.title = element_blank(),
			legend.position = "bottom"
		)
	
		ggsave(here("output", "plots", "May 2024", "forecast skill", paste0(sp, "rho_plot.png")), 
	 	 	height = 6, width = 12)
		
	}
	
	sp <- unique(dat_all$short_name)[-1]

	map(sp, cor_plot_fun)
	
	### for atooth ###
	atooth_dat <- dat_all %>% filter(short_name == "atooth")
	
	f_yrs <- forecast_yrs_dfs %>% filter(short_name == "atooth") %>%
			rename(year = forecast_yrs)
	
	atooth_obs <- atooth_dat %>%
			select(age, log_wt, year, age_f)
	
	atooth_obs_rep <- replicate(length(f_yrs$year), atooth_obs, simplify = FALSE) %>% bind_rows()

	atooth_preds <- pred_df %>% filter(short_name == "atooth")
		
	atooth_preds_no_cov <- pred_df_no_cov %>% filter(short_name == "atooth") %>%
			rename(mean_est_nocov = mean_est,
						 mean_est_se_nocov = mean_se) %>%
			select(-mean_sd, -mod_run, -short_name)

	atooth_preds_all <- bind_cols(atooth_preds, atooth_preds_no_cov, atooth_obs_rep)
	
	atooth_preds_all_trim <- atooth_preds_all %>% filter(year %in% f_yrs$year)
	

	corr_dfs_atooth <- atooth_preds_all_trim %>%
		group_by(short_name, age_f, year) %>%
		group_split()

	# correlation between observed and model-predicted with temp
	cor_test_mt_fun <- function(df){
		
		cor <- cor.test(df$log_wt, df$mean_est, method = 'spearman')
		
		rho <- try(cor$estimate)
		
		sp <- unique(df$short_name)
		
		new_dat <- tibble(
			"year" = df$year, 
			"age_f" = df$age,
			"rho_mt" = rho,
			"short_name" = sp) %>%
			distinct_all()
		
		new_dat
	}
	
	cor_mt_dfs_atooth <- map(corr_dfs_atooth, cor_test_mt_fun) %>% 
		bind_rows() 
	
	# correlation between observed and model-predicted without temp
	cor_test_m_fun <- function(df){
		
		cor <- cor.test(df$log_wt, df$mean_est_nocov, method = 'spearman')
		
		rho <- try(cor$estimate)
		
		sp <- unique(df$short_name)
		
		new_dat <- tibble(
			"year" = df$year, 
			"age_f" = df$age,
			"rho_m" = rho,
			"short_name" = sp) %>%
			distinct_all()
		
		new_dat
	}
	
	cor_m_dfs_atooth <- map(corr_dfs_atooth, cor_test_m_fun) %>% 
		bind_rows() 

	cor_test_df_atooth <- left_join(cor_mt_dfs_atooth, cor_m_dfs_atooth)	
	
	cor_test_df_atooth <- cor_test_df_atooth %>%
		pivot_longer(
			cols = contains("rho"),
			names_to = "type",
			values_to = "value") 
	


	# plot
	
	cor_plot_fun <- function(sp){
		
	
	sp_cor_df <- cor_test_df_atooth %>%
		mutate(type_name = case_when(
			type == "rho_m" ~ "rho: model without temperature",
			type == "rho_mt" ~ "rho: model with temperature",
		))
	
	sp_cor_df$type_name <- as.factor(sp_cor_df$type_name)
	
	colors <- c("#5ca8a3", "#5e718b")
	
	names(colors) <- levels(sp_cor_df$type_name)
	
	sp_cor_df$age_f <- as.factor(sp_cor_df$age_f)

	sp_cor_df <- sp_cor_df %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
	sp_cor_df <- sp_cor_df %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
	
	sp_cor_df$age_label <- paste0("Age ", sp_cor_df$age_f)

	p <- 
	 ggplot(sp_cor_df) +
		geom_point(aes(x = as.factor(year), y = value, group = type_name, color = type_name), alpha = 0.5) +
		facet_wrap(~ fct_reorder(age_label, age)) +
		#scale_y_continuous(
		#	breaks = c(-0.2, 0.2, 0.6),
		#	labels = c(-0.2, 0.2, 0.6),
		#) +
	 	ylab("Rho") +
		xlab("Year") +
		scale_color_manual(values = colors) +
		theme_sleek() +
		theme(
			legend.title = element_blank(),
			legend.position = "bottom"
		)
	
		ggsave(here("output", "plots", "May 2024", "forecast skill", "atooth_rho_plot.png"), 
	 	 	height = 6, width = 12)
		

	