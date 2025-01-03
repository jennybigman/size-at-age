


	file_path_all <- "/output/model output/sdmTMB output/year models/trend/"

	#### fit models ####
	
	a_ages <- c(2, 3, 4, 5, 6, 8)
	
	atooth_dat <- dat_all |>
		filter(short_name == "atooth" & age %in% a_ages)
	
	pc_ages <- c(1, 2, 3, 4, 5, 6, 7, 8)
	
	pcod_dat <- dat_all |>
		filter(short_name == "pcod" & age %in% pc_ages)

	pl_ages <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
	
	pol_dat <- dat_all |>
		filter(short_name == "pollock" & age %in% pl_ages)

	y_ages <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 28)
	
	yfin_dat <- dat_all |>
		filter(short_name == "yfin" & age %in% y_ages)
	
	dat_all_yr <- bind_rows(atooth_dat, pcod_dat, pol_dat, yfin_dat)

	age_split_fun <- function(sp){
		
		new_dat <- dat_all_yr %>% 
			filter(short_name == sp) %>%
			group_split(age)
		
		new_dat
	}
	
	sp <- unique(dat_all_yr$short_name)
	
	sp_age_dat_list <- purrr::map(sp, age_split_fun)
	sp_age_dat_list <- purrr::flatten(sp_age_dat_list)
	
	# not running yfin?
	#yfin_dat <- dat_all_yr |>
	#	filter(short_name == "yfin")
	#
	#yfin_list <- yfin_dat |>
	#	group_split(age, age_f)
		
	# fit model
	sdmTMB_yr_mod_fun <- function(df){
		
		sp <- unique(df$short_name)
		age <- unique(df$age)
		
		mesh <- make_mesh(
			df, 
			c("X", "Y"),
			fmesher_func = fmesher::fm_mesh_2d_inla,
				cutoff = 30,
				#max.edge = 30,
				offset = c(60, 70)
			)

		# model without interaction 
		mod_yr <- 
			sdmTMB(log_wt ~ 0 + year,
				data = df,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
			  time = "year",
			  silent = FALSE)
					
		mod_name <- "_year_trend_mod"
		
	
		write_rds(mod_yr, 
			file = paste0(here(), file_path_all, sp, "_", age, mod_name, ".rds"))
		 					
		
	}
	
	mods <- purrr::map(sp_age_dat_list, sdmTMB_yr_mod_fun)
	#y_mods <- purrr::map(yfin_list, sdmTMB_yr_mod_fun)
	
	##### read in models and remove those that do not converge ####
	mod_list <- list.files(paste0(here(), file_path_all))
	mods <- purrr::map(paste0(here(), file_path_all, mod_list), readRDS)
	mod_names <- str_remove(mod_list, ".rds")

	
 # remove those that didn't converge
	s_df <- purrr::map2_dfr(mods, mod_names, \(mod, name) {
		
		s <- sanity(mod)
		
		
			hess    <- s$hessian_ok 
			eigen   <- s$eigen_values_ok            
			nl      <- s$nlminb_ok       		         
			range 	<- s$range_ok       
			#se_na 	<- s$se_na_ok         
			sigma 	<- s$sigmas_ok        
		
	
			#df <- tibble(name, hess, eigen, nl, range, se_na, sigma)
			df <- tibble(name, hess, eigen, nl, range, sigma)

			df
		
	})

	drop <- s_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- drop$name
	
	mod_list_keep <- s_df %>% 
		filter(name %!in% drop)
	
	mod_list_keep <- mod_list_keep$name

  # add .rds
	mod_keep <- paste0(mod_list_keep, ".rds")
	
	mods <- purrr::map(paste0(here(), file_path_all, mod_keep), readRDS)

	
  #### generate predictions ####
  year_preds <- purrr::map_dfr(mods, \(mod){
  	
  	dat <- mod$data
  	sp <- unique(dat$short_name)
  	age <- unique(dat$age_f)
  	
  	p <- predict(mod, se_fit = TRUE, re_form = NA)
  	
  	p
  	
  })
  
  
  write.csv(year_preds, file = paste0(here(), "/data/year_model_preds_trend.csv"))
  
	year_preds <- read.csv(file = paste0(here(), "/data/year_model_preds_trend.csv"))

	# mean per year
	year_preds_sum <- year_preds |> 
			group_by(short_name, common_name, year, year_f, age_f, age) |> 
			summarise(
				mean_est = mean(est),
   			mean_est_se = mean(est_se),
				mean_wt = mean(log_wt))

	year_preds_sum$age_f <- as.factor(year_preds_sum$age_f)
	year_preds_sum$year_f <- as.factor(year_preds_sum$year_f)
	
	year_preds_sum$year_f <- fct_reorder(year_preds_sum$year_f, year_preds_sum$year)
	year_preds_sum$age_f <- fct_reorder(year_preds_sum$age_f, year_preds_sum$age)

	p <- 
		ggplot(year_preds_sum, aes(x = year, y = mean_est, group = age_f, color = age_f)) +
		geom_line() +
		geom_point(aes(x = year, y = mean_wt)) +
		facet_wrap(~common_name, scales = "free") +
		scale_color_viridis_d() +
		ylab("Predicted mean (log) weight (g)") +
		theme_sleek()

	
