# sdmTMB function - year as factor 
	
	# fit separate models for each age class for each species due to data-poor factor levels (age/year combos)

	###### year as factor #####
	file_path_all <- "/output/model output/sdmTMB output/year models/"

	# function to run each model by age class
	age_split_fun <- function(sp){
		
		new_dat <- dat_all %>% 
			filter(short_name == sp) %>%
			group_split(age_f)
		
		new_dat
	}
	
	sp <- unique(dat_all$short_name)
	
	sp_age_dat_list <- purrr::map(sp, age_split_fun)
	sp_age_dat_list <- purrr::flatten(sp_age_dat_list)
	
	
	# d <- dat_all %>% filter(short_name == "yfin")
	# d <- d %>% group_split(age_f)
	
	# fit model
	sdmTMB_yr_mod_fun <- function(df){
		
		sp <- unique(df$short_name)
		age <- unique(df$age_f)
		
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
			sdmTMB(log_wt ~ 0 + year_f,
				data = df,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
			  time = "year",
			  silent = FALSE)
					
		mod_name <- "_yr_mod_age_"
		
	
		write_rds(mod_yr, 
			file = paste0(here(), file_path_all, sp, mod_name, age, ".rds"))
		 					
		
	}
	
	purrr::map(d, sdmTMB_yr_mod_fun)
	
	
	mod_list <- list.files(file_path)
	mod_list <- str_subset(mod_list, "yfin")
	
	mods <- purrr::map(paste0(file_path, mod_list), readRDS)
	
	s <- purrr::map(mods, \(mod){
		
		sanity(mod)
		
	})
	
	
	#### year as continuous ####
	
	# long term trend by year

	# fit separate models for each age class for each species due to data-poor factor levels (age/year combos)

	file_path <- ("/output/model output/sdmTMB output/year models/year trend/")

	# function to run each model by age class
	age_split_fun <- function(sp){
		
		new_dat <- dat_all %>% 
			filter(short_name == sp) %>%
			group_split(age_f)
		
		new_dat
	}
	
	sp <- unique(dat_all$short_name)
	
	sp_age_dat_list <- purrr::map(sp, age_split_fun)
	sp_age_dat_list <- purrr::flatten(sp_age_dat_list)
	

	# fit model
	sdmTMB_yr_mod_fun <- function(df){
		
		sp <- unique(df$short_name)
		age <- unique(df$age_f)
		
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
			try(sdmTMB(log_wt ~ 0 + year,
				data = df,
				mesh = mesh,
				spatial = "on",
				spatiotemporal = "IID",
			  time = "year",
			  silent = FALSE))
					
		return(mod_yr)				
		
	}
	
	mods <- purrr::map(sp_age_dat_list, sdmTMB_yr_mod_fun)
	
	# remove those that didn't converge
	mods_filter <- keep(mods, ~ length(.x) > 1)
	
	num <- 1:length(mods_filter)
	
	s_df <- purrr::map2_dfr(mods_filter, num, \(mod, num) {
		
		s <- sanity(mod)
		
			hess    <- s$hessian_ok 
			eigen   <- s$eigen_values_ok            
			nl      <- s$nlminb_ok       		         
			range 	<- s$range_ok       
			#se_na 	<- s$se_na_ok         
			sigma 	<- s$sigmas_ok        
		
			df <- tibble(num, hess, eigen, nl, range, sigma)

			df
		
	})

	drop <- s_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- drop$num
	drop <- as.numeric(drop)
	
	mods_keep <- mods_filter[-drop]

	
  # predictions
  year_preds <- purrr::map_dfr(mods_keep, \(mod){
  	
  	dat <- mod$data
  	sp <- unique(dat$short_name)
  	age <- unique(dat$age_f)
  	
  	p <- predict(mod, se_fit = TRUE, re_form = NA)
  	
  	p
  	
  })
  
  
  write.csv(year_preds, file = paste0(here(), "/data/year_model_preds_trend.csv"))
  
	year_preds <- read.csv(file = paste0(here(), "/data/year_model_preds_trend.csv"))
 
	
	#### plots ####
	
	year_preds_sum <- year_preds |> 
			group_by(common_name, short_name, year, year_f, age_f, age) |> 
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
		scale_color_viridis_d(
			name = "age class"
		) +
		ylab("Predicted mean (log) weight (g)") +
		theme_sleek() 
	
	ggsave(filename = paste0(here(), "/output/plots for paper/SI_fig_year_trend.jpg"),
				 p,
				 height = 5, width = 8, units = "in")

	

	
	