# 04 - predictions of weight and environmental variables (temp, oxy)

# last updated: June 24 2024
	
	###########################################################################
	#### Changes in size-at-age with year ####
	###########################################################################

	# load all year models and filter out models that did not converge
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/")))

	file_list <- stringr::str_subset(file_list, 'yr_mod_age')
	
  prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  yr_mod_list <- lapply(mod_names_list, readRDS)
  
	
  # sanity
  
  sanity_fun <- function(mod){
  	
  	s <- sanity(mod)
  	
  }
  
  s <- purrr::map(yr_mod_list, sanity_fun)

  
  return_mod_fun <- function(num) {
		
		mod <- yr_mod_list[[num]]
		
			s <- sanity(mod)
		
			hess    <- s$hessian_ok 
			eigen   <- s$eigen_values_ok            
			nl      <- s$nlminb_ok       		         
			range 	<- s$range_ok       
			se_na 	<- s$se_na_ok         
			sigma 	<- s$sigmas_ok        
		
			name <- names(yr_mod_list)[[num]]
	
			df <- tibble(name, hess, eigen, nl, range, se_na, sigma)
		
			df
		
}

	num = 1:length(yr_mod_list)

	mod_df <- lapply(num, return_mod_fun) %>% bind_rows()	 	
	
	mod_df_drop <- mod_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- mod_df_drop$name

	mod_list_keep <- mod_df %>% 
		filter(name %!in% drop)
	
	# read in models that did converge
	mod_list_keep <- mod_list_keep$name
	
	file_list <- list.files(path = paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/")))

	file_list <- intersect(mod_list_keep, file_list)
	
	prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/year models/"))

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
	yr_mod_list_trim <- lapply(mod_names_list, readRDS)
 
  # set up names
	nums <- qdapRegex::ex_between(names(yr_mod_list_trim), "yr_mod_", ".rds")
	nums <- unlist(nums)

	ages <- str_sub(nums, start= -2)
	ages <- str_remove(ages, pattern = "_")

	# predictions 
	
	preds_fun <- function(mod){
		
		preds <- predict(mod, se_fit = TRUE)
	
	}
	
	preds <- purrr::map(yr_mod_list_trim, preds_fun) %>% 
			bind_rows() 
	
	preds_df <- preds %>% bind_rows()
	
	#write.csv(preds_df, file = here("data", "preds_yr_mods.csv"))

	#preds_df <- read.csv(file = here("data", "preds_yr_mods.csv"))

	
	# year plots

	#preds_df <- read.csv(file = here("data", "preds_yr_mods.csv"))
	
	preds_df$age_f <- as.factor(preds_df$age_f)
	preds_df$year_f <- as.factor(preds_df$year_f)


	#### plots ####
	
	# make plots and save 
	
	yr_plots <- function(sp){
	
	 sp_dat <- preds_df %>%
	 	filter(short_name == sp)
	 
 	 sp_dat_sum <- sp_dat %>%
		group_by(year_f, age_f, age, year) %>%
	 	summarise(mean_est = mean(est),
	 						mean_se = mean(est_se))
	 
 	 name <- unique(sp_dat$common_name)
 	 num <- length(unique(sp_dat$age_f))/3
 	 num <- floor(num)
 	 
 	 x_pos <- unique(sp_dat_sum$year_f)[[2]]
 	 y_pos <- max(sp_dat_sum$mean_est)
 	 
	 sp_dat_sum <- sp_dat_sum %>% 
  	arrange(age) %>% 
  	mutate(age_f = fct_inorder(age_f)) 
	 
	 sp_dat_sum$age_label <- paste0("Age ", sp_dat_sum$age_f)
	 
	 sp_dat_sum$year_f <- fct_reorder(sp_dat_sum$year_f, sp_dat_sum$year)
	 
	 p <- 
	 	ggplot(sp_dat_sum) +
	 	geom_pointrange(aes(x = year_f, y = mean_est,
	 											ymin = mean_est - mean_se,
	 											ymax = mean_est + mean_se),
	 									size = 0.05) +
	 	facet_wrap(~ fct_reorder(age_label, age), scales = "free_y", ncol = 4) +
	 	ylab("Predicted mean (log) weight (g)") +
	 	xlab("Year") +
	 	scale_x_discrete(guide = guide_axis(angle = 90)) +
		ggtitle(name) +
	 	theme_sleek() +
	 	theme(
	 		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 6),
	 		strip.text = element_text(size = 8),
	 		panel.spacing.y = unit(0, "lines")
	 	)
	 
	 ggsave(here("output", "plots", "May 2024", "year models", paste0(name, ".png")),
	 			 height = 6, width = 9)

	}
	
	sp <- unique(dat_all$short_name)
	
	purrr::map(sp, yr_plots)
	
	

	###########################################################################
	#### Changes in size-at-age with temp/oxygen ####
	###########################################################################

	
	# read in file with top model list
	top_mods <- read.csv(file = "./data/top_mods_list.csv")

	file_list <- top_mods$model
	
	prestring <- paste0(here(), ("/output/model output/sdmTMB output/May 2024/poly models/"))
	 
	mod_names_list <- list()
	 
	 for(i in file_list){
	 	mod_names_list[[i]] <- paste0(prestring, i)
	 }
	 
	mods <- lapply(mod_names_list, readRDS)
	
	temp_mods <- mods[grep("temp", names(mods))]

	oxy_mods <- mods[grep("oxy", names(mods))]
	

	# function to plot weight vs temp by age class
	 
	 weight_temp_df_func <- function(sp){
	 	
	 		mod_temp <- temp_mods[grep(sp, names(temp_mods))] 
			
			mod_temp_obj <- pluck(mod_temp, 1)
			
			mod_temp_data <- pluck(mod_temp_obj, 'data')
			
			var_form <- mod_temp_obj$formula
			
			var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
			
			temp_df <- mod_temp_data %>%
				ungroup %>%
				select(var)

			# new dat for prediction
	 		new_dat_temp_hind <- expand_grid(
				var = seq(
					from = min(temp_df[, 1]),
					to = max(temp_df[ ,1]),
					length.out = 25),
				age_f = sort(unique(mod_temp_data$age_f)),
				year = 2004) # need all years for plotting weight at age vs year

	 	new_dat_temp_hind <- new_dat_temp_hind %>%
	 		rename("{var}" := var)
	 		
	 		# predict
	 		temp_hind_preds <- predict(
	 			mod_temp_obj,
				newdata = new_dat_temp_hind,
				se_fit = TRUE,
				re_form = NA,
				return_tmb_object = FALSE)
	 		
	 		# add se estimates
	 		temp_hind_preds <- temp_hind_preds %>% 
	 			mutate(low = est - est_se,
	 						 high = est + est_se,
	 						 species = sp)
	 		
	 }

	species <- unique(dat_all$short_name) 

	temp_hind_plot_dfs <- purrr::map(species, weight_temp_df_func)

	# wrangle for plotting
	
	temp_df_wrangle_fun <- function(df){
		
		new_dat <- df %>%
			pivot_longer(
				cols = contains("temp"),
				names_to = "var",
				values_to = "value")
		
		new_dat
		
	}
				
	temp_plot_df <- purrr::map(temp_hind_plot_dfs, temp_df_wrangle_fun) %>% bind_rows()
	
	# function to plot weight vs oxygen by age class
	 
	 weight_oxy_df_func <- function(sp){
	 	
			mod_oxy <- oxy_mods[grep(sp, names(oxy_mods))]
			
			mod_oxy_obj <- pluck(mod_oxy, 1)
			
			mod_oxy_data <- pluck(mod_oxy_obj, 'data')

			var_form <- mod_oxy_obj$formula
			
			var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
			
			oxy_df <- mod_oxy_data %>%
				ungroup %>%
				select(var)

	 		# new dat for prediction
	 		new_dat_oxy_hind <- expand_grid(
				var = seq(
					from = min(oxy_df[, 1]),
					to = max(oxy_df[ ,1]),
					length.out = 25),
				age_f = sort(unique(mod_oxy_data$age_f)),
				year = 2004) # need all years for plotting weight at age vs year

	 	new_dat_oxy_hind <- new_dat_oxy_hind %>%
	 		rename("{var}" := var)
	 		
	 	# predict
	 	oxy_hind_preds <- predict(
	 			mod_oxy_obj,
				newdata = new_dat_oxy_hind,
				se_fit = TRUE,
				re_form = NA,
				return_tmb_object = FALSE)
	 		
	 		# add se estimates
	 		oxy_hind_preds <- oxy_hind_preds %>% 
	 			mutate(low = est - est_se,
	 						 high = est + est_se,
	 						 species = sp)
	 		
	}

	species <- unique(dat_all$short_name) 

	oxy_hind_plot_dfs <- purrr::map(species, weight_oxy_df_func)
	
	# wrangle
	oxy_df_wrangle_fun <- function(df){
		
		new_dat <- df %>%
			pivot_longer(
				cols = contains("oxy"),
				names_to = "var",
				values_to = "value")
		
		new_dat
		
	}
				
	oxy_plot_df <- purrr::map(oxy_hind_plot_dfs, oxy_df_wrangle_fun) %>% bind_rows()
	
	#### PLOTS #### 
	
	file_path_plots <- paste0(here("./output/plots/May 2024/"))
	
		# temp plots
	
	temp_plot_func <- function(sp){

		sp_dat <- temp_plot_df %>% filter(species == sp)
		
		sp_dat <- sp_dat %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
		sp_dat <- sp_dat %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
		
		sp_dat$age_label <- paste0("Age ", sp_dat$age_f)
	

		# plot
		plot <-
					ggplot(sp_dat, aes(value, est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ reorder(age_label, age), scales = "free_y", ) +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					ggtitle(sp) +
					theme_sleek() +
					theme(
				 		panel.spacing.y = unit(0, "lines"))

			
			plot_name <- paste0(sp, "_temp_May2024_2.png")
			
			ggsave(plot, file = paste0(file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
		plot
	}
	
	sp <- unique(dat_all$short_name)
	
	purrr::map(sp, temp_plot_func)

	# weight vs oxygen plots

	oxy_plot_func <- function(sp){

		sp_dat <- oxy_plot_df %>% filter(species == sp)
		
		sp_dat <- sp_dat %>%
			mutate(age = age_f,
						 age = as.character(age),
						 age = as.numeric(age))
		
		sp_dat <- sp_dat %>% 
  		arrange(age) %>% 
  		mutate(age_f = fct_inorder(age_f)) 
		
		sp_dat$age_label <- paste0("Age ", sp_dat$age_f)
	

		# plot
		plot <-
					ggplot(sp_dat, aes(value, est)) +
					geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ reorder(age_label, age), scales = "free_y", ) +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("[Dissolved oxygen]") +
					ggtitle(sp) +
					theme_sleek() +
					theme(
				 		panel.spacing.y = unit(0, "lines"))

			
			plot_name <- paste0(sp, "_oxy_May2024_2.png")
			
			ggsave(plot, file = paste0(file_path_plots, plot_name),
						 height = 5, width = 10, units = "in")
			
		plot
	}
	
	sp <- unique(dat_all$short_name)
	
	purrr::map(sp, oxy_plot_func)

	 
	
	
	#### customize axes ####
	
	#### pcod ####
	
	pcod_temp_df <- temp_plot_df %>% filter(species == "pcod")
	
	pcod_temp_df$age_f <- factor(pcod_temp_df$age_f, 
															 levels = c("1", "2", "3", 
															 					 "4", "5", "6", 
															 					 "7", "8", "9", "10"))
	
	lab_fun <- function(num){
		
		lab <- paste0("Age ", num)
		
	}
	
	labs <- map(1:10, lab_fun)
	
	y_val <- pcod_temp_df %>%
		group_by(age_f) %>%
		summarise(y_val = max(high))
	
	label_df <- tibble(
		value = -1.5,
		est = y_val$y_val,
		lab = labs,
		age_f = as.factor(1:10)
	)
	
		
	pcod_plot <-
			ggplot(pcod_temp_df, aes(value, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
			geom_text(data = label_df, label = label_df$lab, size = 3) +
			geom_line(color = "black") +
			ggh4x::facet_wrap2(~ age_f, scales = "free_y", nrow = 3, ncol = 4) +
			ylab("Predicted mean (log) weight (g)") +
	 		xlab("Temperature (˚C)") +
			theme_sleek() +
			theme(
				panel.spacing.y = unit(0.5, "lines"),
				strip.text.x = element_blank())

	
	
	pcod_scales <- list(
		scale_y_continuous(
			breaks = c(1.7, 1.8)), # age 1
		scale_y_continuous(
			breaks = c(2.5, 2.7)), # age 2
		scale_y_continuous(
			breaks = c(2.9, 3.0)), # age 3
		scale_y_continuous(
			breaks = c(3.15, 3.25)),# age 4
		scale_y_continuous(
			breaks = c(3.35, 3.45)),# age 5
		scale_y_continuous(
			breaks = c(3.5, 3.6)),# age 6
		scale_y_continuous(
			breaks = c(3.6, 3.7)),# age 7
		scale_y_continuous(
			breaks = c(3.75, 3.85)),# age 8
		scale_y_continuous(
			breaks = c(3.85, 3.95)),# age 9
		scale_y_continuous(
			breaks = c(3.8, 4.0))# age 10
)
	
	pcod_temp_plot <- pcod_plot + 
		facetted_pos_scales(y = pcod_scales) 
	
		ggsave(pcod_temp_plot, file = paste0(file_path_plots, "pcod_temp_plot.png"),
						 height = 4, width = 8, units = "in")
	
		
	#### pollock ####
		
	pol_temp_df <- temp_plot_df %>% filter(species == "pollock")
	
	pol_temp_df$age_f <- factor(pol_temp_df$age_f, 
															 levels = c("1", "2", "3", 
															 					 "4", "5", "6", 
															 					 "7", "8", "9", 
															 					 "10", "11", "12",
															 					 "13", "14", "15",
															 					 "16", "17", "18",
															 					 "19", "20"))
	
	lab_fun <- function(num){
		
		lab <- paste0("Age ", num)
		
	}
	
	labs <- map(1:20, lab_fun)
	
	y_val <- pol_temp_df %>%
		group_by(age_f) %>%
		summarise(y_val = max(high) - 0.05)
	
	label_df <- tibble(
		value = -1.5,
		est = y_val$y_val,
		lab = labs,
		age_f = as.factor(1:20)
	)
	
		
	pol_plot <-
			ggplot(pol_temp_df, aes(value, est)) +
			geom_ribbon(aes(ymin = low, ymax = high), 
											fill = "lightgrey", alpha = 0.4) +
			geom_text(data = label_df, label = label_df$lab, size = 3) +
			geom_line(color = "black") +
			ggh4x::facet_wrap2(~ age_f, scales = "free_y", nrow = 4, ncol = 5) +
			ylab("Predicted mean (log) weight (g)") +
	 		xlab("Temperature (˚C)") +
			theme_sleek() +
			theme(
				panel.spacing.y = unit(0.5, "lines"),
				strip.text.x = element_blank())

	pol_scales <- list(
		scale_y_continuous(
			breaks = c(1.1, 1.3)), # age 1
		scale_y_continuous(
			breaks = c(1.8, 2.0)), # age 2
		scale_y_continuous(
			breaks = c(2.2, 2.4)), # age 3
		scale_y_continuous(
			breaks = c(2.6, 2.7)),# age 4
		scale_y_continuous(
			breaks = c(2.7, 2.9)),# age 5
		scale_y_continuous(
			breaks = c(2.85, 2.95)),# age 6
		scale_y_continuous(
			breaks = c(2.9, 3.0)),# age 7
		scale_y_continuous(
			breaks = c(2.95, 3.05)),# age 8
		scale_y_continuous(
			breaks = c(3.0, 3.1)),# age 9
		scale_y_continuous(
			breaks = c(3.0, 3.1)),# age 10
		scale_y_continuous(
			breaks = c(3.1, 3.2)),# age 11
		scale_y_continuous(
			breaks = c(3.0, 3.2)),# age 12
		scale_y_continuous(
			breaks = c(3.0, 3.2)),# age 13
		scale_y_continuous(
			breaks = c(3.1, 3.3)),# age 14
		scale_y_continuous(
			breaks = c(3.1, 3.3)),# age 15
		scale_y_continuous(
			breaks = c(3.3, 3.5)),# age 16
		scale_y_continuous(
			breaks = c(3.0, 3.4)),# age 17
		scale_y_continuous(
			breaks = c(2.8, 3.2)),# age 18
		scale_y_continuous(
			breaks = c(3.1, 3.5)),# age 19
		scale_y_continuous(
			breaks = c(3.0, 3.4))# age 20
)
	
	pol_temp_plot <- pol_plot + 
		facetted_pos_scales(y = pol_scales) 
	
		ggsave(pol_temp_plot, file = paste0(file_path_plots, "pol_temp_plot.png"),
						 height = 4, width = 8, units = "in")