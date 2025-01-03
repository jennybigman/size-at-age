# older year code

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
 
 # extract and plot the year effect estimates from the models

 # set up names
	short_name <- 	str_extract(names(yr_mod_list_trim), "[^_yr]+")
	age <- unlist(qdapRegex::ex_between(names(yr_mod_list_trim), "age_", ".rds"))
	sp_age_dat <- tibble(short_name, age)

	sp_age_dfs <- split(sp_age_dat, seq(nrow(sp_age_dat)))
	
	ex_est_fun <- function(mod){
		
		pars <- tidy(mod, conf.int = TRUE)
		
		pars$year <- str_extract(pars$term, "(?<=_f).*")
		
		pars
	
	}
	
	par_dfs <- map(yr_mod_list_trim, ex_est_fun)
	
	## try with df
	
	add_sp_age_fun <- function(par_df, sp_age_df){
		
		pars <- bind_cols(par_df, sp_age_df)
		
		pars
	
	}
	
	fdf <- tibble(
		par_df = par_dfs,
		sp_age_df = sp_age_dfs
	)
	
	par_df <- map2(fdf$par_df, fdf$sp_age_df, add_sp_age_fun) %>% bind_rows()
	
	par_df$year <- as.numeric(par_df$year)
	par_df$age <- as.numeric(par_df$age)
	
	par_df$age_label <- as.factor(paste0("Age ", par_df$age))

#	ggplot(par_df) +
#	 	geom_pointrange(aes(x = year, y = estimate,
#	 											ymin = conf.low,
#	 											ymax = conf.high),
#	 									size = 0.05) +
#	 	ggh4x::facet_grid2(short_name ~ age, scales = "free", independent = "y", render_empty = FALSE) +
#	 	ylab("Predicted mean (log) weight (g)") +
#	 	xlab("Year") +
#	 	scale_x_discrete(guide = guide_axis(angle = 90)) +
#		#ggtitle(name) +
#	 	theme_sleek() +
#	 	theme(
#	 		axis.title = element_text(size = 8),
#	 		axis.text = element_text(size = 6),
#	 		strip.text = element_text(size = 8),
#	 		panel.spacing.y = unit(0, "lines")
#	 	)
	
	# plots
	
	yr_plot_fun <- function(sp){
		
		sp_pars <- par_df %>% filter(short_name == sp)
	
		p <-
			ggplot(sp_pars) +
	 		geom_pointrange(aes(x = year, y = estimate,
	 												ymin = conf.low,
	 												ymax = conf.high),
	 										size = 0.05) +
	 		facet_wrap(~ fct_reorder(age_label, age), scales = "free", nrow = 1) +
	 		#ylab("Predicted mean (log) weight (g)") +
	 		#xlab("Year") +
	 		theme_sleek() +
	 		theme(
	 			axis.title = element_blank(),
	 			axis.text = element_text(size = 6),
	 			strip.text = element_text(size = 8),
	 			panel.spacing.y = unit(0, "lines")
	 		)
	
		p
		
	}
	
	sp <- unique(par_df$short_name)
	yr_plots <- map(sp, yr_plot_fun)
	

	# put together
	plot1 <- yr_plots[[1]]
	plot2 <- yr_plots[[2]]
	plot3 <- yr_plots[[3]]
	plot4 <- yr_plots[[4]]
	
	yr_plot <- plot1/plot2/plot3/plot4 
	
	#+  labs(tag = "Predicted mean (log) weight (g)") +
  #theme(
  #  plot.tag = element_text(size = rel(1), angle = 90),
  #  #plot.tag.position = "left"
  #)
	#
	#plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
#
  #patchwork::wrap_plots(master_list_with_plots, 
  #                      nrow = no_of_rows, ncol = no_of_cols)
#}#

#plot_a_list(yr_plots, 4, 1)
	
	 ggsave(here("output", "plots", "May 2024", "year models", "all_yr_plots.png"), yr_plot,
	 			 height = 7, width = 12)
