#older code time varying int_shift(
	# plotting time-varying effect 

	## read in models ####
	file_path <- "/output/model output/sdmTMB output/August 2024 TV single age models/"
	
	## read in models ####
	file_list <- list.files(path = paste0(here(), file_path))

	file_list <- stringr::str_subset(file_list, '.rds')
	
  prestring <- paste0(here(), file_path)

  mod_names_list <- list()
  
  for(i in file_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  single_age_mod_list <- lapply(mod_names_list, readRDS)
  
  # remove models that did not converge
 # drop <- which(single_age_mod_list %like% 'Error')
  
  #drop <- as.numeric(drop)
  
  #single_age_mod_list_trim <- single_age_mod_list[-drop]
  
	names_mods <- names(single_age_mod_list)
  
	# remove models that did not pass sanity checks
	sanity_fun <- function(num, name) {
		
		mod <- single_age_mod_list[[num]]
		
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
		
}

	num = 1:length(single_age_mod_list)
	
	fdf <- tibble(
		num = num,
		name = names_mods
	)

	mod_df <- map2(fdf$num, fdf$name, sanity_fun) %>% bind_rows()	 	
	
	mod_df_drop <- mod_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	#drop <- mod_df_drop$name
#
	#mod_list_keep <- mod_df %>% 
	#	filter(name %!in% drop)
	#
	#mod_list_keep <- mod_list_keep$name
	#
	#saveRDS(mod_list_keep, file = paste0(here(), file_path, "mod_list_converged.rda"))
	#
	### read in models that did converge ####
	
	#mod_list_keep <- readRDS(file = paste0(here(), file_path, "mod_list_converged.rda"))
	#
	#file_list <- list.files(path = paste0(here(), file_path))
#
	#file_list <- intersect(mod_list_keep, file_list)
	#
	#prestring <- paste0(here(), file_path)
#
  #mod_names_list <- list()
  #
  #for(i in file_list){
  #	mod_names_list[[i]] <- paste0(prestring, i)
  #}
  #
	#mod_list_keep <- lapply(mod_names_list, readRDS)
	#
	# remove '.rds' from names
  names(single_age_mod_list) <- str_replace_all(names(single_age_mod_list), '.rds', '')
  
  name_mods <- names(single_age_mod_list)
  
  mod_type <- unlist(qdapRegex::ex_between(name_mods, "_", "_yrprior"))

  # plot time-varying coefficient

#	tv_df_all <- purrr::map_dfr(mod_list_keep, \(mod){
#		
#		dat <- mod$data
#  
#  	sp <- unique(dat$short_name)
#  	age <- unique(dat$age_f)
#  	year <- unique(dat$year)
#  	
#  	p <- get_pars(mod)
#		tv <- p$b_rw_t
#		
#		tv_df <- tibble(sp, age, year, tv)
#	
#		tv_df
#	
#	})
	
	
	##############
	
	tv_fun <- function(mod, type){
		
		dat <- mod$data
		
		var_form <- mod$formula
			
		var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
	
  	sp <- unique(dat$short_name)
  	age <- unique(dat$age)
  	year <- unique(dat$year)
  	
  	p <- get_pars(mod)
		tv <- p$b_rw_t[,,1]
		
		tv_df <- tibble(sp, type, var, age, year, tv)
	
		tv_df
		
	}
		
	fdf <- tibble(
		mod = mod_list_keep,
		type = mod_type
	)	
	
	tv_df_all <- purrr::map2_dfr(fdf$mod, fdf$type, tv_fun)
	
	# fix linear model type
	tv_df_all$var[tv_df_all$var == "log_wt ~ yrprior_boxy"] <- "yrprior_boxy"
	tv_df_all$var[tv_df_all$var == "log_wt ~ yrprior_btemp"] <- "yrprior_btemp"
		
	sum <- tv_df_all %>%
		group_by(sp, type, var, age, year) %>%
		summarise(count = n())
	
	unique(sum$count)
	
	# plot
	
	sp <- unique(dat_all$short_name)
	vars <- names(dat_all %>% select(contains("yrprior")))
	
	fdf <- crossing(
		sp = sp,
		var = vars
	)
	
	plots <- purrr::map2(fdf$sp, fdf$var, \(sp, var){
  
	dat <- tv_df_all %>% 
		filter(sp == "pcod", var == "yrprior_boxy")
	
	unique(dat$type)
	
	p <- 
		ggplot(dat) +
			geom_point(aes(x = year, y = tv)) +
			facet_grid(age ~ type, scales = "free") +
			ggtitle(paste0(sp, ":", var))
	
	#p
	
	ggsave(p, file = paste0(here(), "/output/plots/", sp, "_", var, ".png"))

	})

)