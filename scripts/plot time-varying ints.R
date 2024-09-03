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
	
	# remove '.rds' from names
  #names(single_age_mod_list) <- str_replace_all(names(single_age_mod_list), '.rds', '')
  
  # separate into cov and no cov models
	no_cov_mod_names <- stringr::str_subset(names(single_age_mod_list), 'no_cov')

	mod_names_list <- list()
  
  for(i in no_cov_mod_names){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  no_cov_mods <- lapply(mod_names_list, readRDS)
  
  no_cov_mod_names <-  names(no_cov_mods)
  no_cov_type <- unlist(qdapRegex::ex_between(no_cov_mod_names, "_", "_age"))

  
	single_age_mod_list <- stringr::str_subset(names(single_age_mod_list), 'no_cov', negate = TRUE)
  
	mod_names_list <- list()
  
  for(i in single_age_mod_list){
  	mod_names_list[[i]] <- paste0(prestring, i)
  }
  
  single_age_mods <- lapply(mod_names_list, readRDS)
 
  name_mods <- names(single_age_mods)

  mod_type <- unlist(qdapRegex::ex_between(name_mods, "_", "_yrprior"))

 
	# extract time-varying intercept? sd?
	
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
		mod = single_age_mods,
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
	f <- function(spp, vars){

		d <- tv_df_all %>% 
			filter(sp == spp, var == vars) 
	
		p <- 	
			ggplot(d) +
			geom_point(aes(x = year, y = tv)) +
			facet_grid(age ~ type, scales = "free") +
			ggtitle(paste0(spp, ":", vars))
			
		ggsave(p, file = paste0(here(), "/output/plots/", spp, "_", vars, ".png"))

			
	}
	
		species <- unique(dat_all$short_name)
		variables <- names(dat_all %>% select(contains("yrprior")))
	
		fdf <- crossing(
			spp = species,
			vars = variables
		)

		p <- purrr::map2(fdf$spp, fdf$vars, f)		
		