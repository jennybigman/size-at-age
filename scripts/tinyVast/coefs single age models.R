	# read in single age mods
	file_path <- paste0(here(), "/output/model output/tinyVAST/sep age models/")

	file_list <- list.files(path = file_path)
	
	# list by model type
	no_cov_mods <- stringr::str_subset(file_list, 'no')
	poly2_mods <- stringr::str_subset(file_list, 'poly2')
	poly3_mods <- stringr::str_subset(file_list, 'poly3')
	lin_mods <- stringr::str_subset(file_list, 'lin')

	#### read in mods ####
	
	mod_path_fun <- function(x){
		
		mod_path <- paste0(file_path, x)
		mod_path
	}
	
	no_cov_mod_paths <- mod_path_fun(no_cov_mods)
	poly2_mod_paths <- mod_path_fun(poly2_mods)
	poly3_mod_paths <- mod_path_fun(poly3_mods)
	lin_mod_paths <- mod_path_fun(lin_mods)


  no_cov_mods <- lapply(no_cov_mod_paths, readRDS)
	poly2_mods <- lapply(poly2_mod_paths, readRDS)
	poly3_mods <- lapply(poly3_mod_paths, readRDS)
	lin_mods <- lapply(lin_mod_paths, readRDS)

	
	 # no cov pars
	 nc_par_fun <- function(mod){
		
		d <- mod$data
		sp <- unique(d$short_name)
		age <- unique(d$age)

		p <- summary(mod, 'fixed')
		int <- p[1,1]
		int_se <- p[1,2]
		int_stat <- p[1,3]
		int_p <- p[1,4]
		
		term <-      "Intercept"
		estimate <-  int
		std.error <- int_se
		statistic <- int_stat
		p.value <-   int_p
		
		pars <- tibble(term, estimate, std.error, statistic, p.value, sp, age)
		
		return(pars)
		
	}
	
	no_cov_pars <- purrr::map_dfr(no_cov_mods, nc_par_fun) %>%
		mutate(mod_type = "no_cov",
					 cat = "stacked") 
	# combine the above with other datasets
	  
	lin_par_fun <- function(mod){
		
		d <- mod$data
		sp <- unique(d$short_name)
		age <- unique(d$age)
		formula <- mod$formula
		var <- gsub(".*[()]([^,]+)[,].*", "\\1", formula)[3]

		p <- summary(mod, 'fixed')
		int <- p[1,1]
		int_se <- p[1,2]
		int_stat <- p[1,3]
		int_p <- p[1,4]
		
		var_est <- p[2,1]
		var_se <- p[2,2]
		var_stat <- p[2,3]
		var_p <- p[2,4]
		
		term <- c("Intercept", var)
		estimate <- c(int, var_est)
		std.error <- c(int_se, var_se)
		statistic <- c(int_stat, var_stat)
		p.value <- c(int_p, var_p)
		
		pars <- tibble(term, estimate, std.error, statistic, p.value, sp, age)
		
		return(pars)
		
	}
	
	lin_pars <- purrr::map_dfr(lin_mods, lin_par_fun) %>%
		mutate(mod_type = "lin",
					 cat = "stacked")

	poly2_par_fun <- function(mod){
		
		d <- mod$data
		sp <- unique(d$short_name)
		age <- unique(d$age)
		formula <- mod$formula
		var <- gsub(".*[()]([^,]+)[,].*", "\\1", formula)[3]

		p <- summary(mod, 'fixed')
		int <- p[1,1]
		int_se <- p[1,2]
		int_stat <- p[1,3]
		int_p <- p[1,4]
		
		poly1_est <- p[2,1]
		poly1_se <- p[2,2]
		poly1_stat <- p[2,3]
		poly1_p <- p[2,4]
		
		poly2_est <- p[3,1]
		poly2_se <- p[3,2]
		poly2_stat <- p[3,3]
		poly2_p <- p[3,4]
		
		
		term <- c("Intercept", "poly1", "poly2")
		estimate <- c(int, poly1_est, poly2_est)
		std.error <- c(int_se, poly1_se, poly2_se)
		statistic <- c(int_stat, poly1_stat, poly2_stat)
		p.value <- c(int_p, poly1_p, poly2_p)
		
		pars <- tibble(term, estimate, std.error, statistic, p.value, sp, age)
		
		return(pars)
		
	}
	
	
	poly2_pars <- purrr::map_dfr(poly2_mods, poly2_par_fun) %>%
		mutate(mod_type = "poly2",
					 cat = "stacked")
	
	poly3_par_fun <- function(mod){
		
		d <- mod$data
		sp <- unique(d$short_name)
		age <- unique(d$age)
		formula <- mod$formula
		var <- gsub(".*[()]([^,]+)[,].*", "\\1", formula)[3]

		p <- summary(mod, 'fixed')
		int <-      p[1,1]
		int_se <-   p[1,2]
		int_stat <- p[1,3]
		int_p <-    p[1,4]
		
		poly1_est <-  p[2,1]
		poly1_se <-   p[2,2]
		poly1_stat <- p[2,3]
		poly1_p <-    p[2,4]
		
		poly2_est <-  p[3,1]
		poly2_se <-   p[3,2]
		poly2_stat <- p[3,3]
		poly2_p <-    p[3,4]
		
		poly3_est <-  p[4,1]
		poly3_se <-   p[4,2]
		poly3_stat <- p[4,3]
		poly3_p <-    p[4,4]
		
		term <- c("Intercept", "poly1", "poly2", "poly3")
		estimate <- c(int, poly1_est, poly2_est, poly3_est)
		std.error <- c(int_se, poly1_se, poly2_se, poly3_se)
		statistic <- c(int_stat, poly1_stat, poly2_stat, poly3_stat)
		p.value <- c(int_p, poly1_p, poly2_p, poly3_p)
		
		pars <- tibble(term, estimate, std.error, statistic, p.value, sp, age)
		
		return(pars)
		
	}
	
	poly3_pars <- purrr::map_dfr(poly3_mods, poly3_par_fun) %>%
		mutate(mod_type = "poly3")

	
	# save files
	output_path <- paste0(here(), "/output/model output/tinyVAST/par comps/")
	
	saveRDS(no_cov_pars, file = paste0(output_path, "no_cov_pars_stacked.rds"))
	saveRDS(lin_pars, file = paste0(output_path, "lin_pars_stacked.rds"))
	saveRDS(poly2_pars, file = paste0(output_path, "poly2_pars_stacked.rds"))
	saveRDS(poly3_pars, file = paste0(output_path, "poly3_pars_stacked.rds"))
	
