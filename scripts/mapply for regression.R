# use mapply for regression

	wt <- rnorm(100, 5, 1)
	temp <- rnorm(100, 10, 5)
	oxy <- rnorm(100, 3, 2)

	sim_dat1 <- tibble(wt, temp, oxy) %>%
		mutate(species = rep("cod"))
	
	sim_dat2 <- tibble(wt, temp, oxy) %>%
		mutate(species = rep("sole"))


	lm_func <- function(var){
	
			mod <- lm(wt ~ var, data = sim_dat1)

	}
	
	vars <- list(temp, oxy)

	mod_list <- lapply(vars, lm_func)
	
	# function with multiple dfs
	
	lm_func2 <- function(df){
	
			mod <- lm(wt ~ temp, data = df)

	}
	
	dfs <- list(sim_dat1, sim_dat2)
	
	mod_list <- lapply(dfs, lm_func2)
	
	# mapply with multiple dfs and variables
	
	lm_func3 <- function(var, df){
		lm(wt ~ var, data = df)
	}
	
	dfs <- list(sim_dat1, sim_dat2)
	vars <- c("temp", "oxy")
	
	test <- expand_grid(dfs, vars)
	
	mod_list <- mapply(lm_func3,
										 var = test$vars,
										 df = test$dfs,
										 SIMPLIFY = F)
	
	# try again
	
	test <- mapply(function(df, z) mutate_(df, z=z), dfs, 3:4, SIMPLIFY=FALSE)

	test <- mapply(function(var, df) lm(wt ~ var, data = df), df = test$dfs, var = test$vars, SIMPLIFY=FALSE)
	
	# again
	
	library(purrr)

	map2(person_ids, time_ranges, F, dt, date_col, z)
	
	map2(test$vars, test$dfs, lm_func3)

	# try again
	
		df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins), SIMPLIFY = FALSE)

		test_list <- mapply(lm_func3, df = dfs, var = vars, SIMPLIFY = FALSE)

		
		
		
	
	# try something else
	
	 vars = c("temp", "oxy")

	 lm_func <- function(preds){
	 		 formulas <- paste("wt ~", preds)
			 fit <- lm(as.formula(formulas), data = sim_dat1)
	 }
	 
	 test <- lapply(vars, lm_func)
	 
	 summary(test[[1]])
	 summary(lm(wt ~ temp, data = sim_dat1))  

	 summary(test[[2]])
	 summary(lm(wt ~ oxy, data = sim_dat1))
	 
	 # try it with multiple dfs
	 vars = c("temp", "oxy")
	 dfs <- list(sim_dat1, sim_dat2)

	 lm_func <- function(preds, df){
	 		 formulas <- paste("wt ~", preds)
			 fit <- lm(as.formula(formulas), data = df)
	 }
	 
	 test <- mapply(lm_func, preds = vars, df = dfs, SIMPLIFY = F)


	 ### try soemthing else
	 PREDS = c("temp", "oxy")

	 DVMOD <- function(PREDS, data){
     theFormula <- paste("wt ~", PREDS)
     t <- lm(as.formula(theFormula), data = data)
     return((c(PREDS, coef(t)[1], confint(t)[1,])))
	}

	 DVMOD("temp", sim_dat1)

	 all_models <- lapply(c("age"),function(x,y){
     DVMOD(x,y)
	},justices)
#### temperature ####
	
	## presurvey temp ####
	
	# pollock #	
  
	#1. weight ~ age + temp

  weight_age_temp_func <- function(var){
  	
  	mod <- bam(log_wt ~ age_f + s(var) + 
														 	s(julian_day) + te(latitude, longitude) + 
											  			s(ID_f, bs = "re") + # haul in year random effect
								 			  			s(haul_id_f, bs = "re") + # haul in year random effect
								 			  			s(cohort_f, bs = "re"),
											  			data = pollock_dat, 
															nthreads = 8,
															method = "ML")
  }
  
  
  vars <- list("presurvey_btemp", "presurvey_boxy")
  
	pol_pre_age_bam_ML_list <- lapply(vars, weight_age_temp_func)
  
  pol_pretemp_age_bam_ML <- bam(log_wt ~ age_f + s(presurvey_mean_temp) + 
														 	s(julian_day) + te(latitude, longitude) + 
											  			s(ID_f, bs = "re") + # haul in year random effect
								 			  			s(haul_id_f, bs = "re") + # haul in year random effect
								 			  			s(cohort_f, bs = "re"),
											  			data = pollock_dat, 
															nthreads = 8,
															method = "ML")
	
  #saveRDS(pol_pretemp_age_bam, 
  #				file = here("./output/model output/ACLIM temps/pol_pretemp_age_bam.rds"))
  
  saveRDS(pol_pretemp_age_bam_ML, 
  				file = here("./output/model output PC/pol_pretemp_age_bam_ML.rds"))
 ############################################
  
 
		df <- df %>%
		mutate(year_age0 = cohort,
					 year_age0_f = as.factor(year_age0))
		
		yr_age0_dat <- df %>%
			ungroup() %>%
			dplyr::select(year_age0_f, year_age0) %>%
			rename(year = year_age0) %>%
			distinct()
		
		yr_age0_vars <- left_join(yr_age0_dat, yr_prior_short) %>%
			ungroup() %>%
			mutate(cohort = year) %>%
			rename(age0_btemp = yrprior_btemp,
						 age0_boxy = yrprior_boxy) %>%
			select(cohort, age0_btemp, age0_boxy) %>%
			distinct(cohort, .keep_all = T)
		
		dat <- left_join(df, yr_age0_vars)
	

	}	