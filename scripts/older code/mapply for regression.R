# use mapply for regression

	wt1 <- rnorm(100, 5, 1)
	temp1 <- rnorm(100, 10, 5)
	oxy1 <- rnorm(100, 3, 2)

	sim_dat1 <- tibble(wt1, temp1, oxy1) %>%
		mutate(species = rep("cod"))
	
	wt2 <- rnorm(100, 3, 1)
	temp2 <- rnorm(100, 7, 5)
	oxy2 <- rnorm(100, 7, 2)

	sim_dat2 <- tibble(wt2, temp2, oxy2) %>%
		mutate(species = rep("sole"))

	wt1 <- rnorm(100, 5, 1)
	temp1 <- rnorm(100, 10, 5)
	temp2 <- rnorm(100, 3, 2)

	sim_dat_temp1 <- tibble(wt1, temp1, temp2) %>%
		mutate(species = rep("cod"))
	
	wt1 <- rnorm(100, 8, 1)
	temp1 <- rnorm(100, 6, 5)
	temp2 <- rnorm(100, 9, 2)

	sim_dat_temp2 <- tibble(wt1, temp1, temp2) %>%
		mutate(species = rep("sole"))

	# apply different models to 1 df
	lm_func <- function(var){
	
			mod <- lm(wt ~ var, data = sim_dat1)

	}
	
	vars <- list(temp, oxy)

	mod_list <- lapply(vars, lm_func)
	
	# apply two models to different dfs
	
	lm_func2 <- function(df){
	
			mod_temp <- lm(wt ~ temp, data = df)
			mod_oxy <- lm(wt ~ oxy, data = df)
			list(mod_temp, mod_oxy)
	}
	
	dfs <- list(sim_dat1, sim_dat2)
	
	mod_list <- lapply(dfs, lm_func2)
	
	
	# mapply with multiple dfs and variables
	
	
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

	 lm_func <- function(preds, df){
	 		 formulas <- paste("wt ~", preds)
			 fit <- lm(as.formula(formulas), data = df)
	 }
	 
	 test <- mapply(lm_func, preds = vars, df = df_list, SIMPLIFY = F)
	
	 ###########
	 
	 
	 
	 
	 ### try soemthing else -- WORKS ####
	 PREDS = c("temp", "oxy")

	 DVMOD <- function(PREDS, data){
     theFormula <- paste("wt ~", PREDS)
     t <- lm(as.formula(theFormula), data = data)
     return((c(PREDS, coef(t)[1], confint(t)[1,])))
	}

	 DVMOD("temp", sim_dat1)

	 all_models <- lapply(c("temp", "oxy"),function(x,y){
     DVMOD(x,y)
	}, sim_dat1)

	 
	###### try again
	 
	df_list <- list (sim_dat1, sim_dat2)
	
	lm_func <- function(df){
		
		temp_fit <- lm(wt ~ temp, data = df)
		oxy_fit <- lm(wt ~ oxy, data = df)
		
	}
	
	dfs <- list(sim_dat1, sim_dat2)

	mod_list <- lapply(df_list, lm_func)
	
	#### again
	
	models <- function(.x) {
		
  	temp <- lm(wt ~ temp, data = .x)
  	oxy <- lm(wt ~ oxy, data = .x)

   list(temp, oxy) |>
      map_dfr(broom::glance, .id = "model")
	}
	
	mod_list <- lapply(df_list, models)
	
	######### again
	
	foo <- function(n1, n2) {
      as.formula(paste("class~", paste(n1, n2, sep = "+")))
  }
> foo(name1, name2)
# class ~ field1 + field2
# <environment: 0x4d0da58>
svm(foo(name1, name2), data = df)
	
foo <- function(x) {
      as.formula(paste("wt~", paste(x, sep = "+")))
}

vars <- c("temp", "oxy")

formulas <- lapply(vars, foo)

lm_func <- function(x){
	fit <- lm(x, data = sim_dat1)
	fit_tidy <- broom::glance(fit)
}

mod_lists <- lapply(formulas, lm_func)

# try again

 model_func <- function(x)
 	lm(paste0(wt, "~", x), data = sim_dat1)
 
 map(model_func, vars)

	 
	reg <- function(y,x) {
   lm(paste0(y,"~",x),data=sim_dat1)
}
	mapply(reg, y = "wt", x = vars, SIMPLIFY = F)
	# above works
	
	# try with dfs
	reg <- function(x, df) {
   lm(paste0("wt ~",x),data=df)
}
	
	output <- mapply(reg, x = vars, df = df_list, SIMPLIFY = F)

	
	## try again
	
	lm_func <- function(x){
		paste0("wt ~ ", x)
	}
	
	forms <- sapply(vars, lm_func)
		
	run_lm_func <- function(df){
		lm(as.formula(forms), data = df)
	}	
		
	mod_list <- lapply(df_list, run_lm_func)

	#### try again
	
	results = map_df(1:length(df_list), function(i){
  
  rf_model <- randomForest::randomForest(Species ~., data = df_list[[i]])
  rf_model_roc <- roc(iris$Species,rf_model$votes[,2])
  df_auc <- auc(rf_model_roc)
  
  data.frame(
    dataset = paste0("dataset", i),
    auc = as.numeric(df_auc)
  )
  
})

results


	
	results = purrr::map_df(1:length(df_list), function(i){
  
  temp_model <- lm(wt ~ temp, data = df_list[[i]])
  oxy_model <- lm(wt ~ oxy, data = df_list[[i]])
  
})

results



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