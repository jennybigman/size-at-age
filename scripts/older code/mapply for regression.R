# use mapply for regression

	# simulate two dataframes
	wt <- rnorm(100, 5, 1)
	temp1 <- rnorm(100, 10, 5)
	temp2 <- rnorm(100, 3, 2)
	age_class <- rep(as.factor(1:10), each = 10)
	age_class_f <- ordered(age_class, levels = c("1", "2", "3", "4", "5", 
		"6", "7", "8", "9", "10")) 
		
	cod_dat <- tibble(wt, temp1, temp2, age_class_f) %>%
		mutate(species = rep("cod"))
	
	wt <- rnorm(100, 8, 1)
	temp1 <- rnorm(100, 6, 5)
	temp2 <- rnorm(100, 9, 2)
	age_class <- rep(as.factor(1:10), each = 10)
	age_class_f <- ordered(age_class, levels = c("1", "2", "3", "4", "5", 
		"6", "7", "8", "9", "10")) 

	sole_dat <- tibble(wt, temp1, temp2, age_class_f) %>%
		mutate(species = rep("sole"))

	# apply different models to 1 df
	lm_func <- function(var){
	
			mod <- lm(wt ~ var, data = cod_dat)

	}
	
	vars <- list(temp1, temp2)

	mod_list <- lapply(vars, lm_func)
	
	
	# mapply with multiple dfs and variables
	
	 lm_func <- function(pred){
	 		 form1 <- paste("wt ~ 0 +", pred)
	 		 form2 <- paste("wt ~ 0 + age_class_f *", pred)
			 fit <- lm(as.formula(form1), data = cod_dat)
			 fit_int <- lm(as.formula(form2), data = cod_dat)
			 
			 mod_list <- list(fit, fit_int)
	 }
	 
	 vars = c("temp1", "temp2")

	 test <- lapply(vars, lm_func)
	 
	 test[[1]][1] # this is the no interaction model with the first predictor
	 test[[1]][2] # this is the interaction model with the first predictor
	 
	# multiple species and variables #####

	# combine data
	dat_all_test <- bind_rows(cod_dat, sole_dat)

	# function to fit models with and without an interaction
	mod_func <- function(sp){
		
		new_dat <- dat_all_test %>% filter(species == sp)
		
		 form1 <- paste("wt ~ 0 +", "temp1")
		 form2 <- paste("wt ~ 0 + age_class_f *", "temp1")
		 
		 fit <- lm(as.formula(form1), data = new_dat)
		 fit_int <- lm(as.formula(form2), data = new_dat)
	 
		 mod_list <- list(fit, fit_int)
		
	}
	
	species <- unique(dat_all_test$species)
	
	
	mod_list_test <- lapply(species, mod_func)
	
	# try with multiple species and predictors --- GO BACK TO THIS #####
	
	mod_func <- function(sp, y){
		
		new_dat <- dat_all_test %>% filter(species == sp)
		
		 form1 <- paste("wt ~ 0 +", y)
		 form2 <- paste("wt ~ 0 + age_class_f *", y)
		 
		 fit <- lm(as.formula(form1), data = new_dat)
		 fit_int <- lm(as.formula(form2), data = new_dat)
	 
		 mod_list <- list(fit, fit_int)
		
	}
	
	species <- unique(dat_all_test$species)
	
	
	mod_list_test <- lapply(species, mod_func)

	vars <- dat_all_test %>% 
		select(contains("temp")) %>%
		names()
	
	df_func <- expand_grid(
		sp = species,
		y = vars
	)
	
	mod_lists <- mapply(mod_func,
											sp = df_func$sp,
											y = df_func$y)
	

 ##### with map ###########################
	
	mod_func <- function(sp, y){
		
		 new_dat <- dat_all_test %>% filter(species == sp)
		
		 form1 <- paste("wt ~ 0 +", y)
		 form2 <- paste("wt ~ 0 + age_class_f *", y)
		 
		 fit <- tidy(lm(as.formula(form1), data = new_dat)) %>%
		 	#mutate_if(is.numeric, round, digits = 2) %>%
		 	mutate(species = sp,
		 				 mod = "no interaction",
		 				 variable = y)
		 
		 fit_int <- tidy(lm(as.formula(form2), data = new_dat)) %>%
		 	#mutate_if(is.numeric, round, digits = 2) %>%
		 	mutate(species = sp,
		 				 mod = "interaction", 
		 				 variable = y)
	 
		 mod_list <- list(fit, fit_int)
		
		}
	
	sp = unique(dat_all_test$species)
	vars <- c("temp1", "temp2")
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	test <- map2(df_func$sp, df_func$y, mod_func)

	# for comparison
	cod_map_lms <- bind_rows(
		test[[1]][1],
		test[[1]][2],
		test[[2]][1],
		test[[2]][2]
		)
	
	# make sure get correct results
	cod_lm_temp1 <- tidy(lm(wt ~ 0 + temp1, dat = cod_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "no interaction",
					 variable = "temp1")
	
	cod_lm_int_temp1 <- tidy(lm(wt ~ 0 + temp1 * age_class_f, data = cod_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "interaction",
					 variable = "temp1")

	cod_lm_temp2 <- tidy(lm(wt ~ 0 + temp2, dat = cod_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "no interaction",
					 variable = "temp2")
	
	cod_lm_int_temp2 <- tidy(lm(wt ~ 0 + temp2 * age_class_f, data = cod_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "interaction",
					 variable = "temp2")

	sole_lm_temp1 <- tidy(lm(wt ~ 0 + temp1, dat = sole_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2)%>%
		mutate(species = "sole",
					 model = "no interaction",
					 variable = "temp1")
		
	sole_lm_int_temp1 <- tidy(lm(wt ~ 0 + temp1 * age_class_f, data = sole_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "sole",
					 model = "interaction",
					 variable = "temp1")
	
	sole_lm_temp2 <- tidy(lm(wt ~ 0 + temp2, dat = sole_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2)  %>%
		mutate(species = "sole",
					 model = "no interaction",
					 variable = "temp2")
												
	sole_lm_int_temp2 <- tidy(lm(wt ~ 0 + temp2 * age_class_f, data = sole_dat)) %>%
		#mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "sole",
					 model = "interaction",
					 variable = "temp2")

	# table for comparison
	cod_mod_output <- bind_rows(
		cod_lm_temp1,
		cod_lm_temp2,
		cod_lm_int_temp1,
		cod_lm_int_temp2
	)
	
	sole_mod_output <- bind_rows(
		sole_lm_temp1,
		sole_lm_temp2,
		sole_lm_int_temp1,
		sole_lm_int_temp2
	)
	
	
	