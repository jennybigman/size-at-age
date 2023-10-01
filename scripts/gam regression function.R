##### with map ###########################
	
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
	
	dat_all_test <- bind_rows(cod_dat, sole_dat)

	# function to fit multiple models - here, 2 predictors for each species 
	mod_func <- function(sp, y){
		
		 new_dat <- dat_all_test %>% filter(species == sp)
		
		 form1 <- paste0("wt ~ 0 + s(" , y, ")")
		 form2 <- paste0("wt ~ 0 + age_class_f + s(" , y, ", by = age_class_f)")
		 
		 fit <- tidy(gam(as.formula(form1), data = new_dat)) %>%
		 	mutate_if(is.numeric, round, digits = 2) %>%
		 	mutate(species = sp,
		 				 mod = "no interaction",
		 				 variable = y)
		 
		 fit_int <- tidy(gam(as.formula(form2), data = new_dat)) %>%
		 	mutate_if(is.numeric, round, digits = 2) %>%
		 	mutate(species = sp,
		 				 mod = "interaction", 
		 				 variable = y)
	 
		 mod_list <- list(fit, fit_int)
		
		}
	
	sp = unique(dat_all_test$species)
	
	vars <- dat_all_test %>%
		select(contains("temp")) %>%
		names()
	
	df_func <- expand_grid(
		sp = sp,
		y = vars
	)

	test <- map2(df_func$sp, df_func$y, mod_func)

	# results from function
	cod_map_gams <- bind_rows(
		test[[1]][1],
		test[[1]][2],
		test[[2]][1],
		test[[2]][2]
		)
	
	# make sure get correct results so compare with following models
	cod_temp1 <- tidy(gam(wt ~ 0 + s(temp1), dat = cod_dat)) %>%
		mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "no interaction",
					 variable = "temp1")
	
	cod_int_temp1 <- tidy(gam(wt ~ 0 + age_class_f + s(temp1, by = age_class_f), data = cod_dat)) %>%
		mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "interaction",
					 variable = "temp1")

	cod_temp2 <- tidy(gam(wt ~ 0 + s(temp2), dat = cod_dat)) %>%
		mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "no interaction",
					 variable = "temp2")
	
	cod_int_temp2 <- tidy(gam(wt ~ 0 + age_class_f + s(temp2, by = age_class_f), data = cod_dat)) %>%
		mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "cod",
					 model = "interaction",
					 variable = "temp2")

	sole_temp1 <- tidy(gam(wt ~ 0 + s(temp1), dat = sole_dat)) %>%
		mutate_if(is.numeric, round, digits = 2)%>%
		mutate(species = "sole",
					 model = "no interaction",
					 variable = "temp1")
		
	sole_int_temp1 <- tidy(gam(wt ~ 0 + age_class_f + s(temp1, by = age_class_f), data = sole_dat)) %>%
		mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "sole",
					 model = "interaction",
					 variable = "temp1")
	
	sole_temp2 <- tidy(gam(wt ~ 0 + s(temp2), dat = sole_dat)) %>%
		mutate_if(is.numeric, round, digits = 2)  %>%
		mutate(species = "sole",
					 model = "no interaction",
					 variable = "temp2")
												
	sole_int_temp2 <- tidy(gam(wt ~ 0 + age_class_f + s(temp2, by = age_class_f), data = sole_dat)) %>%
		mutate_if(is.numeric, round, digits = 2) %>%
		mutate(species = "sole",
					 model = "interaction",
					 variable = "temp2")

	
	# dfs for comparison
	cod_mod_output <- bind_rows(
		cod_temp1,
		cod_temp2,
		cod_int_temp1,
		cod_int_temp2
	)
	
	sole_mod_output <- bind_rows(
		sole_temp1,
		sole_temp2,
		sole_int_temp1,
		sole_int_temp2
	)
	
	
	