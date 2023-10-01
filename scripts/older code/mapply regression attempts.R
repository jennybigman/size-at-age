 ###########
	 
	 
	 
	
	 ### try soemthing else -- WORKS ####
	 vars = c("temp", "oxy")

	 mod_func <- function(var, data){
     m_formula <- paste("wt ~", var)
     t <- lm(as.formula(m_formula), data = data)
     t
	}

	 mod_func("temp", sim_dat1)
	 
	 mods <- mapply(mod_func)
	 

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




###### functions that do not work

#### 1. 
lm_func3 <- function(var, df){
		fit <- lm(wt ~ var, data = df)
		fits <- list(fit)
	}
	
	dfs <- list(sim_dat1, sim_dat2)
	vars <- c("temp", "oxy")
	
	test <- expand_grid(dfs, vars)
	
	mod_list <- mapply(lm_func3,
										 var = test$vars,
										 df = test$dfs,
										 SIMPLIFY = F)
	
#### 2.
	
	lm_func2 <- function(df){
	
			mod_temp <- lm(wt ~ temp, data = df)
			mod_oxy <- lm(wt ~ oxy, data = df)
			list(mod_temp, mod_oxy)
	}
	
	dfs <- list(sim_dat1, sim_dat2)
	
	mod_list <- lapply(dfs, lm_func2)
	

	#### 3.
	
	lm_func <- function(x, i){
	
			form <- paste0("wt ~", x)
			fits <- lm(as.formula(form), data = df_list[[i]])
			list(fits)
	}
	
	dfs <- list(sim_dat1, sim_dat2)
	
	mod_list <- mapply(lm_func, x = vars, i = 1:length(df_list), SIMPLIFY = F)
	
	
	#### 4. 
	
	temp_metrics = c("temp1", "temp2")
	
	df_list <- list(sim_dat_temp1, sim_dat_temp2)
	
	temp_forms_func <- function(temp_vars){
	 		 formulas <- paste("wt ~", temp_vars)
	 }
	 
	temp_forms <- lapply(temp_metrics, temp_forms_func)
	 
	
	temp_results = map_df(1:length(df_list), function(i){
  
   fits <- list(lm(as.formula(temp_forms), data = df_list[[i]]))
  
})

results

### 5.

	vars = c("temp1", "temp2")

	 form_func <- function(preds){
	 		 formulas <- paste("wt ~", preds)
	 		 
	 }
	 
	forms <- sapply(vars, form_func)
	
	df_list <- list(sim_dat_temp1, sim_dat_temp2)
	 
	fits_func <- function(x){
			 for (i in length(forms))
			 fit <- lm(as.formula(forms[[i]]), data = x)
			 
	 }
	 
 test <- lapply(df_list, fits_func)

 ### 6. 
 	#test <- mapply(function(df, z) mutate_(df, z=z), dfs, 3:4, SIMPLIFY=FALSE)

	test <- mapply(function(var, df) lm(wt ~ var, data = df), df = test$dfs, var = test$vars, SIMPLIFY=FALSE)

	### 7. 
	
	# try again
		
	# add col for temp or oxy
	sim_dat1_temp <- sim_dat1 %>% mutate(var = "temp")
	sim_dat1_oxy <- sim_dat1 %>% mutate(var = "oxy")
	sim_dat1_all <- bind_rows(sim_dat1_temp, sim_dat1_oxy)
	
	sim_dat2_temp <- sim_dat2 %>% mutate(var = "temp")
	sim_dat2_oxy <- sim_dat2 %>% mutate(var = "oxy")
	sim_dat2_all <- bind_rows(sim_dat2_temp, sim_dat2_oxy)
	
	df_list <- list(sim_dat1_all, sim_dat2_all)
	
	regression_func <- function(df, x){
    
     if(var ==temp){
            
            fit <- lm(wt ~ temp, data = df)
            return(coef(fit))
     }
    
     if(var ==oxy){
         
            fit <- lm(wt ~ oxy, data = df)
            return(coef(fit))

     }}


vars <- unique(sim_dat1_all$var)

mod_list <- mapply(regression_func, df = df_list, x = vars)


	#### 8. 

	 vars = c("temp", "oxy")
	 dfs <- list(sim_dat1, sim_dat2)

	 form_func <- function(preds){
	 		  paste("wt ~", preds)
	 }
	 
	 formulas <- sapply(vars, form_func)
	 
	 fits <- list()
	 
	 	for (i in length(formulas)){
	 		for (j in 1:4){
	 		
			 fits[[j]] <- lm(as.formula(formulas[[i]]), data = sim_dat1)
	 }}
	 
	 test <- mapply(lm_func, preds = vars, df = dfs, SIMPLIFY = F)

