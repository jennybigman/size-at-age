


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

