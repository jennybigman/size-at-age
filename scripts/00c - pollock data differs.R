# compare datasets - Krista vs direct

	# data from GAP

	specimen_dat <- read.csv(file = here("./data/df_list_wrangled_names.csv"))

 	pol_test_df <- specimen_dat %>% 
 		filter(species == "df_pollock") %>%
 		mutate(age_f = as.factor(age))
	
 	pollock_age_sum <- pol_test_df %>%
		group_by(age_f) %>%
		summarize(n())

 	pollock_yr_sum <- pol_test_df %>%
		group_by(year) %>%
		summarize(n())
 	
 	# Oke et al. 2022 removed data prior to 1999
 	
 	pol_test_df_match <- pol_test_df %>%
 		filter(year > 1998) 
 	
 	pollock_age_sum_match <- pol_test_df_match %>%
		group_by(age_f) %>%
		summarize(n())

 	pollock_yr_sum_match <- pol_test_df_match %>%
		group_by(year) %>%
		summarize(n())
 	
 	
 	# Krista's data
 	lagdat <- read_csv(here("./data/Krista data/lagdat.csv")) %>%
 		mutate(age_f = as.factor(AGE)) %>%
 		rename(year = YEAR)

 	KO_age_sum <- lagdat %>%
		group_by(age_f) %>%
		summarize(n())

 	KO_yr_sum <- lagdat %>%
		group_by(year) %>%
		summarize(n())
 
 	# from Mike Litzow directly: https://github.com/mikelitzow/bold-new-pollock/tree/master/data/survey%20data
 	
 	ML_dat <- read_csv(here("./data/Litzow_pollock_02032021.csv"))
 	
 	ML_dat <- ML_dat %>% drop_na(WEIGHT, AGE) 
 	
 	ML_dat_trim <- ML_dat %>%
 		mutate(date = mdy(TOW_DATE),
 					 year = year(date),
 					 age_f = as.factor(AGE)) %>%
 		filter(year > 1998)
 	
 	ML_dat_trim_age_sum <- ML_dat_trim %>%
		group_by(age_f) %>%
		summarize(n())

 	ML_dat_trim_year_sum <- ML_dat_trim %>%
		group_by(year) %>%
		summarize(n())
 	
 	## summarize

	pol_age_sum_comp <- list(pollock_age_sum_match, KO_age_sum, ML_dat_trim_age_sum) %>%
  	reduce(full_join, by = "age_f") %>%
		setNames(c("age", "JSB", "KO", "ML"))
	
	pol_yr_sum_comp <- list(pollock_yr_sum_match, KO_yr_sum, ML_dat_trim_year_sum) %>%
  	reduce(full_join, by = "year") %>%
		setNames(c("year", "JSB", "KO", "ML"))
		

	