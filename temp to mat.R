

# avg temp to maturity 

 test_dat <- 	pollock_dat %>%
 			dplyr::select(age, cohort) 
 

 test_dat2 <- test_dat %>%
 	mutate(year_range_start = cohort - 1)
 				 	
 	test_dat2 <- test_dat2 %>%
 		mutate(year_range_end = case_when(
 			age == "1" ~ as.numeric(cohort),
 			age == "2" ~ as.numeric(cohort + 1),
 			age == "3" ~ as.numeric(cohort + 2),
 			age >= "4" ~ as.numeric(cohort + 3)))
 	
 	test_dat3 <- test_dat2 %>%
 		mutate(temp_yr_range = paste(year_range_start, year_range_end, sep = ":"))