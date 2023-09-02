

# avg temp to maturity 

 test_dat <- 	pollock_dat %>%
 			dplyr::select(age, cohort) 
 
 test_dat2 <- test_dat %>%
 	mutate(year_range_start = cohort)
 				 	
 	test_dat2 <- test_dat2 %>%
 		mutate(year_range_end = case_when(
 			age == "1" ~ as.numeric(cohort),
 			age == "2" ~ as.numeric(cohort + 1),
 			age == "3" ~ as.numeric(cohort + 2),
 			age >= "4" ~ as.numeric(cohort + 3)))
 	
 	test_dat3 <- test_dat2 %>%
 		mutate(temp_yr_range = paste(year_range_start, year_range_end, sep = ":"),
 					 year = temp_yr_range)
 	
 	a[a$type=="DEBIT",]$Transaction <- -a[a$type=="DEBIT",]$Transaction
 	
 	test_dat3
 	
 	
 	##### try something else
 	
 	# unique age and cohorts
  pollock_dat_cohorts <- pollock_dat %>%
 		dplyr::select(age, cohort) %>%
  	distinct() %>%
  	group_by(age)

	pollock_dat_list <- group_split(pollock_dat_cohorts)

	# age 1
	pollock_age1_dat <- pollock_dat_list[[1]] %>% 
		mutate(year0 = cohort,
					 year1 = cohort + 1)
	
	pollock_age1_dat <- 
		left_join(pollock_age1_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) 

	pollock_age1_dat <- pollock_age1_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)


	# age 2
	pollock_age2_dat <- pollock_dat_list[[2]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2)
	
	pollock_age2_dat <- 
		left_join(pollock_age2_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) 

	
	pollock_age2_dat <- pollock_age2_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 3
	pollock_age3_dat <- pollock_dat_list[[3]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3)
																 
	pollock_age3_dat <- 
		left_join(pollock_age3_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	pollock_age3_dat <- pollock_age3_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 4
	pollock_age4_dat <- pollock_dat_list[c(4:10)] %>%
		bind_rows()
	
	pollock_age4_dat <- pollock_age4_dat %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4)
		
	pollock_age4_dat <- 
		left_join(pollock_age4_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) 
	
	pollock_age4_dat <- pollock_age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS")))) %>%
		select(age, cohort, temp_mat)
	
	pollock_temp_mat <- bind_rows(pollock_age1_dat, pollock_age2_dat, pollock_age3_dat, pollock_age4_dat)

	pollock_dat <- left_join(pollock_dat, pollock_temp_mat, by = c("age", "cohort"))
	
	
	
	
	
	
	
	
	#### create a function to do this for all species ####
	
	temp_to_mat_func <- function(df){
		
 	# unique age and cohorts
  dat_cohorts <- pollock_dat %>% ### change this
 		dplyr::select(age, cohort) %>%
  	distinct() %>%
  	group_by(age)

	dat_list <- group_split(pollock_dat_cohorts)

	# age 1
	age1_dat <- dat_list[[1]] %>% 
		mutate(year0 = cohort-1,
					 year1 = cohort)
	
	age1_dat <- 
		left_join(age1_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) 
	
	age1_dat <- age1_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	

	# age 2
	age2_dat <- dat_list[[2]] %>%
		mutate(year0 = cohort - 1,
					 year1 = cohort,
					 year2 = cohort + 1)
	
	age2_dat <- 
		left_join(age2_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year'))

	age2_dat <- age2_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 3
	age3_dat <- dat_list[[3]] %>%
		mutate(year0 = cohort - 1,
					 year1 = cohort,
					 year2 = cohort + 1,
					 year3 = cohort + 2)
																 
	age3_dat <- 
		left_join(age3_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	age3_dat <- age3_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 4
	age4_dat <- dat_list[c(4:10)] %>%
		bind_rows()
	
	age4_dat <- age4_dat %>%
		mutate(year0 = cohort - 1,
					 year1 = cohort,
					 year2 = cohort + 1,
					 year3 = cohort + 2,
					 year4 = cohort + 3)
		
	age4_dat <- 
		left_join(age4_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) 
	
	age4_dat <- age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS")))) %>%
		select(age, cohort, temp_mat)
	
	temp_mat <- bind_rows(age1_dat, age2_dat, age3_dat, age4_dat)

	dat <- left_join(df, temp_mat, by = c("age", "cohort"))
	
	}

	dat_test <- lapply(list(pollock_dat, pcod_dat), temp_to_mat_func)
	
										
								



	######################## from older code
	
		# temp avg to maturity ####
	
	# pollock ####

 	# unique age and cohorts
  pollock_dat_cohorts <- pollock_dat %>%
 		dplyr::select(age, cohort) %>%
  	distinct() %>%
  	group_by(age)

	pollock_dat_list <- group_split(pollock_dat_cohorts)

	# age 1
	pollock_age1_dat <- pollock_dat_list[[1]] %>% 
		mutate(year0 = cohort,
					 year1 = cohort + 1)
	
	pollock_age1_dat <- 
		left_join(pollock_age1_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) 

	pollock_age1_dat <- pollock_age1_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)


	# age 2
	pollock_age2_dat <- pollock_dat_list[[2]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2)
	
	pollock_age2_dat <- 
		left_join(pollock_age2_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) 

	
	pollock_age2_dat <- pollock_age2_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 3
	pollock_age3_dat <- pollock_dat_list[[3]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3)
																 
	pollock_age3_dat <- 
		left_join(pollock_age3_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	pollock_age3_dat <- pollock_age3_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 4
	pollock_age4_dat <- pollock_dat_list[c(4:10)] %>%
		bind_rows()
	
	pollock_age4_dat <- pollock_age4_dat %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4)
		
	pollock_age4_dat <- 
		left_join(pollock_age4_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) 
	
	pollock_age4_dat <- pollock_age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS")))) %>%
		select(age, cohort, temp_mat)
	
	pollock_temp_mat <- bind_rows(pollock_age1_dat, pollock_age2_dat, pollock_age3_dat, pollock_age4_dat)

	pollock_dat <- left_join(pollock_dat, pollock_temp_mat, by = c("age", "cohort"))
	
	# pcod ####
	
 	# unique age and cohorts
  pcod_dat_cohorts <- pcod_dat %>%
 		dplyr::select(age, cohort) %>%
  	distinct() %>%
  	group_by(age)

	pcod_dat_list <- group_split(pcod_dat_cohorts)

	# age 1
	pcod_age1_dat <- pcod_dat_list[[1]] %>% 
		mutate(year0 = cohort,
					 year1 = cohort + 1)
	
	pcod_age1_dat <- 
		left_join(pcod_age1_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) 

	pcod_age1_dat <- pcod_age1_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)


	# age 2
	pcod_age2_dat <- pcod_dat_list[[2]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2)
	
	pcod_age2_dat <- 
		left_join(pcod_age2_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) 

	
	pcod_age2_dat <- pcod_age2_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 3
	pcod_age3_dat <- pcod_dat_list[[3]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3)
																 
	pcod_age3_dat <- 
		left_join(pcod_age3_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	pcod_age3_dat <- pcod_age3_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 4
	pcod_age4_dat <- pcod_dat_list[[4]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4)
																 
	pcod_age4_dat <- 
		left_join(pcod_age4_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year'))
	
	pcod_age4_dat <- pcod_age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 5
	pcod_age5_dat <- pcod_dat_list[c(5:10)] %>%
		bind_rows()
	
	pcod_age5_dat <- pcod_age5_dat %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4,
					 year5 = cohort + 5)
		
	pcod_age5_dat <- 
		left_join(pcod_age5_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) %>%
		left_join(., hind_temps, by=c('year5'='year')) 

	
	pcod_age5_dat <- pcod_age5_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS")))) %>%
		select(age, cohort, temp_mat)
	
	pcod_temp_mat <- bind_rows(pcod_age1_dat, pcod_age2_dat, pcod_age3_dat, pcod_age4_dat)

	pcod_dat <- left_join(pcod_dat, pcod_temp_mat, by = c("age", "cohort"))
	
	# yellowfin ####
	
 	# unique age and cohorts
  yfin_dat_cohorts <- yfinsole_dat %>%
 		dplyr::select(age, cohort) %>%
  	distinct() %>%
  	group_by(age)

	yfin_dat_list <- group_split(yfin_dat_cohorts)

	# age 1
	yfin_age1_dat <- yfin_dat_list[[1]] %>% 
		mutate(year0 = cohort,
					 year1 = cohort + 1)
	
	yfin_age1_dat <- 
		left_join(yfin_age1_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) 

	yfin_age1_dat <- yfin_age1_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)


	# age 2
	yfin_age2_dat <- yfin_dat_list[[2]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2)
	
	yfin_age2_dat <- 
		left_join(yfin_age2_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) 

	
	yfin_age2_dat <- yfin_age2_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 3
	yfin_age3_dat <- yfin_dat_list[[3]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3)
																 
	yfin_age3_dat <- 
		left_join(yfin_age3_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year'))
	
	yfin_age3_dat <- yfin_age3_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 4
	yfin_age4_dat <- yfin_dat_list[[4]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4)
																 
	yfin_age4_dat <- 
		left_join(yfin_age4_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year'))
	
	yfin_age4_dat <- yfin_age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 5
		yfin_age5_dat <- yfin_dat_list[[5]] %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4,
					 year5 = cohort + 5)
																 
	yfin_age5_dat <- 
		left_join(yfin_age5_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) %>%
		left_join(., hind_temps, by=c('year5'='year'))
	
	yfin_age4_dat <- yfin_age4_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS"))))  %>%
		select(age, cohort, temp_mat)
	
	# age 6
	
	# age 7
	
	# age 8
	
	# age 9 
	
	# age 10
	

	# age 11 through 17 ---- EDIT ########
	pcod_age5_dat <- pcod_dat_list[c(11:10)] %>%
		bind_rows()
	
	pcod_age5_dat <- pcod_age5_dat %>%
		mutate(year0 = cohort,
					 year1 = cohort + 1,
					 year2 = cohort + 2,
					 year3 = cohort + 3,
					 year4 = cohort + 4,
					 year5 = cohort + 5)
		
	pcod_age5_dat <- 
		left_join(pcod_age5_dat, hind_temps, by=c('year0'='year')) %>%
		left_join(., hind_temps, by=c('year1'='year')) %>%
		left_join(., hind_temps, by=c('year2'='year')) %>%
		left_join(., hind_temps, by=c('year3'='year')) %>%
		left_join(., hind_temps, by=c('year4'='year')) %>%
		left_join(., hind_temps, by=c('year5'='year')) 

	
	pcod_age5_dat <- pcod_age5_dat %>% 
		select(age, cohort, contains("SEBS")) %>%
		mutate(temp_mat = rowMeans(select(., starts_with("SEBS")))) %>%
		select(age, cohort, temp_mat)
	
	pcod_temp_mat <- bind_rows(pcod_age1_dat, pcod_age2_dat, pcod_age3_dat, pcod_age4_dat)

	pcod_dat <- left_join(pcod_dat, pcod_temp_mat, by = c("age", "cohort"))
	
	
	
	
	