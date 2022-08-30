# gams using gam() and bs = "re"

	# filter out 

	#### PCOD ####
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	pcod_dat <- spec_temp_dat[[1]] 
	
	pcod_dat <- pcod_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	pcod_dat <- pcod_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	pcod_age_sum <- pcod_dat %>%
		group_by(age_f) %>%
		summarize(n())
	
	pcod_dat <- pcod_dat  %>% filter(between(age, 2, 28))
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	base_mod <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pcod_dat,
									method = "REML")
  
	# 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_mod <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) + s(mean_temp) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pcod_dat,
									method = "REML")
	
	# 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) + s(mean_temp, by = age_f) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pcod_dat,
									method = "REML")
	
	visreg(temp_age_int_mod, xvar = "mean_temp", by = "age_f")
	
	
	### with age as a covariate
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	base_mod <- gam(log_wt ~  age_f + t2(latitude, longitude) + s(jday) +
								s(cohort, bs = 're') +
								s(haul, by=year, bs ="re"),
								data = pcod_dat,
								method = "REML")
	
	 
	# 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_mod <- gam(log_wt_scaled ~  age_f + t2(latitude, longitude) + s(jday) + s(mean_temp) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pcod_dat,
									method = "REML")
	
	# 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod <- gam(log_wt_scaled ~  age_f + t2(latitude, longitude) + s(jday) + s(mean_temp, by = age_f) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pcod_dat,
									method = "REML")


	#### pollock ####
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	pollock_dat <- spec_temp_dat[[2]] 
	
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	pollock_age_sum <- pollock_dat %>%
		group_by(age_f) %>%
		summarize(n())
	
	pollock_dat <- pollock_dat  %>% filter(between(age, 1, 10))
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	base_mod_pol <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pollock_dat,
									method = "REML")
  
	# 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_mod_pol <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) + s(mean_temp) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pollock_dat,
									method = "REML")
	
	# 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_pol <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) + s(mean_temp, by = age_f) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pollock_dat,
									method = "REML")
	
	visreg(temp_age_int_mod, xvar = "mean_temp", by = "age_f")
	
	
#	### with age as a covariate
#	
#	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
#	base_mod <- gam(log_wt ~  age_f + t2(latitude, longitude) + s(jday) +
#								s(cohort, bs = 're') +
#								s(haul, by=year, bs ="re"),
#								data = pcod_dat,
#								method = "REML")
#	
#	 
#	# 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
#	temp_mod <- gam(log_wt_scaled ~  age_f + t2(latitude, longitude) + s(jday) + s(mean_temp) +
#									s(cohort, bs = 're') +
#									s(haul, by=year, bs ="re"),
#									data = pcod_dat,
#									method = "REML")
#	
#	# 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
#	temp_age_int_mod <- gam(log_wt_scaled ~  age_f + t2(latitude, longitude) + s(jday) + s(mean_temp, by = age_f) +
#									s(cohort, bs = 're') +
#									s(haul, by=year, bs ="re"),
#									data = pcod_dat,
#									method = "REML")
#
	### yfin sole ####
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	yfinsole_dat <- spec_temp_dat[[3]] 
	
	yfinsole_dat <- yfinsole_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	yfinsole_dat <- yfinsole_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	yfinsole_age_sum <- yfinsole_dat %>%
		group_by(age_f) %>%
		summarize(n())
	
	pollock_dat <- pollock_dat  %>% filter(between(age, 1, 10))
	
	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
	base_mod_pol <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pollock_dat,
									method = "REML")
  
	# 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_mod_pol <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) + s(mean_temp) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pollock_dat,
									method = "REML")
	
	# 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod_pol <- gam(log_wt_scaled ~  t2(latitude, longitude) + s(jday) + s(mean_temp, by = age_f) +
									s(cohort, bs = 're') +
									s(haul, by=year, bs ="re"),
									data = pollock_dat,
									method = "REML")
	
	visreg(temp_age_int_mod, xvar = "mean_temp", by = "age_f")
	
	
#	### with age as a covariate
#	
#	# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year
#	base_mod <- gam(log_wt ~  age_f + t2(latitude, longitude) + s(jday) +
#								s(cohort, bs = 're') +
#								s(haul, by=year, bs ="re"),
#								data = pcod_dat,
#								method = "REML")
#	
#	 
#	# 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
#	temp_mod <- gam(log_wt_scaled ~  age_f + t2(latitude, longitude) + s(jday) + s(mean_temp) +
#									s(cohort, bs = 're') +
#									s(haul, by=year, bs ="re"),
#									data = pcod_dat,
#									method = "REML")
#	
#	# 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
#	temp_age_int_mod <- gam(log_wt_scaled ~  age_f + t2(latitude, longitude) + s(jday) + s(mean_temp, by = age_f) +
#									s(cohort, bs = 're') +
#									s(haul, by=year, bs ="re"),
#									data = pcod_dat,
#									method = "REML")
#
	
	