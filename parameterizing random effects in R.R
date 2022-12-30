# figure out how to parameterize random effects in gams and bams

	library(lme4)
	library(gratia)

	# data to play with 

	pollock_dat <- spec_temp_dat[[2]] %>%
		rename(julian_day = jday)

	pollock_dat$age_f <- as_factor(pollock_dat$age)
	
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

	
	# example from Gavin Simpson

	m1_lmer <- lmer(response ~ treatment:transf_time +
                    (1 | subject) + (0 + transf_time | subject),
                data = rats)

	m1_gam_reml <- gam(response ~ treatment:transf_time +
	                  s(subject, bs = 're') +
	                  s(subject, transf_time, bs = 're'),
	              data = rats, method = 'REML')
	
	m1_gam <- gam(response ~ treatment:transf_time +
	                 s(subject, bs = 're') +
	                 s(subject, transf_time, bs = 're'),
	             data = rats)


	# extract random effect variance for comparison
	summary(m1_lmer)$varcor 
	
	variance_comp(m1_gam)

	#### test with my data ####

	## exactly like example -- not the same fixed effect values ####

	lmer_mod <- lmer(log_wt ~ julian_day +
									 (1 | year) +
									 (0 + haul|year),
									 data = pollock_dat)
	
	bam_mod <- bam(log_wt ~ julian_day +
								 	s(year, bs = "re") + 
								 	s(year, haul, bs = "re"),
														data = pollock_dat,
														REML = TRUE)
	
	gam_mod <- gam(log_wt ~ julian_day + 
								 	s(year, bs = "re") + 
								 	s(year, haul, bs = "re"),
														data = pollock_dat,
														REML = TRUE)
	
	fixef(lmer_mod)
	coef(bam_mod)[1:2]
	coef(gam_mod)[1:2]

	# same random effect variance? 
	summary(lmer_mod)
	variance_comp(bam_mod)
	summary(gam_mod)

	## intercept only with year/haul -- same fixed effects, different variance of RE ####

	lmer_mod <- lmer(log_wt ~ julian_day + (1|year/haul),
														data = pollock_dat)
	
	bam_mod <- bam(log_wt ~ julian_day + 
								 	s(year, haul, bs = "re"),
														data = pollock_dat,
														REML = TRUE)
	
	gam_mod <- gam(log_wt ~ julian_day + 
								 	s(year, haul, bs = "re"),
														data = pollock_dat,
														REML = TRUE)

	#gamm4_mod <- gamm4(log_wt ~ julian_day,
	#                			 random=~(1|year/haul), data= pollock_dat, REML=TRUE) 
	#
	#gamm_mod <- gamm(log_wt ~ julian_day,
	#                random = list(year = ~1, haul = ~1), data= pollock_dat, REML=TRUE) 
	#
	#gamm_mod_gam <- gamm_mod$gam
	
	fixef(lmer_mod)
	coef(bam_mod)
	coef(gam_mod)
	
	summary(lmer_mod)$varcor
	variance_comp(bam_mod)
	
	## specify random effect with by -- same coef values, different RE variances ####

	lmer_mod <- lmer(log_wt ~ julian_day + (1|year/haul),
														data = pollock_dat)
	
	bam_mod <- bam(log_wt ~ julian_day + 
								 	s(haul, by = year, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	gam_mod <- gam(log_wt ~ julian_day + 
								 	s(haul, by = year, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	# fixed effects
	fixef(lmer_mod)
	coef(bam_mod)[1:2]
	coef(gam_mod)[1:2]

	# random effects
	summary(lmer_mod)$varcor
	variance_comp(bam_mod)
	
	summary(bam_mod)


	## compare with and without by= ####
	
	bam_mod1 <- bam(log_wt ~ julian_day + 
								 	s(haul, year, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	bam_mod2 <- bam(log_wt ~ julian_day + 
								 	s(haul, by = year, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	bam_mod3 <- bam(log_wt ~ julian_day + 
								 	s(year, haul, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	variance_comp(bam_mod1)
	variance_comp(bam_mod2)
	variance_comp(bam_mod3)
	


	# separate smooths -- different fixed effects 
	lmer_mod <- lmer(log_wt ~ julian_day + (1|year/haul),
														data = pollock_dat)
	
	bam_mod <- bam(log_wt ~ julian_day + 
								 	s(haul, bs = "re") +
								  s(year, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	gam_mod <- gam(log_wt ~ julian_day + 
								 	s(haul, bs = "re") +
								 	s(year, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	# fixed effects
	fixef(lmer_mod)
	coef(bam_mod)[1:2]
	coef(gam_mod)[1:2]
	
	# random effects
	summary(lmer_mod)$varcor
	variance_comp(bam_mod)
	
	summary(bam_mod)


	### try setting up data in a way where don't need random effects #### 

	year <- unique(pollock_dat$year)
	ID <- letters[1:23]
	
	years_id <- data.frame(year, ID)
	
	pollock_dat <- merge(pollock_dat, years_id, by = "year")
	
	pollock_dat <- pollock_dat %>%
		mutate(haul_id = paste(ID, haul, sep = "_"))
	
	pollock_dat <- pollock_dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))

	lmer_mod <- lmer(log_wt ~ julian_day + (1|year/haul),
														data = pollock_dat)
	
	lmer_mod2 <- lmer(log_wt ~ julian_day + (1|ID_f) + (1|haul_id_f),
														data = pollock_dat)
	
	bam_mod_reml <- bam(log_wt ~ s(julian_day) + 
								 	s(ID_f, bs = "re") +
								 	s(haul_id_f, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	gam_mod_reml <- gam(log_wt ~ s(julian_day) + 
								 	s(ID_f, bs = "re") +
								 	s(haul_id_f, bs = "re"),
								  data = pollock_dat,
									REML = TRUE)
	
	bam_mod <- bam(log_wt ~ s(julian_day) + 
							 	s(ID_f, bs = "re") +
							 	s(haul_id_f, bs = "re"),
								data = pollock_dat,
								method = "GCV.Cp")
	
	gam_mod <- gam(log_wt ~ s(julian_day) + 
								 	s(ID_f, bs = "re") +
								 	s(haul_id_f, bs = "re"),
								  data = pollock_dat)


	 # save
  saveRDS(bam_mod, 
  				file = here("./output/model output/pollock/test_bam_mod_gcv.rds"))
  saveRDS(gam_mod, 
  				file = here("./output/model output/pollock/test_gam_mod.rds"))
  saveRDS(bam_mod_reml, 
  				file = here("./output/model output/pollock/test_bam_mod_reml.rds"))
  saveRDS(gam_mod_reml, 
  				file = here("./output/model output/pollock/test_gam_mod_reml.rds"))

  # load
  bam_mod_gcv <- readRDS(here("./output/model output/pollock/test_bam_mod.rds"))
  gam_mod <- readRDS(here("./output/model output/pollock/test_gam_mod.rds"))

  bam_mod_reml <- readRDS(here("./output/model output/pollock/test_bam_mod_reml.rds"))
  gam_mod_reml <- readRDS(here("./output/model output/pollock/test_gam_mod_reml.rds"))

	# fixed effects
	fixef(lmer_mod)
	fixef(lmer_mod2)
	
	coef(bam_mod)[1:2]
	coef(gam_mod)[1:2]
	
	# random effects
	summary(lmer_mod)$varcor
	summary(lmer_mod2)$varcor
	
	variance_comp(bam_mod)
	mgcv::gam.vcomp()
	
	summary(bam_mod)
	summary(gam_mod)

	## with cohort ####


	lmer_mod_co <- lmer(log_wt ~ julian_day + (1|year/haul) + (1|cohort),
														data = pollock_dat)
	
	lmer_mod2_co <- lmer(log_wt ~ julian_day + (1|ID_f) + (1|haul_id_f) + (1|cohort),
														data = pollock_dat)
	
	bam_mod_co <- bam(log_wt ~ s(julian_day) + 
								 	s(ID_f, bs = "re") +
								 	s(haul_id_f, bs = "re") +
								 	s(cohort_f, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	gam_mod_co <- gam(log_wt ~ s(julian_day) + 
							 	s(ID_f, bs = "re") +
							 	s(haul_id_f, bs = "re") +
							 	s(cohort_f, bs = "re"),
								data = pollock_dat,
								REML = TRUE)

 # save
  saveRDS(bam_mod_co, 
  				file = here("./output/model output/pollock/test_bam_mod_co.rds"))
  saveRDS(gam_mod_co, 
  				file = here("./output/model output/pollock/test_gam_mod_co.rds"))

  # load
  bam_mod_co <- readRDS(here("./output/model output/pollock/test_bam_mod_co.rds"))
  gam_mod_co <- readRDS(here("./output/model output/pollock/test_gam_mod_co.rds"))

	# fixed effects
	fixef(lmer_mod_co)
	fixef(lmer_mod2_co)
	
	coef(bam_mod_co)[1:2]
	coef(gam_mod_co)[1:2]

		# random effects
	summary(lmer_mod_co)$varcor
	summary(lmer_mod2_co)$varcor
	
	variance_comp(bam_mod_co)
	mgcv::gam.vcomp(gam_mod_co)
	
	summary(bam_mod_co)
	summary(gam_mod_co)

	


 ## compare without setting REML = TRUE ####
	
	year <- unique(pollock_dat$year)
	ID <- letters[1:23]
	
	years_id <- data.frame(year, ID)
	
	pollock_dat <- merge(pollock_dat, years_id, by = "year")
	
	pollock_dat <- pollock_dat %>%
		mutate(haul_id = paste(ID, haul, sep = "_"))
	
	pollock_dat <- pollock_dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))

	lmer_mod <- lmer(log_wt ~ julian_day + (1|cohort),
														data = pollock_dat)
	
	bam_mod <- bam(log_wt ~ s(julian_day) + 
								 	s(cohort, bs = "re"),
									data = pollock_dat)
	
	bam_mod_reml <- bam(log_wt ~ s(julian_day) + 
								 	s(cohort, bs = "re"),
									data = pollock_dat,
									REML = TRUE)
	
	gam_mod <- gam(log_wt ~ s(julian_day) + 
								 	s(cohort, bs = "re") +
								  data = pollock_dat)
	
	gam_mod <- gam(log_wt ~ s(julian_day) + 
								 	s(cohort, bs = "re") +
								  data = pollock_dat,
								  REML = TRUE)


	fixef(lmer_mod)

  coef(bam_mod)[1:2]
  coef(bam_mod_reml)[1:2]
  
  summary(lmer_mod)$varcor
  variance_comp(bam_mod)    
  