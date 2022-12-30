# models with cohort effect


#* change julian day to s(julian_day)

	pcod_temp_age_gam_co <- brm(log_wt ~ age_f + s(mean_sum_temp) + t2(latitude, longitude) +  s(julian_day) +
													(1|year/haul) + (1|cohort),
													data = pcod_dat,
													family = gaussian(),
													save_pars = save_pars(all = TRUE),
													#control = list(max_treedepth = 15, adapt_delta = 0.99),
                  				warmup = 20, iter = 100, chains = 4, cores = 4)
														

  # 2. weight ~ age * temp
	#* change julian day to s(julian_day)
	pcod_temp_int_age_gam_co <- brm(log_wt ~ age_f + s(mean_sum_temp, by = age_f) + 
																		t2(latitude, longitude) + s(julian_day) +
													(1|year/haul) +  (1|cohort),
													data = pcod_dat,
													family = gaussian(),
													save_pars = save_pars(all = TRUE),
													control = list(max_treedepth = 15, adapt_delta = 0.99),
                  				warmup = 2000, iter = 10000, chains = 4, cores = 4)
												
  # save models
	save(pcod_temp_age_gam_co, file = "./output/model output/pcod_temp_age_gam_co.rda")
	save(pcod_temp_int_age_gam_co, file = "./output/model output/pcod_temp_int_age_gam_co.rda")
	
	# load models
	pcod_temp_age_gam_co <- load(file = "./output/model output/pcod_temp_age_gam_co.rda")
	pcod_temp_int_age_gam_co <- load(file = "./output/model output/pcod_temp_int_age_gam_co.rda")

	loo(pcod_temp_age_gam_co) #looic = -17132.5 #elpd_loo = 8566.3
	loo(pcod_temp_int_age_gam_co) #looic =  -18680.3 #elpd_loo = 9340.2
