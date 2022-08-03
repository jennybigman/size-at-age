	gam_func <- function(x){

# 1. weight ~ age + julian day + random effects of cohort +  haul nested within year

	base_mod <- bam(log_wt ~ s(age) + t2(latitude, longitude) + s(jday),
									random = ~ (1|cohort) + (1|year/haul),
									data = x)
	
	summary(base_mod)
  AICc(base_mod)

  # 2. weight ~ age + sst + julian day + random effects of cohort +  haul nested within year
	temp_age_mod <- bam(log_wt ~  s(age) + s(mean_temp) + 
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = pcod_dat)
	summary(temp_age_mod)
  AICc(temp_age_mod)
 
  # 3. weight ~ age + age*sst + julian day + random effects of cohort +  haul nested within year
	temp_age_int_mod <- bam(log_wt ~  s(age) + s(mean_temp) + s(mean_temp, by = age)  +
											t2(latitude, longitude) + s(jday),
											random = ~ (1|cohort) + (1|year/haul),
											data = pcod_dat)
	
	summary(temp_age_int_mod)
  AICc(temp_age_int_mod)
 