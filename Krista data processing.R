# Krista data processing -- not complete because she lagged years

	#### data wrangling ####
	
	# read data 
	
	# from github
	# pollock_dat <- read_csv("https://raw.githubusercontent.com/mikelitzow/bold-new-pollock/master/data/survey%20data/Litzow_pollock_02032021.csv")

		
	# format dates for github data
	#pollock_dat$date <- mdy(pollock_dat$tow_date)
	#pollock_dat$month <- month(pollock_dat$date) # month of year
	#pollock_dat$week <- week(pollock_dat$date) # week of year
	#pollock_dat$year <- year(pollock_dat$date)
	#pollock_dat$julian_day <- yday(pollock_dat$date)
	
	# change all column names to lower
	#names(pollock_dat) <- tolower(names(pollock_dat))

	#write_csv(pollock_dat, here("./data/pollock_dat.csv"))

	# read from file
	pollock_dat <- read_csv(here("./data/pollock_dat.csv"))
	
	# SST dat
	clim_dat <- read_csv(file = here("./data/Krista data/climate data.csv"))
	
	# calculate cohort
	pollock_dat <- pollock_dat %>%
		mutate(cohort = year - age)
	
	# turn cohort and age into factors for the model and log variables
	pollock_dat <- pollock_dat %>%
		filter(weight > 0) %>%
		mutate(age_f = as.factor(age),
					 cohort_f = as.factor(cohort),
					 log_wt = log10(weight))
	
	# scale weights for each age
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		mutate(mean_wt_age = mean(log_wt),
					 sd_wt_age = sd(log_wt))
	
	pollock_dat <- pollock_dat %>%
		group_by(age_f) %>%
		rowwise() %>%
		mutate(log_wt_scaled = (log_wt - mean_wt_age)/sd_wt_age)
	
	# trim to 1999 forward & EBS only
	yrs_keep <- 1999:2019
	
	pollock_dat <- pollock_dat %>%
		filter(year %in% yrs_keep) 
	
	# filter by age following Oke et al. 2022
	pollock_dat <- pollock_dat  %>% filter(between(age, 1, 15))

	# SST
	clim_dat <- clim_dat %>%
		filter(year %in% yrs_keep) %>%
		select(year, south.sst.amj)
	
	# join SST and data together
	pollock_dat <- merge(pollock_dat, clim_dat, by = "year")
	
	# plot
	ggplot(pollock_dat, aes(x = south.sst.amj, y = log_wt_scaled)) +
		geom_point() +
		facet_wrap(~ age_f)

	# scale weight by age
	#scale.dat <- plyr::ddply(pollock_dat, "age", transform, sc.weight = scale(WEIGHT))
