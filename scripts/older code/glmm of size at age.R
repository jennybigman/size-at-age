# read in pollock data from Litzow pollock-size gituhb repo

	library(here)
	library(tidyverse)
	library(lme4)
	library(readr)
	library(sjPlot)

	pollock_dat <- read_csv("https://raw.githubusercontent.com/mikelitzow/bold-new-pollock/master/data/survey%20data/Litzow_pollock_02032021.csv")

	pollock_dat <- pollock_dat %>%
		filter(weight > 0) %>%
		filter(Age > 0) %>%
		mutate(log_wt = log10(weight))

	glimpse(pollock_ebs_dat)	
	
	years <- sort(unique(pollock_ebs_dat$year))
	
	# 1. we fit a model with size changing over time
	size_time <- lmer(log_wt ~ year + (1 | year/Haul), dat = pollock_ebs_dat)
	summary(size_time)

	plot_model(size_time)
	AIC(size_time)
	
	# fit a model with size at age changing over time with and without an interaction
	
	size_age_time <- lmer(log_wt ~ as.factor(Age) + year + (1 | year/Haul), dat = pollock_ebs_dat)
	plot_model(size_age_time)

	summary(size_age_time)
	AIC(size_age_time)
	
	size_age_x_time <- lmer(log_wt ~ as.factor(Age) * year + (1 | year/Haul), dat = pollock_ebs_dat)
	plot_model(size_age_x_time)

	summary(size_age_x_time)
	AIC(size_age_x_time)
	
	
	
	