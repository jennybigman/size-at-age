# remotes::install_github("pbs-assess/sdmTMB", ref = "mvrfrw")
library(sdmTMB)

library(ggplot2)
library(dplyr)

load("data/pcod_dat.Rdata")

# need to drop age = 10 because there are none in many years?
# could bin into plus group?

d <- pcod_dat
d <- mutate(d, age = ifelse(age >= 9, 9, age))
d <- droplevels(d)
d$year_age <- factor(paste(d$year, d$age))
(all_time <- seq(min(d$year), max(d$year)))

mesh <- make_mesh(
	d, c("X", "Y"),
	fmesher_func = fmesher::fm_mesh_2d_inla,
	cutoff = 50,
	offset = c(60, 70)
)

# try adding 'groups' for a multivariate random walk on the mean:
fit <-
	sdmTMB(
		log_wt ~ 0 + age_f * poly(yrprior_btemp, 2),
		data = d,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "iid",
		groups = "age_f",
		control = sdmTMBcontrol(profile = "b_j"),
		time = "year",
		share_range = FALSE,
		extra_time = all_time,
		silent = FALSE
	)

p <- get_pars(fit)
# correlation of random walks:
2 * plogis(p$mvrw_rho) - 1

matplot(t(p$mvrw_u), type = "l", lty = 1)

mvrw <- p$mvrw_u |> reshape::melt() |>
	rename(year = X2, age = X1)

ggplot(mvrw, aes(year, value, colour = age, group = age)) + 
	geom_line() +
	scale_colour_viridis_c() +
	ggtitle("Random walk value, starting at 0")

# grab fixed effects to add on:
b <- tidy(fit) |> 
	filter(grepl("age_f[0-9]$", term)) |> 
	mutate(age = gsub("age_f", "", term)) |> 
	mutate(age = as.numeric(age)) |> 
	select(age, fixed_est = estimate)
mvrw <- left_join(mvrw, b)

ggplot(mvrw, aes(year, value + fixed_est, colour = age, group = age)) +
	geom_line() +
	scale_colour_viridis_c() +
	ggtitle("Random walk value, starting at fixed effect estimate")

# now do again with spatiotemporal fields by age too...
# all SDs of those fields shared...
# this many are needed:
n_year_age <- length(unique(d$age)) * length(unique(d$year))

fit2 <-
	sdmTMB(
		log_wt ~ 0 + age_f * poly(yrprior_btemp, 2),
		data = d,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "off", # done in the spatial_varying argument
		spatial_varying = ~ 0 + year_age,
		groups = "age_f",
		control = sdmTMBcontrol(
			profile = "b_j",
			map = list(ln_tau_Z = factor(rep(1, n_year_age))),
			multiphase = FALSE
		),
		time = "year",
		silent = FALSE
	)
sanity(fit2)
fit2

get_mvrw <- function(object) {
	p <- get_pars(object)
	cat("Correlation of random walks:", 2 * plogis(p$mvrw_rho) - 1, "\n")
	
	mvrw <- p$mvrw_u |> reshape::melt() |>
		rename(year = X2, age = X1)
	b <- tidy(object) |> 
		filter(grepl("age_f[0-9]$", term)) |> 
		mutate(age = gsub("age_f", "", term)) |> 
		mutate(age = as.numeric(age)) |> 
		select(age, fixed_est = estimate)
	left_join(mvrw, b, by = join_by(age))
}

mvrw <- get_mvrw(fit2)

ggplot(mvrw, aes(year, value + fixed_est, colour = age, group = age)) +
	geom_line() +
	scale_colour_viridis_c() +
	ggtitle("Random walk value, starting at fixed effect estimate")

# what about allowing each age to have its own SD?
# create a map vector for that...
mm <- model.matrix(~0 + year_age, data = d) |> colnames()
map_vec <- gsub("year_age[0-9]+ ", "", mm)

# but share last 2 are too data sparse in last age bin:
table(d$age, d$year)
# mirror their SD estimate:
map_vec[map_vec == "9"] <- "8"
map_vec <- factor(map_vec)

fit3 <-
	sdmTMB(
		log_wt ~ 0 + age_f * poly(yrprior_btemp, 2),
		data = d,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "off",
		spatial_varying = ~ 0 + year_age,
		groups = "age_f",
		control = sdmTMBcontrol(
			profile = "b_j",
			map = list(ln_tau_Z = map_vec),
			multiphase = FALSE
		),
		time = "year",
		silent = FALSE
	)
sanity(fit3)
fit3
AIC(fit2, fit3) # much better

get_mvrw(fit3) |> 
  ggplot(aes(year, value + fixed_est, colour = age, group = age)) +
	geom_line() +
	scale_colour_viridis_c()

# fields?
tidy(fit3, "ran_pars")
# SD of fields decreases with age

p <- predict(fit3, filter(d, year == max(d$year))) # pick a year for visualization
p |> 
	# filter(year == max(d$year)) |> # pick one
  ggplot(aes(X, Y, colour = omega_s)) + geom_point() +
	coord_fixed() +
	scale_colour_gradient2() +
	ggtitle("Overall spatial random field")

# OK, this is a mess! https://xkcd.com/208/
fields <- p |> 
	# filter(year == max(d$year)) |> # pick any one
  select(X, Y, starts_with("zeta_s")) |> 
  tidyr::pivot_longer(cols = -c(X, Y)) |> 
	mutate(short = gsub("zeta_s_year_age", "", name)) |> 
	mutate(year = substr(short, 1, 4)) |> 
	mutate(age = substr(short, 6, 99))

ggplot(fields, aes(X, Y, colour = value)) + geom_point() +
	scale_colour_gradient2() +
	facet_grid(year~age) +
	coord_fixed() +
	ggtitle("Age-year-specific random fields")

# alternative: each age gets a random walk random field:
# not implemented... could be...
# fit4 <-
# 	sdmTMB(
# 		log_wt ~ 0 + age_f * poly(yrprior_btemp, 2),
# 		data = d,
# 		mesh = mesh,
# 		spatial = "on",
# 		spatiotemporal = "rw", # BY CATEGORY NOW!
# 		# spatial_varying = ~ 0 + year_age, # CHANGED
# 		groups = "age_f",
# 		control = sdmTMBcontrol(
# 			profile = "b_j",
# 			map = list(ln_tau_Z = map_vec),
# 			multiphase = FALSE
# 		),
# 		time = "year",
# 		silent = FALSE
# 	)
