library(sdmTMB)
library(ggplot2)
library(dplyr)

load("data/pcod_dat.Rdata")

mesh <- make_mesh(
  pcod_dat, c("X", "Y"),
  fmesher_func = fmesher::fm_mesh_2d_inla,
  cutoff = 50,
  # max.edge = 30,
  offset = c(60, 70)
)

plot(mesh)
mesh$mesh$n

# model sent:

fit1 <-
  sdmTMB(
    log_wt ~ 0 + age_f * poly(yrprior_btemp, 3, raw = TRUE),
    data = pcod_dat,
    mesh = mesh,
    spatial = "on",
    spatiotemporal = "iid",
    time = "year",
    share_range = FALSE,
    silent = FALSE,
    priors = sdmTMBpriors(
      matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
      matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
    )
  )

sanity(fit1)
fit1

# need to drop age = 10 because there are none in many years:
# could bin into plus group?
d <- droplevels(filter(pcod_dat, age < 10))
mesh <- make_mesh(
	d, c("X", "Y"),
	fmesher_func = fmesher::fm_mesh_2d_inla,
	cutoff = 50,
	offset = c(60, 70)
)
fit3 <-
	sdmTMB(
		log_wt ~ age_f * year_f + age_f * poly(yrprior_btemp, 3, raw = TRUE),
		data = d,
		mesh = mesh,
		spatial = "on",
		spatiotemporal = "iid",
		control = sdmTMBcontrol(profile = "b_j"),
		time = "year",
		share_range = FALSE,
		silent = FALSE,
		priors = sdmTMBpriors(
			matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
			matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
		)
	)

fit4 <- update(fit3, 
	formula. = log_wt ~ year_f + age_f * poly(yrprior_btemp, 3, raw = TRUE))

fit5 <- update(fit3, 
	formula. = log_wt ~ age_f * poly(yrprior_btemp, 3, raw = TRUE)) ##### this is the same as the model I was fitting in the very beginning

fit6 <- update(fit3,
	formula. = log_wt ~ s(year, by = age_f, k = 4) + age_f * poly(yrprior_btemp, 3, raw = TRUE), do_fit = FALSE)
.n <- length(as.numeric(fit6$tmb_params$ln_smooth_sigma))
# mapping smoother SDs to be shared
fit6 <- update(fit3,
	formula. = log_wt ~ s(year, by = age_f, k = 4) + age_f * poly(yrprior_btemp, 3, raw = TRUE), control = sdmTMBcontrol(map = list(ln_smooth_sigma = factor(rep(1, .n)))))

# doesn't converge:
# fit7 <- update(fit3,
# 	formula. = log_wt ~ s(age, by = year_f, k = 4) + age_f * poly(yrprior_btemp, 3, raw = TRUE), do_fit = FALSE)
# .n <- length(as.numeric(fit7$tmb_params$ln_smooth_sigma))
# # mapping smoother SDs to be shared
# fit7 <- update(fit3,
# 	formula. = log_wt ~ s(age, by = year_f, k = 4) + age_f * poly(yrprior_btemp, 3, raw = TRUE), control = sdmTMBcontrol(map = list(ln_smooth_sigma = factor(rep(1, .n)))))

d$fake_time <- factor(paste0(d$year, d$age))
fit8 <- update(fit3,
	formula. = log_wt ~ age_f * year_f + age_f * poly(yrprior_btemp, 3, raw = TRUE),
	time = "fake_time")
fit8
AIC(fit3, fit4, fit5, fit6, fit8) |> 
	as.data.frame() |> 
	arrange(AIC)

logLik(fit1)
logLik(fit2)

glimpse(pcod_dat)

# try a model with an AR1 on year for one age class:

max_year <- max(pcod_dat$year)
d <- dplyr::filter(pcod_dat, age_f == 1, year < max_year)
mesh_sub <- make_mesh(d, c("X", "Y"), mesh = mesh$mesh)
all_yrs <- seq(min(d$year), max(d$year) + 1L)
fit_age <-
  sdmTMB(
    log_wt ~ 1 + poly(yrprior_btemp, 3, raw = TRUE),
    data = d,
    mesh = mesh_sub,
    spatial = "on",
    time_varying = ~1,
    time_varying_type = "ar1",
    spatiotemporal = "iid",
    control = sdmTMBcontrol(profile = "b_j"),
    time = "year",
    extra_time = all_yrs,
    share_range = FALSE,
    silent = FALSE,
    priors = sdmTMBpriors(
      matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
      matern_s = pc_matern(range_gt = 300, sigma_lt = 2)
    )
  )


p <- get_pars(fit_age)
p$b_rw_t
p$b_rw_t[,,1]

fit_age
fit_age$sd_report
theta <- get_pars(fit_age)
2 * plogis(theta$rho_time_unscaled[1, 1]) - 1

nd <- dplyr::filter(pcod_dat, age_f == 1)
p <- predict(fit_age, newdata = nd)
psim <- predict(fit_age, newdata = nd, nsim = 200)

out <- purrr::map_dfr(unique(p$year), \(y) {
  i <- which(p$year == y)
  s <- apply(psim[i, ], 2, mean)
  l <- quantile(s, probs = 0.05)
  u <- quantile(s, probs = 0.95)
  e <- mean(s)
  data.frame(year = y, est_sim = e, lwr_sim = l, upr_sim = u)
})

obs <- group_by(p, year) |>
  summarise(obs_log_wt = mean(log_wt))

ggplot(out, aes(year, est_sim, ymin = lwr_sim, ymax = upr_sim)) +
  geom_ribbon(fill = "grey50") +
  geom_line() +
  theme_light() +
  geom_line(aes(year, obs_log_wt), inherit.aes = FALSE, colour = "red", data = obs)

# now do it over all ages?

fit_one_age <- function(this_age, yrs_forecast = 1L, 
	.formula = log_wt ~ 1 + poly(yrprior_btemp, 3, raw = TRUE)) {
	max_year <- max(pcod_dat$year)
  print(this_age)
  max_fit_year <- max_year - yrs_forecast
  d <- dplyr::filter(pcod_dat, age_f == this_age, year <= max_fit_year)
  mesh_sub <- make_mesh(d, c("X", "Y"), mesh = mesh$mesh)
  all_yrs <- seq(min(pcod_dat$year), max(pcod_dat$year))
  fit_age <-
    sdmTMB(
    	.formula,
      data = d,
      mesh = mesh_sub,
      spatial = "on",
      time_varying = ~1,
      time_varying_type = "rw0",
      spatiotemporal = "iid",
      # control = sdmTMBcontrol(profile = "b_j"),
      time = "year",
      extra_time = all_yrs,
      share_range = FALSE,
      silent = T,
      priors = sdmTMBpriors(
        matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
        matern_s = pc_matern(range_gt = 300, sigma_lt = 2),
        sigma_V = gamma_cv(0.2, 0.5) # Need sigma_V_priors branch!!
      )
    )

  # turned off AR1 for now, sometimes has convergence issues...

  # theta <- get_pars(fit_age)
  # ar1_rho <- 2 * plogis(theta$rho_time_unscaled[1,1]) - 1

  b <- tidy(fit_age, "ran_pars")
  if (b$estimate[b$term == "sigma_E"] < 0.01) {
    fit_age <- update(fit_age, spatiotemporal = "off")
  }

  nd <- dplyr::filter(pcod_dat, age_f == this_age)
  set.seed(1)
  psim <- predict(fit_age, newdata = nd, nsim = 200L)
  out <- purrr::map_dfr(unique(nd$year), \(y) {
    i <- which(nd$year == y)
    if (length(i) <= 2) {
      return(data.frame(year = y, est_sim = NA, lwr_sim = NA, upr_sim = NA, ar1_rho = NA))
    }
    s <- apply(psim[i, ], 2, mean)
    l <- quantile(s, probs = 0.05)
    u <- quantile(s, probs = 0.95)
    e <- mean(s)
    data.frame(year = y, est_sim = e, lwr_sim = l, upr_sim = u, ar1_rho = NA)
  })

  obs <- group_by(nd, year) |>
    summarise(obs_log_wt = mean(log_wt), n_obs = n())

  b <- tidy(fit_age)
  b0 <- b$estimate[1]
  b1 <- b$estimate[2]
  b2 <- b$estimate[3]
  b3 <- b$estimate[4]

  nd2 <- dplyr::filter(nd, year > max_fit_year)
  p <- predict(fit_age, newdata = nd2)
  phi <- exp(get_pars(fit_age)$ln_phi)
  elpd <- sum(dnorm(p$est, nd2$log_wt, phi, log = TRUE))

  dplyr::left_join(obs, out, by = join_by(year)) |>
    mutate(age_f = this_age) |>
    mutate(b0 = b0, b1 = b1, b2 = b2, b3 = b3, elpd = elpd)
}

# test
# o <- fit_one_age(1)

# Fit all ages:
library(future)
plan(multisession)
all_ages <- as.numeric(sort(unique(as.character(pcod_dat$age_f))))
# out <- purrr::map_dfr(all_ages, fit_one_age)
out <- furrr::future_map_dfr(all_ages, fit_one_age)

out |>
  filter(n_obs > 2) |>
  ggplot(aes(year, est_sim, ymin = lwr_sim, ymax = upr_sim)) +
  geom_ribbon(fill = "grey50") +
  geom_linerange() +
  theme_light() +
  geom_line(aes(year, obs_log_wt), inherit.aes = FALSE, colour = "red") +
  facet_wrap(~age_f)

# 2 years forecast?

# some more convergence issues; switch off AR1 for now:
# out <- purrr::map_dfr(all_ages, fit_one_age, yrs_forecast = 2L)
out <- furrr::future_map_dfr(all_ages, fit_one_age, yrs_forecast = 2L)

out |>
  filter(n_obs > 2) |>
  ggplot(aes(year, est_sim, ymin = lwr_sim, ymax = upr_sim)) +
  geom_ribbon(fill = "grey50") +
  geom_linerange() +
  theme_light() +
  geom_line(aes(year, obs_log_wt), inherit.aes = FALSE, colour = "red") +
  facet_wrap(~age_f, scales = "free_y")

# what about poly params?
bb <- select(out, age_f, b0:b3) |> distinct()
bb |>
  tidyr::pivot_longer(cols = -age_f) |>
  ggplot(aes(value, age_f)) +
  facet_wrap(~name, scales = "free_x") +
  geom_point()

xx <- seq(min(pcod_dat$yrprior_btemp), max(pcod_dat$yrprior_btemp), length.out = 200L)

polys <- purrr::map_dfr(all_ages, \(a) {
  b0 <- bb[bb$age_f == a, "b0", drop = TRUE]
  b1 <- bb[bb$age_f == a, "b1", drop = TRUE]
  b2 <- bb[bb$age_f == a, "b2", drop = TRUE]
  b3 <- bb[bb$age_f == a, "b3", drop = TRUE]
  data.frame(age_f = a, yrprior_btemp = xx, y = b0 + b1 * xx + b2 * xx^2 + b3 * xx^3)
})

polys |>
  ggplot(aes(yrprior_btemp, y, colour = as.factor(age_f))) +
  geom_line()

polys |>
  ggplot(aes(yrprior_btemp, y, colour = as.factor(age_f))) +
  geom_line() +
  facet_wrap(~age_f, scales = "free_y")

# without intercept!?

polys2 <- purrr::map_dfr(all_ages, \(a) {
  b0 <- bb[bb$age_f == a, "b0", drop = TRUE]
  b1 <- bb[bb$age_f == a, "b1", drop = TRUE]
  b2 <- bb[bb$age_f == a, "b2", drop = TRUE]
  b3 <- bb[bb$age_f == a, "b3", drop = TRUE]
  data.frame(age_f = a, yrprior_btemp = xx, y = b1 * xx + b2 * xx^2 + b3 * xx^3)
})
polys2 |>
  ggplot(aes(yrprior_btemp, y, colour = as.factor(age_f))) +
  geom_line() +
  scale_color_viridis_d(option = "C") +
  theme_light() +
  geom_point(data = pcod_dat, mapping = aes(x = yrprior_btemp), y = jitter(rep(-0.3, nrow(pcod_dat)), 0.5), inherit.aes = FALSE, pch = 21, alpha = 0.02, col = "grey40")

# ??

# only show where we have plenty of data:

polys3 <- purrr::map_dfr(all_ages, \(a) {
  dd <- dplyr::filter(pcod_dat, age_f == a)
  l <- quantile(dd$yrprior_btemp, probs = 0.05)
  u <- quantile(dd$yrprior_btemp, probs = 0.95)
  xx <- seq(l, u, length.out = 200L)
  b0 <- bb[bb$age_f == a, "b0", drop = TRUE]
  b1 <- bb[bb$age_f == a, "b1", drop = TRUE]
  b2 <- bb[bb$age_f == a, "b2", drop = TRUE]
  b3 <- bb[bb$age_f == a, "b3", drop = TRUE]
  data.frame(age_f = a, yrprior_btemp = xx, y = b1 * xx + b2 * xx^2 + b3 * xx^3)
})

polys3 |>
  ggplot(aes(yrprior_btemp, y, colour = as.factor(age_f))) +
  geom_line() +
  scale_color_viridis_d(option = "C") +
  theme_light() +
  facet_wrap(~age_f, scales="fixed") +
  geom_point(data = pcod_dat, mapping = aes(x = yrprior_btemp), y = jitter(rep(-0.1, nrow(pcod_dat)), 0.5), inherit.aes = FALSE, pch = 21, alpha = 0.02, col = "grey40")

polys3 |>
  ggplot(aes(yrprior_btemp, y, colour = as.factor(age_f))) +
  geom_line() +
  scale_color_viridis_d(option = "C") +
  theme_light()
  # # facet_wrap(~age_f, scales="free_y") +
  # geom_point(data = pcod_dat, mapping = aes(x = yrprior_btemp), y = jitter(rep(-0.1, nrow(pcod_dat)), 0.5), inherit.aes = FALSE, pch = 21, alpha = 0.02, col = "grey40")

# interesting??

# would a quadratic be a bit less crazy?

# compare ELPD with model without temperature:

out_no_temp <- furrr::future_map_dfr(all_ages, fit_one_age, yrs_forecast = 2L, 
	.formula = log_wt ~ 1)

e1 <- out |> select(age_f, elpd) |> distinct() |> 
	rename(elpd_temp = elpd)
e2 <- out_no_temp |> select(age_f, elpd) |> distinct() |> 
	rename(elpd = elpd)

left_join(e1, e2) |> 
	tidyr::pivot_longer(-age_f) |> 
	ggplot(aes(age_f, value, colour = name)) + geom_line()
 
left_join(e1, e2) |> 
	mutate(e_diff = elpd_temp - elpd) |> 
	ggplot(aes(age_f, e_diff)) + geom_line() +
	geom_hline(yintercept = 0, lty = 2) +
	ylab("Difference in expected log likelihood\n(positive = better with temperature)")

