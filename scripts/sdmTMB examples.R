# example sdmTMB models

#1. Cam Freshwater - Freshwater et al 2023 CJFAS Nass Rover sockeye size declines with time

fit <- sdmTMB(fl ~ s(year, m = 2) +
                 s(yday_c, by = age_sex, m = 2) +
                 s(year, by = age_sex, m = 1) +
                age + sex,
              dispformula = ~ 0 + period,
              data = dat,
              spatial = "off",
              spatiotemporal = "off",
              control = sdmTMBcontrol(
                newton_loops = 1
              ),
              silent = FALSE)

saveRDS(fit, here::here("outputs", "model_fits", "fit.rds"))

# 2. Lewis Barnett - Barnett et al 2023 Ecography method to model a spatially explicit temporal trend (local triend)
# alongside spatial (temporally constant) and spatiotemporal (time-varying) components to uncover spatial shifts

density_model <- sdmTMB(formula = cpue_kg_km2 ~ log_depth_scaled + log_depth_scaled2 + as.factor(year),
        data = haul_new,
        time = "year", spde = c_spde, anisotropy = TRUE,
        silent = TRUE, spatial_trend = TRUE, spatial_only = FALSE,
        family = tweedie(link = "log"),
        control = sdmTMBcontrol(step.min = 0.01, step.max = 1))

#3. Ward et al 2022 ICES JMS new approach for modeling spatiotemporal variability

	# constant spatiotemporal variance
	m = sdmTMB(cpue_kg_km2 ~ 0 + depth_scaled + depth_scaled2 + as.factor(year),
    data = sub,
    time = "year",
    spde = spde,
    family = Gamma(link = "log"))
  
  # dynamic spatiotemporal variance 
 ad_fit <-  sdmTMB(cpue_kg_km2 ~ 0 + depth_scaled + depth_scaled2 + fyear,
           spatiotemporal = "ar1",
           spatial = "off",
           data = sub,
           time = "year",
           mesh = spde,
           family = tweedie(link = "log"),
           control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
           priors = sdmTMBpriors(
             #phi = halfnormal(0, 10),
             #tweedie_p = normal(1.5, 2),
             #ar1_rho = normal(0.5, 0.1),
             matern_st = pc_matern(range_gt = 10, sigma_lt = 5)))
 
 #4. Liu et al preprint - fit sdms for species in CA Current
 
  m <- try( sdmTMB(
    formula = as.formula(formula),
    time_varying = time_varying,
    mesh = spde,
    time = time,
    family = tweedie(link = "log"),
    data = modeldat,
    anisotropy = FALSE,
    spatial = spatial_field,
    #extra_time argument necessary for prediction?
    extra_time=1980:2100),
  silent=F)