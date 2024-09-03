set.seed(123)

  # make fake predictor(s) (a1) and sampling locations:
  predictor_dat <- data.frame(
    X = runif(300), Y = runif(300),
    a1 = rnorm(300), year = rep(1:6, each = 50)
  )
  mesh <- make_mesh(predictor_dat, xy_cols = c("X", "Y"), cutoff = 0.1)

  sim_dat <- sdmTMB_simulate(
    formula = ~ 1 + a1,
    data = predictor_dat,
    time = "year",
    mesh = mesh,
    family = gaussian(),
    range = 0.5,
    sigma_E = 0.1,
    phi = 0.1,
    sigma_O = 0.2,
    seed = 42,
    B = c(0.2, -0.4) # B0 = intercept, B1 = a1 slope
  )
 
  head(sim_dat)
  
  fit <- sdmTMB(observed ~ a1, data = sim_dat, mesh = mesh, time = "year")
  
  #### 
  
  predictors <- tibble(
  	temp = rnorm(1000),
		year = rep(1:10, each = 100),
		X = runif(1000),
		Y = runif(1000))

  mesh <- make_mesh(predictors, xy_cols = c("X", "Y"), cutoff = .1)  
  
  sim_dat <- sdmTMB_simulate(
    formula = ~ temp,
    data = predictors,
    time = "year",
    mesh = mesh,
    family = gaussian(),
    range = 0.5,
    sigma_E = 0.1,
    phi = 0.1,
    sigma_O = 0.2,
    seed = 42,
    B = c(1, 1) # intercept, slope
  )
 
  head(sim_dat)
  
  fit <- sdmTMB(
  	observed ~ temp,
  	data = sim_dat,
  	mesh = mesh,
  	spatial = "on",
  	spatiotemporal = "iid",
  	time = "year"
  )
  
  #### now do this for each age class
  
    predictors <- tibble(
  	temp = rnorm(1000),
		year = rep(1:10, each = 100),
		X = runif(1000),
		Y = runif(1000))

  mesh <- make_mesh(predictors, xy_cols = c("X", "Y"), cutoff = .1)  
  
  sim_dat <- sdmTMB_simulate(
    formula = ~ temp,
    data = predictors,
    time = "year",
    mesh = mesh,
    family = gaussian(),
    range = 0.5,
    sigma_E = 0.1,
    phi = 0.1,
    sigma_O = 0.2,
    seed = 42,
    B = c(1, 1) # intercept, slope
  )
 