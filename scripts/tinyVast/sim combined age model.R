	### separate models for each age class 
	
	d <- dat_all %>% filter(short_name == "pcod")
	d1 <- d %>% filter(age == 1)
	d2 <- d %>% filter(age == 2)
	d3 <- d %>% filter(age == 3)
	d4 <- d %>% filter(age == 4)
	d5 <- d %>% filter(age == 5)
	d6 <- d %>% filter(age == 6)
	d7 <- d %>% filter(age == 7)
	d8 <- d %>% filter(age == 8)
	d9 <- d %>% filter(age == 9)
	d10  <- d %>% filter(age == 10)
	
	d_list <- list(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
	
	# sdmTMB
	
	meshes <- purrr::map(d_list, \(d){
		
		mesh <- make_mesh(
  		d, 
  		c("X", "Y"),
  		cutoff = 50)
	
	})
	
	
	# fit models to find intercepts
	fdf <- tibble(
		d = d_list,
		m = meshes
	)
	
	ints <- purrr::map2(fdf$d, fdf$m, \(d, m){
		
		fit <- 
			sdmTMB(
			log_wt ~ yrprior_btemp,
    	data = d,
    	mesh = m,
    	spatial = "off",
    	spatiotemporal = "off",
    	control = sdmTMBcontrol(profile = "b_j"))
		
		pars <- tidy(fit)
		
		int <- round(as.numeric(pars[1,2]), 2)
		
		int
		
	}) 
	
	# simulate data
	year = rep(1:10, each = 100)
	
	temp <- replicate(10, rnorm(1000))
	X <- replicate(10, runif(1000))
	Y <- replicate(10, runif(1000))
	
	
	pred_list <- purrr::map(1:10, \(col){
		d <- tibble(year, temp = temp[, col], X = X[, col], Y = Y[, col], age = col)
	})
	
	mesh_list <- purrr::map(pred_list, \(d){ 
		mesh <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = .1)  
		
	})
	

	arg_list <- list(
		d = pred_list,
		m = mesh_list,
		int = ints,
		age = ages
	)
	
	sim_dats <- purrr::pmap(arg_list, \(d, m, int, age){
		
		sim_dat <- sdmTMB_simulate(
  	  formula = ~ temp,
  	  data = d,
  	  time = "year",
  	  mesh = m,
  	  family = gaussian(),
  	  range = 400,
  	  sigma_E = 0.12,
  	  phi = 0.16,
  	  sigma_O = 0.1,
  	  seed = 42,
  	  B = c(int, 1) # intercept, slope
  	)
		
		sim_dat$age <- age
		
		sim_dat
	
	})
 
 
	# fit a combined age model
	
	sim_dat <- sim_dats %>% bind_rows()
	
	sim_dat$age_f <- as.factor(sim_dat$age) 
	
	mesh_s <- make_mesh(
  	sim_dat, 
  	c("X", "Y"),
  	cutoff = .1)
	
	mod_combo <- 
		sdmTMB(
		formula = observed ~ 0 + age_f * temp,
		data = sim_dat,
		mesh = mesh_s,
  	family = gaussian(),
		spatial = "on",
		spatiotemporal = "iid",
  	time = "year",
    control = sdmTMBcontrol(profile = "b_j"))

	# tinyvast
	
	sim_dat$age_f2 <- as.factor(paste0("age_", sim_dat$age))
	
	sem = "
	age_1 <-> age_1, ssd,
	age_2 <-> age_2, ssd,
	age_3 <-> age_3, ssd,
	age_4 <-> age_4, ssd,
	age_5 <-> age_5, ssd,
	age_6 <-> age_6, ssd,
	age_7 <-> age_7, ssd,
	age_8 <-> age_8, ssd,
	age_9 <-> age_9, ssd,
	age_10 <-> age_10, ssd,
	"
	
	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a1,
	age_3 <-> age_3, 0, a1,
	age_4 <-> age_4, 0, a1,
	age_5 <-> age_5, 0, a1,
	age_6 <-> age_6, 0, a1,
	age_7 <-> age_7, 0, a1,
	age_8 <-> age_8, 0, a1,
	age_9 <-> age_9, 0, a1,
	age_10 <-> age_10, 0, a1,
	"

	mesh_t <- fm_mesh_2d(loc = sim_dat[,c("X","Y")],
  	      	cutoff = 0.1)


	mod_combo_tv <- 
		tinyVAST(
		data = sim_dat,
		formula = observed ~ 0 + age_f2 * temp,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1:10,
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	
	
	
	
	
	
	
	
	###############
	

	# age 1 ####
	
	# sdmTMB
	mod_s_a1 <- 
		sdmTMB(
			log_wt ~ yrprior_btemp,
    	data = d1,
    	mesh = mesh_s1,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "year",
    	control = sdmTMBcontrol(profile = "b_j"))
	
	# tinyvast
	mod_tv_a1 <- 
		tinyVAST(
		data = d1,
  	formula = log_wt ~ yrprior_btemp,
  	family = gaussian(),
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t1,
  	time_column = "year",
  	times = 1991:2023,
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	
	# age 2 ####
	
	# sdmTMB
	mod_s_a2 <- 
		sdmTMB(
			log_wt ~ yrprior_btemp,
    	data = d2,
    	mesh = mesh_s2,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "year",
    	control = sdmTMBcontrol(profile = "b_j"))
	
	# tinyvast
	mod_tv_a2 <- 
		tinyVAST(
		data = d2,
  	formula = log_wt ~ yrprior_btemp,
  	family = gaussian(),
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t2,
  	time_column = "year",
  	times = 1991:2023,
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	# age 3 ####
	
	# sdmTMB
	mod_s_a3 <- 
		sdmTMB(
			log_wt ~ yrprior_btemp,
    	data = d3,
    	mesh = mesh_s3,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "year",
    	control = sdmTMBcontrol(profile = "b_j"))
	
	# tinyvast
	mod_tv_a3 <- 
		tinyVAST(
		data = d3,
  	formula = log_wt ~ yrprior_btemp,
  	family = gaussian(),
  	sem = "", 
  	dsem = "",
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t3,
  	time_column = "year",
  	times = 1991:2023,
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	