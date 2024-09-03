
# comparing modeling frameworks
library(sdmTMB)
library(tinyVAST)
library(fmesher)

	d <- dat_all %>% 
		filter(short_name == "pollock") %>%
		filter(age < 3)
	
	mesh_s <- make_mesh(
  	d, 
  	c("X", "Y"),
  	cutoff = 50)
	
	mesh_t <- fm_mesh_2d(loc = d[,c("X","Y")],
            	cutoff = 50)


	# sdmTMB models ####
	
	# shared ST field and spatial field SDs across age class
	mod_s1 <- 
		sdmTMB(
			log_wt ~ 0 + age_f * yrprior_btemp,
    	data = d,
    	mesh = mesh_s,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "year",
    	control = sdmTMBcontrol(profile = "b_j"))
	
	# shared ST field variance across age class with AR process
	range(d$year)
	
	mod_s2 <- 
		sdmTMB(
			log_wt ~ 0 + age_f * yrprior_btemp,
    	data = d,
    	mesh = mesh_s,
    	spatial = "on",
    	spatiotemporal = "ar1",
    	time = "year",
			extra_time = 1991:2023,
    	control = sdmTMBcontrol(profile = "b_j"))
	
	# separate ST field variances by age class, still iid
	d$fake_time <- factor(paste0(d$year, d$age))

	mod_s3 <- 
		sdmTMB(
			log_wt ~ 0 + age_f * yrprior_btemp,
    	data = d,
    	mesh = mesh_s,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "fake_time",
			extra_time = 1991:2023,
    	control = sdmTMBcontrol(profile = "b_j"))

	
#	mod_s4 <- 
#		sdmTMB(
#			log_wt ~ 0 + age_f * yrprior_btemp,
#    	data = d,
#    	mesh = mesh_s,
#    	spatial = "on",
#    	spatiotemporal = "ar1",
#    	time = "fake_time",
#			extra_time = 1991:2023,
#    	control = sdmTMBcontrol(profile = "b_j"))

# can't do AR1 ST fields with fake_time hack
	
	# tinyVAST models ####

	d$age_f2 <- as.factor(paste0("age_", d$age))

	# shared spatial and ST field variances across age class

	sem = "
	age_1 <-> age_1, ssd,
	age_2 <-> age_2, ssd,
	"
	
	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a1,
	"

	mod_tv1 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 * yrprior_btemp,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	# separate response dist - equivalent to using one distribution
	mod_tv1.2 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 * yrprior_btemp,
  	family = list(age_1 = gaussian(), age_2 = gaussian()),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f2",
		distribution_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	# predictions
	
	# shared spatial and ST field variance across age class with AR process
	sem = "
	age_1 <-> age_1, ssd,
	age_2 <-> age_2, ssd,
	age_3 <-> age_3, ssd,
	"

	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a1,
	age_3 <-> age_3, 0, a1,
	
	age_1 -> age_1, 1, lag1,
	age_2 -> age_2, 1, lag1,
	age_3 -> age_3, 1, lag1,
	"

	mod_tv2 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 * yrprior_btemp,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	

	# shared spatial and ST field variance across age class with AR process
	sem = "
	age_1 <-> age_1, ssd,
	age_2 <-> age_2, ssd,
	age_3 <-> age_3, ssd,
	"

	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a1,
	age_3 <-> age_3, 0, a1,
	
	age_1 -> age_1, 1, lag1,
	age_2 -> age_2, 1, lag1,
	age_3 -> age_3, 1, lag1,
	"

	mod_tv2 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 * yrprior_btemp,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	
	### separate models for each age class 
	
	d <- dat_all %>% filter(short_name == "pollock")
	d1 <- d %>% filter(age == 1)
	d2 <- d %>% filter(age == 2)
	d3 <- d %>% filter(age == 3)
	
	# sdmTMB
	
	mesh_s1 <- make_mesh(
  	d1, 
  	c("X", "Y"),
  	cutoff = 50)
	
	mesh_s2 <- make_mesh(
  	d2, 
  	c("X", "Y"),
  	cutoff = 50)
		
	mesh_s3 <- make_mesh(
  	d3, 
  	c("X", "Y"),
  	cutoff = 50)
			
	mesh_t1 <- fm_mesh_2d(loc = d1[,c("X","Y")],
            	cutoff = 50)
	
	mesh_t2 <- fm_mesh_2d(loc = d2[,c("X","Y")],
            	cutoff = 50)
		
	mesh_t3 <- fm_mesh_2d(loc = d3[,c("X","Y")],
            	cutoff = 50)


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
	
	
#####################################################



	# tinyVAST models ####
	d <- dat_all %>% 
		filter(short_name == "pcod")
	
	mesh_t <- fm_mesh_2d(loc = d[,c("X","Y")],
            	cutoff = 50)


	d$age_f <- as.factor(paste0("age_", d$age))

	# shared spatial and ST field variances across age class

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

	mod_tv_shared <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	# predictions
	  nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(mod_tv_shared$data$yrprior_btemp, na.rm = TRUE),
					to = max(mod_tv_shared$data$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				age_f = unique(mod_tv_shared$data$age_f),
				year = 2004,
				X = mod_tv_shared$data$X[1],
				Y = mod_tv_shared$data$Y[1]) 
  
  nd <- data.frame(nd)
  
  p <- predict(mod_tv_shared, newdata = nd, what = "p_g", se.fit = TRUE) # get error around each value?
  
  #p <- predict(mod_tv_shared, newdata = nd, se.fit = FALSE) # get error around each value?

  p <- bind_cols(p)
  
  preds <- tibble(nd, p) 
  
  preds <- preds %>%
		mutate(age = as.character(age_f))
  
	ages <- str_extract(preds$age, "(?<=_)[0-9]+")
  
	preds <- preds %>%
		mutate(age = as.numeric(ages))
  
  ggplot(preds) +
  	#geom_ribbon() +
  	geom_line(aes(x = yrprior_btemp, y = fit)) +
		facet_wrap(~ reorder(age_f, age), scales = "free_y")  

  
  # sdmTMB
  
	mesh_s <- make_mesh(
  	d, 
  	c("X", "Y"),
  	cutoff = 50)
	
	mod_tv_shared <- 
		sdmTMB(
		formula = log_wt ~ 0 + age_f * poly(yrprior_btemp, 2, raw = TRUE),
		data = d,
		mesh = mesh_s,
  	family = gaussian(),
		spatial = "on",
		spatiotemporal = "iid",
  	time = "year",
    control = sdmTMBcontrol(profile = "b_j"))










##################################################################################

# comparing modeling frameworks
library(sdmTMB)
library(tinyVAST)
library(fmesher)

	d <- dat_all %>% 
		filter(short_name == "pollock") %>%
		filter(age < 4)
	
	mesh_s <- make_mesh(
  	d, 
  	c("X", "Y"),
  	cutoff = 50)
	
	mesh_t <- fm_mesh_2d(loc = d[,c("X","Y")],
            	cutoff = 50)


	# sdmTMB models ####
	
	# shared ST field variances across age class
	mod_s1 <- 
		sdmTMB(
			log_wt ~ 0 + age_f * yrprior_btemp,
    	data = d,
    	mesh = mesh_s,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "year",
    	control = sdmTMBcontrol(profile = "b_j"))
	
	# shared ST field variance across age class with AR process
	range(d$year)
	
	mod_s2 <- 
		sdmTMB(
			log_wt ~ 0 + age_f * yrprior_btemp,
    	data = d,
    	mesh = mesh_s,
    	spatial = "on",
    	spatiotemporal = "ar1",
    	time = "year",
			extra_time = 1991:2023,
    	control = sdmTMBcontrol(profile = "b_j"))
	
	# separate ST field variances by age class, still iid
	d$fake_time <- factor(paste0(d$year, d$age))

	mod_s3 <- 
		sdmTMB(
			log_wt ~ 0 + age_f * yrprior_btemp,
    	data = d,
    	mesh = mesh_s,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "fake_time",
			extra_time = 1991:2023,
    	control = sdmTMBcontrol(profile = "b_j"))

	
#	mod_s4 <- 
#		sdmTMB(
#			log_wt ~ 0 + age_f * yrprior_btemp,
#    	data = d,
#    	mesh = mesh_s,
#    	spatial = "on",
#    	spatiotemporal = "ar1",
#    	time = "fake_time",
#			extra_time = 1991:2023,
#    	control = sdmTMBcontrol(profile = "b_j"))

# can't do AR1 ST fields with fake_time hack
	
	# tinyVAST models ####

	d$age_f2 <- as.factor(paste0("age_", d$age))

	# shared spatial and ST field variances across age class

	sem = "
	age_1 <-> age_1, ssd,
	age_2 <-> age_2, ssd,
	age_3 <-> age_3, ssd,
	"
	
	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a1,
	age_3 <-> age_3, 0, a1,
	"

	mod_tv1 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 * yrprior_btemp,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	# predictions
	
	# shared spatial and ST field variance across age class with AR process
	sem = "
	age_1 <-> age_1, ssd,
	age_2 <-> age_2, ssd,
	age_3 <-> age_3, ssd,
	"

	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a1,
	age_3 <-> age_3, 0, a1,
	
	age_1 -> age_1, 1, lag1,
	age_2 -> age_2, 1, lag1,
	age_3 -> age_3, 1, lag1,
	"

	mod_tv2 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 * yrprior_btemp,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	

	# shared spatial and ST field variance across age class with AR process
	sem = "
	age_1 <-> age_1, ssd,
	age_2 <-> age_2, ssd,
	age_3 <-> age_3, ssd,
	"

	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a1,
	age_3 <-> age_3, 0, a1,
	
	age_1 -> age_1, 1, lag1,
	age_2 -> age_2, 1, lag1,
	age_3 -> age_3, 1, lag1,
	"

	mod_tv2 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 * yrprior_btemp,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = 1991:2023,
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	
	### separate models for each age class 
	
	d <- dat_all %>% filter(short_name == "pollock")
	d1 <- d %>% filter(age == 1)
	d2 <- d %>% filter(age == 2)
	d3 <- d %>% filter(age == 3)
	
	# sdmTMB
	
	mesh_s1 <- make_mesh(
  	d1, 
  	c("X", "Y"),
  	cutoff = 50)
	
	mesh_s2 <- make_mesh(
  	d2, 
  	c("X", "Y"),
  	cutoff = 50)
		
	mesh_s3 <- make_mesh(
  	d3, 
  	c("X", "Y"),
  	cutoff = 50)
			
	mesh_t1 <- fm_mesh_2d(loc = d1[,c("X","Y")],
            	cutoff = 50)
	
	mesh_t2 <- fm_mesh_2d(loc = d2[,c("X","Y")],
            	cutoff = 50)
		
	mesh_t3 <- fm_mesh_2d(loc = d3[,c("X","Y")],
            	cutoff = 50)


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
	
	