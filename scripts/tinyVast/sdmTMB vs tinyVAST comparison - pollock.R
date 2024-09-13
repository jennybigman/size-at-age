
# comparing modeling frameworks
library(sdmTMB)
library(tinyVAST)
library(fmesher)

	d <- dat_all %>% 
		filter(short_name == "pollock") 
	
	mesh_s <- make_mesh(
  	d, 
  	c("X", "Y"),
  	cutoff = 50)
	
	mesh_t <- fm_mesh_2d(loc = d[,c("X","Y")],
            	cutoff = 50)

	X_pred <- d$X[1]
	Y_pred <- d$Y[1]
	
	min_temp <- min(d$yrprior_btemp)
	max_temp <- max(d$yrprior_btemp)

	# sdmTMB model ####
	
	# shared ST field and spatial field SDs across age class
	mod_s1 <- 
		sdmTMB(
			log_wt ~ 0 + age_f + age_f:poly(yrprior_btemp, 2, raw = TRUE),
    	data = d,
    	mesh = mesh_s,
    	spatial = "on",
    	spatiotemporal = "iid",
    	time = "year",
    	control = sdmTMBcontrol(profile = "b_j"))
	
	
	nd <- crossing(
		age_f = droplevels(unique(d$age_f)),
		yrprior_btemp =
			seq(from = min_temp, to = max_temp, length.out = 25),
		X = X_pred,
		Y = Y_pred,
		year = 2004
	)
	
	preds_s <- predict(mod_s1, newdata = nd, se_fit = FALSE)
	
	preds_s <- preds_s |>
		mutate(age = as.numeric(age_f))
	
	ggplot(preds_s) +
		geom_line(aes(x = yrprior_btemp, y = est)) +
		facet_wrap(~ reorder(age_f, age), scales = "free_y")
	

	# tinyVAST model ####

	d$age_f2 <- as.factor(paste0("age_", d$age))

	# shared spatial and ST field variances across age class

	sem = "
  age_1 <-> age_1, spatial_sd
  age_2 <-> age_2, spatial_sd
  age_3 <-> age_3, spatial_sd
  age_4 <-> age_4, spatial_sd
  age_5 <-> age_5, spatial_sd
  age_6 <-> age_6, spatial_sd
  age_7 <-> age_7, spatial_sd
  age_8 <-> age_8, spatial_sd
  age_9 <-> age_9, spatial_sd
  age_10 <-> age_10, spatial_sd
  age_11 <-> age_11, spatial_sd
  age_12 <-> age_12, spatial_sd
  age_13 <-> age_13, spatial_sd
  age_14 <-> age_14, spatial_sd
  age_15 <-> age_15, spatial_sd
  age_16 <-> age_16, spatial_sd
  age_17 <-> age_17, spatial_sd
  age_18 <-> age_18, spatial_sd
  age_19 <-> age_19, spatial_sd
  age_20 <-> age_20, spatial_sd
 "
	
	
	dsem = "
	age_1 <-> age_1, 0, st_sd
	age_2 <-> age_2, 0, st_sd 
	age_3 <-> age_3, 0, st_sd 
	age_4 <-> age_4, 0, st_sd 
	age_5 <-> age_5, 0, st_sd 
	age_6 <-> age_6, 0, st_sd 
	age_7 <-> age_7, 0, st_sd 
	age_8 <-> age_8, 0, st_sd 
	age_9 <-> age_9, 0, st_sd 
	age_10 <-> age_10, 0, st_sd
	age_11 <-> age_11, 0, st_sd
  age_12 <-> age_12, 0, st_sd
  age_13 <-> age_13, 0, st_sd
  age_14 <-> age_14, 0, st_sd
  age_15 <-> age_15, 0, st_sd
  age_16 <-> age_16, 0, st_sd
  age_17 <-> age_17, 0, st_sd
  age_18 <-> age_18, 0, st_sd
  age_19 <-> age_19, 0, st_sd
  age_20 <-> age_20, 0, st_sd
	"

	mod_tv1 <- 
		tinyVAST(
		data = d,
  	formula = log_wt ~ 0 + age_f2 + age_f2:poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_t,
  	time_column = "year",
  	times = min(d$year):max(d$year),
  	variable_column = "age_f2",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	
	nd_t <- nd |>
		mutate(age_f2 = paste0("age_", age_f)) |>
		select(-age_f)
	
	nd_t <- data.frame(nd_t)
	
	preds_t <- predict(mod_tv1, newdata = nd_t)
	
	preds_t <- bind_cols(preds_t, nd_t) 
	
	preds_t <- preds_t %>% rename(est = "...1")
	
	ggplot(preds_t) +
		geom_line(aes(x = yrprior_btemp, y = est)) +
		facet_wrap(~ age_f2, scales = "free_y")
	
	# plot together
	preds_t <- preds_t |> 
		rename(age_f = age_f2) |>
		mutate(pkg = "tinyVAST")

	preds_s <- preds_s |>
		select(age_f, yrprior_btemp, est, year, X, Y) |>
		mutate(pkg = "sdmTMB",
					 age_f = paste0("age_", age_f))
	
	preds <- bind_rows(preds_s, preds_t)
	
	ages <- str_split(preds$age_f, "_")
	ages <- sapply(ages, "[[", 2)
	
	preds$age <- as.numeric(ages)
	

	ggplot(preds) +
		geom_line(aes(x = yrprior_btemp, y = est, color = pkg)) +
		facet_wrap(~ reorder(age_f, age), scales = "free_y")

	
	
