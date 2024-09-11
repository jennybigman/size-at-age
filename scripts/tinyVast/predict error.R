library(tinyVAST)
library(fmesher)

	# wrangle data and mesh
	d <- pcod_dat <- 
		dat_all |> 
		filter(short_name == "pcod") %>%
		filter(age == 2)
	
	d$age_f <- as.factor(paste0("age_", d$age_f))
	
	m <- fm_mesh_2d(loc = d[,c("X","Y")],
	                       cutoff = 37)

	# fit model
	fit <- 
			tinyVAST(
				data = d,
  			formula = log_wt ~ 1,
  			family = gaussian(),
  			sem = "", 
  			dsem = "",
  			space_column = c("X", "Y"), 
  			spatial_graph = m,
  			time_column = "year",
  			times = min(d$year):max(d$year),
  			control = tinyVASTcontrol(profile = "alpha_j"))

	# predictions
	dat <- fit$data

	nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(dat$yrprior_btemp),
					to = max(dat$yrprior_btemp),
					length.out = 25),
				year = d$year[1],
				X = d$X[1],
				Y = d$Y[1])
	
	nd <- data.frame(nd)
 
  p <- predict(fit, newdata = nd)
		
	# error: "Error in `[.data.frame`(newdata, , object$internal$variable_column) : 
  #undefined columns selected"	
  
  fit$internal$variable_column
  #"var" ? 

	nd <- expand.grid(
				yrprior_btemp = seq(
					from = min(dat$yrprior_btemp),
					to = max(dat$yrprior_btemp),
					length.out = 25),
				year = d$year[1],
				X = d$X[1],
				Y = d$Y[1],
				var = 1) # what is this?
	
	nd <- data.frame(nd)
 
  p <- predict(fit, newdata = nd)			
		
  