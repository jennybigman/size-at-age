# predict error



 m <- 
	tinyVAST(
		data = pcod_dat,
  	formula = log_wt ~ 0 + age_f + poly(yrprior_btemp, 2, raw = TRUE),
  	family = gaussian(),
  	#sem = "", 
  	#dsem = NULL,
  	#space_column = c("X", "Y"), 
  	#spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
  
  saveRDS(m, file = paste0(file_path, "ex_mod.rds"))

  m <- readRDS(file = paste0(file_path, "ex_mod.rds")) 
  #m <- reload_model(m) # doesn't matter whether this is run or not
  
  nd <- crossing(
				yrprior_btemp = seq(
					from = min(m$data$yrprior_btemp),
					to = max(m$data$yrprior_btemp),
					length.out = 25),
				age_f = unique(m$data$age_f),
				year = 2004) 
  
  nd <- data.frame(nd)
  
  p <- predict(m, newdata = nd)
  