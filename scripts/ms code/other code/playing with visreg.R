# predictions

	# sdmTMB
	pcod <- dat_all %>% filter(short_name == "pcod")

	mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 10)

	m_s1 <- sdmTMB(
	  data = pcod,
	  formula = log_wt ~ yrprior_btemp,
	  mesh = mesh, 
	  family = gaussian(),
	  spatial = "on",
	  spatiotemporal = "off",
	  #time = "year"
	)
	
	p_s1 <- visreg(m_s1, xvar = "yrprior_btemp", gg = TRUE)


	# tv
	
	library(fmesher)
	library(tinyVAST)

	# create a dummy column for spatial and spatiotemporal fields
	pcod$single_age <- "shared"

	# mesh
	mesh = fm_mesh_2d(loc = pcod[,c("X","Y")],
                            cutoff = 37)

	# set up spatial and spatiotemporal field SDs
	sem = "
  	shared <-> shared, shared_s_SD
  "
	
	dsem = "
		shared <-> shared, 0, shared_st_SD
	"
	

	m_t1 <- tinyVAST(
	  formula = log_wt ~ yrprior_btemp,
  	data = pcod,
  	family = gaussian(), 
  	sem = NULL, 
  	dsem = NULL,
  	#space_column = c("X", "Y"), 
  	#spatial_graph = mesh,
  	#time_column = "year",
  	#times = min(pcod$year):max(pcod$year),
  	#variable_column = "single_age",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	

	visreg(m_t1, xvar = "yrprior_btemp", what = "p_g") # not sure what thorson did to get what = "p_g" argument; cant find source code
	
	# add spatial effect
	m_t2 <- tinyVAST(
	  formula = log_wt ~ yrprior_btemp,
  	data = pcod,
  	family = gaussian(), 
  	sem = sem, 
  	dsem = NULL,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh,
  	#time_column = "year",
  	#times = min(pcod$year):max(pcod$year),
  	variable_column = "single_age",
  	control = tinyVASTcontrol(profile = "alpha_j"))
	

	visreg(m_t2, xvar = "yrprior_btemp", what = "p_g") # not sure what thorson did to get what = "p_g" argument; cant find source code
	
	# need to add space cols & single_age 
	
	visreg(m_t2, xvar = "yrprior_btemp", 
				 data = m_t2$data)
	
	visreg::visreg(m_t2, xvar = "yrprior_btemp", 
				 data = m_t2$data)
	
	
	,
				 cond = list(X = pcod$X[1], 
				 						 Y = pcod$Y[1],
				 						 single_age = "shared",
				 						 year = 2004,
				 						 time = 1,
				 						 dist = "obs"))
				 
		