# tinyVAST & sdmTMB comparison

library(tinyVAST)
library(fmesher)
library(sf)

# pcod example

	pcod_dat <- dat_all %>% filter(short_name == "pcod")
	d2 <- pcod_dat %>% filter(age_f == 2)

	#### sdmTMB models ####
	
	# mesh
	mesh <- make_mesh(d2, c("X", "Y"), cutoff = 10)
	
	# spatial only model
	fit_s_s <- 
		sdmTMB(
			formula = log_wt ~ 1,
			family = gaussian(),
			data = d2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "off",
			#time = "year",
		  silent = TRUE)
	
	# spatial and spatiotemporal model
	fit_s_st <- 
		sdmTMB(
			formula = log_wt ~ 1,
			family = gaussian(),
			data = d2,
			mesh = mesh,
			spatial = "on",
			spatiotemporal = "iid",
			time = "year",
		  silent = TRUE)
	
	
	#### tinyVAST models #### 
	
	d2$var <- 'log_wt'
	
	# spatial only model
	
	# mesh
	mesh_v = fm_mesh_2d(loc = d2[,c("X","Y")],
                            cutoff = 10)

	sem = "
	log_wt <-> log_wt, spatial_sd
	"
	
	fit_v = tinyVAST(
  	data = d2,
  	formula = log_wt ~ 1,
  	sem = sem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v)


	# spatiotemporal model
	
	# mesh
	mesh_v = fm_mesh_2d(loc = d2[,c("X","Y")],
                            cutoff = 10)

	sem = "
	log_wt <-> log_wt, spatial_sd
	"
	
	dsem = "
	log_wt <-> log_wt, 0, st_sd
	"
		
		
	fit_v_st = tinyVAST(
  	data = d2,
  	formula = log_wt ~ 1,
  	family = gaussian(),
  	sem = sem, 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022)

	