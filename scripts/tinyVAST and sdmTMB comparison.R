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
			formula = log_wt ~ 0 + age_f,
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

	# now with separate ST fields for each age class
	
	pcod_dat$age_f = factor(paste0("age_", pcod_dat$age))

	
	# mesh
	mesh_v = fm_mesh_2d(loc = d2[,c("X","Y")],
                            cutoff = 10)

	sem = ""
	
	dsem = "
	age_1 <-> age_1, 0, a1,
	age_2 <-> age_2, 0, a2,
	age_3 <-> age_3, 0, a3,
	age_4 <-> age_4, 0, a4,
	age_5 <-> age_5, 0, a5,
	age_6 <-> age_6, 0, a6,
	age_7 <-> age_7, 0, a7,
	age_8 <-> age_8, 0, a8,
	age_9 <-> age_9, 0, a9,
	age_10 <-> age_10, 0, a10
	"
		
	fit_v_st_a = tinyVAST(
  	data = pcod_dat,
  	formula = log_wt ~ 0 + age_f * yrprior_btemp,
  	family = gaussian(),
  	sem = "", 
  	dsem = dsem,
  	space_column = c("X", "Y"), 
  	spatial_graph = mesh_v,
  	time_column = "year",
  	times = 1993:2022,
  	variable_column = "age_f")

	
	############
	
		# mesh
	mesh_s <- make_mesh(pcod_dat, c("X", "Y"), cutoff = 10)

	
	fit_s_st <- 
		sdmTMB(
			formula = log_wt ~ 0 + age_f,
			family = gaussian(),
			data = pcod_dat,
			mesh = mesh_s,
			spatial = "on",
			spatiotemporal = "ar1",
			time = "year",
			extra_time = unique(pcod_dat$year),
		  silent = TRUE)