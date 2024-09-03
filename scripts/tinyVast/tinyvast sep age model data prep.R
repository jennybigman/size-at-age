# tinyvast single mod data prep

# pcod ####
	
	pcod_dat <- dat_all %>% filter(short_name == "pcod")
	pcod_dat$age_f <- paste0("age_", pcod_dat$age)
	
	
	pcod_dat_list <- purrr::map(1:10, \(ages){
		
		d <- pcod_dat %>% filter(age == ages)
		d
	})
	
	
	
	# mesh
	
	pcod_meshes <- purrr::map(pcod_dat_list, \(d){
		
		mesh <- fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 50)
	
	})
	
	# pollock ####
	
	pol_dat <- dat_all %>% filter(short_name == "pollock")
	pol_dat$age_f <- paste0("age_", pol_dat$age)
	
	pol_dat_list <- purrr::map(1:20, \(ages){
		
		d <- pol_dat %>% filter(age == ages)
		d
	})
	
	# mesh
	
	pol_meshes <- purrr::map(pol_dat_list, \(d){
		
		mesh <- fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 50)
	
	})
	
	# yfin ####
	
	yfin_dat <- dat_all %>% filter(short_name == "yfin")
	yfin_dat$age_f <- paste0("age_", yfin_dat$age)
	
	sort(unique(yfin_dat$age))

	yfin_dat_list <- purrr::map(3:30, \(ages){
		
		d <- yfin_dat %>% filter(age == ages)
		d
	})
	
	# sdmTMB
	
	yfin_meshes <- purrr::map(yfin_dat_list, \(d){
		

		mesh <- fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 50)
	
	})
	

	
	# atooth ####
	
	atooth_dat <- dat_all %>% filter(short_name == "atooth")
	atooth_dat$age_f <- paste0("age_", atooth_dat$age)
	
	sort(unique(atooth_dat$age))
		
	atooth_dat_list <- purrr::map(2:12, \(ages){
		
		d <- atooth_dat %>% filter(age == ages)
		d
	})
	
	# sdmTMB
	
	atooth_meshes <- purrr::map(atooth_dat_list, \(d){
		
	mesh <- fm_mesh_2d(loc = d[,c("X","Y")],
                            cutoff = 50)
	
	})
	