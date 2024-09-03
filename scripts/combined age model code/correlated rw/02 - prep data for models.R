	
	#### pcod ####
	
	pc_d <- dat_all |> filter(short_name == "pcod")
	pc_d <- mutate(pc_d, age = ifelse(age >= 9, 9, age))
	pc_d <- pc_d |>
		filter(age_f %in% age) ### would want to change this if want to do an age+ group (need to remove age_f, bin age, make a factor)
	pc_d$age_f <- droplevels(pc_d$age_f)
	pc_d$year_age <- factor(paste(pc_d$year, pc_d$age))
	(all_time <- seq(min(pc_d$year), max(pc_d$year)))
	
	mesh_pc <- make_mesh(
		pc_d, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
		cutoff = 40,
		offset = c(60, 70)
	)
	
	# what about allowing each age to have its own SD?
	# create a map vector for that...
	pc_mm <- model.matrix(~0 + year_age, data = pc_d) |> colnames()
	#map_vec <- gsub("year_age[0-9]+ ", "", mm)
	pc_map_vec <- gsub("^\\S+ ", "", pc_mm)
	
	# but share last 2 are too data sparse in last age bin:
	table(pc_d$age, pc_d$year)
	# mirror their SD estimate:
	pc_map_vec[pc_map_vec == "9"] <- "8"
	#map_vec[map_vec == "8"] <- "7"

	pc_map_vec <- factor(pc_map_vec)
	
	#### pollock ####
	
	pl_d <- dat_all |> filter(short_name == "pollock")
	table(pl_d$age, pl_d$year)

	pl_d <- mutate(pl_d, age = ifelse(age >= 11, 11, age))
	table(pl_d$age, pl_d$year)
	
	pl_d <- pl_d |>
		filter(age_f %in% age)
	pl_d$age_f <- droplevels(pl_d$age_f)
	pl_d$year_age <- factor(paste(pl_d$year, pl_d$age))
	(all_time <- seq(min(pl_d$year), max(pl_d$year)))
	
	mesh_pl <- make_mesh(
		pl_d, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
		cutoff = 40,
		offset = c(60, 70)
	)
	
	# what about allowing each age to have its own SD?
	# create a map vector for that...
	pl_mm <- model.matrix(~0 + year_age, data = pl_d) |> colnames()
	#map_vec <- gsub("year_age[0-9]+ ", "", mm)
	pl_map_vec <- gsub("^\\S+ ", "", pl_mm)
	
	# but share last 2 are too data sparse in last age bin:
	table(pl_d$age, pl_d$year)
	# mirror their SD estimate:
	#pl_map_vec[pl_map_vec == "12"] <- "11"

	pl_map_vec <- factor(pl_map_vec)
	
	### atooth ####
	
	a_d <- dat_all |> filter(short_name == "atooth")
	table(a_d$age, a_d$year)
	
	a_d <- mutate(a_d, age = ifelse(age <= 2, 3, age))
	table(a_d$age, a_d$year)

	a_d <- mutate(a_d, age = ifelse(age >= 9, 9, age)) 
	table(a_d$age, a_d$year)
	
	a_d$age_f <- as.factor(a_d$age)
	
	a_d$year_age <- factor(paste(a_d$year, a_d$age))

	#a_d$age_f <- droplevels(a_d$age_f)
	#(all_time <- seq(min(a_d$year), max(a_d$year)))
	
	mesh_a <- make_mesh(
		a_d, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
		cutoff = 20)
	
	# what about allowing each age to have its own SD?
	# create a map vector for that...
	a_mm <- model.matrix(~0 + year_age, data = a_d) |> colnames()
	#map_vec <- gsub("year_age[0-9]+ ", "", mm)
	a_map_vec <- gsub("^\\S+ ", "", a_mm)
	
	# but share last 2 are too data sparse in last age bin:
	table(a_d$age, a_d$year)
	# mirror their SD estimate:
	a_map_vec[a_map_vec == "9"] <- "8"

	a_map_vec <- factor(a_map_vec)
	
	#### yfin ####
	
	y_d <- dat_all |> filter(short_name == "yfin")
	table(y_d$age, y_d$year)

	y_d <- mutate(y_d, age = ifelse(age >= 25, 25, age))
	table(y_d$age, y_d$year)
	
	# also combine ages 3 & 4 because age 3 sparse
	y_d <- mutate(y_d, age = ifelse(age <= 3, 4, age))
	table(y_d$age, y_d$year)
		
	
	y_d <- y_d |>
		filter(age_f %in% age)
	y_d$age_f <- droplevels(y_d$age_f)
	y_d$year_age <- factor(paste(y_d$year, y_d$age))
	(all_time <- seq(min(y_d$year), max(y_d$year)))
	
	mesh_y <- make_mesh(
		y_d, c("X", "Y"),
		fmesher_func = fmesher::fm_mesh_2d_inla,
		cutoff = 40,
		offset = c(60, 70)
	)
	
	# what about allowing each age to have its own SD?
	# create a map vector for that...
	y_mm <- model.matrix(~0 + year_age, data = y_d) |> colnames()
	#map_vec <- gsub("year_age[0-9]+ ", "", mm)
	y_map_vec <- gsub("^\\S+ ", "", y_mm)
	
	# but share last 2 are too data sparse in last age bin:
	table(y_d$age, y_d$year)
	# mirror their SD estimate:
	y_map_vec[y_map_vec == "19"] <- "18"
	y_map_vec[y_map_vec == "20"] <- "18"
	y_map_vec[y_map_vec == "21"] <- "18"
	y_map_vec[y_map_vec == "22"] <- "18"
	y_map_vec[y_map_vec == "23"] <- "18"
	y_map_vec[y_map_vec == "24"] <- "18"
	y_map_vec[y_map_vec == "25"] <- "18"

	
	y_map_vec <- factor(y_map_vec)
	