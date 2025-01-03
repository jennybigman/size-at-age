# deal with missing stations
	library(tidyverse)
	library(here)
	library(mapdata)
	
	# read in GAP data and wrangle columns
	GAP_dat <- readRDS(here("./data/ak_bts.RDS"))

	names(GAP_dat) <- tolower(names(GAP_dat)) 
	
	GAP_dat <- GAP_dat %>%
		filter(region != "HG")

	# there are some samples with missing stations - which ones and where are they?
	missing_stations <- GAP_dat %>% 
		filter(is.na(stationid))
	
	missing_stations_sum <- missing_stations %>%
		distinct_at(vars(start_latitude, start_longitude))
	
#	missing_station_ids <-
#		ggplot(missing_stations_sum, aes(x = start_longitude, y = start_latitude)) +
#		geom_point() +
#		geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
#  		fill = "darkgrey", color = NA) +
#		coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) + 
#		theme_sleek() 

	missing_stations_test <- missing_stations %>%
		filter(region == "BS")

	#ggplot(missing_stations_test, aes(x = start_longitude, y = start_latitude)) +
	#	geom_point() +
	#	geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  #		fill = "darkgrey", color = NA) +
	#	coord_fixed(ylim = c(52, 65), xlim = c(-178, -155)) + 
	#	theme_sleek() 

	
	#  give stations without an ID a fake ID for matching with the ROMS grid later
	all_stations <- GAP_dat %>%
		dplyr::select(stationid, start_latitude, start_longitude, stratum) 

	unique_stations <- GAP_dat %>%
		dplyr::select(stationid, start_latitude, start_longitude, stratum) %>%
		distinct_at(vars(start_latitude, start_longitude), .keep_all = TRUE)
	
	stations_NA <- unique_stations %>%
		filter(is.na(stationid)) 

	# assign them a station ID (just for matching to temp)
	new_ids <- ids::random_id(nrow(stations_NA), 1)
	new_ids <- paste0("MU_", new_ids)
	
	stations_NA <- stations_NA %>%
		mutate(new_ids = new_ids) 
	
	stations_NA$stationid <- stations_NA$new_ids
	
	stations_NA <- stations_NA %>%
		select(-new_ids) %>%
		select(-stratum)
	
	# add back into full df
	
	GAP_dat_no_NAs <- GAP_dat %>%
		drop_na(stationid)
	
	GAP_dat_NAs <- GAP_dat %>%
		filter(is.na(stationid))
	
	GAP_dat_NAs <- GAP_dat_NAs %>%
		select(-stationid) 
	
	GAP_dat_NAs <- inner_join(GAP_dat_NAs, stations_NA, by = c("start_longitude", "start_latitude"))	

	GAP_dat_fixed <- bind_rows(GAP_dat_NAs, GAP_dat_no_NAs)	
	
	write.csv(GAP_dat_fixed, file = paste0(here(), "/data/GAP_dat_fixed.csv"))
	
	# station with most samples - for predictions
	sum <- GAP_dat_fixed |>
		group_by(stationid) |>
		summarise(count = n()) |>
		slice_max(order_by = count)

	st_id <- as.character(sum[1,1])
	
	st <- dat_all |>
		filter(stationid == st_id)
	
	st <- st |> 
		select(latitude, longitude) |>
		filter(row_number() == 1) |>
		add_utm_columns(
			ll_names = c("longitude", "latitude"),
			ll_crs = 4236,
			utm_names = c("X", "Y")
		)
	
	X_pred <- as.numeric(st$X)
	Y_pred <- as.numeric(st$Y)

	# temps and oxygen vals for prediction
	min_temp <- min(dat_all$yrprior_btemp, na.rm = TRUE)
	max_temp <- max(dat_all$yrprior_btemp, na.rm = TRUE)
	
	min_oxy <- min(dat_all$yrprior_boxy, na.rm = TRUE)
	max_oxy <- max(dat_all$yrprior_boxy, na.rm = TRUE)
	