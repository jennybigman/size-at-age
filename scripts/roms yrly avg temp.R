# roms yearly avg temp

# read in CMIP6 ROMS output - averaged by basin annually
  #load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hind_mn.Rdata")
  #load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/allEBS_means/ACLIM_weekly_fut_mn.Rdata")
  
  # for PC
  #load("/data/ACLIM_weekly_hind_mn.Rdata")
  #load("/data/ACLIM_weekly_fut_mn.Rdata")
  
  # filter out bottom temp & oxygen and stick to SEBS
  
  # vars <- c("temp_bottom5m", "oxygen_bottom5m")
  
  #hind_var <- ACLIM_weekly_hind %>%
  #  filter(var %in% vars) %>%
  #	filter(year < 2021) %>%
  #	filter(basin == "SEBS")
  
  #proj_var <- ACLIM_weekly_fut %>%
  #  filter(var %in% vars) %>%
  #	filter(year >= 2021) %>%
  #	filter(basin == "SEBS")

	#### 1. Spring/Summer temps preceding survey ####
	
  #presurvey <- 4:6 # (April to June)
  #
  #ra_presurvey_hind_var <- hind_var %>%
  #	filter(mo %in% presurvey) %>%
  #	group_by(year, var) %>%
  #	summarise(presurvey_mean_val = mean(mn_val))
  #
  #ra_presurvey_hind_var_short <- 
  #	spread(ra_presurvey_hind_var, key = var, value = presurvey_mean_val) %>%
  #	rename(presurvey_btemp_ra = "temp_bottom5m",
  #				 presurvey_boxy_ra = "oxygen_bottom5m")
  	
  # plot
  #presurvey_long <- presurvey_hind_var_short %>%
  #	pivot_longer(
  #		cols = starts_with("presurvey"),
  #		names_to = "var",
  #		values_to = "val")
  #
	#ggplot(data = presurvey_long) +
	#	geom_line(aes(x = year, y = val)) +
	#	facet_wrap( ~ var, scales = "free")
	
 
  ## add to dataframe list
	#dat_join_func <- function(x){
 #
 	#		specimen_dat <- left_join(x, ra_presurvey_hind_var_short, by = "year")
	#}
 #
	#specimen_dat <- lapply(specimen_dat_list, dat_join_func)


  ##### 2. Yearly temp -- avg of temp July - Dec of year before survey & Jan - June year of survey) ####
  
  #yr_prior_func <- function(x){
  # 	
  # 	# temp same year as collected, Jan - June
  # 	current <- hind_var %>%
  # 		filter(year == x & mo <= 6)
 #
  # 	# temp previous year July - Dec
	#	previous <- hind_var %>%
  #		filter(year == x - 1 & mo > 6)
 #
	#	# combine and take a mean temp July - June
  #	var_yr <- bind_rows(current, previous) %>%
  #		group_by(var) %>%
  #		summarise(mean_yr = mean(mn_val)) %>%
  #		mutate(year = x)
  # 
  # }
  # 
  #ra_yr_prior <- lapply(1971:2020, yr_prior_func) %>% bind_rows()
  #
  #ra_yr_prior_short <- 
  #	spread(ra_yr_prior, key = var, value = mean_yr) %>%
	#	rename(yrprior_btemp_ra = "temp_bottom5m",
  #				 yrprior_boxy_ra = "oxygen_bottom5m")
  
  # plot
  #yrprior_long <- yr_prior_short %>%
  #	pivot_longer(
  #		cols = starts_with("yrprior"),
  #		names_to = "var",
  #		values_to = "val")
  #
	#ggplot(data = yrprior_long) +
	#	geom_line(aes(x = year, y = val)) +
	#	facet_wrap( ~ var, scales = "free")
	
  
  #dat_join_func <- function(x){
 #
 	#		specimen_dat <- left_join(x, ra_yr_prior_short, by = "year")
	#}
 #
	#specimen_dat <- lapply(specimen_dat, dat_join_func)
	#
	## any NAs with spatially-avg temp from ROMS?
	#ra_specimen_dat <- specimen_dat %>% bind_rows() %>% na.omit()
	#
	#ra_nums <- ra_specimen_dat %>%
	#	group_by(short_name) %>%
	#	summarise(n())
#

	# with survey-replicated indices by ROMS group
	
	# ACLIM survey-replicated hindcast
  # load("../../ACLIM2/Data/out/Mar 2023/K20P19_CMIP6/BC_ACLIMsurveyrep/ACLIMsurveyrep_OperationalHindcast_BC_hind.Rdata")
 
	# filter bottom temp & oxy, keep only some cols, and trim the whitespace around station_id
  # bot_vars <- c("temp_bottom5m", "oxygen_bottom5m") # no oxygen right now?

  # not by month so just avg over year
  #rsr_bot_hind_var <- hind %>%
  #  filter(var %in% bot_vars) %>%
  #	filter(year < 2021) %>%
  #	select(year, station_id, mn_val, val_raw, var) %>%
  #	mutate(stationid = str_trim(station_id))
#
  #rsr_bot_hind_var_short <- 
  #	spread(rsr_bot_hind_var, key = var, value = val_raw) %>%
	#	rename(yr_btemp_rsr = "temp_bottom5m") %>%
  #	select(year, stationid, yr_btemp_rsr)
  #	
 #
  # dat_join_func <- function(x){
 #
 	#		specimen_dat <- left_join(x, rsr_bot_hind_var_short, by = c("year", "stationid"))
	#}
 #
	#specimen_dat <- lapply(specimen_dat, dat_join_func)
#
  ## any NAs with spatially-avg temp from ROMS?
#	rsr_specimen_dat <- specimen_dat %>% 
#		bind_rows() %>% 
#		select(-contains("_ra")) %>%
#		na.omit()
#	
#	rsr_nums <- rsr_specimen_dat %>%
#		group_by(short_name) %>%
#		summarise(n())

	
  #### add in spatially-explicit temperature
	