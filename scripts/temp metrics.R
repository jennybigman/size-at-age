# calculate temp metrics

	# load ACLIM temps
	load("../../ACLIM2/Data/out/Feb 13/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hind_mn.Rdata")
  load("../../ACLIM2/Data/out/Feb 13/K20P19_CMIP6/allEBS_means/ACLIM_weekly_fut_mn.Rdata")

  # filter out bottom temps
  temp_hind <- ACLIM_weekly_hind %>%
    filter(var == "temp_bottom5m")
  
  temp_proj <- ACLIM_weekly_fut %>%
    filter(var == "temp_bottom5m")

	#### Spring/Summer temps preceding survey ####
	
  presurvey <- 4:6 # (April to June)
  
  presurvey_hind <- temp_hind %>%
  	filter(mo %in% presurvey) %>%
  	group_by(year) %>%
  	summarise(presurvey_mean_temp= mean(mn_val))

  #### Yearly temp -- avg of temp July - Dec of year before survey & Jan - June year of survey) ####
  
  temp_yr_prior_func <- function(x){
   	
   	# temp same year as collected, Jan - June
   	temp_current <- temp_hind %>%
   		filter(year == x & mo <= 6)
 
   	# temp previous year July - Dec
		temp_previous <- temp_hind %>%
  		filter(year == x - 1 & mo > 6)
 
		# combine and take a mean temp July - June
  	temp_yr <- bind_rows(temp_current, temp_previous) %>%
  		summarise(mean_yr_temp = mean(mn_val)) %>%
  		mutate(year = x)
   
   }
   
   temp_yr_prior <- lapply(1971:2021, temp_yr_prior_func) %>% bind_rows()
   
   #### add to species datasets to get things moving ####