
	#### read in specimen age & weight data ####
	
	specimen_dat <- read.csv(file = here("./data/df_list_wrangled_names.csv"))

	# how many NAs per species do I have
	spec_dat_0 <- specimen_dat %>%
		dplyr::select(species, age) %>%
		group_by(species) %>%
		summarise(NAs = sum(is.na(age)))
	
	# join specimen data and ROMS temp
	
	# put each species df into a list
	specimen_dat$haul <- specimen_dat$haul.x
	specimen_dat$cohort_f <- as.factor(specimen_dat$cohort_f)
	
	specimen_dat_list <- list()
 
	specimen_dat_list <- specimen_dat %>%
  	group_split(species) 

	## code random effect haul and year ##
	
	ranef_setup_func <- function(x){
		
		year_sort <- sort(unique(x$year))
		ID <- letters[1:length(year_sort)]
		
		year_ID <- data.frame(year_sort, ID) %>%
			rename(year = year_sort)
		
		dat <- merge(x, year_ID, by = "year")
	
		dat <- dat %>%
			mutate(haul_id = paste(ID, haul, sep = "_"))
	
		dat <- dat %>%
		mutate(ID_f = as.factor(ID),
					 haul_id_f = as.factor(haul_id))
		
	}
	
	specimen_dat_list <- lapply(specimen_dat_list, ranef_setup_func)
	
	#### temp output ####
	
	# load ACLIM temps
	
	# bias corrected using delta method
  load("../../ACLIM2/Data/out/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hind_mn.Rdata")
  load("../../ACLIM2/Data/out/K20P19_CMIP6/allEBS_means/ACLIM_weekly_fut_mn.Rdata")

  # newest ACLIM indices but 3 basins (NEBS, SEBS, other?)
	#load("../../ACLIM2/Data/out/Feb 13/K20P19_CMIP6/allEBS_means/ACLIM_weekly_hind_mn.Rdata")
  #load("../../ACLIM2/Data/out/Feb 13/K20P19_CMIP6/allEBS_means/ACLIM_weekly_fut_mn.Rdata")

  # filter out bottom temps and stick to SEBS
  temp_hind <- ACLIM_weekly_hind %>%
    filter(var == "temp_bottom5m") %>%
  	filter(basin == "SEBS")
  
  temp_proj <- ACLIM_weekly_fut %>%
    filter(var == "temp_bottom5m") %>%
  	filter(year >= 2021) %>%
  	filter(basin == "SEBS")

	#### 1. Spring/Summer temps preceding survey ####
	
  presurvey <- 4:6 # (April to June)
  
  presurvey_hind <- temp_hind %>%
  	filter(mo %in% presurvey) %>%
  	group_by(year) %>%
  	summarise(presurvey_mean_temp= mean(mn_val))
  
  # add to dataframe list
	temp_join_func <- function(x){
 
 			specimen_dat <- left_join(x, presurvey_hind, by = "year")
	}
 
	specimen_temp_dat <- lapply(specimen_dat_list, temp_join_func)


  #### 2. Yearly temp -- avg of temp July - Dec of year before survey & Jan - June year of survey) ####
  
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
   
  temp_yr_prior <- lapply(1971:2020, temp_yr_prior_func) %>% bind_rows()
  
  temp_join_func <- function(x){
 
 			specimen_dat <- left_join(x, temp_yr_prior, by = "year")
	}
 
	specimen_temp_dat <- lapply(specimen_temp_dat, temp_join_func)

  df_test <- specimen_temp_dat[[1]]
  
  #### 3. temp during first year of life ####
  

	temp_age0_func <- function(x){
		
		df <- x %>%
		mutate(year_age0 = cohort,
					 year_age0_f = as.factor(year_age0))
		
		yr_age0_dat <- df %>%
			dplyr::select(year_age0_f, year_age0) %>%
			rename(year = year_age0) %>%
			distinct()
		
		yr_age0_temps <- left_join(yr_age0_dat, temp_yr_prior) %>%
			rename(temp_age0 = mean_yr_temp) %>%
			dplyr::select(-year)
	
		dat <- left_join(df, yr_age0_temps)
	

	}	
	
	specimen_temp_dat <- lapply(specimen_temp_dat, temp_age0_func)
	
	pollock_dat <- specimen_temp_dat[[1]]
	pcod_dat <- specimen_temp_dat[[2]]
	yfinsole_dat <- specimen_temp_dat[[3]]

  
 