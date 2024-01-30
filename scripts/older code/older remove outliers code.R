	# remove outliers
	
		#row_rm <- as.numeric(names(test_results$rstudent))
		
		#df_trim <- df[-c(row_rm), ]



percentile_func <- function(x, y){
	
	species_dat <- df_list_wrangled_names %>% 
		filter(common_name == x)
		
	species_dat <- species_dat %>%
		mutate(age_f = as.factor(age)) %>%
		group_by(common_name, age_f) %>%
		mutate(lower_bound = quantile(weight, 0.025),
				   upper_bound = quantile(weight, 0.975)) %>%
		select(common_name, age_f, lower_bound, upper_bound) %>%
		distinct_all()
	

}

ages <- 1:20

species = unique(df_list_wrangled_names$common_name)

df_func <- expand.grid(
	x = species,
	y = ages
)

bounds <- purrr::map2(df_func$x, df_func$y, percentile_func) %>% 
	bind_rows() %>%
	rename(
		"species" = x,
		"age" = y)

row.names(bounds) <- NULL

# find the range of wt for each age class

range_comp_func <- function(x){
	
	species_dat <- df_list_wrangled_names %>% 
		filter(common_name == x)
		
	species_age_dat <- species_dat %>%
		mutate(age_f = as.factor(age)) %>%
		group_by(age_f) %>%
		reframe(range = range(weight))

	ranges <- species_age_dat$range %>% as_tibble()
	
	IDs <- rep(c("low", "high"), nrow(ranges) / 2)
	
	key = rep(1:(nrow(ranges) / 2), each = 2)
	
	ranges <- tibble(ranges, IDs, key)
	
	age_n <- length(unique(species_age_dat$age_f))
	
	ranges <- pivot_wider(
		ranges, id_cols = key, names_from = IDs, values_from = value) %>% 
    select(-key) %>%
		mutate(age = 1:age_n,
					 age_f = as.factor(age),
					 species = x)
	
	# bounds
	
	species_dat <- df_list_wrangled_names %>% 
		filter(common_name == x)
		
	bounds <- species_dat %>%
		mutate(age_f = as.factor(age)) %>%
		group_by(common_name, age_f) %>%
		mutate(lower_bound = quantile(weight, 0.025),
				   upper_bound = quantile(weight, 0.975)) %>%
		select(common_name, age_f, lower_bound, upper_bound) %>%
		rename("species" = common_name) %>%
		distinct_all()
	
	# combine
	
	compare <- left_join(ranges, bounds)
	
}


range_quantile_comparison <- map(species, range_comp_func)

df1 <- range_quantile_comparison[[1]]
