# examining outliers using a Bonferroni correction
	library(car)
	library(conflicted)

		# prefer map from purrr
	conflicts_prefer(purrr::map)
	conflicts_prefer(dplyr::filter)

	# how many samples by species
	sample_sizes_pre <- GAP_dat_fixed %>%
		group_by(common_name) %>%
		summarise(count = n())
	
	# bonferroni correction
	bonf_func <- function(x){	

		df <- GAP_dat_fixed %>% dplyr::filter(common_name ==  x)
	
		df <- df %>%
			mutate(log_wt = log10(weight),
						 log_length = log10(length),
						 age_f = as.factor(age))
		
		mod <- lm(log_wt ~ log_length * age_f, data = df)
	
		test_results <- outlierTest(mod, cutoff = 0.7, n.max = 50000) # keep number bigger than n obs
		
		row_rm <- as.numeric(names(test_results$rstudent))
		
		df_trim <- df[-c(row_rm), ]
	
		df_trim

	}
	
	species <- unique(GAP_dat_fixed$common_name)

	dat_all <- purrr::map(species, bonf_func) %>% bind_rows()
	
	# how many samples by species
	sample_sizes_post <- dat_all %>%
		group_by(common_name) %>%
		summarise(count = n())
	
	# sample size comparison
	sample_sizes_pre <- sample_sizes_pre %>%
		rename(count_before = count)
	
	sample_sizes_post <- sample_sizes_post %>%
		rename(count_after = count)
	
	sample_size_comp <- full_join(sample_sizes_pre, sample_sizes_post) %>%
		rowwise() %>%
		mutate(diff = count_before - count_after)
	
	# split into list for plotting
	dat_split <- dat_all %>% 
		group_by(common_name) %>%
		group_split()
	
	# histograms of weight by age
	file_path <- "/plots/"
	
	hist_func <- function(df){
	
		plot <-
			ggplot(data = df, aes(log10(weight))) +
			geom_histogram() +
			facet_wrap(~ age, scales = "free") +
			ggtitle(unique(df$common_name))
		
			
		ggsave(paste0(here(), file_path, unique(df$common_name), "hist_post_out_rm_May2024.png"),
					 plot, height = 5, width = 10, units = "in")

	}
	
#	lapply(dat_split, hist_func)
	
	
	
	write.csv(dat_all, file = here("./data/dat_all.csv"))

