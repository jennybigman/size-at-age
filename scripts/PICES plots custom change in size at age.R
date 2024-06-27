# plots for ESSAS 2024

	preds <- read.csv(file = here("data", "preds_yr_mods.csv"))

	pcod_preds <- preds %>% filter(short_name == "pcod")
	
	pcod_preds$year_f <- as.factor(pcod_preds$year_f)
	pcod_preds$age_f <- as.factor(pcod_preds$age_f)

	pcod_preds_sum <- pcod_preds %>%
		group_by(year_f, age_f, age, year) %>%
		droplevels() %>%
	 	summarise(mean_est = mean(est),
	 						mean_se = mean(est_se))
	
	pcod_preds_sum <- pcod_preds_sum %>% 
  	arrange(age) %>% 
  	mutate(age_f = fct_inorder(age_f)) 
	 
	pcod_preds_sum$year_f <- fct_reorder(pcod_preds_sum$year_f, pcod_preds_sum$year)

	# set up labels in each facet
	age_labs <- paste0("Age", " ", unique(pcod_preds_sum$age_f))
	age_f <- unique(sort(pcod_preds_sum$age_f))
	year_f <- pcod_preds_sum %>% 
						group_by(age_f) %>% 
						summarise(year_f = max(year) - 22)
	year_f <- as_vector(year_f$year_f)
	
	mean_est <- pcod_preds_sum %>% 
							group_by(age_f) %>% 
							summarise(mean_est = max(mean_est))
	mean_est <- mean_est$mean_est
	
	label_df <- tibble(
		year_f = as.factor(year_f),
		mean_est = mean_est,
		lab = age_labs,
		age_f = age_f
	)
		
	 p <- 
	 	ggplot(pcod_preds_sum, aes( x = year_f, y = mean_est)) +
	 	geom_pointrange(aes(x = year_f, y = mean_est,
	 											ymin = mean_est - mean_se,
	 											ymax = mean_est + mean_se),
	 									size = 0.001) +
	 	facet_wrap(~ age_f, scales = "free_y", nrow = num) +
	 	ylab("Predicted mean (log) weight (g)") +
	 	xlab("Year") +
		geom_text(data = label_df, label = label_df$lab, size = 2) +
	 	scale_x_discrete(guide = guide_axis(angle = 90)) +
		#ggtitle(name) +
	 	theme_sleek() +
	 	theme(
	 		strip.text.x  = element_blank(),
	 		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 6),
	 		panel.spacing.y = unit(0, "lines")
	 	)
	 
	scales <- list(
		scale_y_continuous(
			breaks = c(1.5, 2.0)), 
		scale_y_continuous(
			breaks = c(2.4, 2.8)), 
		scale_y_continuous(
			breaks = c(2.7, 3.1)),
		scale_y_continuous(
			breaks = c(3.1, 3.3)),
		scale_y_continuous(
			breaks = c(3.3, 3.5)),
		scale_y_continuous(
			breaks = c(3.5, 3.7)),
		scale_y_continuous(
			breaks = c(3.6, 3.8)),
		scale_y_continuous(
			breaks = c(3.6, 3.9))
)

	p_scales <- p + facetted_pos_scales(y = scales)
	
	 	 ggsave(here("output", "plots", "May 2024", "year models", paste0("pcod_custom", ".png")), 
	 	 			 height = 3.5, width = 7)

	 	 
	########## POLLOCK #########
	 	 
	# plots for ESSAS 2024

	preds <- read.csv(file = here("data", "preds_yr_mods.csv"))

	pol_preds <- preds %>% filter(short_name == "pollock")
	
	pol_preds$year_f <- as.factor(pol_preds$year_f)
	pol_preds$age_f <- as.factor(pol_preds$age_f)

	pol_preds_sum <- pol_preds %>%
		group_by(year_f, age_f, age, year) %>%
		droplevels() %>%
	 	summarise(mean_est = mean(est),
	 						mean_se = mean(est_se))
	
	pol_preds_sum <- pol_preds_sum %>% 
  	arrange(age) %>% 
  	mutate(age_f = fct_inorder(age_f)) 
	 
	pol_preds_sum$year_f <- fct_reorder(pol_preds_sum$year_f, pol_preds_sum$year)

	# set up labels in each facet
	age_labs <- paste0("Age", " ", unique(pol_preds_sum$age_f))
	age_f <- unique(sort(pol_preds_sum$age_f))
	year_f <- pol_preds_sum %>% 
						group_by(age_f) %>% 
						summarise(year_f = floor(max(year)) - 22)
	year_f <- as_vector(year_f$year_f)
	
	mean_est <- pol_preds_sum %>% 
							group_by(age_f) %>% 
							summarise(mean_est = max(mean_est))
	mean_est <- mean_est$mean_est
	
	label_df <- tibble(
		year_f = as.factor(year_f),
		mean_est = mean_est,
		lab = age_labs,
		age_f = age_f
	)
		
	 p <- 
	 	ggplot(pol_preds_sum, aes( x = year_f, y = mean_est)) +
	 	geom_pointrange(aes(x = year_f, y = mean_est,
	 											ymin = mean_est - mean_se,
	 											ymax = mean_est + mean_se),
	 									size = 0.001) +
	 	facet_wrap(~ age_f, scales = "free_y", nrow = num) +
	 	ylab("Predicted mean (log) weight (g)") +
	 	xlab("Year") +
		geom_text(data = label_df, label = label_df$lab, size = 2) +
	 	scale_x_discrete(guide = guide_axis(angle = 90)) +
		#ggtitle(name) +
	 	theme_sleek() +
	 	theme(
	 		strip.text.x  = element_blank(),
	 		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 6),
	 		panel.spacing.y = unit(0, "lines")
	 	)
	 
	 	 
	scales <- list(
		scale_y_continuous(
			breaks = c(1.0, 1.3)), # age 1
		scale_y_continuous(
			breaks = c(1.8, 2.0)), # age 2
		scale_y_continuous(
			breaks = c(2.2, 2.4)), # age 3
		scale_y_continuous(
			breaks = c(2.6, 2.7)),# age 4
		scale_y_continuous(
			breaks = c(2.7, 2.8)),# age 5
		scale_y_continuous(
			breaks = c(2.8, 2.9)),# age 6
		scale_y_continuous(
			breaks = c(2.9, 3.0)),# age 7
		scale_y_continuous(
			breaks = c(2.9, 3.0)),# age 8
		scale_y_continuous(
			breaks = c(3.0, 3.1)),# age 9
		scale_y_continuous(
			breaks = c(3.0, 3.1)),# age 10
		scale_y_continuous(
			breaks = c(3.0, 3.1)),# age 11
		scale_y_continuous(
			breaks = c(3.0, 3.2)),# age 12
		scale_y_continuous(
			breaks = c(3.0, 3.2)),# age 13
		scale_y_continuous(
			breaks = c(3.0, 3.2))# age 14
)

	p_scales <- p + facetted_pos_scales(y = scales)
	 
	 	 ggsave(here("output", "plots", "May 2024", "year models", paste0("pol_custom", ".png")), 
	 	 			 height = 4, width = 8)
