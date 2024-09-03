
	############### OLDER PLOTS WITH LABELS INSIDE PLOT ##########
	
	# make plots and save 
	
	yr_plots <- function(sp){
	
	 sp_dat <- preds_df %>%
	 	filter(short_name == sp)
	 
 	 sp_dat_sum <- sp_dat %>%
		group_by(year_f, age_f, age, year) %>%
	 	summarise(mean_est = mean(est),
	 						mean_se = mean(est_se))
	 
 	 name <- unique(sp_dat$common_name)
 	 num <- length(unique(sp_dat$age_f))/3
 	 num <- floor(num)
 	 
 	 x_pos <- unique(sp_dat_sum$year_f)[[2]]
 	 y_pos <- max(sp_dat_sum$mean_est)
 	 
	 sp_dat_sum <- sp_dat_sum %>% 
  	arrange(age) %>% 
  	mutate(age_f = fct_inorder(age_f)) 
	 
	 sp_dat_sum$year_f <- fct_reorder(sp_dat_sum$year_f, sp_dat_sum$year)
	 
	 labels <- paste0("Age", " ", unique(sp_dat_sum$age_f))

	 p <- 
	 	ggplot(sp_dat_sum) +
	 	geom_pointrange(aes(x = year_f, y = mean_est,
	 											ymin = mean_est - mean_se,
	 											ymax = mean_est + mean_se),
	 									size = 0.05) +
	 	facet_wrap(~ fct_reorder(age_f, age), scales = "free_y", nrow = num) +
	 	ylab("Predicted mean (log) weight (g)") +
	 	xlab("Year") +
	 	#annotate("text", x = x_pos, y = y_pos, label = labels) +
	 	scale_x_discrete(guide = guide_axis(angle = 90)) +
		ggtitle(name) +
	 	theme_sleek() +
	 	theme(
	 		axis.title = element_text(size = 8),
	 		axis.text = element_text(size = 7),
	 		strip.text = element_text(size = 8),
	 		panel.spacing.y = unit(0, "lines")
	 	)
	 
	 ggsave(here("output", "plots", "May 2024", "year models", paste0(name, ".png")))

	}
	
	sp <- unique(dat_all$short_name)
	
	purrr::map(sp, yr_plots)
	