# comparing model fit to all age classes vs. models fit to separate age classes - Pcod

	#### models for each age class ####
	pcod_dat <- dat_all %>% 
		filter(short_name == "pcod") %>%
		filter(age < 9)
	
	# df by age class
	pcod_dat_split <- pcod_dat %>%
		group_by(age_f) %>%
		group_split()

	# function to fit models for all age classes
	mod_sep_fun <- function(df){
		
		mesh <- make_mesh(df, c("X", "Y"), cutoff = 10)

		m <- sdmTMB(
		  data = df,
		  formula = log_wt ~ poly(presurvey_btemp, 3, raw = TRUE),
		  mesh = mesh,
		  time_varying = ~ 1,
			time_varying_type = "ar1",
			control = sdmTMBcontrol(profile = "b_j"),
		  family = gaussian(),
		  spatial = "on",
		  spatiotemporal = "iid",
		  time = "year",
		  silent = FALSE
		)
		
		m
		
	}
	
	age_mods <- map(pcod_dat_split, mod_sep_fun)
	
	
	#### combined mod ####
	
	mesh <- make_mesh(pcod_dat, c("X", "Y"), cutoff = 10)

	mod_com <- sdmTMB(
	  data = pcod_dat,
	  formula = log_wt ~ 0 + age_f * year_f + age_f * poly(presurvey_btemp, 3, raw = TRUE),
	  control = sdmTMBcontrol(profile = "b_j"),
	  mesh = mesh,
	  family = gaussian(),
	  spatial = "on",
	  spatiotemporal = "IID",
	  time = "year",
	  silent = FALSE)
	
	sanity(mod_com)
	
	
	# plot predictions from sep age class models
	
	
	# first remove models that did not converge
	sanity_fun <- function(num) {
		
		mod <- age_mods[[num]]
		
		s <- sanity(mod)
		
			hess    <- s$hessian_ok 
			eigen   <- s$eigen_values_ok            
			nl      <- s$nlminb_ok       		         
			range 	<- s$range_ok       
			#se_na 	<- s$se_na_ok         
			sigma 	<- s$sigmas_ok        
		
			dat <- mod$data
			
			age <- unique(dat$age_f)
			
			name <- paste0("age_", age)
	
			#df <- tibble(name, hess, eigen, nl, range, se_na, sigma)
			df <- tibble(name, hess, eigen, nl, range, sigma)

			df
		
}

	num = 1:length(age_mods)

	mod_df <- lapply(num, sanity_fun) %>% bind_rows()	 	
	
	mod_df_drop <- mod_df %>%
		filter(if_any(everything(), ~ .x == FALSE))
	
	drop <- mod_df_drop$name

	mod_list_keep <- mod_df %>% 
		filter(name %!in% drop)
	
	
	# predict and plot
	
	sep_age_preds_fun <- function(mod){
	 	
		dat <- mod$data
		yrs <- unique(dat$year)
		age <- unique(dat$age)
		
		max_temp <- max(pcod_dat$presurvey_btemp)
		min_temp <- min(pcod_dat$presurvey_btemp)
		
		yr <- yrs[1]
		
		presurvey_btemp = seq(from = min_temp, to = max_temp, length.out = 200)
		
		nd <- expand_grid(
			presurvey_btemp = presurvey_btemp,
			year = yr,
			age = age) 

		psim <- predict(mod, newdata = nd, re_form = NA, nsim = 200)

		m <- apply(psim, 1, mean)
		se <- apply(psim, 1, sd)
		u <- m + (1.96*se)
		l <- m - (1.96*se)

		out <- bind_cols(nd, m, u, l)
		
		names_out <- c("presurvey_btemp", "year", "age", "est", "upr", "lwr")
		
		names(out) <- names_out
			
		out

	}
	
	preds_sep_age <- map(age_mods, sep_age_preds_fun) %>% bind_rows()
	

	# prediction for combined mod
	
	dat <- mod_com$data
	yrs <- unique(dat$year)
	ages <- sort(unique(dat$age))
		
	max_temp <- max(dat$presurvey_btemp)
	min_temp <- min(dat$presurvey_btemp)
		
	yr <- yrs[1]
		
	presurvey_btemp = seq(from = min_temp, to = max_temp, length.out = 200)
		
	nd <- expand_grid(
		presurvey_btemp = presurvey_btemp,
		year = yr,
		age_f = as.factor(ages),
		year_f = as.factor(yr))

	psim <- predict(mod_com, newdata = nd, re_form = NA, nsim = 200)

	m <- apply(psim, 1, mean)
	se <- apply(psim, 1, sd)
	u <- m + (1.96*se)
	l <- m - (1.96*se)

	out <- bind_cols(nd, m, u, l)
		
	names_out <- c("presurvey_btemp", "year", "age", "year_f", "est", "upr", "lwr")
		
	names(out) <- names_out
	
	out$age_f <- as.factor(out$age)
		
	out <- out %>%
  	arrange(age) %>% 
  	mutate(age_f = fct_inorder(age_f)) 
		
	out$age_label <- paste0("Age ", out$age_f)


	#### plot ####
	
	file_path_plots <- paste0(here("./output/plots/May 2024/"))
	
	# pcod ####
 
	preds_sep_age$age_f <- as.factor(preds_sep_age$age)
	
	preds_sep_age <- preds_sep_age %>%
  	arrange(age) %>% 
  	mutate(age_f = fct_inorder(age_f)) 
		
	preds_sep_age$age_label <- paste0("Age ", preds_sep_age$age_f)

	# plot
		plot <-
					ggplot(preds_sep_age, aes(presurvey_btemp,  est)) +
					geom_ribbon(aes(ymin = lwr, ymax = upr), 
											fill = "lightgrey", alpha = 0.4) +
					geom_line(color = "black") +
					facet_wrap(~ age_f) +
					facet_wrap(~ reorder(age_label, age), scales = "fixed", nrow = 2) +
 					ylab("Predicted mean (log) weight (g)") +					
					xlab("Temperature (˚C)") +
					theme_sleek() +
					ggtitle("single age mods") +
					theme(
				 		panel.spacing.y = unit(0, "lines"))
		
		ggsave(file = paste0(here("./output/plots/pcod_sep_age_plot.png")), plot,
					  height = 5, width = 10, units = "in")
		
	# all ages combined
	plot_com <-
			ggplot(out, aes(presurvey_btemp,  est)) +
			geom_ribbon(aes(ymin = lwr, ymax = upr), 
									fill = "lightgrey", alpha = 0.4) +
			geom_line(color = "black") +
			facet_wrap(~ age_f) +
			facet_wrap(~ reorder(age_label, age), scales = "fixed", nrow = 2) +
 			ylab("Predicted mean (log) weight (g)") +					
			xlab("Temperature (˚C)") +
			ggtitle("combined age mods") +
			theme_sleek() +
			theme(
		 		panel.spacing.y = unit(0, "lines"))
	
		ggsave(file = paste0(here("./output/plots/pcod_combined_plot.png")), plot_com,
					  height = 5, width = 10, units = "in")
		
		
			
	#### do they have the same coefficients ? #####
			
	# separate models for each age class
	
	out_fun <- function(mod){
  
  	mod_dat <- mod$data
  
  	age <- unique(mod_dat$age)
  	
  	term <- paste0("age_f", age)

  	out <- tidy(mod)  
  
  	out$age <- as.character(age)

  	out <- out %>%
  		rename(estimate_sep = estimate,
  					 std.error_sep = std.error) %>%
  		mutate(age = as.character(age),
  					 low_sep = estimate_sep - (1.96 * std.error_sep),
  					 high_sep = estimate_sep + 1.96 * std.error_sep)
  	
  	out
	}
	
	outs_ages <- map(age_mods, out_fun) %>% bind_rows()
	
	# model with age classes combined
	out <- tidy(mod_com)
	
	out <- out %>%
		 dplyr::filter(!grepl("year_f", term))
		
	mod_dat <- mod_com$data
	
	ages <- str_extract(out$term, "[^_age_f]+")

	ages[ages == "poly(pr"] <- "1"

	ages <- gsub("*:", "", ages)

	ages <-  str_extract(ages, "[^_poly]+")

	out$age <- ages
	
	age_len <- length(unique(ages))
	
	term_start <- rep("(Intercept)", age_len)
	term_up <- sub(".*:", "", out$term)
	term_up[1:age_len] <- term_start
	
	out <- out %>%
		mutate(term = term_up,
					 high_com = estimate + (1.96 * std.error),
					 low_com = estimate - (1.96 * std.error)) %>%
		rename(term = term,
					 estimate_com = estimate,
					 std.error_com = std.error)
		
	#### compare ####
	
	preds_comp <- left_join(out, outs_ages, by = c("term", "age"))

	col_order <- c("age", "term", "estimate_com",
	               "estimate_sep", "high_com",
								 "low_com", "high_sep", "low_sep",
								 "std.error_com", "std.error_sep")

	preds_comp <- preds_comp[, col_order]
 
	preds_comp <- preds_comp %>%
  	mutate(
  		in_com_CI = case_when(
  			estimate_sep <= high_com & estimate_sep >= low_com ~ "YES",
  			estimate_sep >= high_com ~ "NO",
  			estimate_sep <= low_com ~ "NO"),
  		in_sep_CI = case_when(
  			estimate_com <= high_sep & estimate_com >= low_sep ~ "YES",
  			estimate_com >= high_sep ~ 'NO',
  			estimate_com <= low_sep ~ 'NO')
  		) %>%
		mutate_if(is.numeric, round, 2)
	
	kbl(preds_comp, digits = 3) %>%
		kable_classic(full_width = TRUE) %>%
		save_kable(file = "output/tables/com_sep_mod_comparison_pcod_CORRECT.png")
 
