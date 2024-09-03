	# comparing polynomial syntax

	mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 10)

	m1 <- sdmTMB(
	  data = pcod,
	  formula = present ~ depth_scaled + depth_scaled2,
	  mesh = mesh,
	  family = binomial(link = "logit"),
	  spatial = "on",
	  spatiotemporal = "IID",
	  time = "year"
	)


	m2 <- sdmTMB(
	  data = pcod,
	  formula = present ~ poly(depth_scaled, 2, raw = TRUE),
	  mesh = mesh,
	  family = binomial(link = "logit"),
	  spatial = "on",
	  spatiotemporal = "IID",
	  time = "year"
	)

	# with my data 
	pcod_dat <- dat_all %>% filter(short_name == "pcod")
	mesh <- make_mesh(pcod_dat, c("X", "Y"), cutoff = 10)

	m1 <- sdmTMB(
	  data = pcod_dat,
	  formula = log_wt ~ age_f * poly(presurvey_btemp, 3, raw = TRUE),
	  mesh = mesh,
	  family = gaussian(),
	  spatial = "on",
	  spatiotemporal = "IID",
	  time = "year"
	)
	
	out_com <- tidy(m1)
	
	###
	ages <- out_com$term
	ages[ages == "(Intercept)"] <- "1"
	ages <- str_extract(ages, "[^_age_f]+")
	ages <- gsub("*:", "", ages)
	ages <-  str_extract(ages, "[^_poly]+")
	ages[ages == "("] <- "1"
	
	out_com$age <- ages
	
	age_len <- length(unique(ages))
	
	term_start <- rep("(Intercept)", age_len)
	term_up <- sub(".*:", "", out_com$term)
	term_up[1:age_len] <- term_start
	
	out_com <- out_com %>%
		mutate(term = term_up,
					 high_com = estimate + (1.96 * std.error),
					 low_com = estimate - (1.96 * std.error)) %>%
		rename(term = term,
					 estimate_com = estimate,
					 std.error_com = std.error)
	
	col_order <- c("age", "term", "estimate_com",
	             "low_com", "high_com",
							 "std.error_com")

	out_com <- out_com[, col_order]

		
	###########
	

	# df by age class
	pcod_dat_split <- pcod_dat %>%
		group_by(age_f) %>%
		group_split()

	# function to fit models for all age classes
	mod_test_fun <- function(df){
		
		mesh <- make_mesh(df, c("X", "Y"), cutoff = 10)

		m2 <- sdmTMB(
		  data = df,
		  formula = log_wt ~ poly(presurvey_btemp, 3, raw = TRUE),
		  mesh = mesh,
		  family = gaussian(),
		  spatial = "on",
		  spatiotemporal = "IID",
		  time = "year"
		)
		
		out <- tidy(m2)
		
		dat <- m2$data
		
		age <- unique(dat$age_f)
		
		out$age <- age
		
		out

	}

	age_mods <- map(pcod_dat_split, mod_test_fun)
	
	# wrangle so can join
	
	sep_wrangle_fun <- function(df){

  	df <- df %>%
  		rename(estimate_sep = estimate,
  					 std.error_sep = std.error) %>%
  		mutate(low_sep = estimate_sep - (1.96 * std.error_sep),
  					 high_sep = estimate_sep + 1.96 * std.error_sep)
  	
  	df
  	
	}
	
	age_mods_df <- map(age_mods, sep_wrangle_fun) %>% bind_rows()

	mods_comp_out <- left_join(out_com, age_mods_df)

	col_order <- c("age", "term", "estimate_com",
	              "estimate_sep", "high_com",
							  "low_com", "high_sep", "low_sep",
							  "std.error_com", "std.error_sep")
	
	mods_comp_out <- mods_comp_out[, col_order]
 
	mods_comp_out <- mods_comp_out %>%
  	mutate(in_CI = case_when(
  		estimate_sep < high_com & estimate_sep > low_com ~ "YES",
  		estimate_sep > high_com ~ "NO",
  		estimate_sep < low_com ~ "NO")) 
	
	kbl(mods_comp_out, digits = 3) %>%
		kable_classic(full_width = TRUE) %>%
		save_kable(file = "output/tables/com_sep_mod_comparison_example.png")

	
	
