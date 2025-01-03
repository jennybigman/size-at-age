# model checks and comparisons

	# file list for mods
	MV_file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	MV_file_list <- list.files(path = paste0(MV_file_path))

	ssf_file_path <- paste0(here(), "/output/model output/tinyVAST/spatial field only/")
	ssf_file_list <- list.files(path = paste0(ssf_file_path)) 
	
	# read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	## residual function
	#res_fun <- function(mod){
	#	
	#	y <- replicate(n = 100, expr = mod$obj$simulate()$y_i)
  #	res <- DHARMa::createDHARMa(simulatedResponse = y,
  #															observedResponse = mod$data$log_wt,
  #															fittedPredictedResponse = fitted(mod))
  #	res
	#}
	#
	## deviance explained function
	#dev_exp_fun <- function(mod){
	#	
  #	dev <- mod$deviance_explained
  #	dev <- unlist(dev)
  #	dev
	#}
	
	# AIC function
	AIC_fun <- function(mod) {
  	
	  	AIC <- AIC(mod)
	  	AIC
  	
  }
 
	MV_mods <- purrr::map2(MV_file_path, MV_file_list, read_mod_fun)
	ssf_mods <- purrr::map2(ssf_file_path, ssf_file_list, read_mod_fun)
		
	# plot residuals
	#MV_res <- purrr::map(MV_mods, res_fun)
	#sharedres <- purrr::map(shared_mods, res_fun)
  
	# compare deviance explained
  #MV_dev_expl <- purrr::map(MV_mods, dev_exp_fun) %>% 
  #	bind_cols() %>% 
  #	t() %>% 
  #	as_tibble()
#
  #MV_n <- MV_file_list
  #
  #MV_dev_exp <- tibble(MV_n, MV_dev_expl) %>%
  #	rename(mod_name = MV_n,
  #				 dev_exp = V1)
  #
  ## all around 0.95
  
  # AICs
  MV_n <- MV_file_list
  MV_AICs <- purrr::map(MV_mods, AIC_fun)
  MV_AICs <- tibble(MV_n, MV_AICs) %>% rename(AIC = MV_AICs)
  MV_AICs$AIC <- unlist(MV_AICs$AIC)
  
  sp <- str_extract(MV_n, "atooth|pcod|yfin|pollock")

  MV_top_mods <- MV_AICs %>% 
  	mutate(sp = sp) %>%
  	group_by(sp) %>%
  	slice_min(order_by = AIC)
  
  MV_top_mods <- MV_top_mods$MV_n
  
  ssf_n <- ssf_file_list
  ssf_AICs <- purrr::map(ssf_mods, AIC_fun)
  ssf_AICs <- tibble(ssf_n, ssf_AICs) %>% rename(AIC = ssf_AICs)
  ssf_AICs$AIC <- unlist(ssf_AICs$AIC)
  
  sp <- str_extract(ssf_n, "atooth|pcod|yfin|pollock")

  ssf_top_mods <- ssf_AICs %>% 
  	mutate(sp = sp) %>%
  	group_by(sp) %>%
  	slice_min(order_by = AIC)
  
  ssf_top_mods <- ssf_top_mods$ssf_n

  saveRDS(MV_top_mods, file = paste0(here(), "/output/tables/MV_top_mods.rds"))
  saveRDS(ssf_top_mods, file = paste0(here(), "/output/tables/ssf_top_mods.rds"))
  
  # make nice tables
  
  # MV
  MV_no_cov <- MV_AICs %>% filter(str_detect(MV_n, "no_cov"))
  MV_AICs <- MV_AICs %>% filter(!str_detect(MV_n, "no_cov"))

  MV_mod_names <- MV_AICs$MV_n
  
  sp <- str_extract(MV_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(MV_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int", MV_mod_names), 'yes', 'no')
	cov <- ifelse(grepl("temp", MV_mod_names), 'temperature', 'oxygen')
	
	MV_AICs <- MV_AICs %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	MV_no_cov_names <- MV_no_cov$MV_n
	
	sp <- str_extract(MV_no_cov_names, "atooth|pcod|yfin|pollock")
	form <- "no covariate"
	int <- "no"
	cov <- "no covariate"
	
	MV_no_cov <- MV_no_cov %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	MV_AICs <- bind_rows(MV_AICs, MV_no_cov)

	MV_AICs <- MV_AICs %>% 
		select(-MV_n) %>%
		rename(MV_AIC = AIC)

	# shared
	ssf_no_cov <- ssf_AICs %>% filter(str_detect( ssf_n, "no_cov"))
  ssf_AICs <-   ssf_AICs %>% filter(!str_detect(ssf_n, "no_cov"))

  ssf_mod_names <- ssf_AICs$ssf_n
  
  sp <- str_extract(ssf_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(ssf_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int", ssf_mod_names), 'yes', 'no')
	cov <- ifelse(grepl("temp", ssf_mod_names), 'temperature', 'oxygen')
	
	ssf_AICs <- ssf_AICs %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	ssf_no_cov_names <- ssf_no_cov$ssf_n
	
	sp <- str_extract(ssf_no_cov_names, "atooth|pcod|yfin|pollock")
	form <- "no covariate"
	int <- "no"
	cov <- "no covariate"
	
	ssf_no_cov <- ssf_no_cov %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	ssf_AICs <- bind_rows(ssf_AICs, ssf_no_cov)
	
	ssf_AICs <- ssf_AICs %>% 
		select(-ssf_n) %>%
		rename(ssf_AIC = AIC)
	
	AIC_comp <- left_join(MV_AICs, ssf_AICs) %>%
		mutate(sp = case_when(
			sp == "atooth" ~ "Arrowtooth flounder",
			sp == "pcod" ~ "Pacific cod",
			sp == "pollock" ~ "Walleye pollock",
			sp == "yfin" ~ "Yellowfin sole"
		)) %>%
		mutate(form = case_when(
			form == "lin" ~ "linear",
			form == "poly2" ~ "2nd order polynomial",
			form == "poly3" ~ "3rd order polynomial",
			form == "no covariate" ~ "no covariate"
		)) 	|>
		select(sp, form, cov, int, MV_AIC, ssf_AIC)

	AIC_comp_list <- AIC_comp %>% group_by(sp) |> group_split()
	
	atooth <- AIC_comp_list[[1]]
	pcod <- AIC_comp_list[[2]]
	pollock <- AIC_comp_list[[3]]
	yfin <- AIC_comp_list[[4]]
	
	AIC_tables <- purrr::map(AIC_comp_list, \(df){ 
	
	sp <- unique(df$sp)
	
	# tables
	AIC_tab <- 
		df |> 
 		arrange(MV_AIC) |>
			gt::gt() |>
			cols_label(
				MV_AIC =      md("**Multivariate <br> model AIC**"),
				sp =          md("**Species**"),
				form =        md("**Functional <br> form**"),
				int =         md("**Interaction?**"),
				cov =         md("**Covariate**"),
				shared_AIC =  md("**Shared <br> model AIC**")
			)  
	
	gtsave(AIC_tab, file = paste0(here(), "/output/tables/", sp, "_AIC_MV_vs_shared.pdf"))
	
	
	})
	
	atooth <- AIC_comp_list[[1]]
	pcod <- AIC_comp_list[[2]]
	pollock <- AIC_comp_list[[3]]
	yfin <- AIC_comp_list[[4]]

	
	
		
	# plot residuals
	MV_pcod_res <- purrr::map(MV_pcod_mods, res_fun)
  
	shared_pcod_res <- purrr::map(shared_pcod_mods, res_fun)
  
	# compare deviance explained
  MV_pcod_dev_expl <- purrr::map(MV_pcod_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  MV_n <- MV_pcod_file_list
  
  MV_pcod_dev_exp <- tibble(MV_n, MV_pcod_dev_expl) %>%
  	rename(mod_name = MV_n,
  				 dev_exp = V1)
  
  # all around 0.95
  
  # AICs
  MV_n <- MV_pcod_file_list
  MV_pc_AICs <- purrr::map(MV_pcod_mods, AIC_fun)
  MV_pc_AICs <- tibble(MV_n, MV_pc_AICs) %>% rename(AIC = MV_pc_AICs)
  MV_pc_AICs$AIC <- unlist(MV_pc_AICs$AIC)
  
  shared_n <- shared_pcod_file_list
  shared_pc_AICs <- purrr::map(shared_pcod_mods, AIC_fun)
  shared_pc_AICs <- tibble(shared_n, shared_pc_AICs) %>% rename(AIC = shared_pc_AICs)
  shared_pc_AICs$AIC <- unlist(shared_pc_AICs$AIC)
  
  ## kappa
  #pcod_kappas <- purrr::map(pcod_mods, kappa_fun)
  #pcod_kappas <- tibble(n, pcod_kappas) %>% rename(kappa = pcod_kappas)
  #pcod_kappas$kappa <- unlist(pcod_kappas$kappa)
  
  
  # plot predictions - first need to rerun top model
  
  # need to refit model without expressions
   
  # predict
  nd <- expand.grid(
				y = seq(
					from = min(top_mod$data$yrprior_btemp, na.rm = TRUE),
					to = max(top_mod$data$yrprior_btemp, na.rm = TRUE),
					length.out = 25),
				age_f = unique(pcod_dat$age_f),
				year = 2004) 
  
  nd <- data.frame(nd)
  
  p <- predict(top_mod, newdata = nd, se.fit = FALSE)
  
  

  # atooth ####
  
	atooth_file_list <- stringr::str_subset(file_list, 'atooth')
	
	atooth_mods <- purrr::map(atooth_file_list, read_mod_fun)
	
  # plot residuals
	atooth_res <- purrr::map(atooth_mods, res_fun)
  
	# compare deviance explained
  atooth_dev_expl <- purrr::map(atooth_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  n <- atooth_file_list
  
  atooth_dev_expl <- tibble(n, atooth_dev_expl) %>%
  	rename(mod_name = n,
  				 dev_exp = V1)
  
  # AICs
  
  a_AICs <- purrr::map(atooth_mods, AIC_fun)
  	
  a_AICs <- tibble(n, a_AICs) %>% rename(AIC = a_AICs)
  
  a_AICs$AIC <- unlist(a_AICs$AIC)
  
  # kappa
  atooth_kappas <- purrr::map(atooth_mods, kappa_fun)
  
  atooth_kappas <- tibble(n, atooth_kappas) %>% rename(kappa = atooth_kappas)

  atooth_kappas$kappa <- unlist(atooth_kappas$kappa)
 
  
  # pollock ####
  pol_file_list <- stringr::str_subset(file_list, 'pol_')
	
	pol_mods <- purrr::map(pol_file_list, read_mod_fun)
	
  # plot residuals
	pol_res <- purrr::map(pol_mods, res_fun)
  

	# compare deviance explained
  pol_dev_expl <- purrr::map(pol_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  n <- pol_file_list
  
  pol_dev_expl <- tibble(n, pol_dev_expl) %>%
  	rename(mod_name = n,
  				 dev_exp = V1)
  
  # AICs
  
  pol_AICs <- purrr::map(pol_mods, AIC_fun)
  	
  pol_AICs <- tibble(n, pol_AICs) %>% rename(AIC = pol_AICs)
  
  pol_AICs$AIC <- unlist(pol_AICs$AIC)
  
  # some issue with some of the pollock mods
  
  # predict
  
  
  m <- tinyVAST(
		data = pol_dat,
  	formula = log_wt ~ 0 + age_f * yrprior_btemp,
  	family = gaussian(),
  	#sem = "", 
  	#dsem = dsem,
  	#space_column = c("X", "Y"), 
  	#spatial_graph = mesh_v,
  	#time_column = "year",
  	#times = 1993:2022,
  	variable_column = "age_f",
  	control = tinyVASTcontrol(profile = "alpha_j"))
  
  
  
  nd <- crossing(
				yrprior_btemp = seq(
					from = min(mod$data$yrprior_btemp),
					to = max(mod$data$yrprior_btemp),
					length.out = 25),
				age_f = unique(mod$data$age_f),
				year = 2004) 
  
  nd <- data.frame(nd)
  
  p <- predict(m, newdata = nd, se.fit = FALSE)
  


  #### yfin ####
  yfin_file_list <- stringr::str_subset(file_list, 'yfin')
	
	yfin_mods <- purrr::map(yfin_file_list, read_mod_fun)
	
  # plot residuals
	yfin_res <- purrr::map(yfin_mods, res_fun)
  
	plot(yfin_res[[1]])
  
	# compare deviance explained
  
  yfin_dev_expl <- purrr::map(yfin_mods, dev_exp_fun) %>% 
  	bind_cols() %>% 
  	t() %>% 
  	as_tibble()

  n <- yfin_file_list
  
  yfin_dev_expl <- tibble(n, d) %>%
  	rename(mod_name = n,
  				 dev_exp = V1)
  
  # all around 0.95
  
  # AICs
  
  yfin_AICs <- purrr::map(yfin_mods, AIC_fun)
  	
  yfin_AICs <- tibble(n, yfin_AICs) %>% rename(AIC = yfin_AICs)
  
  yfin_AICs$AIC <- unlist(yfin_AICs$AIC)
  