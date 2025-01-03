# model checks and comparisons

	# file list for mods
	MV_file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	MV_file_list <- list.files(path = paste0(MV_file_path))
	MV_file_list <- MV_file_list[-40]
	
	shared_file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	shared_file_list <- list.files(path = paste0(shared_file_path)) 
	shared_file_list <- shared_file_list[-c(40:42)]
	
	ssf_file_path <- paste0(here(), "/output/model output/tinyVAST/spatial field only/")
	ssf_file_list <- list.files(path = paste0(ssf_file_path)) 
	ssf_file_list <- ssf_file_list[-40]

	# read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	# residual function
	res_fun <- function(mod){
		
		y <- replicate(n = 1000, expr = mod$obj$simulate()$y_i)
  	res <- DHARMa::createDHARMa(simulatedResponse = y,
  															observedResponse = mod$data$log_wt,
  															fittedPredictedResponse = fitted(mod))
 
		res
		
	
	}
	
	# deviance explained function
	dev_exp_fun <- function(mod){
		
  	dev <- mod$deviance_explained
  	dev <- unlist(dev)
  	dev
	}
	
	# AIC function
	AIC_fun <- function(mod) {
  	
	  	AIC <- AIC(mod)
	  	AIC
  	
  }
  

	# read mods in
	MV_mods <- purrr::map2(MV_file_path, MV_file_list, read_mod_fun)
	shared_mods <- purrr::map2(shared_file_path, shared_file_list, read_mod_fun)
	ssf_mods <- purrr::map2(ssf_file_path, ssf_file_list, read_mod_fun)

	
	## residuals
	#MV_res <- purrr::map(MV_mods, res_fun)
	#shared_res <- purrr::map(shared_mods, res_plot_fun)
	#ssf_res <- purrr::map(ssf_mods, res_plot_fun)
	#
#
	#
	#
	## plots 
	#MV_res_plots <- purrr::map2(MV_res, MV_file_list, \(df, name){
	#	
	#		p <- plotQQunif(df, testDispersion = F, testOutliers = F, testUniformity = F)
	#		ggsave(filename = paste0(here(), "/output/plots/residual plots tinyVAST/residual_plot_reml_", name, "_MV", ".png"),
	#						 plot)
	#})
	#
	#	shared_res_plots <- purrr::map2(shared_res, shared_file_list, \(plot, name){
	#	ggsave(filename = paste0(here(), "/output/plots/residual plots tinyVAST/residual_plot_reml_", name, "_shared_S_ST", ".png"),
	#				 plot)
	#})
	#	
	#	ssf_res_plots <- purrr::map2(ssf_res, ssf_file_list, \(plot, name){
	#	ggsave(filename = paste0(here(), "/output/plots/residual plots tinyVAST/residual_plot_reml_", name, "_shared_S", ".png"),
	#				 p)
	#})
	## compare deviance explained
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
	
	# models with age-specific fields
  MV_n <- MV_file_list
  MV_AICs <- purrr::map(MV_mods, AIC_fun)
  MV_AICs <- tibble(MV_n, MV_AICs) %>% rename(AIC = MV_AICs)
  MV_AICs$AIC <- unlist(MV_AICs$AIC)
  
	# models with shared spatial and spatiotemporal fields
  shared_n <- shared_file_list
  shared_AICs <- purrr::map(shared_mods, AIC_fun)
  shared_AICs <- tibble(shared_n, shared_AICs) %>% rename(AIC = shared_AICs)
  shared_AICs$AIC <- unlist(shared_AICs$AIC)
  
	# models with shared spatial field only
  ssf_n <- ssf_file_list
  ssf_AICs <- purrr::map(ssf_mods, AIC_fun)
  ssf_AICs <- tibble(ssf_n, ssf_AICs) %>% rename(AIC = ssf_AICs)
  ssf_AICs$AIC <- unlist(ssf_AICs$AIC)
 
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
	shared_no_cov <- shared_AICs %>% filter(str_detect( shared_n, "no_cov"))
  shared_AICs <-   shared_AICs %>% filter(!str_detect(shared_n, "no_cov"))

  shared_mod_names <- shared_AICs$shared_n
  
  sp <- str_extract(shared_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(shared_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int", shared_mod_names), 'yes', 'no')
	cov <- ifelse(grepl("temp", shared_mod_names), 'temperature', 'oxygen')
	
	shared_AICs <- shared_AICs %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	shared_no_cov_names <- shared_no_cov$shared_n
	
	sp <- str_extract(shared_no_cov_names, "atooth|pcod|yfin|pollock")
	form <- "no covariate"
	int <- "no"
	cov <- "no covariate"
	
	shared_no_cov <- shared_no_cov %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	shared_AICs <- bind_rows(shared_AICs, shared_no_cov)
	
	shared_AICs <- shared_AICs %>% 
		select(-shared_n) %>%
		rename(shared_AIC = AIC)
	
	# shared spatial field
	ssf_no_cov <- ssf_AICs %>% filter(str_detect(ssf_n, "no_cov"))
  ssf_AICs <- ssf_AICs %>% filter(!str_detect(ssf_n, "no_cov"))

  ssf_mod_names <- ssf_AICs$ssf_n
  
  sp <- str_extract(        ssf_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(       ssf_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int",  ssf_mod_names), 'yes', 'no')
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

	ssf_AICs <-ssf_AICs %>% 
		select(-ssf_n) %>%
		rename(ssf_AIC = AIC)

	
	AIC_comp <- left_join(MV_AICs, shared_AICs) %>%
		left_join(ssf_AICs) |>
		mutate(sp = case_when(
			sp == "atooth" ~ "arrowtooth flounder",
			sp == "pcod" ~ "Pacific cod",
			sp == "pollock" ~ "walleye pollock",
			sp == "yfin" ~ "yellowfin sole"
		)) %>%
		mutate(form = case_when(
			form == "lin" ~ "linear",
			form == "poly2" ~ "2nd order polynomial",
			form == "poly3" ~ "3rd order polynomial",
			form == "no covariate" ~ "no covariate"
		)) 	|>
		select(sp, form, cov, int, MV_AIC, shared_AIC, ssf_AIC)
	
	AIC_comp_list <- AIC_comp %>% group_by(sp) |> group_split()

	
	# make pretty AIC tables
	AIC_tables <- purrr::map(AIC_comp_list, \(df){ 
	
	sp <- unique(df$sp)
	
	# tables
	AIC_tab <- 
		df |> 
 		dplyr::arrange(shared_AIC) |>
			gt::gt() |>
			gt::cols_label(
				MV_AIC =      md("age-specific fields"),
				sp =          md("Species"),
				form =        md("Functional form"),
				int =         md("Interaction?"),
				cov =         md("Covariate"),
				shared_AIC =  md("shared spatial/spatiotemporal fields"),
				ssf_AIC    =  md("shared spatial field")) |>
	#	tab_style(
	#		style = cell_text(align = "center"),
	#		locations = cells_column_labels(columns = everything())
	#	) |>
		tab_spanner(
			label = "AIC values",
			columns = contains("AIC")
		) |>
		 tab_style(
        style = cell_borders(sides = "left", color = "white", weight = px(12), style = "solid"),
        locations = cells_body(
          columns = everything()))
  #    ) |>
	#	cols_width(
	#		contains("AIC") ~ px(300),
	#		form ~ px(100),
	#		sp ~ px(100)
	#	)

	
	gt::gtsave(AIC_tab, file = paste0(here(), "/output/tables/for paper/", sp, "_all_mods_AIC_MV_vs_shared_ML.png"))
	
	
	})
	