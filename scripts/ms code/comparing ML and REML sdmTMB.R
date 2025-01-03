# comparing sdmTMB models fit with REML and ML

	# load sdmTMB REML mods
	reml_file_path <- paste0(here(), "/output/model output/sdmTMB output/reml/")
	reml_file_list <- list.files(path = reml_file_path)
	reml_mods <- purrr::map(paste0(reml_file_path, reml_file_list), readRDS)

	ml_file_path <- paste0(here(), "/output/model output/sdmTMB output/ml/")
	ml_file_list <- list.files(path = ml_file_path)
	ml_mods <- purrr::map(paste0(ml_file_path, ml_file_list), readRDS)

	# AIC function
	AIC_fun <- function(mod) {
  	
	  	AIC <- AIC(mod)
	  	AIC
  	
  }
  
	# tinyVAST mods
	reml_n <- reml_file_list
  reml_AICs <- purrr::map(reml_mods, AIC_fun)
  reml_AICs <- tibble(reml_n, reml_AICs)
  reml_AICs$reml_AICs <- unlist(reml_AICs$reml_AICs)
  
  reml_no_cov <- reml_AICs %>% filter(str_detect( reml_n, "no_cov"))
  reml_AICs <-   reml_AICs %>% filter(!str_detect(reml_n, "no_cov"))

  reml_mod_names <- reml_AICs$reml_n
  
  sp <- str_extract(         reml_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(        reml_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int",  reml_mod_names), 'yes', 'no')
	cov <- ifelse(grepl("temp", reml_mod_names), 'temperature', 'oxygen')
	
	reml_AICs <- reml_AICs %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	reml_no_cov_names <- reml_no_cov$reml_n
	
	sp <- str_extract(reml_no_cov_names, "atooth|pcod|yfin|pollock")
	form <- "no covariate"
	int <- "no"
	cov <- "no covariate"
	
	reml_no_cov <- reml_no_cov %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	reml_AICs <- bind_rows(reml_AICs, reml_no_cov)

	reml_AICs <- reml_AICs %>% 
		select(-reml_n) 

	# ml mods
  ml_n <- ml_file_list
  ml_AICs <- purrr::map(ml_mods, AIC_fun)
  ml_AICs <- tibble(ml_n, ml_AICs)
  ml_AICs$ml_AICs <- unlist(ml_AICs$ml_AICs)

  ml_no_cov <- ml_AICs %>% filter(str_detect( ml_n, "no_cov"))
  ml_AICs <-   ml_AICs %>% filter(!str_detect(ml_n, "no_cov"))

  ml_mod_names <- ml_AICs$ml_n
  
  sp <- str_extract(          ml_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(        ml_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int",  ml_mod_names), 'yes', 'no')
	cov <- ifelse(grepl("temp", ml_mod_names), 'temperature', 'oxygen')
	
	ml_AICs <- ml_AICs %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	ml_no_cov_names <- ml_no_cov$ml_n
	
	sp <- str_extract(ml_no_cov_names, "atooth|pcod|yfin|pollock")
	form <- "no covariate"
	int <- "no"
	cov <- "no covariate"
	
	ml_no_cov <- ml_no_cov %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	ml_AICs <- bind_rows(ml_AICs, ml_no_cov)

	ml_AICs <- ml_AICs %>% 
		select(-ml_n) 

	# join
	AIC_comp_ml_reml <- left_join(reml_AICs, ml_AICs)
	AIC_comp_ml_reml <- AIC_comp_ml_reml |> relocate(ml_AICs)
	
	AIC_comp_ml_reml$diff <- abs(AIC_comp_ml_reml$ml_AICs - AIC_comp_ml_reml$reml_AICs)
		
	AIC_comp_ml_reml <- gt::gt(AIC_comp_ml_reml)

	gt::gtsave(AIC_comp_ml_reml, file = paste0(here(), "/output/tables/comparison of ML amd REML sdmTMB mods with (shared) S & ST fields.pdf"))
