# comparing tinyvast and sdmTMB ML mods

	# load tinyVAST mods
	shared_file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	shared_file_list <- list.files(path = shared_file_path) 
	shared_file_list <- str_subset(shared_file_list, "shared", negate = TRUE)
	tv_mods <- purrr::map(paste0(shared_file_path, shared_file_list), readRDS)

	ml_file_path <- paste0(here(), "/output/model output/sdmTMB output/ml/")
	ml_file_list <- list.files(path = ml_file_path)
	ml_mods <- purrr::map(paste0(ml_file_path, ml_file_list), readRDS)

	# AIC function
	AIC_fun <- function(mod) {
  	
	  	AIC <- AIC(mod)
	  	AIC
  	
  }
  
	# tinyVAST mods
	tv_n <- shared_file_list
  tv_AICs <- purrr::map(tv_mods, AIC_fun)
  tv_AICs <- tibble(tv_n, tv_AICs)
  tv_AICs$tv_AICs <- unlist(tv_AICs$tv_AICs)
  
  tv_no_cov <- tv_AICs %>% filter(str_detect(tv_n, "no_cov"))
  tv_AICs <-   tv_AICs %>% filter(!str_detect(tv_n, "no_cov"))

  tv_mod_names <- tv_AICs$tv_n
  
  sp <- str_extract(tv_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(tv_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int", tv_mod_names), 'yes', 'no')
	cov <- ifelse(grepl("temp", tv_mod_names), 'temperature', 'oxygen')
	
	tv_AICs <- tv_AICs %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	tv_no_cov_names <- tv_no_cov$tv_n
	
	sp <- str_extract(tv_no_cov_names, "atooth|pcod|yfin|pollock")
	form <- "no covariate"
	int <- "no"
	cov <- "no covariate"
	
	tv_no_cov <- tv_no_cov %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	tv_AICs <- bind_rows(tv_AICs, tv_no_cov)

	tv_AICs <- tv_AICs %>% 
		select(-tv_n) 

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
	AIC_comp_ml <- left_join(tv_AICs, ml_AICs)
	AIC_comp_ml <- AIC_comp_ml |> relocate(ml_AICs)
	
	AIC_comp_ml$diff <- abs(AIC_comp_ml$ml_AICs - AIC_comp_ml$tv_AICs)
	
	AIC_comp_ml <- gt::gt(AIC_comp_ml)
	
	gt::gtsave(AIC_comp_ml, file = paste0(here(), "/output/tables/comparison of ML tinyVAST and ML sdmTMB mods with (shared) S & ST fields.pdf"))
