# comparing sdmTMB models fit with REML and ML

	# load tinyVAST mods
	shared_file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	shared_file_list <- list.files(path = shared_file_path) 
	shared_file_list <- str_subset(shared_file_list, "shared", negate = TRUE)
	tv_mods <- purrr::map(paste0(shared_file_path, shared_file_list), readRDS)

	# load sdmTMB mods
	sdmTMB_file_path <- paste0(here(), "/output/model output/sdmTMB output/reml/")
	sdmTMB_file_list <- list.files(path = sdmTMB_file_path)
	sdmTMB_mods <- purrr::map(paste0(sdmTMB_file_path, sdmTMB_file_list), readRDS)

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

	# sdmTMB mods
  sdmTMB_n <- shared_file_list
  sdmTMB_AICs <- purrr::map(sdmTMB_mods, AIC_fun)
  sdmTMB_AICs <- tibble(sdmTMB_n, sdmTMB_AICs)
  sdmTMB_AICs$sdmTMB_AICs <- unlist(sdmTMB_AICs$sdmTMB_AICs)

  sdmTMB_no_cov <- sdmTMB_AICs %>% filter(str_detect( sdmTMB_n, "no_cov"))
  sdmTMB_AICs <-   sdmTMB_AICs %>% filter(!str_detect(sdmTMB_n, "no_cov"))

  sdmTMB_mod_names <- sdmTMB_AICs$sdmTMB_n
  
  sp <- str_extract(          sdmTMB_mod_names, "atooth|pcod|yfin|pollock")
	form <- str_extract(        sdmTMB_mod_names, "poly3|poly2|lin")
	int <- ifelse(grepl("int",  sdmTMB_mod_names), 'yes', 'no')
	cov <- ifelse(grepl("temp", sdmTMB_mod_names), 'temperature', 'oxygen')
	
	sdmTMB_AICs <- sdmTMB_AICs %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	sdmTMB_no_cov_names <- sdmTMB_no_cov$sdmTMB_n
	
	sp <- str_extract(sdmTMB_no_cov_names, "atooth|pcod|yfin|pollock")
	form <- "no covariate"
	int <- "no"
	cov <- "no covariate"
	
	sdmTMB_no_cov <- sdmTMB_no_cov %>%
		mutate(sp = sp,
					 form = form,
					 int = int,
					 cov = cov)
	
	sdmTMB_AICs <- bind_rows(sdmTMB_AICs, sdmTMB_no_cov)

	sdmTMB_AICs <- sdmTMB_AICs %>% 
		select(-sdmTMB_n) 

	# join
	AIC_comp <- left_join(tv_AICs, sdmTMB_AICs)
	AIC_comp <- AIC_comp |> relocate(sdmTMB_AICs)
	
	AIC_comp$diff <- abs(AIC_comp$sdmTMB_AICs - AIC_comp$tv_AICs)
	
	AIC_comp <- gt::gt(AIC_comp)
	
	gt::gtsave(AIC_comp, file = paste0(here(), "/output/tables/comparison of ML tinyVAST mods amd REML sdmTMB mods with (shared) S & ST fields.pdf"))
