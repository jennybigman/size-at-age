# tables for paper - only age-specific RE

# model checks and comparisons

	# file list for mods
	MV_file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	MV_file_list <- list.files(path = paste0(MV_file_path))
	MV_file_list <- MV_file_list[-40]

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

	## residuals
	#MV_res <- purrr::map(MV_mods, res_fun)
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

	
	AIC_comp <- MV_AICs %>%
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
		select(sp, form, cov, int, MV_AIC)

	
	AIC_comp_list <- AIC_comp %>% group_by(sp) |> group_split()

	
	# make pretty AIC tables
	AIC_tables <- purrr::map(AIC_comp_list, \(df){ 
	
	sp <- unique(df$sp)
	
	min_AIC <- min(df$MV_AIC)
	
	cutoff <- min_AIC + 2
	
	# tables
	AIC_tab <- 
		df |> 
 		dplyr::arrange(MV_AIC) |>
			gt::gt() |>
			gt::cols_label(
				MV_AIC =      md("AIC (age-specific fields model)"),
				sp =          md("Species"),
				form =        md("Functional form"),
				int =         md("Interaction?"),
				cov =         md("Covariate")
				) |>
		tab_style(
			style = cell_fill(color = "lightgrey"),
				locations = cells_body(
					columns = everything(),
					rows = MV_AIC < cutoff)
			) |>
			tab_style(
			style = cell_text(weight = "bold"),
				locations = cells_body(
					columns = everything(),
					rows = MV_AIC == min_AIC))

		
	
	gt::gtsave(AIC_tab, file = paste0(here(), "/output/tables/for paper/", sp, "_AIC_MV_top_mods.png"))
	
	
	})
	
	AIC_tables[1]
	