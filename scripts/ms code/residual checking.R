# top model residual plots

	file_path_MV <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	shared_file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	ssf_file_path <- paste0(here(), "/output/model output/tinyVAST/spatial field only/")
	
	MV_top_mods <- readRDS(file = paste0(here(), "/output/tables/MV_top_mods.rds"))
	shared_top_mods_temp <- readRDS(file = paste0(here(), "/output/tables/shared_top_mods_temp.rds"))
  ssf_top_mods_temp <- readRDS(file = paste0(here(), "/output/tables/ssf_top_mods_temp.rds"))
  
  shared_top_mods <- readRDS(file = paste0(here(), "/output/tables/shared_top_mods.rds"))
  ssf_top_mods <- readRDS(file = paste0(here(), "/output/tables/ssf_top_mods.rds"))
  
  
  shared_top_mods <- unique(c(shared_top_mods, shared_top_mods_temp))

  # read mod function
	read_mod_fun <- function(file_path, mod){
		
		mod <- readRDS(paste0(file_path, mod))
		mod
		
	}
	
	# read in mods
	top_mods_MV <- purrr::map2(file_path_MV, MV_top_mods, read_mod_fun)
	top_mods_shared <- purrr::map2(shared_file_path, shared_top_mods, read_mod_fun)
	top_mods_ssf <- purrr::map2(ssf_file_path, ssf_top_mods_temp, read_mod_fun)

	# residual function
	res_fun <- function(mod){
		
		y <- replicate(n = 500, expr = mod$obj$simulate()$y_i)
  	res <- DHARMa::createDHARMa(simulatedResponse = y,
  															observedResponse = mod$data$log_wt,
  															fittedPredictedResponse = fitted(mod))
 
		res
		
	
	}
	
	MV_res <- purrr::map(top_mods_MV, res_fun)
	shared_res <- purrr::map(top_mods_shared, res_fun)
	ssf_res <- purrr::map(top_mods_ssf, res_fun)

	# qq plot
	
	qq_fun <- function(df, mod_name, type){
		
		jpeg(file = paste0(here(), "/output/residuals/qq plots/", mod_name, "_", type, ".jpeg"))
		plotQQunif(df, testDispersion = F, testOutliers = F, testUniformity = F)
		dev.off()
		
	}
	
	
	purrr::pmap(list(MV_res, MV_top_mods, "qq_MV"), qq_fun)
	purrr::pmap(list(shared_res, shared_top_mods, "qq_shared"), qq_fun)
	purrr::pmap(list(ssf_res, ssf_top_mods_temp, "qq_ssf"), qq_fun)
	
	# resid vs fitted
	rvf_fun <- function(df, mod, mod_name, type){
		
		jpeg(file = paste0(here(), "/output/residuals/resid vs fitted/", mod_name, "_", type, ".jpeg"))
		plotResiduals(df, rank = F, smoothScatter = FALSE)
		dev.off()
		
	}
	
	
	purrr::pmap(list(MV_res, top_mods_MV, MV_top_mods, "rvf_MV"), rvf_fun)
	purrr::pmap(list(shared_res, top_mods_shared, shared_top_mods, "rvf_shared"), rvf_fun)
	purrr::pmap(list(ssf_res, top_mods_ssf, ssf_top_mods, "rvf_ssf"), rvf_fun)

	# resid vs fitted
	rvt_fun <- function(df, mod, mod_name, type){
		
		dat <- mod$data
		var_form <- mod$formula
			
		var <- gsub(".*[()]([^,]+)[,].*", "\\1", var_form)
		var <- var[3]
		
		
		if (var == "yrprior_btemp"){
	
			jpeg(file = paste0(here(), "/output/residuals/resid vs temp or oxy/", mod_name, "_", type, ".jpeg"))
			plotResiduals(df, rank = F, form = dat$yrprior_btemp, smoothScatter = FALSE)
			dev.off()
			
		} else if (var == "yrprior_boxy"){
	
			jpeg(file = paste0(here(), "/output/residuals/resid vs temp or oxy/", mod_name, "_", type, ".jpeg"))
			plotResiduals(df, rank = F, form = dat$yrprior_boxy, smoothScatter = FALSE)
			dev.off()
		
		}
		}
	
	
	purrr::pmap(list(MV_res, top_mods_MV, MV_top_mods, "rvt_MV"), rvt_fun)
	purrr::pmap(list(ssf_res, top_mods_ssf, ssf_top_mods, "rvf_sst"), rvt_fun)
	purrr::pmap(list(shared_res, top_mods_shared, shared_top_mods, "rvt_shared"), rvt_fun)

	
	# for all models
	MV_file_path <- paste0(here(), "/output/model output/tinyVAST/fully MV/")
	MV_file_list <- list.files(path = paste0(MV_file_path))
	MV_mods <- purrr::map2(MV_file_path, MV_file_list, read_mod_fun)

	shared_file_path <- paste0(here(), "/output/model output/tinyVAST/all shared/")
	shared_file_list <- list.files(path = paste0(shared_file_path)) 
	shared_mods <- purrr::map2(shared_file_path, shared_file_list, read_mod_fun)

	ssf_file_path <- paste0(here(), "/output/model output/tinyVAST/spatial field only/")
	ssf_file_list <- list.files(path = paste0(shared_file_path)) 
	ssf_mods <- purrr::map2(ssf_file_path, ssf_file_list, read_mod_fun)

	# calculate DHARMa residuals
	MV_res <- purrr::map(MV_mods, res_fun)
	shared_res <- purrr::map(shared_mods, res_fun)
	ssf_res <- purrr::map(ssf_mods, res_fun)

	# QQ plots
	purrr::pmap(list(MV_res, MV_mods, "other_qq_MV"), qq_fun)
	purrr::pmap(list(shared_res, shared_mods, "other_qq_shared"), qq_fun)
	purrr::pmap(list(ssf_res, ssf_mods, "other_qq_ssf"), qq_fun)
	
	# residual plots
	purrr::pmap(list(MV_res, MV_mods, MV_file_list, "rvf_MV"), rvf_fun)
	purrr::pmap(list(shared_res, shared_mods, shared_file_list, "rvf_shared"), rvf_fun)
	purrr::pmap(list(ssf_res, ssf_mods, ssf_file_list, "rvf_ssf"), rvf_fun)

	purrr::pmap(list(MV_res, MV_mods, MV_file_list, "rvt_MV"), rvt_fun)
	purrr::pmap(list(shared_res, shared_mods, shared_file_list, "rvf_ssf"), rvt_fun)
	purrr::pmap(list(ssf_res, ssf_mods, ssf_file_list, "rvf_ssf"), rvt_fun)

	# mapping residuals