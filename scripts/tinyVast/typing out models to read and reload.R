	# error if run in a function - ugh, load them below
	
	pol_no_cov                        <- readRDS(paste0(file_path, pol_file_list[[1]]))
	pol_yrprior_boxy_int_lin_tvt      <- readRDS(paste0(file_path, pol_file_list[[2]]))
	pol_yrprior_boxy_int_poly2_tvt    <- readRDS(paste0(file_path, pol_file_list[[3]]))
	pol_yrprior_boxy_int_poly3_tvt    <- readRDS(paste0(file_path, pol_file_list[[4]]))
	pol_yrprior_boxy_lin_tvt          <- readRDS(paste0(file_path, pol_file_list[[5]]))
	pol_yrprior_boxy_poly2_tvt        <- readRDS(paste0(file_path, pol_file_list[[6]]))
	pol_yrprior_boxy_poly3_tvt        <- readRDS(paste0(file_path, pol_file_list[[7]]))
	pol_yrprior_btemp_int_lin_tvt     <- readRDS(paste0(file_path, pol_file_list[[8]]))
	pol_yrprior_btemp_int_poly2_tvt   <- readRDS(paste0(file_path, pol_file_list[[9]]))
	pol_yrprior_btemp_int_poly3_tvt   <- readRDS(paste0(file_path, pol_file_list[[10]]))
	pol_yrprior_btemp_lin_tvt         <- readRDS(paste0(file_path, pol_file_list[[11]]))
	pol_yrprior_btemp_poly2_tvt       <- readRDS(paste0(file_path, pol_file_list[[12]]))
	pol_yrprior_btemp_poly3_tvt       <- readRDS(paste0(file_path, pol_file_list[[13]]))
	
	pol_no_cov                        <- reload_model(pol_no_cov)
	pol_yrprior_boxy_int_lin_tvt      <- reload_model(pol_yrprior_boxy_int_lin_tvt)
	pol_yrprior_boxy_int_poly2_tvt    <- reload_model(pol_yrprior_boxy_int_poly2_tvt)
	pol_yrprior_boxy_int_poly3_tvt    <- reload_model(pol_yrprior_boxy_int_poly3_tvt)
	pol_yrprior_boxy_lin_tvt          <- reload_model(pol_yrprior_boxy_lin_tvt)
	pol_yrprior_boxy_poly2_tvt        <- reload_model(pol_yrprior_boxy_poly2_tvt)
	pol_yrprior_boxy_poly3_tvt        <- reload_model(pol_yrprior_boxy_poly3_tvt)
	pol_yrprior_btemp_int_lin_tvt     <- reload_model(pol_yrprior_btemp_int_lin_tvt)
	pol_yrprior_btemp_int_poly2_tvt   <- reload_model(pol_yrprior_btemp_int_poly2_tvt)
	pol_yrprior_btemp_int_poly3_tvt   <- reload_model(pol_yrprior_btemp_int_poly3_tvt)
	pol_yrprior_btemp_lin_tvt         <- reload_model(pol_yrprior_btemp_lin_tvt)
	pol_yrprior_btemp_poly2_tvt       <- reload_model(pol_yrprior_btemp_poly2_tvt)
	pol_yrprior_btemp_poly3_tvt       <- reload_model(pol_yrprior_btemp_poly3_tvt)
	
	pol_mods <- list(pol_no_cov,
									 pol_yrprior_boxy_int_lin_tvt,
									 pol_yrprior_boxy_int_poly2_tvt,
									 pol_yrprior_boxy_int_poly3_tvt,
									 pol_yrprior_boxy_lin_tvt,
									 pol_yrprior_boxy_poly2_tvt,
									 pol_yrprior_boxy_poly3_tvt,
									 pol_yrprior_btemp_int_lin_tvt,
									 pol_yrprior_btemp_int_poly2_tvt,
									 pol_yrprior_btemp_int_poly3_tvt,
									 pol_yrprior_btemp_lin_tvt,
									 pol_yrprior_btemp_poly2_tvt,
									 pol_yrprior_btemp_poly3_tvt)
	