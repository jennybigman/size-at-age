	plot_file_path <- paste0(here(), "/output/residuals/models no yr RE/")

	resid_check_func <- function(mod){
		
		if (sanity(mod)$all_ok == "FALSE") {
		
			sims <- simulate(mod, nsim = 250)
			dharma_sims <- sims %>% dharma_residuals(mod, plot = FALSE)
		
			p <- 
				ggplot(dharma_sims) +
				geom_point(aes(x = expected, y = observed)) +
				geom_abline(intercept = 0, slope = 1, color = "red")
			
			ggsave(filename = paste0(plot_file_path, mod, ".png"),
						 plot = p,
						 width = 5, height = 5, units = "in")
	
			} else { 
				
				print("nothing needed")
				
				}
	}
	
	check_resid_list <- lapply(pol_mod_list, resid_check_func)
	
	
	
	
	
	# calc resids
	
	resid_df_func <- function(x){
	
		if (class(check_resid_list$x) == "character") {
			
			print('do nothing')
			
		} else {
			
			sims <- simulate(x, nsim = 250)
			dharma_sims <- sims %>% dharma_residuals(x)
			
			fwrite(dharma_sims,
						 file = paste0(here(), "./output/residuals/models no yr RE/",
						 							"resids", x, ".rds"))
		}
	} 
	
	lapply(check_resid_list, resid_df_func)
	
	
		
			
	
