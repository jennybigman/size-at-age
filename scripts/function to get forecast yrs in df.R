	
	yrs_fun <- function(sp){

		new_dat <- dat_all %>% filter(short_name == sp)

		len <- length(unique(new_dat$year))
		yrs <- sort(unique(new_dat$year))
		forecast_n_yrs <- len - 10
		forecast_yrs <- tail(yrs, forecast_n_yrs)
	
		df <- tibble(forecast_yrs, short_name = sp)
		
		df
	
	}
	
	sp <- unique(dat_all$short_name)[-1] # we only have one forecasted year for atooth
	
	forecast_yrs_dfs <- purrr::map(sp, yrs_fun) %>% bind_rows()
	
	
	