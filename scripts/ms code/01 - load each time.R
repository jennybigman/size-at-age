
# load each time

	# libraries

	library(here)
	library(tidyverse)
	library(lubridate)
	library(mgcv)
	library(MuMIn)
	library(visreg)
	library(data.table)
	library(gamm4)
#	library(brms)
	library(mgcViz)
	library(patchwork)
	library(grid)
	library(gratia)
	library(sf)
	library(AICcmodavg)
	library(sdmTMB)
	#library(sdmTMBextra)
	library(future)
	library(DHARMa)
	#library(tidymv)
	library(ggsidekick)
	library(see)
	library(rnaturalearth)
	library(knitr)
	library(kableExtra)
	library(ggh4x)
	library(tinyVAST)
	library(fmesher)
	
	`%!in%` = Negate(`%in%`)


	# ggsave func
	ggsave_func <- function(x, y, w = 10, h = 10){
  	ggsave(plot = x,
    file = paste(y,".png", sep = ""),
    width = w, height = h, units = "in")
  }
  
	# beep function
	beep <- function(x = "fanfare"){
		beepr::beep(x)
	}
	
	# theme function
	black_facet_theme <- function(x = 12, y = 14){
			theme_bw() +
 			theme(
 				legend.position = "none", 
 				strip.text = element_text(size = 14, face = "bold", color = "white"),
 				strip.background = element_blank(),
 				panel.grid.major = element_blank(),
  			panel.grid.minor = element_blank(),
  			panel.border = element_rect(fill = NA, color = "grey50"),
 				axis.text=element_text(size = x, colour = "white"),
  			axis.title= element_text(size = y, color = "white"),
  			axis.line = element_line(color = "white"),
  			axis.ticks = element_line(colour = "white"),
 				panel.background = element_rect(fill = "black"),
				panel.grid = element_blank(),
  			plot.background = element_rect(fill = "black", color = "black"))
	}
	#### read in specimen age & weight data ####
	dat_all <- fread(file = here("./data/sp_dat_all_May_2024.csv"))

		
	dat_list <- dat_all %>% group_by(short_name) %>%
		group_split()

	dat_all$year_f <- as.factor(dat_all$year_f)
	dat_all$age_f <- as.factor(dat_all$age_f)
 
	# land polygons for plotting 
	world <- ne_countries(scale = "medium",
                      returnclass = "sf") 

	# specify grid cell and covariate range for predictions

	GAP_dat_fixed <- read.csv(file = paste0(here(), "/data/GAP_dat_fixed.csv"))
	
	# station with most samples - for predictions
	sum <- GAP_dat_fixed |>
		group_by(stationid) |>
		summarise(count = n()) |>
		slice_max(order_by = count)

	st_id <- as.character(sum[1,1])
	
	st <- dat_all |>
		filter(stationid == st_id)
	
	st <- st |> 
		select(latitude, longitude) |>
		filter(row_number() == 1) |>
		add_utm_columns(
			ll_names = c("longitude", "latitude"),
			ll_crs = 4236,
			utm_names = c("X", "Y")
		)
	
	X_pred <- as.numeric(st$X)
	Y_pred <- as.numeric(st$Y)

	# temps and oxygen vals for prediction
	min_temp <- min(dat_all$yrprior_btemp, na.rm = TRUE)
	max_temp <- max(dat_all$yrprior_btemp, na.rm = TRUE)
	
	min_oxy <- min(dat_all$yrprior_boxy, na.rm = TRUE)
	max_oxy <- max(dat_all$yrprior_boxy, na.rm = TRUE)
		 
	# split dfs
	
	pcod_dat <- dat_all |>
		filter(short_name == "pcod")
	
	pol_dat <- dat_all |>
		filter(short_name == "pollock")
	
	atooth_dat <- dat_all |>
		filter(short_name == "atooth")
	
	yfin_dat <- dat_all |>
		filter(short_name == "yfin")

	
	# make a table of samples by year 
	r <- dat_all |>
		group_by(short_name, year) %>%
		summarize(count = n())
	
	r_list <- r %>% ungroup |>
		group_split(short_name)

	
	table_SX <- purrr::map_dfr(r_list, \(df){
		
		species <- unique(df$short_name)

		df <- df |>
			summarize(min_yr = min(year),
								max_yr = max(year),
								samples = sum(count))
	
		min_yr <- df$min_yr
		max_yr <- df$max_yr
		count = as.integer(df$samples)
		year_range = paste0(min_yr, "-", max_yr)
	
		df <- tibble(
			Species = species, 
			"Year range" = year_range,
			"Sample size" = count)
		
	})
	
	table_SX$Species[table_SX$Species == "atooth"] <- "arrowtooth flounder"
	table_SX$Species[table_SX$Species == "pcod"] <- "Pacific cod"
	table_SX$Species[table_SX$Species == "pollock"] <- "walleye pollock"
	table_SX$Species[table_SX$Species == "yfin"] <- "yellowfin sole"

	table_SX <- table_SX |> gt::gt()
	
	#gt::gtsave(table_SX, file = paste0(here(), "/output/tables/table_sample_size.pdf"))
	gt::gtsave(table_SX, file = paste0(here(), "/output/tables/table_sample_size.png"))
