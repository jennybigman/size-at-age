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
	library(brms)
	library(mgcViz)
	library(patchwork)
	library(grid)
	library(gratia)
	library(sf)
	library(AICcmodavg)
	library(sdmTMB)
	library(sdmTMBextra)
	library(future)
	library(DHARMa)
	library(tidymv)
	library(ggsidekick)
	library(see)
	library(rnaturalearth)
	
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
	dat_all <- fread(file = here("./data/sp_dat_all.csv")) 
	
	dat_all$age_f <- as.factor(dat_all$age_f)
	dat_all$year_f <- as.factor(dat_all$year_f)
	
	# convert X/Y cols to KM
	#dat_all <- dat_all %>%  
	#	mutate(X_m = X,
	#				 Y_m = Y,
	#				 X = X/1000,
	#				 Y = Y/1000)
	
	# land polygons for plotting 
	world <- ne_countries(scale = "medium",
                      returnclass = "sf") 

	US_map <- world %>%
  	filter(name %in% c("United States"))

	# Crop the polygon for plotting and efficiency:
	# st_bbox(map_data) # find the rough coordinates
	AK_coast <- suppressWarnings(suppressMessages(
  	st_crop(US_map,
    c(xmin = -179, ymin = 54, xmax = -155, ymax = 65))))

	utm_zone <- 4326
	AK_coast_proj <- sf::st_transform(AK_coast, crs = utm_zone)
	
