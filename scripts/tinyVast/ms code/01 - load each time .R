
# load each time

	# libraries

	library(here)
	library(tidyverse)
	library(lubridate)
	library(MuMIn)
	library(visreg)
	library(data.table)
	library(gamm4)
	library(mgcViz)
	library(patchwork)
	library(grid)
	library(gratia)
	library(sf)
	library(AICcmodavg)
	library(sdmTMB)
	library(future)
	library(DHARMa)
	library(ggsidekick)
	library(see)
	library(rnaturalearth)
	library(knitr)
	library(kableExtra)
	library(ggh4x)
	library(tinyVAST)
	
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

