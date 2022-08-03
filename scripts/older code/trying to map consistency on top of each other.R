 
   consis_maps_core <-   
   	hist20_consistency09 + 
		current20_consistency09 + 
  	last20_consistency09 +
   	plot_layout(ncol = 3, widths = c(1.3, 1.3, 4))
   	
   consis_maps_pot <-
  	hist20_consistency05 + 
		current20_consistency05 + 
  	last20_consistency05 +
  	legend_plot 
  	
  	
 	consis_maps_test <- 
 		(consis_maps_core) - (consis_maps_pot) + 
 		plot_layout(ncol = 1)
 	
  ggsave("./output/plots/consis_maps_test.png",
		consis_maps_test,
		width = 15,
		height = 10)
 	
  
  ## try another way
  
  consis_maps_test <- 
  	(hist20_consistency09)/(plot_spacer())/(hist20_consistency05) |
  	(current20_consistency09)/(plot_spacer())/(current20_consistency05) |
  	(last20_consistency09)/(plot_spacer())/(last20_consistency05) +
		plot_layout(widths = c(1.3, -0.5, 1.3, 1.3, -0.5, 1.3, 4, 0, 4))
  
   ggsave("./output/plots/consis_maps_test.png",
		consis_maps_test,
		width = 15,
		height = 10)
 	
   ## and another
   
   consis_maps_test <- 
  	(hist20_consistency09/hist20_consistency05) |
  	(current20_consistency09/current20_consistency05) |
  	(last20_consistency09) - (last20_consistency05) +
		plot_layout(widths = c(50, 2, 10),
								design = " 
								112
								112")
  
   ggsave("./output/plots/consis_maps_test.png",
		consis_maps_test,
		width = 15,
		height = 10)
   
   ## and another
   
   consis_maps_test <- 
  	(hist20_consistency09/hist20_consistency05) +
  	(current20_consistency09/current20_consistency05) +
  	(last20_consistency09/last20_consistency05) +
		plot_layout(nrow = 2)
			
			widths = c(50, 2, 10),
								design = " 
								112
								112")
  
   ggsave("./output/plots/consis_maps_test.png",
		consis_maps_test,
		width = 15,
		height = 10)
   
   ### and another
   
   consis_maps_test1 <- 
  	hist20_consistency09 + current20_consistency09 + plot_spacer() + last20_consistency09 +
   	plot_layout(widths = c(0.75, 0.75, -1.1, 4))
  	 	
   consis_maps_test2 <- 
  	hist20_consistency05 + current20_consistency05 + plot_spacer() + last20_consistency05 +
   	plot_layout(widths = c(0.75, 0.75, -1.1, 4))
   
   consis_maps_test <- consis_maps_test1/consis_maps_test2
 		
   ggsave("./output/plots/consis_maps_test.png",
		consis_maps_test,
		width = 15,
		height = 10)
  
   ## and another
   
   consis_maps_test1 <- 
  	hist20_consistency09 + current20_consistency09 + last20_consistency09 +
   	plot_layout(widths = c(0.75, 0.75, 4))
  	 	
   consis_maps_test2 <- 
  	hist20_consistency05 + current20_consistency05 + last20_consistency05 +
   	plot_layout(widths = c(0.75, 0.75, 4))
   
   consis_maps_test <- consis_maps_test1/consis_maps_test2
 		
   ggsave("./output/plots/consis_maps_test.png",
		consis_maps_test,
		width = 15,
		height = 10)
  