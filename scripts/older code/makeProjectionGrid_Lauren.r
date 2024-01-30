## Make GOA Prediction grid
## 
## 
## Start with GOAIERP grid, then subset to stations within typical area
## 
## 
## 
library(sf)
library(ggplot2)
library(dplyr)

goagrid<-read.csv("Data/EcoFOCI_GOAIERP_spreadsheet.csv")
goagrid$LON<- -1* goagrid$LON

plot(goagrid$LON,goagrid$LAT)

goagrid <- goagrid %>%
  st_as_sf(crs=4326,coords=c("LON","LAT")) %>%
  st_transform(crs=32604) # why do this?


#read in sampled locations
goa<-read.csv("Data/Processed/GOA_spring_d131to160_37spp.csv")
#subset to unique hauls

goa1 <- goa %>%
  dplyr::select(LAT,LON, HAUL_ID) %>%
  distinct() %>%
  st_as_sf(crs=4326,coords=c("LON","LAT")) %>%
  st_transform(crs=32604)

buffdist <- 20000 #13890 # 7.5 nm in  meters ##### HOW CHOOSE THIS NUMBER?
buff<-st_buffer(goa1,buffdist,dissolve=TRUE) %>%
  st_union()

conv<-st_convex_hull(st_union(goa1))

gridinconv <- goagrid %>%
  st_intersection(conv) %>%
  st_transform(crs=4326) %>%
  mutate(LON=st_coordinates(.)[,1],
         LAT=st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  dplyr::select(c("GRID.STA","LON","LAT"))

plot(gridinconv)

write.csv(gridinconv,"Data/goagrid.csv",row.names=F)


## Try following code from: https://docs.google.com/document/d/12ZVzOmjbBa8VVTK0tmaNEcN1eP93g6iagDM2c8sSsfo/edit#
library(tidyverse);library(sf);library(sdmTMB)

#tmp <- goa %>% 
#  st_as_sf(coords = c("X", "Y"), crs = suppressWarnings(sdmTMB::get_crs(dt, ll_names = c("LON", "LAT")))) %>% 
#  summarise(geometry = st_combine(geometry)) %>%
#  st_convex_hull() 

## use both data observations (goa) and IERP grid (goagrid) to filter
# play with buffer distance and grid size 

buffdist<-15000
conv<-st_convex_hull(st_union(goa1))
gridinconv_buff <- goagrid %>%
  st_intersection(conv) %>%
  st_buffer(buffdist,dissolve=TRUE) %>%
  st_union()
  


gridutm <- gridinconv_buff %>%  #use either buff or conv (convex hull)
  st_make_grid(cellsize = rep(12000,2), square = TRUE, what = "centers") %>% 
  st_sf() %>% 
  st_filter(gridinconv_buff) %>% 
  st_coordinates() %>% 
  as_tibble()

write.csv(gridutm,"Data/goagrid_utm.csv",row.names=F)
