



#########################################################
# Add in missing GEAR TEMPs based on IWD interpolation  #
#########################################################



#######################################################################################################################
# We want to have the predictors of bottom depth and temperature that are the SAME as those used to fit the model     #
# 1. Bring in EBS raster of bottom depth.                                                                             #
# 2. Grab bottom depth, make a predictor variable BOTTOM_DEPTH for each lat long.                                     #
# 3. Predict BOTTOM_TEMP for prediction of GEAR_TEMP - make sure raster BOTTOM_TEMP is the same as model BOTTOM_TEMP  #           #
#######################################################################################################################

###########################################
# 1. Bring in EBS raster of bottom depth. #
###########################################


#Load up raster of EBS area, depths obtained from: https://www.gebco.net/data_and_products/gridded_bathymetry_data/#area, which is broken into 450m2 cells 
library(stars)
library(sp)
library(reshape2)
library(ggplot2)
library(sdmTMB)
library(sf)




########################################################################################
# 2. Grab bottom depth, make a predictor variable BOTTOM_DEPTH_pred for each lat long. #
########################################################################################

# Load diet data, get into same coordinates as ebs raster UTM
load("C:/Users/Jonathan.Reum/Work/SizeDiets/BenthosMaps/Data/EBS_diets_wide_benthos.Rdata") # loads aggdat - also containes the observed gear temps. 


ebs <- stars::read_stars("C:/Users/Jonathan.Reum/Work/SizeDiets/EBSgrids/AllEBSstrata/EBS_NBS_depth_14pt37km2.tif") #Already in UTM coordinates, 14.37 km2 (did that in ArcGIS) (for fast consumption calcs and plotting)
plot(ebs,  breaks="equal")


aggdat<- add_utm_columns(
          aggdat,
          ll_names = c("RLONG", "RLAT"),
          ll_crs = 4326,
          utm_names = c("X", "Y"),
          units = "m"
        )

aggdat$BOTTOM_DEPTH_pred<-0
bottom_pred<-st_extract(ebs, as.matrix(aggdat[, c("X", "Y")]))
aggdat$BOTTOM_DEPTH_pred<- bottom_pred$EBS_NBS_depth_14pt37km2.tif


# Repeat for all hauls to evaluate how good the values are 

hauls<-read.csv("C:/Users/Jonathan.Reum/Work/SizeDiets/SurveyDesign/queries/foodlab_haul_raw.csv") # Change this so we're using the updated pull with the 2023 data as well 
hauls<-subset(hauls, REGION=="BS" & CRUISE_TYPE=="Race_Groundfish")
hauls<-subset(hauls,STRATUM>6 &STRATUM<160 )
hauls<- add_utm_columns(
          hauls,
          ll_names = c("RLONG", "RLAT"),
          ll_crs = 4326,
          utm_names = c("X", "Y"),
          units = "m"
        )

hauls$BOTTOM_DEPTH_pred<-0
bottom_pred<-st_extract(ebs, as.matrix(hauls[, c("X", "Y")]))
hauls$BOTTOM_DEPTH_pred<- bottom_pred$EBS_NBS_depth_14pt37km2.tif

ggplot(hauls) +geom_point( aes(y=Y, x=X))

plot(-1*GEAR_DEPTH~BOTTOM_DEPTH_pred, data=subset(hauls, BOTTOM_DEPTH_pred<0)) 
abline(a=0, b=1)

plot(GEAR_DEPTH~BOTTOM_DEPTH, data=hauls) 
abline(a=0, b=1)


ggplot(subset(hauls, YEAR==2019)) + geom_point(aes(y=Y, x=X)) 
ggplot(subset(hauls, BOTTOM_DEPTH_pred<0 & YEAR==2019)) + geom_point(aes(y=Y, x=X)) + geom_point(data=subset(hauls, BOTTOM_DEPTH_pred==0 & YEAR==2019), aes(y=Y, x=X), size=.1, color="red") 






###################################################################################################################
# 3. Predict BOTTOM_TEMP from GEAR_TEMP - make sure raster BOTTOM_TEMP is the same as model BOTTOM_TEMP           #
###################################################################################################################


#Get rid of excess cells 
  ebs_df<- st_coordinates(  ebs) # set to km UTMs
  # get depth column 
  ebs_df$BOTTOM_DEPTH_pred<- melt(ebs)[,3]

  ebs_df<- ebs_df[!ebs_df$BOTTOM_DEPTH_pred==0, ] 
  colnames(ebs_df)[1:2]<-c("X", "Y")
  ebs_df<- ebs_df %>% subset(BOTTOM_DEPTH_pred <0)  

  #Confirm map looks alright
  ggplot(ebs_df)+
  geom_raster(aes(x=X, y=Y, fill=BOTTOM_DEPTH_pred)) + geom_point(data=hauls, aes(x=X, y=Y))

  ggplot(ebs_df)+
      geom_point(data=aggdat, aes(x=X, y=Y)) + facet_wrap(~YEAR)


  # Build data frame for predictions of temp each year 
  # Add in Year (numeric), YEAR (factor), PRED_LEN = 50, and GEAR_TEMP
  
  # Add in Year (numeric), YEAR (factor), PRED_LEN = 50, and GEAR_TEMP
  years<-sort(unique(hauls$YEAR))
  outL<-list()
  
  for(i in 1:length(years)){
    outL[[i]]<- cbind(ebs_df, YEAR=factor(years[i]))
  }

  

############################################################################################################################################
# Now use IDW on gear temp to interpolate bottom temperature across survey area within years and for stations with no values in the aggdat #
############################################################################################################################################

# need to get predictions at stations where gear temp is missing - you can predict to hauls, then hust transfer to aggdat based on hauljoin 

  for(i in 1:length(years)){
  
    dattemp<-subset(hauls, YEAR==years[i])
    datprey<-subset(aggdat, YEAR==years[i])
    
    datpred<- outL[[i]]
    
    coordinates(dattemp) <- ~X+Y
    coordinates(datpred)<- ~X+Y

  


    dattemp<- sf::st_as_sf(dattemp)
    #idw_fit <- gstat::gstat(formula = GEAR_TEMP ~ 1, locations = x, nmax=5, data=subset(dattemp, !is.na(GEAR_TEMP))) # I looked at 3 and 5, and 4 seems just right. 
   
        idw_fit <- gstat::gstat(formula = GEAR_TEMP ~ 1,  nmax=5, data=subset(dattemp, !is.na(GEAR_TEMP))) # I looked at 3 and 5, and 4 seems just right. 
   

    # With idw, predicted station temps are the SAME as observed at each haul 

    fit<-as.data.frame(predict(idw_fit, newdata=datpred))
    
    outL[[i]]<- cbind(outL[[i]], BOTTOM_TEMP=fit$var1.pred)

    # Add in missing temps if (present in diet data set) 

    if(sum(datprey$GEAR_TEMP==-999)>0){

      coordinates(datprey)<- ~X+Y

      fitprey<-as.data.frame(predict(idw_fit, newdata=datprey))

      aggdat$GEAR_TEMP[aggdat$YEAR==years[i]]<- fitprey$var1.pred


    }


    print(years[i])
  }


  outdf<- do.call("rbind", outL)

  outdf$YEAR.f<-as.factor(outdf$YEAR)
  outdf$GEAR_DEPTH<- outdf$BOTTOM_DEPTH_pred

  save(outdf, file="C:/Users/Jonathan.Reum/Work/SizeDiets/BenthosMaps/Grids/raster_for_prediction.Rdata")

  save(aggdat, file = "C:/Users/Jonathan.Reum/Work/SizeDiets/BenthosMaps/Data/EBS_diets_wide_benthos_temp_depth_ready.Rdata")

 #
 load(file = here("./data/from Jon Reum/raster_for_prediction.Rdata"))

ggplot(outdf)+
  geom_raster(aes(x=X, y=Y, fill=BOTTOM_TEMP)) + 
	facet_wrap(~YEAR, ncol=8) + 
	scale_fill_gradient2(low=muted('blue'), mid='white', high=muted('orangered'))
