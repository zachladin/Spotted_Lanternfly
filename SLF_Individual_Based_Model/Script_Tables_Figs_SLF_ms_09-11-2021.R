#Script to produce figs for SLF manuscript

#clear environment
rm(list=ls())

#load packages and source function
library(tigris)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(rgeos)
library(sp)
library(mapproj)
library(rgdal)
library(raster)
library(sf)
library(gdalUtils)
library(raster)
library(combinat)
library(viridis)
library(gganimate)
library(ggnewscale)
library(pals)
library(gifski)
library(gapminder)
library(Rcpp)
library(data.table)
library(scales)
library(doParallel)
library(parallel)
library(foreach)
library(dclone)
library(agricolae)


#source functions
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/SLF_source_functions.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/summaryFunction.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/processSimulationResults.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/xyToLatLong.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/getCummulativeSums.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/formatRasters.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/preComputeBestCells.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/loadMaxENTraster.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/getCounties.R")

#set my Google API key
myAPIkey = "YOUR KEY"
register_google(myAPIkey)

#function to threshold raster by cell values
rasterThreshold <- function(r, min, max) {
  rr <- r[]
  rr[rr < min | rr > max] <- NA
  r[] <- rr
  r
}


#################################################################################################################
#get county polygons for study area

#crop raster to mid-Atlantic region
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

#subset counties to only inclued Northeast/Mid-Atlantic region
counties.sub <- subset(counties, c(grepl("maine", counties$ID) |
                                     grepl("vermont", counties$ID) |
                                     grepl("massachusetts", counties$ID) |
                                     grepl("new hampshire", counties$ID) |
                                     grepl("rhode island", counties$ID) |
                                     grepl("connecticut", counties$ID) |
                                     grepl("new york", counties$ID) |
                                     grepl("pennsylvania", counties$ID) |
                                     grepl("new jersey", counties$ID) |
                                     grepl("maryland", counties$ID) |
                                     grepl("delaware", counties$ID) |
                                     grepl("virginia", counties$ID) |
                                     grepl("west virginia", counties$ID) |
                                     grepl("ohio", counties$ID) |
                                     grepl("north carolina", counties$ID) |
                                     grepl("kentucky", counties$ID) |
                                     grepl("tennessee", counties$ID) |
                                     grepl("indiana",counties$ID)))

#remove 3 counties not in states
removeCountyList<-c("oklahoma,delaware","indiana,delaware","indiana,ohio","iowa,delaware")
counties.sub.2<-subset(counties.sub, ! ID %in% removeCountyList)

#make spatial polygons
countysp <- as_Spatial(counties.sub.2, IDs=counties.sub.2$GEOID)

#make data.frame
counties.df <- fortify(countysp)

#reproject
counties.proj<-st_transform(counties, "+proj=longlat +datum=WGS84")


#get all counties for plotting
counties.all.proj<-as_Spatial(counties, IDs=counties.sub.2$GEOID)

###################################################################################

#get center of map
#get corners of map
meanLong<-mean(counties.df$long)
meanLat<-mean(counties.df$lat)

#get basemap
myMap<-ggmap::get_map(location=c(meanLong, meanLat), maptype="terrain-background", zoom=5)
ggmap(myMap)

#get bounding box info from map
mapBounds<-attr(myMap, "bb")

minLong<-mapBounds$ll.lon*0.98
maxLong<-mapBounds$ur.lon*1
minLat<-mapBounds$ll.lat*1.19
maxLat<-mapBounds$ur.lat*1

myMap2<-NULL
myMap2<-ggmap::get_map(location=c(minLong,minLat,maxLong,maxLat), maptype="terrain-background",color="bw",force=TRUE)
ggmap(myMap2)

ggmap(myMap2, darken=c(0.6, "gray20"))+
  geom_polygon(data=counties.df, 
               mapping=aes(y=lat , x=long, group=group), 
               color="gray80", alpha=0.2,size=0.5)

counties.extent<-extent(counties.proj)

minLong_US<-as.numeric(counties.extent@xmin)*1.02
maxLong_US<-as.numeric(counties.extent@xmax)*0.98
minLat_US<-as.numeric(counties.extent@ymin)*0.98
maxLat_US<-as.numeric(counties.extent@ymax)*1.02


#get ggmap for whole contiguous US
myMapUS<-NULL
myMapUS<-ggmap::get_map(location=c(minLong_US,minLat_US,maxLong_US,maxLat_US), maptype="terrain-background",color="bw",force=TRUE,style=c(feature="administrative.state",element="borders",visibility="off")
)

ggmap(myMapUS)



###################################################################################
#create MaxENT plot with counties on top



###################################################################################

#crop polygons to mid-Atlantic region
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
plot(states)

#subset counties to only inclued Northeast/Mid-Atlantic region
states.sub <- subset(states, c(grepl("maine", states$ID) |
                                     grepl("vermont", states$ID) |
                                     grepl("massachusetts", states$ID) |
                                     grepl("new hampshire", states$ID) |
                                     grepl("rhode island", states$ID) |
                                     grepl("connecticut", states$ID) |
                                     grepl("new york", states$ID) |
                                     grepl("pennsylvania", states$ID) |
                                     grepl("new jersey", states$ID) |
                                     grepl("maryland", states$ID) |
                                     grepl("delaware", states$ID) |
                                     grepl("virginia", states$ID) |
                                     grepl("west virginia", states$ID) |
                                     grepl("ohio", states$ID) |
                                     grepl("north carolina", states$ID) |
                                     grepl("kentucky", states$ID) |
                                     grepl("tennessee", states$ID) |
                                     grepl("indiana",states$ID)))
plot(states.sub)


# # modified version of http://gis.stackexchange.com/a/109635/29544
# # see https://gis.stackexchange.com/questions/178843/tearing-of-polygons-using-ggmap-and-readogr
# gClip <- function(shp, bb){
#   if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
#   else b_poly <- as(extent(bb), "SpatialPolygons")
#   proj4string(b_poly) <- proj4string(shp)
#   gIntersection(shp, b_poly, byid = T)
# }
# 
# # clip the states 
# states.clip <- gClip(shp=statessp, bb=matrix(c(minLong, minLat, maxLong,maxLat), ncol=2))

#make spatial polygons
states_shp <- as_Spatial(states, IDs=states.sub$ID)

#make data.frame
states_shp.df <- fortify(states_shp)


ggplot(data=states_shp.df, aes(x=long, y=lat, group=group))+
  geom_polygon(color="blue",fill="lightblue")


################################################################################
###################################################################################
#function to export no.SLF polygons for a given year
createPoly.no.SLF<-function(dataIn,countyDataIn){
  
  new.data<-dataIn
  
  county.data<-countyDataIn
  
  #create column with "state,county" lowercase
  new.data$ID<-paste(tolower(new.data$State),tolower(new.data$County),sep=",")
  
  #add factor level to counties.sub.2
  county.data$SLF_present<-ifelse(county.data$ID %in% new.data$ID,"Present","Absent")
  
  #make a factor
  county.data$SLF_present<-as.factor(county.data$SLF_present)
  
  #get counties without SLF
  counties.no.SLF<-subset(county.data, SLF_present=="Absent")
  
  #now reproject
  counties.proj.no.SLF<-st_transform(counties.no.SLF, "+proj=longlat +datum=WGS84")
  #plot(counties.proj.no.SLF[2])
  
  return(counties.proj.no.SLF)
}


#function to export no.SLF polygons for a given year
createPoly.SLF<-function(dataIn,countyDataIn){
  
  new.data<-dataIn
  
  county.data<-countyDataIn
  
  #create column with "state,county" lowercase
  new.data$ID<-paste(tolower(new.data$State),tolower(new.data$County),sep=",")
  
  #add factor level to counties.sub.2
  county.data$SLF_present<-ifelse(county.data$ID %in% new.data$ID,"Present","Absent")
  
  #make a factor
  county.data$SLF_present<-as.factor(county.data$SLF_present)
  
  #get counties without SLF
  counties.SLF<-subset(county.data, SLF_present=="Present")
  
  #now reproject
  counties.proj.SLF<-st_transform(counties.SLF, "+proj=longlat +datum=WGS84")
  #plot(counties.proj.no.SLF[2])
  
  return(counties.proj.SLF)
}

#readin county occurence data
SLF_county_occurence<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/SLF_Shiny_App/Data/SLF_County_Level_occurrence_All_years.csv")

#List of States in 2019
SLF_county_occrence_2019<-subset(SLF_county_occurence, Year=="2019")

unique(SLF_county_occrence_2019$State)

#get count of total counties in each year 
county.count<-aggregate(County~Year, FUN=length,data=SLF_county_occurence)

county.state.count<-aggregate(County~Year+State, FUN=length,data=SLF_county_occurence)


#get count of total states in each year 
state.count<-aggregate(State~Year, FUN=function(x){length(unique(x))},data=SLF_county_occurence)

##################################################################################
#test functions

# #get counties from 2015
# counties.SLF_2015<-subset(SLF_county_occurence, Year=="2015")
# 
# test.poly<-createPoly.SLF(dataIn=counties.SLF_2015, countyDataIn=counties.sub.2)
# plot(test.poly)
##################################################################################

#get MAXENT layer

#MAXENT surface details

#read in MaxEnt raster surface
maxEnt_surface<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_grayscale_4km.tiff", values=TRUE)

#summary stats for maxEnt_surface
range(unique(maxEnt_surface), na.rm=TRUE)
max(maxEnt_surface,na.rm=TRUE)



#get corners of map
minLong<-min(counties.df$long)
maxLong<-max(counties.df$long)
minLat<-min(counties.df$lat)
maxLat<-max(counties.df$lat)

#now map these to coords 0,0
min_X<-0
max_X<-dim(maxEnt_surface)[2]
min_Y<-0
max_Y<-dim(maxEnt_surface)[1]

#get list of all possible (universe)
new.matrix<-matrix(0,nrow=nrow(maxEnt_surface), ncol=ncol(maxEnt_surface))
new.raster<-raster(nrows=nrow(new.matrix), ncols=ncol(new.matrix),xmn=0,ymn=0,xmx=ncol(new.matrix),ymx=nrow(new.matrix))
values(new.raster)<-0
range(values(maxEnt_surface))

#create a maxEnt_surface with only 0s and NAs
maxEnt_surface_zeros<-maxEnt_surface
maxEnt_surface_zeros[maxEnt_surface_zeros!=58137]<-0
maxEnt_surface_zeros[is.na(maxEnt_surface_zeros)]<-0
maxEnt_surface_zeros[maxEnt_surface_zeros==58137]<-NA

#create data.frame with list of all cell XY cords
all.cells.df<-data.frame(floor(rasterToPoints(na.omit(maxEnt_surface_zeros))))[,-3]

#add Lat/Long
#now apply these functions to slf.simu data
all.cells.df$Longitude<-coord2long(inX=all.cells.df$x,minX = min_X, maxX = max_X, minlong = minLong, maxlong = maxLong)
all.cells.df$Latitude<-coord2lat(inY=all.cells.df$y, minY = min_Y, maxY = max_Y, minlat = minLat, maxlat = maxLat)

#convert colnames
names(all.cells.df)[names(all.cells.df)=="x"]<-"X"
names(all.cells.df)[names(all.cells.df)=="y"]<-"Y"

slf_maxent_predict_4km<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Results/SLF_predicted_distribution_1km_All_8-17-2020_small.tif")
plot(slf_maxent_predict_4km, col=parula(100))

res(slf_maxent_predict_4km)

#make into a data.frame
slf_maxent_predict_df<-as.data.frame(slf_maxent_predict_4km, xy=TRUE)
colnames(slf_maxent_predict_df)[3]<-"Probability"
unique(slf_maxent_predict_df)


states_map_data <- map_data("state")
usamap <- ggplot(states_map_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")+
  coord_map()
usamap
crs(states_map_data)




#################################################################



################
#load raster data

folderList<-list.dirs(path="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output_rasters",full.names=TRUE)[-1]

#make one giant raster stack
master.raster.list<-list()
for(i in 1:length(folderList)){
  print(i)
  
  new.fileList.full<-list.files(folderList[i], full.names=TRUE)
  #new.fileList.short<-list.files(folderList[1], full.names=FALSE)
  
  new.list.keep<-grep('Binary', new.fileList.full, value=TRUE)
  
  #remove any Year_0 rasters
  new.list.keep.2<-new.list.keep[!grepl("Year_0", new.list.keep)]
  
  rasters.list<-list()
  for(j in 1:length(new.list.keep.2)){
    
    new.raster<-raster(new.list.keep.2[j])
    
    new.raster.df<-read.table(text=as.character(new.list.keep.2[j]),sep="/")
    
    #remove '.tif'
    new.raster.fileName<-read.table(text=gsub(".tif","",as.character(new.raster.df[ncol(new.raster.df)])), sep="_")
    
    rasters.list<-append(rasters.list, new.raster)
    
  }
  
  master.raster.list<-append(master.raster.list, rasters.list)
  
}

#make raster stack
master.raster.stack<-stack(master.raster.list)

#create date data.frame
year.df<-data.frame(Year=seq(2013,2025,1))

yearList<-year.df$Year

#create probability range data.frame
prob.df<-data.frame(Probability=seq(0,1,by=0.01))

#create lists for dropdown menus
growthRateList<-c("0.25","0.5","1","1.5")

#humanPopsList
humanPopsList<-c("0","3","5","7","10")

#create movement type list
moveTypeList<-c("Random","Non-Random")

######################################################################################
#dynamically define counties.proj

#counties.proj.no.SLF
counties.proj.no.SLF<-function(yearIn){
  if(as.character(yearIn) %in% c("2021","2022","2023","2024","2025")){
    SLF_county_occurence_2021<-subset(SLF_county_occurence, Year=="2021")
    out<-createPoly.no.SLF(dataIn=SLF_county_occurence_2021,countyDataIn = counties.sub.2)
    
  } else{
    if(as.character(yearIn)=="2013"){
      SLF_county_occurence_2013<-subset(SLF_county_occurence, Year=="2014")
      out<-createPoly.no.SLF(dataIn=SLF_county_occurence_2013,countyDataIn = counties.sub.2)
      
    }else{
      SLF_county_occurence.sub<-subset(SLF_county_occurence, Year==as.character(yearIn))
      out<-createPoly.no.SLF(dataIn=SLF_county_occurence.sub,countyDataIn = counties.sub.2)
      }
    }
  return(out)
}

#counties.proj.SLF
  counties.proj.SLF<-function(yearIn){
    if(as.character(yearIn) %in% c("2021","2022","2023","2024","2025")){
      SLF_county_occurence_2021<-subset(SLF_county_occurence, Year=="2021")
      out<-createPoly.SLF(dataIn=SLF_county_occurence_2021,countyDataIn = counties.sub.2)

    } else{
      if(as.character(yearIn)=="2013"){
        SLF_county_occurence_2013<-subset(SLF_county_occurence, Year=="2014")
        out<-createPoly.SLF(dataIn=SLF_county_occurence_2013,countyDataIn = counties.sub.2)

      }else{
        SLF_county_occurence.sub<-subset(SLF_county_occurence, Year==as.character(yearIn))
        out<-createPoly.SLF(dataIn=SLF_county_occurence.sub,countyDataIn = counties.sub.2)
      }
    }
    return(out)
  }
    


#############################################################################
#loop through rasters, and get 95% prob cells for each simulation

  
new.thresh = 0.5
  
all.save<-list()
for(i in 1:length(humanPopsList)){
  
  num.human.pops<-humanPopsList[i]
  
  growthRate.df<-list()
  for(j in 1:length(growthRateList)){
    
    new.growth.rate<-growthRateList[j]
    
    moveType.df<-list()
    for(y in 1:length(moveTypeList)){
      
      move.type<-moveTypeList[y]
      
      new.movement.value<-switch(as.character(move.type),
                                 "Random"=0.99,
                                 "Non-Random"=0.01
                                 )
      
      results.df<-list()
      for(z in 1:length(yearList)){
        
        #get max years
        new.year.fun<-function(yearIn){
          out<-switch(as.character(yearIn),
                 "2013" = "1",
                 "2014" = "2",
                 "2015" = "3",
                 "2016" = "4",
                 "2017" = "5",
                 "2018" = "6",
                 "2019" = "7",
                 "2020" = "8",
                 "2021" = "9",
                 "2022" = "10",
                 "2023" = "11",
                 "2024" = "12",
                 "2025" = "13")
          return(out)
        }
        
        new.year.in<-yearList[z]
        
        new.year<-new.year.fun(yearIn=new.year.in)
        
        #get SLF present/absent counties as function of year
        counties.no.SLF.poly<-counties.proj.no.SLF(yearIn=yearList[z])
        counties.SLF.poly<-counties.proj.SLF(yearIn=yearList[z])
        
  
        new.raster.name<-paste("HumanPops",num.human.pops, "Years_13","GrowthRate",new.growth.rate, "q2mean",new.movement.value,"q2sd_0.05_Binary","Year",new.year,sep="_")

        #subset raster from raster stack
        new.raster<-function(newRasterNameIn){
          
          my.raster<-master.raster.stack[[newRasterNameIn]]
          #plot(my.raster)
          
          #threshold by probability
          thresh.raster<-rasterThreshold(r=my.raster, min=min(new.thresh), max=max(1))
          
          raster.cells.df<-data.frame(rasterToPoints(na.omit(thresh.raster)))
          colnames(raster.cells.df)<-c("x","y","z")
          
          #add Lat/Long
          #now apply these functions to slf.simu data
          
          #get raster min_X, max_X, min_Y, and max_Y
          raster.cells.df$Longitude<-coord2long(inX=raster.cells.df$x,minX = min_X, maxX = max_X, minlong = minLong, maxlong = maxLong)
          raster.cells.df$Latitude<-coord2lat(inY=raster.cells.df$y, minY = min_Y, maxY = max_Y, minlat = minLat, maxlat = maxLat)
          
          new.raster.2<-rasterFromXYZ(xyz=data.frame(raster.cells.df$Longitude, raster.cells.df$Latitude, raster.cells.df$z),
                                      digits=3, crs="+proj=longlat +datum=WGS84")
          return(new.raster.2)
        }
        
        
  #use function to get new raster output      
  new.raster.2<-new.raster(newRasterNameIn = new.raster.name)
  plot(new.raster.2,col=parula(100))
  plot(counties.SLF.poly, col="transparent",border="gray80",add=TRUE)
  
  #create new name to save raster
  raster::writeRaster(x=new.raster.2, filename=paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_predicted_rasters",paste(paste(new.raster.name,"Thresh",new.thresh,sep="_"),".tif",sep=""),sep="/"),overwrite=TRUE)
  

  ###########################################################################
  #get model performance metrics
  #get number of all US Counties with predicted SLF
  USpolygonList<-raster::extract(new.raster.2, counties.proj)
  count.all.predicted.polygons<-length(USpolygonList)
  
  #get number of correctly predicted counties
  polygonList<-raster::extract(new.raster.2, counties.SLF.poly)
  
  #count of all SLF polygons (observed)
  count.all.slf.polygons<-length(polygonList)
  #count of counties with SLF correctly predicted (accurate)
  count.slf.polygons<-length(polygonList[!unlist(lapply(polygonList, is.null))])
  #count of counties with SLF missing (false negatives)
  count.null.polygonList<-length(polygonList[unlist(lapply(polygonList, is.null))])
  #count of counties where SLF do not occur, but were predicted (false positives)
  
  #proportion of cells within SLF counties
  pct.accurate<-round(count.slf.polygons/count.all.slf.polygons*100,3)
  
  #false negatives
  false.negatives<-100-pct.accurate
  
  #get count of polygons (with No SLF present, where predictions were made by model)
  false.county.predictions<-raster::extract(new.raster.2, counties.no.SLF.poly)
  
  count.false.counties<-length(false.county.predictions[!unlist(lapply(false.county.predictions, is.null))])
  
  #false positives (intersections of new.raster.2 with mask.zero.rater.inverse)
  false.positives<-round(count.false.counties/count.all.predicted.polygons*100,3)
  
  #compute F1 score for model performance
  
  #precision = true positives/true+false positives
  mod.precision<-count.slf.polygons/(count.slf.polygons+count.false.counties)
  
  #recall = true positives/ all true positives
  mod.recall<-count.slf.polygons/count.all.slf.polygons
  
  #F1 = 2*((precision*recall)/(precision+recall)); F1=1 is good performance, 0 is bad
  mod.F1<-2*((mod.precision*mod.recall)/(mod.precision+mod.recall))
  
  #save output results in data.frame
  new.results.df<-data.frame("Raster_Name"=new.raster.name,
                         "HumanPops"=num.human.pops,
                         "GrowthRate"=new.growth.rate,
                         "MovementType"=move.type,
                         "ProbThresh"="0.95",
                         "Year"=new.year.in,
                         "Percent_Accurate"=pct.accurate,
                         "False_Negatives"=false.negatives,
                         "False_Positives"=false.positives,
                         "Precision"=mod.precision,
                         "Recall"=mod.recall,
                         "F1"=mod.F1
                         )

      results.df<-rbind(results.df, new.results.df)
      }
      moveType.df<-rbind(moveType.df, results.df)
    }
    growthRate.df<-rbind(growthRate.df,moveType.df)
  }
  all.save<-rbind(all.save, growthRate.df)
}

#save results
write.csv(all.save, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Performance_Results_0.5thresh_09-20-2021.csv",row.names=FALSE)

##############################################################################################
#visualize model performance resutls
#model.results<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Performance_Results_0.5thresh_09-20-2021.csv")

model.results<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Performance_Results_09-11-2021.csv")

model.perf<-subset(model.results, ! Year %in% c("2013","2022","2023","2024","2025"))

#make Year a factor
model.perf$Year<-as.factor(model.perf$Year)

# #subset to include only 2020 and 2021
# model.perf_sub<-subset(model.perf, c(Year==2020 | Year==2021))
# 
# #make Year a factor
# model.perf_sub$Year<-as.factor(model.perf_sub$Year)

# #melt
# model.perf_sub.melt<-data.table::melt(model.perf_sub, measure.vars=c("F1"))
# names(model.perf_sub.melt)[names(model.perf_sub.melt)=="variable"]<-"Performance_Metric"
# names(model.perf_sub.melt)[names(model.perf_sub.melt)=="value"]<-"Value"

#make GrowthRate a factor
model.perf$GrowthRate<-as.factor(model.perf$GrowthRate)

#make HumanPops a factor
model.perf$HumanPops<-as.factor(model.perf$HumanPops)

######################################################################
#get beta coef plot

#fit model using Recall (Accuracy) as response variable
mod.recall.1<-glm(Recall~HumanPops*Year+GrowthRate*Year+MovementType*Year, data=model.perf)

#Recall
mod.recall.summary<-summary(mod.recall.1)

#Plot beta coefficients
new.coefs<-as.data.frame(mod.recall.summary$coefficients)
new.coefs$Variable<-row.names(new.coefs)
row.names(new.coefs)<-NULL

#coompute 95% CIs
new.coefs$`Lower_95%_CI` <- new.coefs$Estimate - 1.96*new.coefs$`Std. Error`
new.coefs$`Upper_95%_CI` <- new.coefs$Estimate + 1.96*new.coefs$`Std. Error`

#first remove intercepts
new.coefs.sub<-subset(new.coefs, Variable != "(Intercept)")

#add ZeroCross column
new.coefs.sub$ZeroCross<-ifelse(new.coefs.sub$`Lower_95%_CI`>0 | new.coefs.sub$`Upper_95%_CI` < 0,"No_Zero_Cross","Zero_Cross")

#make a factor
new.coefs.sub$ZeroCross<-as.factor(new.coefs.sub$ZeroCross)

#sort Variables 
new.coefs.recall.order<-new.coefs.sub[order(new.coefs.sub$Estimate,decreasing = FALSE),]

#set factor levels for Variable
new.coefs.recall.order$Variable<-factor(new.coefs.recall.order$Variable, levels=unique(new.coefs.recall.order$Variable))
#Add Metric column
new.coefs.recall.order$Metric<-"Recall"





#F1
#fit model using F1 as response variable
mod_f1.1<-glm(F1~HumanPops*Year+GrowthRate*Year+MovementType*Year, data=model.perf)

#Plot beta coefficients
new.coefs<-as.data.frame(mod.f1.summary$coefficients)
new.coefs$Variable<-row.names(new.coefs)
row.names(new.coefs)<-NULL

#coompute 95% CIs
new.coefs$`Lower_95%_CI` <- new.coefs$Estimate - 1.96*new.coefs$`Std. Error`
new.coefs$`Upper_95%_CI` <- new.coefs$Estimate + 1.96*new.coefs$`Std. Error`

#first remove intercepts
new.coefs.sub<-subset(new.coefs, Variable != "(Intercept)")

#add ZeroCross column
new.coefs.sub$ZeroCross<-ifelse(new.coefs.sub$`Lower_95%_CI`>0 | new.coefs.sub$`Upper_95%_CI` < 0,"No_Zero_Cross","Zero_Cross")

#make a factor
new.coefs.sub$ZeroCross<-as.factor(new.coefs.sub$ZeroCross)

#sort Variables 
new.coefs.f1.order<-new.coefs.sub[order(new.coefs.sub$Estimate,decreasing = FALSE),]

#set factor levels for Variable
new.coefs.f1.order$Variable<-factor(new.coefs.f1.order$Variable, levels=unique(new.coefs.f1.order$Variable))
#add F1 column
new.coefs.f1.order$Metric<-"F1"

#combine recall and f1 mod estimates
coefs.all<-rbind(new.coefs.f1.order, new.coefs.recall.order)

#make Metric a factor
coefs.all$Metric<-as.factor(coefs.all$Metric)

#create new column with 3 factor levels (i.e., F1, Recall, ZeroCross)
coefs.all$Metric_2<-ifelse(coefs.all$Metric=="F1" & coefs.all$ZeroCross=="No_Zero_Cross","F1",
                           ifelse(coefs.all$Metric=="Recall" & coefs.all$ZeroCross=="No_Zero_Cross","Recall",
                                  "Zero_Cross"))

#make a factor
coefs.all$Metric_2<-as.factor(coefs.all$Metric_2)

#set colors
#myBetaColors<-c("royalblue1","seagreen4","gray60")
parula(20)
myBetaColors<-c("#036CE0","#E6B94F","gray60")

#plot beta coefficient
betaCoefPlot<-ggplot(data=coefs.all, aes(x=Variable, y=Estimate))+
  geom_hline(yintercept=0,linetype=2,alpha=0.7)+
  geom_errorbar(aes(ymin=`Lower_95%_CI`, ymax=`Upper_95%_CI`, color = Metric_2),width=0)+
  geom_point(aes(color=Metric_2),alpha=0.8)+
  scale_color_manual(values=myBetaColors)+
  #scale_color_gradient2(low="red",mid="gray40",high="royalblue2",midpoint = 0)+
  theme(legend.position="none",
        panel.background = element_rect(fill="white",color="black"),
        panel.border = element_rect(fill="transparent",color="black"))+
  labs(y="Beta Coefficient Estimate", x="Variable")+
  coord_flip()
betaCoefPlot

betaCoefPlot_facet<-betaCoefPlot+facet_wrap(.~Metric,ncol=2)

#save figure
ggsave(betaCoefPlot_facet, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_Manuscript_Figures/Fig_4_BetaCoefs_plot.png",width=9, height=10, dpi=600)




##################################################################################
#fit model using Recall (Accuracy) as response variable
mod.recall.1<-glm(Recall~GrowthRate*Year+MovementType*Year+HumanPops*Year, data=model.perf)

#model summary
mod.recall.summary<-summary(mod.recall.1)
summary(aov(mod.recall.1))

#                    Df Sum Sq Mean Sq  F value   Pr(>F)    
# GrowthRate          3  0.069   0.023   20.151 1.01e-11 ***
# Year                7  4.059   0.580  510.142  < 2e-16 ***
# MovementType        1  0.002   0.002    1.552 0.214074    
# HumanPops           4 18.249   4.562 4013.480  < 2e-16 ***
# GrowthRate:Year    21  0.059   0.003    2.469 0.000528 ***
# Year:MovementType   7  0.018   0.003    2.243 0.031503 *  
# Year:HumanPops     28  3.384   0.121  106.331  < 2e-16 ***
# Residuals         248  0.282   0.001                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Tukey's post hoc tests for differnences among 

#Year
tukey.test.recall.year <- HSD.test(aov(mod.recall.1), trt = "Year")
tukey.test.recall.year

# $statistics
# MSerror  Df      Mean      CV        MSD
# 0.00113673 248 0.8150575 4.13657 0.02304674
# 
# $parameters
# test name.t ntr StudentizedRange alpha
# Tukey   Year   8         4.323256  0.05
# 
# $means
# Recall       std  r        Min       Max       Q25       Q50       Q75
# 2014 1.0000000 0.0000000 40 1.00000000 1.0000000 1.0000000 1.0000000 1.0000000
# 2015 0.8666667 0.2700638 40 0.33333333 1.0000000 1.0000000 1.0000000 1.0000000
# 2016 0.9208333 0.1644619 40 0.50000000 1.0000000 1.0000000 1.0000000 1.0000000
# 2017 0.8750000 0.2560458 40 0.27272727 1.0000000 1.0000000 1.0000000 1.0000000
# 2018 0.7621795 0.3204661 40 0.10256410 1.0000000 0.8461538 0.9102564 0.9487179
# 2019 0.7490196 0.3254835 40 0.07843137 1.0000000 0.8235294 0.9019608 0.9215686
# 2020 0.7089506 0.3206574 40 0.06172840 0.9629630 0.7654321 0.8580247 0.9012346
# 2021 0.6378099 0.2978318 40 0.04132231 0.8842975 0.7004132 0.7768595 0.8099174
# 
# $comparison
# NULL
# 
# $groups
# Recall groups
# 2014 1.0000000      a
# 2016 0.9208333      b
# 2017 0.8750000      c
# 2015 0.8666667      c
# 2018 0.7621795      d
# 2019 0.7490196      d
# 2020 0.7089506      e
# 2021 0.6378099      f

#use summary funciton to get SE
year.recall.summary<-summaryFunction(DataIn =model.perf, response="Recall", factor="Year" )
# Year  n      mean        var        SD         SE
# 1 2014 40 1.0000000 0.00000000 0.0000000 0.00000000
# 2 2015 40 0.8666667 0.07293447 0.2700638 0.04270084
# 3 2016 40 0.9208333 0.02704772 0.1644619 0.02600371
# 4 2017 40 0.8750000 0.06555944 0.2560458 0.04048439
# 5 2018 40 0.7621795 0.10269855 0.3204661 0.05067015
# 6 2019 40 0.7490196 0.10593953 0.3254835 0.05146347
# 7 2020 40 0.7089506 0.10282116 0.3206574 0.05070038
# 8 2021 40 0.6378099 0.08870379 0.2978318 0.04709134

#GrowthRate
tukey.test.recall.growthRate<- HSD.test(aov(mod.recall.1), trt = "GrowthRate")
tukey.test.recall.growthRate

# $statistics
# MSerror  Df      Mean      CV       MSD
# 0.00113673 248 0.8150575 4.13657 0.0137885
# 
# $parameters
# test     name.t ntr StudentizedRange alpha
# Tukey GrowthRate   4         3.657912  0.05
# 
# $means
# Recall       std  r        Min Max       Q25       Q50 Q75
# 0.25 0.8098047 0.2905426 80 0.04132231   1 0.8024691 0.9245014   1
# 0.5  0.7944222 0.2860662 80 0.04132231   1 0.7623457 0.8717949   1
# 1    0.8220654 0.2883486 80 0.05785124   1 0.8161157 0.9449472   1
# 1.5  0.8339375 0.2835336 80 0.05785124   1 0.8568258 0.9607843   1
# 
# $comparison
# NULL
# 
# $groups
# Recall groups
# 1.5  0.8339375      a
# 1    0.8220654     ab
# 0.25 0.8098047      b
# 0.5  0.7944222      c

#use summary funciton to get SE
growthRate.recall.summary<-summaryFunction(DataIn =model.perf, response="Recall", factor="GrowthRate" )
#   GrowthRate  n      mean        var        SD         SE
# 1       0.25 80 0.8098047 0.08441499 0.2905426 0.03248365
# 2        0.5 80 0.7944222 0.08183385 0.2860662 0.03198317
# 3          1 80 0.8220654 0.08314490 0.2883486 0.03223835
# 4        1.5 80 0.8339375 0.08039129 0.2835336 0.03170002


#HumanPop groups
tukey.test.recall.humanPops <- HSD.test(aov(mod.recall.1), trt = "HumanPops")
tukey.test.recall.humanPops

# $statistics
# MSerror  Df      Mean      CV        MSD
# 0.00113673 248 0.8150575 4.13657 0.01637808
# 
# $parameters
# test    name.t ntr StudentizedRange alpha
# Tukey HumanPops   5         3.886192  0.05
# 
# $means
# Recall        std  r        Min Max        Q25       Q50       Q75
# 0  0.3375015 0.31098299 64 0.04132231   1 0.09858388 0.2132867 0.4659091
# 10 0.9365843 0.08225020 64 0.70247934   1 0.88117186 1.0000000 1.0000000
# 3  0.9274452 0.08945092 64 0.69421488   1 0.86953243 0.9803922 1.0000000
# 5  0.9366586 0.07984843 64 0.71074380   1 0.88580247 0.9871795 1.0000000
# 7  0.9370976 0.08433149 64 0.68595041   1 0.88725490 1.0000000 1.0000000
# 
# $comparison
# NULL
# 
# $groups
# Recall groups
# 7  0.9370976      a
# 5  0.9366586      a
# 10 0.9365843      a
# 3  0.9274452      a
# 0  0.3375015      b

#use summary funciton to get SE
humanPops.recall.summary<-summaryFunction(DataIn =model.perf, response="Recall", factor="HumanPops" )

#   HumanPops  n      mean         var         SD          SE
# 1         0 64 0.3375015 0.096710420 0.31098299 0.038872874
# 2         3 64 0.9274452 0.008001468 0.08945092 0.011181365
# 3         5 64 0.9366586 0.006375772 0.07984843 0.009981054
# 4         7 64 0.9370976 0.007111800 0.08433149 0.010541436
# 5        10 64 0.9365843 0.006765095 0.08225020 0.010281275

#MovementType
movement.type.recall.means<-summaryFunction(DataIn=model.perf, factor="MovementType",response="Recall")

#  MovementType   n      mean        var        SD         SE
# 1   Non-Random 160 0.8127097 0.08687060 0.2947382 0.02330110
# 2       Random 160 0.8174052 0.07740594 0.2782192 0.02199516


######################################################################
#fit models F1

#fit model using F1 as response variable
mod_f1.1<-glm(F1~HumanPops*Year+GrowthRate*Year+MovementType*Year, data=model.perf)

#model summary
mod.f1.summary<-summary(mod_f1.1)
summary(aov(mod_f1.1))

#                      Df Sum Sq Mean Sq  F value   Pr(>F)    
# HumanPops           4  0.038  0.0095   11.046 2.93e-08 ***
# Year                7  8.114  1.1591 1346.633  < 2e-16 ***
# GrowthRate          3  0.006  0.0019    2.207 0.087761 .  
# MovementType        1  0.012  0.0124   14.409 0.000185 ***
# HumanPops:Year     28  8.496  0.3034  352.514  < 2e-16 ***
# Year:GrowthRate    21  0.009  0.0004    0.481 0.975018    
# Year:MovementType   7  0.008  0.0011    1.306 0.247939    
# Residuals         248  0.213  0.0009                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Tukey's post hoc tests for differnences among 

#Year
tukey.test.f1.year <- HSD.test(aov(mod_f1.1), trt = "Year")
tukey.test.f1.year

# $statistics
# MSerror  Df      Mean       CV        MSD
# 0.0008607219 248 0.3119469 9.404826 0.02005452
# 
# $parameters
# test name.t ntr StudentizedRange alpha
# Tukey   Year   8         4.323256  0.05
# 
# $means
# F1        std  r        Min       Max        Q25        Q50        Q75
# 2014 0.1203153 0.19233191 40 0.01562500 0.5000000 0.02266343 0.02547874 0.03727273
# 2015 0.1026542 0.09315526 40 0.03896104 0.2857143 0.05106658 0.05882918 0.07362805
# 2016 0.2146797 0.23738658 40 0.07272727 0.7272727 0.09022556 0.10345596 0.11684149
# 2017 0.2285180 0.14746307 40 0.12222222 0.5882353 0.14839924 0.16927083 0.18487395
# 2018 0.3665622 0.07761469 40 0.18181818 0.4761905 0.33936141 0.38774257 0.42159091
# 2019 0.4100285 0.11211713 40 0.14285714 0.5164835 0.40569831 0.45331968 0.48061497
# 2020 0.5009090 0.17813123 40 0.11627907 0.6418605 0.53944034 0.58233463 0.60558671
# 2021 0.5519082 0.22601403 40 0.07936508 0.6953125 0.63075720 0.66312741 0.67286449
# 
# $comparison
# NULL
# 
# $groups
# F1 groups
# 2021 0.5519082      a
# 2020 0.5009090      b
# 2019 0.4100285      c
# 2018 0.3665622      d
# 2017 0.2285180      e
# 2016 0.2146797      e
# 2014 0.1203153      f
# 2015 0.1026542      f

year.f1.summary<-summaryFunction(DataIn =model.perf, response="F1", factor="Year" )
# Year  n      mean         var         SD         SE
# 1 2014 40 0.1203153 0.036991565 0.19233191 0.03041035
# 2 2015 40 0.1026542 0.008677903 0.09315526 0.01472914
# 3 2016 40 0.2146797 0.056352390 0.23738658 0.03753411
# 4 2017 40 0.2285180 0.021745358 0.14746307 0.02331596
# 5 2018 40 0.3665622 0.006024040 0.07761469 0.01227196
# 6 2019 40 0.4100285 0.012570251 0.11211713 0.01772727
# 7 2020 40 0.5009090 0.031730735 0.17813123 0.02816502
# 8 2021 40 0.5519082 0.051082342 0.22601403 0.03573596


#HumanPop groups
tukey.test.f1.humanPops <- HSD.test(aov(mod_f1.1), trt = "HumanPops")
tukey.test.f1.humanPops

# $statistics
# MSerror  Df      Mean       CV        MSD
# 0.0008607219 248 0.3119469 9.404826 0.01425167
# 
# $parameters
# test    name.t ntr StudentizedRange alpha
# Tukey HumanPops   5         3.886192  0.05
# 
# $means
# F1       std  r        Min       Max        Q25       Q50       Q75
# 0  0.3327903 0.1975106 64 0.07936508 0.7272727 0.17869111 0.2732919 0.5000000
# 10 0.3083894 0.2365650 64 0.01801802 0.6870748 0.07432229 0.2781952 0.5020028
# 3  0.3088261 0.2412685 64 0.02197802 0.6953125 0.07443182 0.2631233 0.5149814
# 5  0.3091689 0.2427545 64 0.01562500 0.6903915 0.07384824 0.2712783 0.5276788
# 7  0.3005598 0.2355051 64 0.01834862 0.6853147 0.08081081 0.2569939 0.4900966
# 
# $comparison
# NULL
# 
# $groups
# F1 groups
# 0  0.3327903      a
# 5  0.3091689      b
# 3  0.3088261      b
# 10 0.3083894      b
# 7  0.3005598      b

#use summary funciton to get SE
humanPops.f1.summary<-summaryFunction(DataIn =model.perf, response="F1", factor="HumanPops" )
# HumanPops  n      mean        var        SD         SE
# 1         0 64 0.3327903 0.03901043 0.1975106 0.02468882
# 2         3 64 0.3088261 0.05821051 0.2412685 0.03015857
# 3         5 64 0.3091689 0.05892976 0.2427545 0.03034432
# 4         7 64 0.3005598 0.05546267 0.2355051 0.02943814
# 5        10 64 0.3083894 0.05596302 0.2365650 0.02957063


#MovementType
movement.type.f1.means<-summaryFunction(DataIn=model.perf, factor="MovementType",response="F1")
# MovementType   n      mean        var        SD         SE
# 1   Non-Random 160 0.3057215 0.05144348 0.2268116 0.01793103
# 2       Random 160 0.3181723 0.05473854 0.2339627 0.01849637






#######################################################################################

#get means accuracies 
#aggregate to get mean Percent accuracy
mean.recall.accuracy<-aggregate(Recall~HumanPops+GrowthRate, FUN=mean, data=model.perf)

#what model has highest F1
best.models<-subset(model.perf, F1>0.6)



#######################################################################################
#function to add letters to facet panels
tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, size=6,family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, size=size,family = family, inherit.aes = FALSE) 
}

#######################################################################################
#box plot by habitat type

#get summarized means to order by
mod_summary.human.pops<-aggregate(F1~HumanPops+Year, FUN=mean, data=model.perf)

#define colors
myColors<-c("darkred","deepskyblue2","seagreen3","yellow3","orange3")

boxPlot<-ggplot(data=model.perf, aes(x=Year, y=F1))+
  #geom_hline(yintercept = 0, linetype=2, size=1, color=alpha("gray80",0.8))+
  geom_path(data=mod_summary.human.pops, alpha=0.7,aes(x=Year, y=F1, color=HumanPops, group=HumanPops),inherit.aes = FALSE,
            position=position_dodge(width=0.75))+
  geom_boxplot(alpha=0.7,aes(color=HumanPops))+
  geom_boxplot(alpha=0.7,aes(fill=HumanPops),color="gray30",outlier.color = NA)+
  scale_color_manual(values=myColors)+
  scale_fill_manual(values=myColors)+
  theme(panel.background = element_rect(fill="white",color="black"),
        panel.border = element_rect(fill="transparent", color="black"),
        legend.position = "right",
        legend.text=element_text(size=13),
        legend.key =element_blank())+
  #geom_text(data=mod_summary.human.pops, label=c(rev(tukey.test.humanPops$groups$groups)),aes(x=HumanPops, y=1.2*(mean+SD*1.96)),inherit.aes = FALSE, color="black",size=7)+
  ylim(c(0,0.8))+
  labs(y="Model Performance (F1)", x="Year")
boxPlot

##########################################################################################
#make Year a factor
model.perf$Year<-factor(as.character(model.perf$Year), levels=unique(model.perf$Year))

#Point plots
pointPlot<-ggplot(data=model.perf, aes(x=Year, y=F1, group=HumanPops,color=HumanPops))+
  #geom_hline(yintercept = 0, linetype=2, size=1, color=alpha("gray80",0.8))+
  geom_path()+
  geom_point(alpha=0.7, aes(color=HumanPops))+
  scale_color_manual(values=myColors)+
  scale_fill_manual(values=myColors)+
  theme(panel.background = element_rect(fill="white",color="black"),
        panel.border = element_rect(fill="transparent", color="black"),
        legend.position = "bottom",
        axis.title=element_text(size=14),
        legend.text=element_text(size=13),
        legend.key =element_blank())+
  #geom_text(data=mod_summary.human.pops, label=c(rev(tukey.test.humanPops$groups$groups)),aes(x=HumanPops, y=1.2*(mean+SD*1.96)),inherit.aes = FALSE, color="black",size=7)+
  ylim(c(0,0.8))+
  labs(y="Model Performance (F1)", x="Year", 
       color="Max. number of\nhuman-mediated\nmovements")
pointPlot

# A data frame with labels for each facet
f_labels <- data.frame(drv = c("4", "f", "r"), label = c("4wd", "Front", "Rear"))

mpg_plot +
  geom_text(x = 6, y = 40, aes(label = label), data = f_labels)

#facet 
pointPlot_facet<-pointPlot+facet_grid(MovementType~GrowthRate)
  #theme(plot.margin=margin(0, 2, 0, 0, "cm"))+
  #annotate("text", angle=0, x = 2014 , y = 0.8, label = c("A","B","C","D","E","F","G","H"))
pointPlot_facet

pointPlot_facet_out<-tag_facet(pointPlot_facet, 
         x = -Inf, y = 0.75, 
         vjust = 0.7, hjust=-0.3,
         open = "", close = "",
         tag_pool = LETTERS)+
  theme(plot.title=element_text(size=13,hjust=0.5),
        strip.text=element_text(size=12))+
  ggtitle("Intrinsic rate of growth (r)")
pointPlot_facet_out

#add second y-axis to right side of figure
pointPlot_save<-pointPlot_facet_out+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Movement Type", labels = NULL, breaks = NULL))
pointPlot_save

#save plot
ggsave(pointPlot_save, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_Manuscript_Figures/Fig_4_Model_Performance_9-12-2021.png",width=12, height=6, dpi=600)

#########################################################################
#Combine Recall and F1 into one plot

#define colors
myColors<-c("darkred","deepskyblue2")

# colfunc <- colorRampPalette(c("darkred", "white"))
# myReds<-colfunc(10)
# 
# colfunc <- colorRampPalette(c("deepskyblue2", "white"))
# myBlues<-colfunc(10)
# 
# 
# f1Colors<-c("darkred")
# recallColors<-c("deepskyblue2")

#melt to stack F1, Recall, and Precision
model.perf.melt<-data.table::melt(model.perf, id.vars=c("Raster_Name","HumanPops","GrowthRate","MovementType","Year"), measure.vars=c("Percent_Accurate", "False_Negatives",  "False_Positives",  "Precision","Recall", "F1"))
#fix names
names(model.perf.melt)[names(model.perf.melt)=="variable"]<-"Metric"

#subset 
model.perf.melt.sub<-subset(model.perf.melt, c(Metric=="F1" | Metric=="Recall"))

#make Metric a factor with two levels
model.perf.melt.sub$Metric<-factor(model.perf.melt.sub$Metric,levels=c("Recall","F1"))

#make MovementType a factor
model.perf.melt.sub$MovementType<-as.factor(model.perf.melt.sub$MovementType)

#subset remove Random movement
model.perf.melt.sub.2<-subset(model.perf.melt.sub, MovementType=="Non-Random")

#make Year a

levels(model.perf.melt.sub$Year)


#Point plots
pointPlotComb<-ggplot(data=model.perf.melt.sub, aes(x=Year, y=value, color=HumanPops,group=HumanPops))+
  #geom_hline(yintercept = 0, linetype=2, size=1, color=alpha("gray80",0.8))+
  geom_path(data=subset(model.perf.melt.sub, MovementType=="Random"),linetype=2,alpha=0.6)+
  geom_path(data=subset(model.perf.melt.sub, MovementType=="Non-Random"),linetype=1,alpha=0.6)+
  geom_point(alpha=0.6,size=1)+
  scale_color_manual(values=myColors)+
  scale_fill_manual(values=myColors)+
  theme(panel.background = element_rect(fill="white",color="black"),
        panel.border = element_rect(fill="transparent", color="black"),
        legend.position = "bottom",
        axis.title=element_text(size=14),
        legend.text=element_text(size=13),
        legend.key =element_blank())+
  #geom_text(data=mod_summary.human.pops, label=c(rev(tukey.test.humanPops$groups$groups)),aes(x=HumanPops, y=1.2*(mean+SD*1.96)),inherit.aes = FALSE, color="black",size=7)+
  ylim(c(0,1))+
  labs(y="Model Performance", x="Year", 
       color="Maximum number of human-mediated\nmovements per year")
pointPlotComb

#facet 
pointPlot_facet<-pointPlotComb+facet_grid(Metric~GrowthRate)
#theme(plot.margin=margin(0, 2, 0, 0, "cm"))+
#annotate("text", angle=0, x = 2014 , y = 0.8, label = c("A","B","C","D","E","F","G","H"))
pointPlot_facet

pointPlot_facet_out<-tag_facet(pointPlot_facet, 
                               x = -Inf, y = 0.9, 
                               vjust = 0.7, hjust=-0.3,
                               open = "", close = "",
                               tag_pool = LETTERS)+
  theme(plot.title=element_text(size=13,hjust=0.5),
        strip.text=element_text(size=12))+
  ggtitle("Intrinsic rate of growth (r)")
pointPlot_facet_out

#add second y-axis to right side of figure
# pointPlot_save<-pointPlot_facet_out+
#   scale_y_continuous(sec.axis = sec_axis(~ . , name = "Movement Type", labels = NULL, breaks = NULL))
# pointPlot_save

#save plot
ggsave(pointPlot_facet_out, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_Manuscript_Figures/Fig_4_Model_Performance_Recall_and_F1.png",width=12, height=6, dpi=600)

######################################################################
#pull in one simulation as an example of how human-mediated movement is modeled

###NEED TO COMPILE BINARY RASTERS (0.95 Prob), and then plot using facet

#get list of files
fileList<-list.files("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_predicted_rasters/",full.names=TRUE)

#remove any xml files
new.list.keep<-fileList[!grepl(".aux.xml", fileList)]

new.list.keep.2<-new.list.keep[grepl("HumanPops_10",new.list.keep)]



####################################################
#set max extent
max.raster<-raster(new.list.keep.2[96])
plot(max.raster)
#get bbox
max.raster.bbox<-bbox(max.raster)

#get extent
xmin <- min(max.raster.bbox[1,1])
xmax <- max(max.raster.bbox[1,2])  
ymin <- min(max.raster.bbox[2,1])  
ymax <- max(max.raster.bbox[2,2])  
newextent=c(xmin, xmax, ymin, ymax)

###################################################
#compile all rasters (0.95 Prob threshold), and set to max extent (so they can be plotted in ggplot())
save.file<-list()
for(i in 1:length(new.list.keep)){
  
  new.file<-new.list.keep[i]
  print(new.file)
  
  new.file.name_split_1<-read.table(text=as.character(new.file), sep="/")
  new.file.name_split_2<-new.file.name_split_1[,length(new.file.name_split_1)]
  new.file.name_split_3<-read.table(text=as.character(new.file.name_split_2), sep="_")
  
  #define simulation parameters
  new.file.df<-data.frame("HumanPops"=new.file.name_split_3$V2,
                          "NumYears"=new.file.name_split_3$V4,
                          "GrowthRate"=new.file.name_split_3$V6,
                          "MovementType"=new.file.name_split_3$V8,
                          "DataType"=new.file.name_split_3$V11,
                          "Year"=new.file.name_split_3$V13,
                          "ProbThreshold"=gsub(".tif","",new.file.name_split_3$V15),
                          "File_Name"=new.file.name_split_2)
  
  
  #get a file and plot it to test
  new.ras<-raster(new.file)
  #plot(new.ras, col=parula(100))
  
  #extend the extent
  new.ras.1<-extend(new.ras, newextent)
  
  #convert to data.frame
  new.ras.df<-raster::as.data.frame(new.ras.1,xy=TRUE)
  
  #gather file info
  ras.info<-names(new.ras.df)[3]
  
  #fix column names
  names(new.ras.df)<-c("Longitude","Latitude","Probability")
  
  #merge with new.file.df
  new.ras.merge<-merge(new.ras.df,new.file.df, all=TRUE)
  
  #add Year (e.g., 2013)
  names(new.ras.merge)[names(new.ras.merge)=="Year"]<-"YearNum"
  new.ras.merge$Year<-new.ras.merge$YearNum+2012
  
  #compile saved data
  save.file<-rbind(save.file, new.ras.merge)
  
}

#write.csv.
write.csv(save.file, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/All_rasters_df_95_thresh.csv",row.names = FALSE)


###############################################################################################################
#read in All_rasters_df_95_thresh.csv
all.rasters.df<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/All_rasters_df_95_thresh.csv")

all.rasters.df.max<-subset(all.rasters.df, c(Year=="2025" & HumanPops=="10" & GrowthRate=="1.5",MovementType=="0.99"))

#subset to only include years < 2022 and > 2015
all.rasters.df.sub<-subset(all.rasters.df, c(Year>2015 & Year <2022))


#make variables factors
all.rasters.df.sub$HumanPops<-as.factor(all.rasters.df.sub$HumanPops)
all.rasters.df.sub$GrowthRate<-as.factor(all.rasters.df.sub$GrowthRate)
all.rasters.df.sub$MovementType<-as.factor(all.rasters.df.sub$Movement)
all.rasters.df.sub$Year<-as.factor(all.rasters.df.sub$Year)

#subset growth rate
all.rasters.df.sub.2<-subset(all.rasters.df.sub, GrowthRate=="1.5")

#subset movement type
all.rasters.df.sub.3<-subset(all.rasters.df.sub.2, MovementType=="0.99")

#get hex color from SLF wing to use for fill color

#test with one year
#all.rasters.df.sub.4<-subset(all.rasters.df.sub.3, c(Year=="2021" & HumanPops=="0"))

###########################################################
#get polygons where SLF were detected by year
new.yearList<-c("2014","2015","2016","2017","2018","2019","2020","2021")

counties.SLF.save<-list()
for(i in 1:length(new.yearList)){
  
  new.counties.df<-counties.proj.SLF(yearIn=new.yearList[i])

  new.countysp <- as_Spatial(new.counties.df, IDs=new.counties.df$ID)
  #make data.frame
  new.counties.df <- fortify(new.countysp)
  new.counties.df$Year<-new.yearList[i]
  
  counties.SLF.save<-rbind(counties.SLF.save, new.counties.df)
}

#subset to only inlcude years 2016-2021
counties.SLF.save.sub<-subset(counties.SLF.save, c(Year != "2013" & Year !="2014" & Year !="2015"))

###########################################################
counties.all<-map_data("county")

#pull out lowest values of maxent
range(slf_maxent_predict_df$Probability,na.rm = TRUE)
#1.070351e-07 8.062719e-01
#0.0000001070351e-07

#get summary stats
maxEnt_mean<-mean(slf_maxent_predict_df$Probability,na.rm=TRUE)
maxEnt_N<-length(slf_maxent_predict_df$Probability)
maxEnt_sd<-sd(slf_maxent_predict_df$Probability,na.rm=TRUE)
maxEnt_se<-maxEnt_sd/sqrt(maxEnt_N)


#subset (threshold to make map)
maxent.sub<-subset(slf_maxent_predict_df, Probability>0.0009)

#plot in ggplot
maxEnt.plot<-ggmap(myMapUS)+
  geom_tile(data=maxent.sub, aes(x=x, y=y, color=Probability, fill=Probability),alpha=0.7)+
  geom_polygon(data=states_map_data,aes(long, lat, group=group),size=0.5,fill = "transparent", color = alpha("gray80",0.2),inherit.aes = FALSE)+
  geom_polygon(data=counties.all,aes(long, lat, group=group),size=0.5,fill = "transparent", color = alpha("gray80",0.1),inherit.aes = FALSE)+
  scale_fill_gradientn(colors=parula(100), na.value="transparent", name="Probability")+
  scale_color_gradientn(colors=parula(100),na.value="transparent", guide="none")+
  labs(x="Longitude",y="Latitude")+
  #coord_quickmap()+
  theme(panel.background = element_rect(fill="white", color="black"),
        panel.border = element_rect(fill="transparent",color="black"),
        legend.position=c(0.9,0.2),
        legend.background=element_blank())
#maxEnt.plot


ggsave(maxEnt.plot, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_Manuscript_Figures/Fig_2_Maxent_predicted_slf_distribution.tiff",width=11, height=7,dpi=600)


###################################################################
#zoom in to Max ENT surface ond averlay SLF counties


counties.SLF_2021<-subset(counties.SLF.save,Year=="2021")

#add SLF_present
counties.SLF.save$SLF<-"Present"
counties.SLF.save$SLF<-as.factor(counties.SLF.save$SLF)

#plot in ggplot
maxEnt.plot.2<-ggplot(data=slf_maxent_predict_df, aes(x=x, y=y))+
  geom_tile(aes(color=Probability, fill=Probability),alpha=0.6)+
  scale_fill_gradientn(colors=parula(100), na.value="transparent", name="Probability")+
  scale_color_gradientn(colors=parula(100),na.value="transparent", guide="none")+
  new_scale("fill") +
  geom_polygon(data=counties.SLF.save, aes(long, lat, group=group,fill=SLF),size=0.5,inherit.aes = FALSE)+
  scale_fill_manual(values=c(alpha("red",0.8),"transparent"),name="SLF")+
  geom_polygon(data=states,aes(long, lat, group=group),size=0.5,fill = "transparent", color = alpha("gray80",0.2),inherit.aes = FALSE)+
geom_polygon(data=counties.all,aes(long, lat, group=group),size=0.5,fill = "transparent", color = alpha("gray80",0.1),inherit.aes = FALSE)+
  labs(x="Longitude",y="Latitude")+
  coord_quickmap()+
  theme(panel.background = element_rect(fill="white", color="black"),
        panel.border = element_rect(fill="transparent",color="black"))+
  lims(x=c(minLong, maxLong),y=c(minLat, maxLat))
#maxEnt.plot.2

maxEnt.plot_facet<-maxEnt.plot.2+facet_wrap(.~Year, ncol=3)+
  theme(legend.position=c(0.85,0.15),legend.box="horizontal")
#maxEnt.plot_facet

ggsave(maxEnt.plot_facet, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_Manuscript_Figures/Fig_3_Maxent_predicted_slf_distribution_with_SLF.tiff",width=8, height=7,dpi=600)

###########################################################

counties.SLF.save.2021<-subset(counties.SLF.save, Year=="2021")

#clip state and county polgons

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  proj4string(b_poly) <- proj4string(shp)
  shp_buf<-gBuffer(shp, byid=TRUE, width=0)
  gIntersection(shp_buf, b_poly, byid = T)
}

# clip the states
states.clip <- gClip(shp=states_shp, bb=matrix(c(minLong*0.96, minLat*1.08, 
                                                 maxLong*1.01,maxLat*0.97), ncol=2))
plot(states.clip)
#make data.frame
states_shp.df <- fortify(states.clip)

#clip counties
counties.clip<-gClip(shp=counties.all.proj, bb=matrix(c(minLong*0.96, minLat*1.08, 
                                                        maxLong*1.01,maxLat*0.97), ncol=2))
#make data.frame
counties.all_clip.df <- fortify(counties.clip)

state_minLong<-min(states_shp.df$long, na.rm=TRUE)
state_maxLong<-max(states_shp.df$long, na.rm=TRUE)
state_minLat<-min(states_shp.df$lat, na.rm=TRUE)
state_maxLat<-max(states_shp.df$lat, na.rm=TRUE)

unique(counties.SLF.save.sub$Year)

#now plot in ggplot
RasterMap_out=NULL
RasterMap_out<-ggplot()+
  #ggmap(myMapUS, darken=c(0.6, "gray10"))+
  geom_polygon(data=states_shp.df,aes(long, lat, group=group),size=0.1,fill = "gray90", color = "transparent",inherit.aes = FALSE)+
  geom_polygon(data=counties.SLF.save.sub,aes(x=long, y=lat,group=id),fill=alpha("red",0.7),inherit.aes = FALSE)+
  new_scale("fill")+
  geom_tile(data=all.rasters.df.sub.3, aes(y=Latitude, x=Longitude, fill=Probability),na.rm=TRUE,inherit.aes = FALSE)+
  scale_fill_gradientn(colors=c(alpha("royalblue3",0.7)), na.value="transparent")+
  geom_polygon(data=counties.all_clip.df,
               mapping=aes(y=lat , x=long, group=group),
               color=alpha("gray90",0.8),fill="transparent", size=0.075)+
  geom_polygon(data=states_shp.df,aes(long, lat, group=group),size=0.1,fill = "transparent", color = alpha("gray80",0.8),inherit.aes = FALSE)+
  
  theme(panel.background=element_rect(fill="transparent",color="black"),
        panel.border=element_rect(fill="transparent",color="black"),
        legend.position="none")+
  coord_quickmap()+
  lims(x=c(state_minLong*1, state_maxLong),y=c(state_minLat, state_maxLat))+
  labs(x="Longitude",y="Latitude")

#RasterMap_out


RasterMap_out_facet<- RasterMap_out+facet_grid(HumanPops~Year)+
  theme(plot.title=element_text(size=13,hjust=0.5),
        strip.text=element_text(size=12))+
  ggtitle("Year")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Max. number of human-mediated moves per year", labels = NULL, breaks = NULL))




ggsave(RasterMap_out_facet, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_predicted_spread_maps/Fig_5_panels_raster_map.png",width=9, height=8,dpi=600)

###############################################################################################








example.simu.data<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/HumanPops_3_Years_13_GrowthRate_1.5_q2_mean_0.99_q2_sd_0.05_Compiled/Combined_counts_HumanPops_3_Years_13_GrowthRate_1.5_q2mean_0.99_q2sd_0.05.csv")

#subset year
example.simu.data.sub<-subset(example.simu.data, c(Simulation_ID=="Simulation_1" & Year==12))

#get basemap
basemap<-get_map(location=c(mean(example.simu.data.sub$Longitude), mean(example.simu.data.sub$Latitude)),zoom=8, maptype="terrain")
ggmap(basemap)

#get map extent
mapExtent<-attr(basemap, "bb")
#38.80475	-75.62104	39.48637	-74.74214

#refine map extent to include all sampling locations around DE Bay
basemap<-get_map(location=c(-75.7,38.7,-74.5,39.7),maptype="toner-background")


#zoomed in pixels
simulation.raster.plot<-ggmap(basemap, darken = c(0.3, "black"))+
  geom_tile(data=example.simu.data.sub, aes(x=Longitude, y=Latitude,fill=Total_Visits, color=Total_Visits),alpha=0.8)+
  scale_fill_gradientn(colors=parula(100), na.value="transparent")+
  scale_color_gradientn(colors=parula(100),na.value="transparent")+
  theme(panel.background=element_rect(fill="white",color="black"),
        panel.border=element_rect(fill="transparent",color="black"),
        legend.position="none")+
  #lims(x=range(example.simu.data.sub$Longitude),y=range(example.simu.data.sub$Latitude))+
  labs(x="Longitude",y="Latitude")
#coord_equal()
simulation.raster.plot

ggsave(simulation.raster.plot, file="/Users/zach/Dropbox (ZachTeam)/Manuscripts/SLF_Spread/Figures/Model_Description/Example_human_spread_map.png",width=5, height=5, dpi=300)






######################################################################
#compare model performance using only 0.99 prob cells from simulations



######################################################################
#compare model out put for predictions for quarantined areas only


  



