#Web scraping of Google Street View data, predict TOH presence/absence, save coordinates 

#clear environment
rm(list=ls())

#set working directory
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH")

#load packages
library(rgdal)
library(rgeos)
library(raster)
library(ggmap)
library(ggplot2)
library(imager)
library(reticulate)
library(googleway)

## your valid API key here
key <- "AIzaSyAPrRGk21jtlX5iTUgrLkfqWerUDInSQYo"


#read in coordinate data
coords<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/All_PA_coords.csv")

#sample every 20th point
coords.samp<- coords[seq(1, nrow(coords), 150), ]

#look at points on a map
xmean<-mean(coords.samp$Longitude)
ymean<-mean(coords.samp$Latitude)

map.center<-c(xmean, ymean)

myMap<-get_map(location=map.center, maptype = "terrain-background",zoom=7)
ggmap(myMap)

myMapPoints<-ggmap(myMap)+
  geom_point(data=coords.samp, aes(x=Longitude, y=Latitude), color="red",size=0.4, alpha=0.4)
myMapPoints


#get subset of locations from road.sub.coords.df
myLocations<-coords.samp

#make directory to save streetview images
saveDir<-"./Downloaded_images"
dir.create(saveDir)

#set up directions and pitches for google images
dirList<-c(seq(0,315, by=45))
#pitchList<-c(-90, -60, -30, 0, 30, 60, 90)
pitchList<-c(10)
fovList<-c(90)

resultsOut<-list()
for(i in 1:length(row.names(myLocations))){
  
  new.loc<-c(myLocations$Latitude[i], myLocations$Longitude[i])
  longitude=new.loc[2]
  latitude=new.loc[1]
  
  #make new folder for saving point location street view images
  new.loc.name<-paste(latitude, longitude, sep="_")
  dir.create(paste(saveDir, new.loc.name, sep="/"))
  
    for(j in 1:length(dirList)){
      new.dir<-dirList[j]
      
      for(k in 1:length(pitchList)){
        new.pitch<-pitchList[k]
        
        #jpeg(paste(getwd(), "Data","StreetView_images",paste("img",i,"dir",j,"pitch",k,'streetView.jpeg',sep="_"), sep="/"))
        
        jpeg(paste(saveDir,new.loc.name,paste(latitude, longitude,"img",i,"dir",j,"pitch",k,'streetView.jpeg',sep="_"), sep="/"))
        google_streetview(location = new.loc,
                              size = c(640,640),
                              panorama_id = NULL,
                              output = "plot",
                              heading = new.dir,
                              fov = 90,
                              pitch = new.pitch,
                              response_check = FALSE,
                              key = key)
  dev.off()
      }
    }
  
  #prediction step
  use_python("/usr/local/bin/python3", required=TRUE)
  
  source_python("/Users/zach/Dropbox (ZachTeam)/Andrew_Brenner/Projects/TOH_yolo/YOLO_scripts/predict_in_R_runYOLO_multi.py")
  
  myNewDir<-paste(saveDir, new.loc.name, sep="/")
  imagesList<-dir(myNewDir)
  
  resultsSave<-list()
  for(z in 1:length(imagesList)){
    
    newImageName<-imagesList[z]
    #run YOLO
    yoloResults<-try(yolo_in_R(myDir=myNewDir,imageName=newImageName))
    
    ifelse(length(yoloResults)==0, 
           {
             results.df<-data.frame(State="Pennsylvania",County="Berks",ImageName=newImageName,Longitude=longitude, Latitude=latitude, Label= NA, Confidence=NA)
             results.df$TOH_detected<-"Not_Detected"
           },
           {
             results.df<-data.frame(State="Pennsylvania",County="Berks",ImageName=newImageName,Longitude=longitude, Latitude=latitude, Label= yoloResults[[1]][1]$label, Confidence=yoloResults[[1]][2]$confidence)
             results.df$TOH_detected<-ifelse(results.df$Label=="Tree_of_Heaven","Detected","Not_Detected")
           })
           
    resultsSave<-rbind(resultsSave, results.df)
  }
  resultsOut<-rbind(resultsOut, resultsSave)
}

write.csv(resultsOut, file=paste("/Users/zach/Dropbox (ZachTeam)/Andrew_Brenner/Projects/Predict_TOH/Data", paste("TOH_predictions_saved","_", Sys.Date(),".csv",sep=""),sep="/"),row.names=FALSE)
############################################################################################################################
#open images



img1<-load.image(paste(getwd(), "Data","StreetView_images",paste("img",1,"dir",9,"pitch",4,'streetView.jpeg',sep="_"), sep="/"))
plot(img1)







library(dismo)  # check also the nice 'rgbif' package! 
ToF <- gbif("Ailanthus", "altissima")

# get data frame with spatial coordinates (points)
locs <- subset(ToF, select = c("country", "lat", "lon"))
head(locs)  # a simple data frame with coordinates

# Discard data with errors in coordinates:
locs <- subset(locs, locs$lat < 90)

coordinates(locs) <- c("lon", "lat")  # set spatial coordinates
plot(locs)

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)

plot(locs, pch = 20, col = "steelblue")
library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

table(locs$country)  # see localities of Laurus nobilis by country

locs.gb <- subset(locs, locs$country == "United States")  # select only locs in US
plot(locs.gb, pch = 20, cex = 2, col = "steelblue")
title("Ailanthus altissima occurrences in US")
plot(countriesLow, add = T)

gbmap <- gmap(locs.gb, type = "satellite")
locs.gb.merc <- Mercator(locs.gb)  # Google Maps are in Mercator projection. 
# This function projects the points to that projection to enable mapping
plot(gbmap)
points(locs.gb.merc, pch = 20, col = "red")


require(RgoogleMaps)

locs.gb.coords <- as.data.frame(coordinates(locs.gb))  # retrieves coordinates 
# (1st column for longitude, 2nd column for latitude)
PlotOnStaticMap(lat = locs.gb.coords$lat, lon = locs.gb.coords$lon, zoom = 5, 
                cex = 1.4, pch = 19, col = "red", FUN = points, add = F)

map.lim <- qbbox(locs.gb.coords$lat, locs.gb.coords$lon, TYPE = "all")  # define region 
# of interest (bounding box)
mymap <- GetMap.bbox(map.lim$lonR, map.lim$latR, destfile = "gmap.png", maptype = "satellite")

# see the file in the wd
PlotOnStaticMap(mymap, lat = locs.gb.coords$lat, lon = locs.gb.coords$lon, zoom = NULL, 
                cex = 1.3, pch = 19, col = "red", FUN = points, add = F)