#Web scraping of Google Street View data, predict TOH presence/absence, save coordinates 

#clear environment
rm(list=ls())

#set working directory
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly")

#load packages
library(rgdal)
library(rgeos)
library(raster)
library(ggmap)
library(ggplot2)
library(imager)
library(reticulate)
library(maptools)
library(googleway)

#register Google API key
#google API key
register_google("YOUR KEY")
## your valid API key here
#StreetViewKey <- "YOUR KEY"


#read in PA road data
roads.pa.shp<-readOGR(paste("./Analyses/Predict_TOH/Data/tl_2013_42_prisecroads","tl_2013_42_prisecroads.shp",sep="/"),verbose=FALSE)

#check crs
crs(roads.pa.shp)

# +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 

#plot roads.shp
plot(roads.pa.shp)
#show projection
projection(roads.pa.shp)

########################################################################################################################
#convert to equidistant points





########################################################################################################################
#Add other bordering states
#read in MD road data
# roads.md.shp<-readOGR(paste(getwd(), "Data","GIS", "tl_2013_24_prisecroads","tl_2013_24_prisecroads.shp",sep="/"),verbose=FALSE)

#plot roads.shp
# plot(roads.md.shp, col="royalblue2")
#show projection
#projection(roads.md.shp)

#read in DE road data
# roads.de.shp<-readOGR(paste(getwd(), "Data","GIS", "tl_2014_10_prisecroads","tl_2014_10_prisecroads.shp",sep="/"),verbose=FALSE)

#plot roads.shp
# plot(roads.de.shp)

#read in NJ road data
# roads.nj.shp<-readOGR(paste(getwd(), "Data","GIS", "tl_2015_34_prisecroads","tl_2015_34_prisecroads.shp",sep="/"),verbose=FALSE)

#plot roads.shp
# plot(roads.nj.shp,color="darkgreen",add=TRUE)

#merge roads
# roads.1<-rbind(roads.pa.shp, roads.de.shp)
# roads.2<-rbind(roads.md.shp, roads.nj.shp)

# roads.all<-rbind(roads.1, roads.2)
# plot(roads.all)
#roads.shp <- spTransform(roads.shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
###########################################################################################################################
#If only using PA
roads.all<-roads.pa.shp

#prepare points for ggplot
roads.all@data$id = rownames(roads.all@data)
roads.all.2 = data.frame(roads.all)
names(roads.all.2)[names(roads.all.2) == 'coords.x1'] <- 'Longitude'
names(roads.all.2)[names(roads.all.2) == 'coords.x2'] <- 'Latitude'

head(roads.all.2)

road.coords <- coordinates(roads.all)
road.coords.1 <- sapply(road.coords, function(x) do.call("rbind", x))
road.coords.2 <- do.call("rbind", road.coords.1)
lines(road.coords.2[chull(road.coords.2),], type="l", col="red")
# plot(road.coords.2, cex = 0.5)

#convert road.coords to df
road.coords.df<-as.data.frame(road.coords.2)
colnames(road.coords.df)<-c("Longitude","Latitude")
head(road.coords.df)

dim(road.coords.2)

#save coords 
write.csv(road.coords.df, file="./Analyses/Predict_TOH/Data/All_PA_coords.csv",row.names = FALSE)

####################################################################################################



extent(roads.all)

#get min/max coords
minLon<-min(road.coords.2[,1])
maxLon<-max(road.coords.2[,1])
minLat<-min(road.coords.2[,2])
maxLat<-max(road.coords.2[,2])

myExtent<-extent(c())

#convert road.coords to df
road.sub.coords.df<-as.data.frame(road.sub.coords.2)
colnames(road.sub.coords.df)<-c("Longitude","Latitude")
head(road.sub.coords.df)

#save coords 
write.csv(road.sub.coords.df, file="./Analyses/Predict_TOH/Data/Berks_county_coords.csv",row.names = FALSE)





# #get state boundaries
# us <- getData("GADM", country="USA", level=2)
# state.name<-c("Pennsylvania")
# #state.name <- c("Pennsylvania","New Jersey","Delaware","Maryland")
# states<-us[us@data$NAME_1 %in% state.name,]
# states<-subset(us, NAME_1 %in% state.name)
# plot(states)

#get projection
# projection(states)
# projection(roads.all)
# states.projection<-projection(roads.all)
# 
# projection(states)<-states.projection


#plot nicely
#get bounding box for pa
# states.ext<-extent(states)
# states.bbox<-c(left=extent(states)[1], right=extent(states)[2],bottom=extent(states)[3],top=extent(states)[4])
# 
# #make pa counties into dataframe
# states.df<-fortify(states)

states.map<-get_map(location="Pennsylvania",maptype="terrain",zoom=7)
ggmap(states.map)

# #function to get raster map from ggmap object
# ggmap_rast <- function(map){
#   map_bbox <- attr(map, 'bb') 
#   .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
#   my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
#   rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
#   red <- my_map
#   values(red) <- rgb_cols[['red']]
#   green <- my_map
#   values(green) <- rgb_cols[['green']]
#   blue <- my_map
#   values(blue) <- rgb_cols[['blue']]
#   stack(red,green,blue)
# }
# 
# #make into raster object
# states.rast <- ggmap_rast(map = states.map) # convert google map to raster object
# states.only <- mask(states.rast, states) # clip to bounds of census tracts
# 
# # prep raster as a data frame for printing with ggplot
# states.map.2 <- data.frame(rasterToPoints(states.only))
# 
# 
# 
# new.plot<-ggplot(states.map.2) + 
#   geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + 
#   scale_color_identity()+
#   geom_polygon(data=states.df,aes(x=long, y=lat,group=group),fill="transparent",color="black",size=0.25)+
#   geom_point(data=road.coords.df, aes(x=Longitude,y=Latitude),size=0.01, color="goldenrod4",alpha=0.4)+
#   #theme(panel.background=element_rect(fill='white',color="black"))+
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#   theme(axis.line=element_line(color="black"))+
#   theme(panel.background=element_rect(color="black"))+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
#         axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
#   labs(x="Longitude",y="Latitude")+
#   #coord_quickmap()
#   coord_fixed(ratio = 4/3)
#   #ggtitle(paste(speciesName, " - Count per sampling point",sep=""))
# new.plot



#subset counties with SLF
myCounties<-c("York","Dauphin", "Berks","Chester", "Delaware", "Montgomery","Philadelphia","Lancaster","Lebanon","Schuylkill","Lehigh","Bucks","Northampton","Carbon")

myCounties<-c("Berks")

sub.counties<-states[states@data$NAME_2 %in% myCounties,]
#view counties
plot(sub.counties)


#mask spatial points of roads to inside sub.counties
#get extent of counties
cnty.extent<-extent(sub.counties)

#crop to extent
cnty.crop<-crop(x=roads.all, y=cnty.extent, snap="in")
plot(cnty.crop)
plot(sub.counties, col=alpha("blue",0.2),add=TRUE)

#now mask
roads.mask<-gIntersection(cnty.crop, sub.counties)
plot(roads.mask)
plot(sub.counties, col=alpha("blue",0.2),add=TRUE)

#rasterize sub.counties
road.extent<-extent(cnty.crop)
r <- raster(ncol=7728, nrow=6056)
extent(r) <- road.extent
#sub.county.raster<-rasterize(sub.counties, )
rp <- rasterize(cnty.crop, r)
rp[rp > 0] <- 1


tiff(file = "/Users/zach/Desktop/berks_county_road_raster.tiff", width = 7728, height = 6056, units = "px", res=(43*43),compression="none")
plot(rp)
dev.off()


#convert roads.mask to SpatialLinesDataFrame
roads.mask = as(roads.mask, "SpatialLinesDataFrame")

#prepare points for ggplot
cnty.crop@data$id = rownames(cnty.crop@data)
cnty.crop.2 = data.frame(cnty.crop)
names(cnty.crop.2)[names(cnty.crop.2) == 'coords.x1'] <- 'Longitude'
names(cnty.crop.2)[names(cnty.crop.2) == 'coords.x2'] <- 'Latitude'

road.sub.coords <- coordinates(cnty.crop)
road.sub.coords.1 <- sapply(road.sub.coords, function(x) do.call("rbind", x))
road.sub.coords.2 <- do.call("rbind", road.sub.coords.1)
lines(road.sub.coords.2[chull(road.sub.coords.2),], type="l", col="red")
plot(road.sub.coords.2,cex=0.1)
plot(roads.mask,add=TRUE)

#get equally-spaced points on each line
# numOfPoints  <-  gLength(cnty.crop[2]) /5
# spsample(river, n = numOfPoints, type = "regular")

#convert road.coords to df
road.sub.coords.df<-as.data.frame(road.sub.coords.2)
colnames(road.sub.coords.df)<-c("Longitude","Latitude")
head(road.sub.coords.df)

#export roads as .shp 
writeSpatialShape(cnty.crop,"BerksCountyRoads")

#save coords 
write.csv(road.sub.coords.df, file="./Analyses/Predict_TOH/Data/Berks_county_coords.csv",row.names = FALSE)

########################################################################################################################
#clear environment
rm(list=ls())

#set working directory
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly")

#load packages
library(rgdal)
library(rgeos)
library(raster)
library(ggmap)
library(ggplot2)
library(imager)

#read in coordinate data
coords<-read.csv("./Analyses/Predict_TOH/Data/All_PA_coords.csv")

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


library(googleway)

## your valid API key here
key <- "AIzaSyAaDHA8482JIVvEv8yFPPcpvUukZ1j69zo"

#get subset of locations from road.sub.coords.df
myLocations<-coords.samp

#make directory to save streetview images
saveDir<-"/Users/zach/Dropbox (ZachTeam)/Andrew_Brenner/Projects/Predict_TOH/Data/Street_View_Images"
dir.create(saveDir)


#set up directions and pitches for google images
dirList<-c(seq(0,360, by=45))
#pitchList<-c(-90, -60, -30, 0, 30, 60, 90)
pitchList<-c(10)
fovList<-c(90)

i=1; j=1; k=1
for(i in 1:length(row.names(myLocations))){
  
  new.loc<-c(myLocations$Latitude[i], myLocations$Longitude[i])
  longitude=new.loc[2]
  latitude=new.loc[1]
  
    for(j in 1:length(dirList)){
      new.dir<-dirList[j]
      
      for(k in 1:length(pitchList)){
        new.pitch<-pitchList[k]
        
        #jpeg(paste(getwd(), "Data","StreetView_images",paste("img",i,"dir",j,"pitch",k,'streetView.jpeg',sep="_"), sep="/"))
        
        jpeg(paste(saveDir,paste(latitude, longitude,"img",i,"dir",j,"pitch",k,'streetView.jpeg',sep="_"), sep="/"))
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
  
  #prediction step
  
  
  
  
      }
    }
}


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