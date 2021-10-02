#Web scraping of Google Street View data

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

#read in PA road data
roads.pa.shp<-readOGR(paste(getwd(), "Data","GIS", "tl_2013_42_prisecroads","tl_2013_42_prisecroads.shp",sep="/"),verbose=FALSE)

#plot roads.shp
plot(roads.pa.shp)
#show projection
projection(roads.pa.shp)


#read in MD road data
roads.md.shp<-readOGR(paste(getwd(), "Data","GIS", "tl_2013_24_prisecroads","tl_2013_24_prisecroads.shp",sep="/"),verbose=FALSE)

#plot roads.shp
plot(roads.md.shp, col="royalblue2")
#show projection
#projection(roads.md.shp)

#read in DE road data
roads.de.shp<-readOGR(paste(getwd(), "Data","GIS", "tl_2014_10_prisecroads","tl_2014_10_prisecroads.shp",sep="/"),verbose=FALSE)

#plot roads.shp
plot(roads.de.shp)

#read in NJ road data
roads.nj.shp<-readOGR(paste(getwd(), "Data","GIS", "tl_2015_34_prisecroads","tl_2015_34_prisecroads.shp",sep="/"),verbose=FALSE)

#plot roads.shp
plot(roads.nj.shp,color="darkgreen",add=TRUE)

#merge roads
roads.1<-rbind(roads.pa.shp, roads.de.shp)
roads.2<-rbind(roads.md.shp, roads.nj.shp)

roads.all<-rbind(roads.1, roads.2)
plot(roads.all)
#roads.shp <- spTransform(roads.shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

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
plot(road.coords.2)


#convert road.coords to df
road.coords.df<-as.data.frame(road.coords.2)
colnames(road.coords.df)<-c("Longitude","Latitude")
head(road.coords.df)

dim(road.coords.2)

#get state boundaries
us <- getData("GADM", country="USA", level=2)
state.name <- c("Pennsylvania","New Jersey","Delaware","Maryland")
states<-subset(us, NAME_1%in%state.name)
plot(states)

#get projection
projection(states)
projection(roads.all.shp)
states.projection<-projection(roads.all.shp)

projection(states)<-states.projection


#plot nicely
#get bounding box for pa
states.ext<-extent(states)
states.bbox<-c(left=extent(states)[1], right=extent(states)[2],bottom=extent(states)[3],top=extent(states)[4])

#make pa counties into dataframe
states.df<-fortify(states)

states.map<-get_map(location=states.bbox,maptype="terrain-background")
ggmap(states.map)

#function to get raster map from ggmap object
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}

#make into raster object
states.rast <- ggmap_rast(map = states.map) # convert google map to raster object
states.only <- mask(states.rast, states) # clip to bounds of census tracts

# prep raster as a data frame for printing with ggplot
states.map.2 <- data.frame(rasterToPoints(states.only))



new.plot<-ggplot(states.map.2) + 
  geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + 
  scale_color_identity()+
  geom_polygon(data=states.df,aes(x=long, y=lat,group=group),fill="transparent",color="black",size=0.25)+
  geom_point(data=road.coords.df, aes(x=Longitude,y=Latitude),size=0.01, color="goldenrod4",alpha=0.4)+
  #theme(panel.background=element_rect(fill='white',color="black"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.line=element_line(color="black"))+
  theme(panel.background=element_rect(color="black"))+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
  labs(x="Longitude",y="Latitude")+
  #coord_quickmap()
  coord_fixed(ratio = 4/3)
  #ggtitle(paste(speciesName, " - Count per sampling point",sep=""))
new.plot



#subset counties with SLF
myCounties<-c("Berks","Chester", "Delaware", "Montgomery","Philadelphia","Lancaster","Lebanon","Schuylkill","Lehigh","Bucks","Northampton","Carbon")
sub.counties<-pa[pa@data$NAME_2 %in% myCounties,]
#view counties
plot(sub.counties)


#mask spatial points of roads to inside sub.counties
sub.roads<-mask(road.coords.2, sub.counties)

plot(sub.counties)
cnty.extent<-extent(sub.counties)

#crop to extent
cnty.crop<-crop(x=roads.all.shp, y=cnty.extent, snap="out")
plot(cnty.crop)
plot(sub.counties, col=alpha("blue",0.2),add=TRUE)

#now mask
roads.mask <- mask(cnty.crop, sub.counties)

roads.mask<-gIntersection(cnty.crop, sub.counties)
plot(roads.mask)
plot(sub.counties, col=alpha("blue",0.2),add=TRUE)



library(googleway)

## your valid API key here
key <- "AIzaSyAr16piU1eAQMOMnzoMGPWbp3FeVwEm06w"

df <- data.frame(lat = -37.817714,
                 lon = 144.967260,
                 info = "Flinders Street Station")

new.img<-google_map(key = key, height = 400, search_box = F, data = df)



## other available methods
# add_markers
# add_heatmap
# add_circles
# add_traffic
# add_bicycling
# add_transit







#load libraries
library(RCurl)
library(RJSONIO)
library(plyr)


key="AIzaSyAr16piU1eAQMOMnzoMGPWbp3FeVwEm06w"

myURL<-function(X, Y,heading,pitch,key){
  paste("https://maps.googleapis.com/maps/api/streetview?size=600x300&location=", paste(my.loc$X, ",", my.loc$Y, sep=""), 
        "88&heading=", heading, "&&pitch=",pitch,"&key=", key, sep="")
}

#Pick a location
X=46.414382
Y=10.013988
heading=151.78
pitch=-0.76

my.url<-myURL(X=46.414382, Y=10.013988, heading=151.78, pitch=-0.76, key="AIzaSyAr16piU1eAQMOMnzoMGPWbp3FeVwEm06w")

my.loc<-myLocation(X=X, Y=Y, heading=heading, pitch=pitch)

paste("https://maps.googleapis.com/maps/api/streetview?size=600x300&location=", paste(my.loc$X, ",", my.loc$Y, sep=""), 
      "88&heading=", heading, "&&pitch=",pitch,"&key=", my_key, sep="")



#Build a URL to access the API:
url <- function(address , sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/json"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}


url(address = paste(X, ,",",Y))

#Function to parse the results:
geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}

#Test it
address <- geoCode("The White House, Washington, DC")

address <- c("131 Winslow Rd., Newark, DE 19711",
             "133 Winslow Rd., Newark, DE 19711",
             "135 Winslow Rd., Newark, DE 19711",
             "137 Winslow Rd., Newark, DE 19711"
             )
locations  <- ldply(address, function(x) geoCode(x))
names(locations)  <- c("lat","lon","location_type", "formatted")
head(locations)




install.packages('googleway')
library(googleway)

## your valid API key here
key <- "AIzaSyDHiuygu0h1qpkZd7kNQ3-YTeoWBQyB1JM"

map_key <- "your_api_key"
df <- data.frame(lat = -37.817714,
                 lon = 144.967260,
                 info = "Flinders Street Station")

google_map(key = key, height = 600, search_box = T) %>%
  add_markers(data = df, info_window = "info")


df <- data.frame(lat = -37.817714,
                 lon = 144.967260,
                 info = "Flinders Street Station")

google_map(key = key, height = 600, search_box = T) %>%
  add_markers(data = df, info_window = "info")

#generate lots of locations
40.174310, -75.965753
numbers <- c(1:10)
location<- data.frame(X=-71.514754, Y=41.436027)

myLocations<-list()
for(i in 2: 11){
  new.X<-location$X+(numbers[i-1]/8)
  new.Y<-location$Y+(numbers[i-1]/8)
  new.loc<-data.frame(X=new.X, Y=new.Y)
  myLocations<-rbind(myLocations,new.loc)
}

myLocations<-as.data.frame(myLocations)

for(i in 1:10){
  myLoc<-c( myLocations$Y[i], myLocations$X[i])
  google_streetview(location = myLoc,
                    size = c(1200,1200),
                    panorama_id = NULL,
                    output = "plot",
                    heading = 90,
                    fov = 120,
                    pitch = 0,
                    response_check = FALSE,
                    key = key)
  }

  
  
  
}



