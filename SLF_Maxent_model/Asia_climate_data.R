#Script to format needed climate data from ground-based weather stations in Asia for SLF modeling


#downloading data from here: https://www.worldclim.org/data/worldclim21.html

#clear environment
rm(list=ls())

#load packages
library(ggplot2)
library(raster)
library(pals)
library(maps)       
library(mapdata)
library(maptools)
library(sf)
library(rgeos)
library(sp)
library(rgdal)
library(KernSmooth)

#set working directory
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt")

worldclimelev<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_elevation/wc2.1_30s_elev.tif")

#set projection for cropping and masking rasters
myCRS<-projection(worldclimelev)

#get shapefiles vector of China, India, Vietnam, Korea, and Japan
NativeRangeMap = map('worldHires',list('China(?!:)','India(?!:)',"Vietnam(?!:)","North Korea(?!:)","South Korea(?!:)","Japan(?!:)"),boundary=FALSE, interior = FALSE,fill=TRUE, col="royalblue1")

IntroducedRangeMap<-map('worldHires',"USA(?!:)",boundary=FALSE, interior = FALSE,fill=TRUE, col="royalblue1")

#convert map to spatialPolygons
NativeRangeMap_sp<-map2SpatialPolygons(NativeRangeMap,IDs=NativeRangeMap$names,proj4string = CRS(myCRS), checkHoles=FALSE)

#convert map to spatialPolygons
IntroducedRangeMap_sp<-map2SpatialPolygons(IntroducedRangeMap,IDs=IntroducedRangeMap$names,proj4string = CRS(myCRS), checkHoles=FALSE)
plot(IntroducedRangeMap_sp)

#convert to SpatialPolygonsDataFrame
native_pid <- sapply(slot(NativeRangeMap_sp, "polygons"), function(x) slot(x, "ID"))
native_p.df <- data.frame( ID=1:length(NativeRangeMap_sp), row.names = native_pid)
NativeRangeMap_sp.df <- SpatialPolygonsDataFrame(NativeRangeMap_sp,native_p.df)

introduced_pid <- sapply(slot(IntroducedRangeMap_sp, "polygons"), function(x) slot(x, "ID"))
introduced_p.df <- data.frame( ID=1:length(IntroducedRangeMap_sp), row.names = introduced_pid)
IntroducedRangeMap_sp.df <- SpatialPolygonsDataFrame(IntroducedRangeMap_sp,introduced_p.df)

Comb_map<-rbind(NativeRangeMap_sp.df, IntroducedRangeMap_sp.df)

plot(Comb_map)

#save as a shapefile
writeOGR(obj=Comb_map, dsn="./Data/SLF_Range_Map", layer="SLF_range_countries", driver="ESRI Shapefile",overwrite_layer = TRUE)

#read in shapefile
SLF_shp<-readOGR(dsn="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/SLF_Range_Map/",layer="SLF_range_countries")

plot(SLF_shp, col="seagreen4")

projection(USA_shp_1)



#get shapefiles USA
USA_shp_1<-readOGR(dsn="./Data/GIS/",layer="USA_contiguous")
projection(USA_shp_1)

USA_shp<-spTransform(USA_shp_1, CRS(myCRS))
crs(USA_shp)
extent(USA_shp)

#read in Asia shapefile
Asia_shp<-readOGR(paste("./Data/SLF_native_range_map/","SLF_native_range_countries",".shp",sep=""), layer= "SLF_native_range_countries")
extent(Asia_shp)


########################################################
#test masking
plot(worldclimelev, col=parula(100))

wcElev_crop = crop(worldclimelev, extent(Asia_shp))
wcElev_mask= mask(wcElev_crop, Asia_shp)
plot(wcElev_mask, col=parula(100))

wcElev_crop = crop(worldclimelev, extent(USA_shp))
wcElev_mask= mask(wcElev_crop, USA_shp)
plot(wcElev_mask, col=parula(100))

########################################################
#BioClim variables

#now make lists for USA and for Asia
rasterList<- c("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_1.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_10.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_11.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_12.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_13.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_14.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_15.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_16.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_17.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_18.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_19.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_2.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_3.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_4.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_5.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_6.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_7.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_8.tif",
               "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim/wc2.1_30s_bio/wc2.1_30s_bio_9.tif"
)

#make list of raster names
rasterNameList <-c(gsub(".tif","", list.files("./Data/WorldClim_bioclim/wc2.1_30s_bio")))

#loop through and export rasters clipped to USA and Asia shapefiles
 for(j in 1:length(rasterList)){
    
    new.raster<-raster(rasterList[j])
    new.rasterName<-rasterNameList[j]
    print(new.rasterName)
    
    print("cropping raster to USA")
    ras_crop = raster::crop(new.raster, extent(USA_shp))
    ras_mask = raster::mask(ras_crop, USA_shp)
    #plot(ras_mask)
    
    raster::writeRaster(ras_mask, filename=file.path(paste("./Data/BioClim_WorldClim_rasters/",new.rasterName,"_USA_1km.tif",sep="")), format="GTiff", overwrite=TRUE)
    
    print("cropping raster to Asia")
    ras_crop2 = raster::crop(new.raster, extent(Asia_shp))
    ras_mask2 = raster::mask(ras_crop2, Asia_shp)
    #plot(ras_mask2, col=parula(100))
    print("saving raster")
    raster::writeRaster(ras_mask2, filename=file.path(paste("./Data/BioClim_WorldClim_rasters/",new.rasterName,"_Asia_1km.tif",sep="")), format="GTiff", overwrite=TRUE)
}

########################################################
#Elevation

#save and export worldclim elevation data

new.raster<-worldclimelev
new.rasterName<-"wc_elev"
print(new.rasterName)

print("cropping raster to USA")
ras_crop = raster::crop(new.raster, extent(USA_shp))
ras_mask = raster::mask(ras_crop, USA_shp)
#plot(ras_mask)

raster::writeRaster(ras_mask, filename=file.path(paste("./Data/WorldClim_elev_rasters/",new.rasterName,"_USA_1km.tif",sep="")), format="GTiff", overwrite=TRUE)

print("cropping raster to Asia")
ras_crop2 = raster::crop(new.raster, extent(Asia_shp))
ras_mask2 = raster::mask(ras_crop2, Asia_shp)
#plot(ras_mask2, col=parula(100))
print("saving raster")
raster::writeRaster(ras_mask2, filename=file.path(paste("./Data/WorldClim_elev_rasters/",new.rasterName,"_Asia_1km.tif",sep="")), format="GTiff", overwrite=TRUE)

########################################################
#Human Footprint
  
#reproject
#set destination filepath
src_dataset="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/human_footprint_data/human.footprint.raster.tif"

#use gdalUtils::gdalwarp
gdalUtils::gdalwarp(srcfile=src_dataset,dstfile=file.path(getwd(),"Data/human_footprint_data/human.footprint.raster.proj_latlong.tif"),t_srs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",overwrite=TRUE,verbose=TRUE)

crs(USA_shp)

new.raster<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/human_footprint_data/human.footprint.raster.proj_latlong.tif")
new.rasterName<-"human_footprint"
print(new.rasterName)

plot(new.raster, col=parula(100))

print("cropping raster to USA")
ras_crop = raster::crop(new.raster, extent(USA_shp))
ras_mask = raster::mask(ras_crop, USA_shp)
#plot(ras_mask)

raster::writeRaster(ras_mask, filename=file.path(paste("./Data/human_footprint_data/",new.rasterName,"_USA_1km.tif",sep="")), format="GTiff", overwrite=TRUE)

print("cropping raster to Asia")
ras_crop2 = raster::crop(new.raster, extent(Asia_shp))
ras_mask2 = raster::mask(ras_crop2, Asia_shp)
#plot(ras_mask2, col=parula(100))
print("saving raster")
raster::writeRaster(ras_mask2, filename=file.path(paste("./Data/human_footprint_data/",new.rasterName,"_Asia_1km.tif",sep="")), format="GTiff", overwrite=TRUE)

ras_mask2

##########################################################################
#Species occurence data

#TOH
toh.gbif<-read.table("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/GBIF_TOH/0048906-200221144449610.csv",sep="\t",fill=TRUE,header=TRUE)

countriesKeep<-c("CN", "IN", "JP", "KP","KR","VN", "US")

#subset
toh.gbif.sub<-subset(toh.gbif, countryCode %in% countriesKeep)
 
#remove any rows without coords
toh.gbif.sub.2<-subset(toh.gbif.sub, !is.na(decimalLatitude))

# #keep only positive detections
# toh.data.US<-subset(toh.gbif.sub.2, countryCode=="US")
# 
# #further filtering of data
# toh.data.US<-subset(toh.data.US, c(stateProvince !="Queensland" & stateProvince !=""))
# 
# #simplify data
toh.data.df<-toh.gbif.sub.2[,c("decimalLatitude","decimalLongitude")]
colnames(toh.data.df)<-c("Latitude","Longitude")
toh.data.df<-na.omit(toh.data.df)

#convert coords to numeric
toh.data.df$Latitude<-as.numeric(as.character(toh.data.df$Latitude))
toh.data.df$Longitude<-as.numeric(as.character(toh.data.df$Longitude))

#now add other TOH data
toh.data.2<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/Tree_of_Heaven_data/24695.csv", skip=3,header=TRUE)

#remove any rows without coords
toh.data.2.sub<-subset(toh.data.2, !is.na(Latitude))

#keep only positive detections
toh.data.2.df<-subset(toh.data.2.sub, OccStatus=="Positive")

#remove Latitude > 50
toh.data.2.df<-subset(toh.data.2.df, Latitude < 50)

#simplify data
toh.data.2.df<-toh.data.2.df[,c("Latitude","Longitude")]

toh.data.2.df<-na.omit(toh.data.2.df)

#convert coords to numeric
toh.data.2.df$Latitude<-as.numeric(as.character(toh.data.2.df$Latitude))
toh.data.2.df$Longitude<-as.numeric(as.character(toh.data.2.df$Longitude))

#combine toh data
toh.data.comb<-unique(rbind(toh.data.df, toh.data.2.df))

# get coordinates of each point
x <- na.omit(cbind(as.numeric(as.character(toh.data.comb$Longitude)), as.numeric(as.character(toh.data.comb$Latitude))))

#set dimensions for raster (area of prediction)
ras.width=4483
ras.height=2660

USA_bbox<-as.data.frame(USA_shp@bbox)
minX<-ceiling(USA_bbox$min[1]*1.3)
maxX<-ceiling(USA_bbox$max[1]*0.7)
minY<-ceiling(USA_bbox$min[2]*0.7)
maxY<-ceiling(USA_bbox$max[2]*1.5)

#get kernel density values
toh.est <- bkde2D(x, bandwidth = c(1, 1), gridsize = c(ras.height, ras.width),
                  range.x=list(c(minX, maxX),c(minY, maxY)))

range(toh.est$fhat)

#get data.frame of output
toh_BKD_df <- data.frame(lat = rep(toh.est$x2, each = ras.height), lon = rep(toh.est$x1, ras.width),count = c(toh.est$fhat))

# create spatial points data frame
toh_spg <- toh_BKD_df
coordinates(toh_spg) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(toh_spg) <- TRUE
# coerce to raster
toh_rasterDF <- raster(toh_spg)

crs(toh_rasterDF)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#plot(toh_rasterDF, col=parula(100))

#now mask, while still in latlong
toh_rasterDF.crop<-raster::crop(toh_rasterDF, extent(USA_shp))
#plot(toh_rasterDF.crop)
toh_rasterDF.mask<-raster::mask(toh_rasterDF.crop, USA_shp)

plot(toh_rasterDF.mask, col=parula(100))

#normalize raster values
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}

r_values<- toh_rasterDF.mask@data@values
r_norm_values<- range01(r_values)
range(r_norm_values,na.rm=TRUE)
toh_rasterDF.mask[]<-r_norm_values

plot(toh_rasterDF.mask, col=parula(100))

#save raster
writeRaster(toh_rasterDF.mask, filename="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/Tree_of_Heaven_data/toh_raster_USA_1km_latlong.tif",overwrite=TRUE)


#Now TOH in Asia
countriesAsia<-c("CN", "IN", "JP", "KP","KR","VN")

#subset
toh.gbif.Asia.sub<-subset(toh.gbif, countryCode %in% countriesAsia)

#remove any rows without coords
toh.gbif.Asia.sub.2<-subset(toh.gbif.Asia.sub, !is.na(decimalLatitude))

#simplify data
toh.data.Asia.2.df<-toh.gbif.Asia.sub.2[,c("decimalLatitude","decimalLongitude")]
colnames(toh.data.Asia.2.df)<-c("Latitude","Longitude")

toh.data.Asia.2.df<-na.omit(toh.data.Asia.2.df)

#convert coords to numeric
toh.data.Asia.2.df$Latitude<-as.numeric(as.character(toh.data.Asia.2.df$Latitude))
toh.data.Asia.2.df$Longitude<-as.numeric(as.character(toh.data.Asia.2.df$Longitude))

# get coordinates of each point
x <- na.omit(cbind(as.numeric(as.character(toh.data.Asia.2.df$Longitude)), as.numeric(as.character(toh.data.Asia.2.df$Latitude))))

#set dimensions for raster (area of prediction)
ras.width=4483
ras.height=2660

Asia_bbox<-as.data.frame(Asia_shp@bbox)
minX<-ceiling(Asia_bbox$min[1]*1)
maxX<-ceiling(Asia_bbox$max[1]*1)
minY<-ceiling(Asia_bbox$min[2]*1)
maxY<-ceiling(Asia_bbox$max[2]*1)

#get kernel density values
toh.Asia.est <- bkde2D(x, bandwidth = c(1, 1), gridsize = c(ras.height, ras.width),
                  range.x=list(c(minX, maxX),c(minY, maxY)))

range(toh.Asia.est$fhat)

#get data.frame of output
toh_Asia_BKD_df <- data.frame(lat = rep(toh.Asia.est$x2, each = ras.height), lon = rep(toh.Asia.est$x1, ras.width),count = c(toh.Asia.est$fhat))

# create spatial points data frame
toh_Asia_spg <- toh_Asia_BKD_df
coordinates(toh_Asia_spg) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(toh_Asia_spg) <- TRUE
# coerce to raster
toh_Asia_rasterDF <- raster(toh_Asia_spg)

crs(toh_Asia_rasterDF)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#plot(toh_rasterDF, col=parula(100))

#now mask, while still in latlong
toh_Asia_rasterDF.crop<-raster::crop(toh_Asia_rasterDF, extent(Asia_shp))
#plot(toh_rasterDF.crop)
toh_Asias_rasterDF.mask<-raster::mask(toh_Asia_rasterDF.crop, Asia_shp)

plot(toh_Asias_rasterDF.mask, col=parula(100))

#normalize raster values
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}

r_values<- toh_Asias_rasterDF.mask@data@values
r_norm_values<- range01(r_values)
range(r_norm_values,na.rm=TRUE)
toh_Asias_rasterDF.mask[]<-r_norm_values

plot(toh_Asias_rasterDF.mask, col=parula(100))

#save raster
writeRaster(toh_Asias_rasterDF.mask, filename="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/Tree_of_Heaven_data/toh_raster_Asia_1km_latlong.tif",overwrite=TRUE)

##############################################################################################
#make one giant raster of global TOH distribution

#remove any rows without coords
toh.gbif.all<-subset(toh.gbif, !is.na(decimalLatitude))

#simplify data
toh.gbif.all.sub<-toh.gbif.all[,c("decimalLatitude","decimalLongitude")]
colnames(toh.gbif.all.sub)<-c("Latitude","Longitude")

toh.gbif.all.sub.2<-na.omit(toh.gbif.all.sub)

#convert coords to numeric
toh.gbif.all.sub.2$Latitude<-as.numeric(as.character(toh.gbif.all.sub.2$Latitude))
toh.gbif.all.sub.2$Longitude<-as.numeric(as.character(toh.gbif.all.sub.2$Longitude))

#combine toh data
toh.data.comb<-na.omit(unique(rbind(toh.gbif.all.sub.2, toh.data.2.df)))

#save as TOH_all_shapefile
xy <- toh.data.comb[,c("Longitude","Latitude")]

library(spatstat)
K1 <- density(xy) # Using the default bandwidth
plot(K1, main=NULL, las=1)

spdf<-SpatialPointsDataFrame(xy, data=toh.data.comb)
#coordinates(toh.data.comb)<- ~Longitude+Latitude

crs(spdf)<-crs(worldclimelev)

spdf.proj<-spTransform(spdf, crs(worldclimelev))
spdf.proj$value<-1

#save as a shapefile
writeOGR(obj=spdf.proj, dsn="./Data/Tree_of_Heaven_data", layer="TOH_all", driver="ESRI Shapefile",overwrite_layer = TRUE)





# get coordinates of each point
x <- na.omit(cbind(as.numeric(as.character(toh.data.comb$Longitude)), as.numeric(as.character(toh.data.comb$Latitude))))

#set dimensions for raster (area of prediction)

#check raster dimensions to get width and height
worldExtent<-extent(worldclimelev)

ras.width=43200/5
ras.height=21600/5

minX<-worldExtent@xmin
maxX<-worldExtent@xmax
minY<-worldExtent@ymin
maxY<-worldExtent@ymax

#get kernel density values
toh.all.est <- bkde2D(x, bandwidth = c(10, 10), gridsize = c(ras.height, ras.width),range.x=list(c(minX, maxX),c(minY, maxY)))

#get data.frame of output
toh.all.est_df <- data.frame(lat = rep(toh.all.est$x2, each = ras.height), lon = rep(toh.all.est$x1, ras.width),count = c(toh.all.est$fhat))

# create spatial points data frame
toh_all_spg <- toh.all.est_df
coordinates(toh_all_spg) <- ~ lon + lat

# coerce to SpatialPixelsDataFrame
gridded(toh_all_spg) <- TRUE

# coerce to raster
toh_all_rasterDF <- raster(toh_all_spg)

crs(toh_all_rasterDF)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

plot(toh_all_rasterDF, col=parula(100))

crs(toh_all_rasterDF)
crs(worldclimelev)
extent(worldclimelev)

ext<-extent(toh_all_rasterDF)

extent(toh_all_rasterDF)<-ceiling(extent(toh_all_rasterDF))

toh_all_rasterDF<-projectRaster(toh_all_rasterDF, worldclimelev)
#try gdalwarp


#now crop and mask
toh_all_crop<-raster::crop(toh_all_rasterDF, extent(worldclimelev))
#plot(toh_rasterDF.crop)
toh_all_mask<-raster::mask(toh_all_crop, worldclimelev)

plot(toh_all_mask)

crs(toh_all_crop)
crs(worldclimelev)


extent(worldclimelev)
extent(toh_all_crop)


#try a gdalUtils option with full size raster!

#destination file
dst <- paste("./Data/Tree_of_Heaven_data/toh_raster_all_1km_latlong.tif",sep="")

new.shp<-readOGR("./Data/Tree_of_Heaven_data/TOH_all.shp", layer= "TOH_all")
plot(new.shp, col="blue")

toh.points    <- as(new.shp, "ppp")
plot(toh.points, main=NULL, cols=rgb(0,0,0,.2), pch=20)
crs(new.shp)

K1 <- raster(density(toh.points)) # Using the default bandwidth
K1.crop<-raster::crop(K1, extent(worldclimelev))
#plot(toh_rasterDF.crop)
K1.mask<-raster::mask(K1.crop, worldclimelev)


new.shp@data$value<-rep(1,length(new.shp))
new.zfield<- names(new.shp@data)[3]

#try gdal_grid (This is flipping the image, need to sort this out!)
gdalUtils::gdal_grid(src_datasource="./Data/Tree_of_Heaven_data/TOH_all.shp", dst_filename = dst, zfield=new.zfield,outsize=c(43200,21600), a="invdist:power=2.0",txe=c(minX, maxX), tye=c(minY, maxY))

#read in raster and plot (GET THIS FOR ENTIRE USA)
new.raster<-raster("./Data/Tree_of_Heaven_data/toh_raster_all_1km_latlong.tif")
plot(new.raster, col=parula(100))
new.raster.flip<-flip(new.raster, direction = "y")
# plot(new.raster.flip, col=parula(100))

#now crop and mask
new.raster.crop<-crop(new.raster.flip, myExtent)
new.raster.mask<-mask(new.raster.crop, USAshp.proj)







range(toh.all.est$fhat)

#get data.frame of output
toh_all_BKD_df <- data.frame(lat = rep(toh.all.est$x2, each = ras.height), lon = rep(toh.all.est$x1, ras.width),count = c(toh.all.est$fhat))

# create spatial points data frame
toh_all_spg <- toh_all_BKD_df
coordinates(toh_all_spg) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(toh_all_spg) <- TRUE
# coerce to raster
toh_all_rasterDF <- raster(toh_all_spg)

crs(toh_all_rasterDF)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

plot(toh_all_rasterDF, col=parula(100))

#now mask, while still in latlong
toh_all_rasterDF.crop<-raster::crop(toh_all_rasterDF, extent(worldclimelev))
#plot(toh_rasterDF.crop)
toh_all_rasterDF.mask<-raster::mask(toh_all_rasterDF.crop, worldclimelev)

plot(toh_all_rasterDF.mask, col=parula(100))

#normalize raster values
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}

r_values<- toh_Asias_rasterDF.mask@data@values
r_norm_values<- range01(r_values)
range(r_norm_values,na.rm=TRUE)
toh_Asias_rasterDF.mask[]<-r_norm_values

plot(toh_Asias_rasterDF.mask, col=parula(100))

#save raster
writeRaster(toh_Asias_rasterDF.mask, filename="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/Tree_of_Heaven_data/toh_raster_Asia_1km_latlong.tif",overwrite=TRUE)


###########################################################################################
#now prepare SLF occurence data in US and Asia for training MAXENT model

#read in SLF location data from gbif
slf.gbif<-read.table("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/GBIF_SLF/0048909-200221144449610.csv",sep="\t",header=TRUE, fill=TRUE)

#subset by countries
countriesAsia<-c("CN", "IN", "JP", "KP","KR","VN")

slf.Asia<-subset(slf.gbif, countryCode %in% countriesAsia)

slf.Asia.coords<-slf.Asia[,c("decimalLongitude","decimalLatitude")]
colnames(slf.Asia.coords)<-c("Longitude","Latitude")



#read in SLF presence data
slf.USA.locations.df<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/SLF_presence_data/SLF_presence_data.csv",header=TRUE)

slf.comb<-na.omit(rbind(slf.Asia.coords, slf.USA.locations.df))

#step one is to spatially join locations with buffers and generate a polygon, to sample environmental data from
coordinates(slf.comb) <- ~Longitude + Latitude
projection(slf.comb)<-CRS('+proj=longlat')

crs(slf.comb)

# circles with a radius of 50 km
x <- dismo::circles(slf.comb, d=300000, lonlat=TRUE)

pol <- polygons(x)
plot(pol)

# sample randomly from all circles
samp1 <- spsample(pol, 250, type='random', iter=25)

# use one of the bioclim rasters to create a mask
mask <- raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_elevation/wc2.1_30s_elev.tif")

# get unique cells
cells <- cellFromXY(mask, samp1)
#length(cells)

cells <- unique(cells)
#length(cells)

xy <- xyFromCell(mask, cells)

# extract cell numbers for the circles
v <- extract(mask, x@polygons, cellnumbers=T)
# use rbind to combine the elements in list v
v <- do.call(rbind, v)
# get unique cell numbers from which you could sample
v <- unique(v[,1])
#head(v)

# to display the results
m <- mask
m[] <- NA
m[v] <- 1

plot(m, ext=extent(x@polygons)+1)

#####################################################################
#combine all variables, save as .Rdata and then test for correlations




#####################################################################
#now stack predictors

bio2 = 
  
bio3 = 
  
bio4 = 
  
bio5 =
  
bio6 =
  
bio7 =
  
bio8 =
  
bio10 =

bio13 =

bio15 =

bio17 =
  
US_dem =
  
humanFootprint =
  
TOH_raster =



predictorsSLF<-stack(bio2, bio3, bio4, bio6, bio7, bio8, bio10, bio13, bio15, bio17, minJanTemp, US_dem,humanFootprint, TOH_raster)

names(predictorsSLF)