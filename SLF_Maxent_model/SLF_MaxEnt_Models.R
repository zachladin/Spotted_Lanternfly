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

###########################################################################################
#get shape files of USA and native range countries created

worldclimelev<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_elevation/wc2.1_30s_elev.tif")

#set projection for cropping and masking rasters
myCRS<-projection(worldclimelev)

#get shapefiles vector of China, India, Vietnam, Korea, and Japan
NativeRangeMap = map('worldHires',list('China(?!:)','India(?!:)',"Vietnam(?!:)","North Korea(?!:)","South Korea(?!:)","Japan"),boundary=FALSE, interior = FALSE,fill=TRUE, col="royalblue1")

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

projection(SLF_shp)

###########################################################################################
#Now set up things for masking and saving rasters

########################################################
#test masking
plot(worldclimelev, col=parula(100))

wcElev_crop = crop(worldclimelev, extent(SLF_shp))
wcElev_mask= mask(wcElev_crop, SLF_shp)
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
    
    print("cropping raster")
    ras_crop = raster::crop(new.raster, extent(SLF_shp))
    ras_mask = raster::mask(ras_crop, SLF_shp)
    #plot(ras_mask)
    
    raster::writeRaster(ras_mask, filename=file.path(paste("./Data/BioClim_WorldClim_rasters/",new.rasterName,"_ALL_1km.tif",sep="")), format="GTiff", overwrite=TRUE)
    
}

########################################################
#Elevation

#save and export worldclim elevation data

new.raster<-worldclimelev
new.rasterName<-"wc_elev"
print(new.rasterName)

print("cropping raster to USA")
ras_crop = raster::crop(new.raster, extent(SLF_shp))
ras_mask = raster::mask(ras_crop, SLF_shp)
#plot(ras_mask)

raster::writeRaster(ras_mask, filename=file.path(paste("./Data/WorldClim_elev_rasters/",new.rasterName,"_ALL_1km.tif",sep="")), format="GTiff", overwrite=TRUE)

########################################################
#Human Footprint
  
#reproject
#set destination filepath
src_dataset="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/human_footprint_data/human.footprint.raster.tif"

#use gdalUtils::gdalwarp
gdalUtils::gdalwarp(srcfile=src_dataset,dstfile=file.path(getwd(),"Data/human_footprint_data/human.footprint.raster.proj_latlong.tif"),t_srs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",overwrite=TRUE,verbose=TRUE)

new.raster<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/human_footprint_data/human.footprint.raster.proj_latlong.tif")
new.rasterName<-"human_footprint"

#project raster
new.raster.proj<-projectRaster(new.raster, wcElev_mask, method='ngb')

plot(new.raster.proj, col=parula(100))

print("cropping raster")
ras_crop = raster::crop(new.raster.proj, extent(SLF_shp))
ras_mask = raster::mask(ras_crop, SLF_shp)
#plot(ras_mask)

raster::writeRaster(ras_mask, filename=file.path(paste("./Data/human_footprint_data/",new.rasterName,"_ALL_1km.tif",sep="")), format="GTiff", overwrite=TRUE)

plot(ras_mask, col=parula(100))
##########################################################################
#Species occurence data

#TOH
toh.gbif<-read.table("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/GBIF_TOH/0048906-200221144449610.csv",sep="\t",fill=TRUE,header=TRUE)

countriesKeep<-c("CN", "IN", "JP", "KP","KR","VN", "US")

#subset
toh.gbif.sub<-subset(toh.gbif, countryCode %in% countriesKeep)

#make table of frequency of detections per country
freq.table.toh<-aggregate(gbifID~countryCode, FUN=length, data=toh.gbif.sub)

# countryCode gbifID_TOH 
# 1          CN    199
# 2          IN      1
# 3          JP    112
# 4          KR      8
 
#remove any rows without coords
toh.gbif.sub<-subset(toh.gbif.sub, !is.na(decimalLatitude))

# #simplify data
toh.data.df<-toh.gbif.sub[,c("decimalLatitude","decimalLongitude")]
colnames(toh.data.df)<-c("Latitude","Longitude")
toh.data.df<-unique(na.omit(toh.data.df))

#convert coords to numeric
toh.data.df$Latitude<-as.numeric(as.character(toh.data.df$Latitude))
toh.data.df$Longitude<-as.numeric(as.character(toh.data.df$Longitude))

#now add other TOH data
toh.data.2<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/Tree_of_Heaven_data/24695.csv", skip=3,header=TRUE)

#remove any rows without coords
toh.data.2<-subset(toh.data.2, !is.na(Latitude))

#keep only positive detections
toh.data.2.df<-subset(toh.data.2, OccStatus=="Positive")

#remove Latitude > 50
toh.data.2.df<-subset(toh.data.2.df, Latitude < 50)

#simplify data
toh.data.2.df<-toh.data.2.df[,c("Latitude","Longitude")]

toh.data.2.df<-unique(na.omit(toh.data.2.df))

#convert coords to numeric
toh.data.2.df$Latitude<-as.numeric(as.character(toh.data.2.df$Latitude))
toh.data.2.df$Longitude<-as.numeric(as.character(toh.data.2.df$Longitude))

#combine toh data
toh.data.comb<-unique(rbind(toh.data.df, toh.data.2.df))

#try only with gbif data
#toh.data.comb<-toh.data.df

#remove weird Latitude values <0
toh.data.comb<-subset(toh.data.comb, ! Latitude < 0)

#remove data point
toh.data.comb<-subset(toh.data.comb, Longitude !=	5.110780)
toh.data.comb<-subset(toh.data.comb, Longitude != -6.595394)

toh.data.comb.coords<-toh.data.comb
#add value of 1 to indicate presence 
toh.data.comb.coords$Value<-1

coordinates(toh.data.comb.coords)<-~Longitude+Latitude

plot(SLF_shp,col="gray90")
points(toh.data.comb.coords, pch=16, cex=0.5, col=alpha("red",0.5))

# get coordinates of each point
#x <- na.omit(cbind(as.numeric(as.character(toh.data.comb$Longitude)), as.numeric(as.character(toh.data.comb$Latitude))))

#rasterize points
temp.rast <- raster()
extent(temp.rast) <- extent(SLF_shp) # this might be unnecessary

# And then ... rasterize it! This creates a grid version 
# of your points using the cells of rast, values from the IP field:
rast.out <- rasterize(toh.data.comb.coords, field=toh.data.comb.coords@data$Value,temp.rast, background=0, fun=mean)

#project raster to get into correct dimensions
rast.proj<-projectRaster(rast.out,wc_elev)

plot(rast.proj)

plot(rast.proj, col=parula(100))
range(values(rast.out))

#now mask, while still in latlong
toh_rasterDF.crop<-raster::crop(rast.proj, extent(SLF_shp))
#plot(toh_rasterDF.crop)
toh_rasterDF.mask<-raster::mask(toh_rasterDF.crop, SLF_shp)

plot(toh_rasterDF.mask, col=parula(100))

#writeRaster
raster::writeRaster(toh_rasterDF.mask, filename=file.path("./Data/Tree_of_Heaven_data/toh_raster_ALL_1km_latlong.tif"), format="GTiff", overwrite=TRUE)

###########################################################################################
#now prepare SLF occurence data in US and Asia for training MAXENT model

#read in SLF location data from gbif
slf.gbif<-read.table("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/GBIF_SLF/0048909-200221144449610.csv",sep="\t",header=TRUE, fill=TRUE)

#subset by countries
countriesAsia<-c("CN", "IN", "JP", "KP","KR","VN")

slf.Asia<-subset(slf.gbif, countryCode %in% countriesAsia)

#make table of frequency of detections per country
freq.table<-aggregate(gbifID~countryCode, FUN=length, data=slf.Asia)

slf.Asia.coords<-slf.Asia[,c("decimalLongitude","decimalLatitude")]
colnames(slf.Asia.coords)<-c("Longitude","Latitude")


#read in SLF presence data
slf.USA.locations.df<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/SLF_presence_data/SLF_presence_data.csv",header=TRUE)

slf.comb<-na.omit(rbind(slf.Asia.coords, slf.USA.locations.df))

#step one is to spatially join locations with buffers and generate a polygon, to sample environmental data from
coordinates(slf.comb) <- ~Longitude + Latitude
projection(slf.comb)<-CRS('+proj=longlat')

crs(slf.comb)

#####################################################################
#combine all variables, save as .Rdata and then test for correlations

#get BioClim data
bioClimFiles<-list.files("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All", full.names = TRUE)

bioClimNames<-list.files("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All", full.names = FALSE)

#compile file paths
modelVariables<-bioClimFiles
#compile list of filenames
fileNames<-bioClimNames


#summary function to get summary stats
summaryFunction <- function(DataIn, factor, response){
  summaryOut <- plyr::ddply(DataIn, factor, .fun = function(xx){
    c(n = length(xx[,response]),
      mean = mean(xx[,response],na.rm=TRUE),
      var = var(xx[,response],na.rm=TRUE),
      SD = sd(xx[,response],na.rm=TRUE),
      SE = sqrt(var(xx[,response])/length(xx[,response])),
      CV = sd(xx[,response],na.rm=TRUE)/mean(xx[,response],na.rm=TRUE) * 100)
  })
  return(summaryOut)
  dev.off()
}

library(plyr)
#read in raster data
variable.summary<-list()
data.compile<-data.frame("Cell"=seq(1:30621928))
for(i in 1:length(modelVariables)){
  
  new.name<-fileNames[i]
  print(new.name)
  
  new.raster<-raster(modelVariables[i])
  
  #plot(new.raster)
  
  new.data<-na.omit(as.data.frame(new.raster, xy=TRUE))
  
  new.data.out<- data.frame(new.data[,3])
  
  colnames(new.data)[3]<-"Value"
  new.data$VariableName<-new.name
  
  #get summary info
  new.data.summary<-summaryFunction(DataIn=new.data, factor="VariableName",response="Value")
  
  colnames(new.data.out)<-gsub(".tif","",new.name)
  
  #compile results
  variable.summary<-rbind(variable.summary, new.data.summary)
  
  data.compile<-rowr::cbind.fill(data.compile, new.data.out)
  
}

#save results
# write.csv(variable.summary, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Results/Tables/WorldClim_bioclim_Variable_summaries.csv", row.names=FALSE)

# write.csv(data.compile, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/All_variables_data.csv",row.names=FALSE)

# Save an object to a file
saveRDS(data.compile, file = "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim_All_variables_data.rds")

#load .rds file
variable.data<-readRDS("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_bioclim_All_variables_data.rds")

#remove 1st column
data.compile.sub<-variable.data[,-1]

#data.compile.sub<-data.compile[,-1]

#create a matrix of these data
data_rcorr <-as.matrix(data.compile.sub)

#get correlation matrix
mat_2 <-Hmisc::rcorr(data_rcorr)
# mat_2 <-rcorr(as.matrix(data)) returns the same output

mat.df<-as.data.frame(mat_2[1])

#save correlation table
write.csv(mat.df, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Results/Tables/WorldClim_bioclim_Variable_PearsonsR.csv",row.names=TRUE)

#plot the correlation matrix with GGally package
corrPlot<-GGally::ggcorr(data_rcorr, midpoint=0.8)

#save plot
jpeg(file = "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Results/Figures/WorldClim_bioclim_variable_correlations.jpeg", width=10, height=10, units="in",res=600)

plot(corrPlot)

dev.off()

#look at heatmap where all correlations > 0.8 are colored
mat.df_mod<-abs(mat.df)
mat.df_mod_2<-ifelse(mat.df_mod<=0.8,0,1)
library(reshape2)
mat.df_melt<-melt(mat.df_mod_2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat=mat.df_mod_2)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

corrPlot_2<-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="gray10")+
  theme(axis.text.x=element_text(angle=90))

ggsave(corrPlot_2, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Results/Figures/WorldClim_bioclim_variable_correlations_2.png", width=10, height=10, units="in",dpi=300)

#remove bio_10, bio_11, bio_5, bio_6, bio_9, bio_13, bio_16, bio_17, bio_7

#####################################################################
#now stack predictors

bio1 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_1_ALL_1km.tif")

bio2 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_2_ALL_1km.tif")

bio3 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_3_ALL_1km.tif")

bio4 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_4_ALL_1km.tif")

bio5 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_5_ALL_1km.tif")

bio6 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_6_ALL_1km.tif")

bio7 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_7_ALL_1km.tif")

bio8 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_8_ALL_1km.tif")

bio9 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_9_ALL_1km.tif")

bio10 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters _All/wc2.1_30s_bio_10_ALL_1km.tif")

bio11 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_11_ALL_1km.tif")

bio12 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_12_ALL_1km.tif")

bio13 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_13_ALL_1km.tif")

bio14 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_14_ALL_1km.tif")

bio15 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_15_ALL_1km.tif")

bio16 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_16_ALL_1km.tif")

bio17 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_17_ALL_1km.tif")

bio18 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_18_ALL_1km.tif")

bio19 = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_19_ALL_1km.tif")

US_dem =raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/WorldClim_elev_rasters/wc_elev_ALL_1km.tif")
  
humanFootprint = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/human_footprint_data/human_footprint_ALL_1km.tif")


TOH_raster = raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/Tree_of_Heaven_data/toh_raster_ALL_1km_latlong.tif")

#remove correlated data (>= 0.8) bio_10, bio_11, bio_5, bio_6, bio_9, bio_13, bio_16, bio_17, bio_7

predictorsSLF<-stack(bio1, bio2, bio3, bio4, bio8, bio12, bio14, bio15, bio18, bio19, US_dem, humanFootprint, TOH_raster)

names(predictorsSLF)

plot(predictorsSLF, col=parula(100))


#########################################################################
# circles with a radius of 300 km
x <- dismo::circles(slf.comb, d=300000, lonlat=TRUE)

pol <- polygons(x)
plot(pol)

# sample randomly from all circles
samp1 <- spsample(pol, 250, type='random', iter=25)

# use one of the bioclim rasters to create a mask
mask <- raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/BioClim_WorldClim_rasters_All/wc2.1_30s_bio_9_ALL_1km.tif")

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




########################################################################
#now extract raster values from SLF locations

#double-check this step, should be using locations with buffers (not just locations)

#extracting values from rasters
presvals <- raster::extract(predictorsSLF, x@polygons)
# setting random seed to always create the same
# random set of points for this example
set.seed(0)
backgr <- dismo::randomPoints(predictorsSLF, 500)
absvals <- raster::extract(predictorsSLF, backgr)
pb <- c(rep(1, nrow(presvals[[1]])), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
#names(sdmdata)[names(sdmdata)=="human.footprint.raster.mask"]<-"human_footprint"
#head(sdmdata)


#create training and test data for MaxENt
group <- dismo::kfold(slf.comb, 5)
pres_train <- slf.comb[group != 1, ]
pres_test <- slf.comb[group == 1, ]

#ext = extent(toh.locations.df)

backg <- dismo::randomPoints(predictorsSLF, n=1000, ext=extent(SLF_shp), extf = 1.25)
colnames(backg) = c('lon', 'lat')
group <- dismo::kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

#fit model (requires Java)
xmSLF <- dismo::maxent(predictorsSLF, pres_train)

#plot variable importance
plot(xmSLF)

#plot response curves
dismo::response(xmSLF)

#predict and map
e <- dismo::evaluate(pres_test, backg_test, xmSLF, predictorsSLF)
e



#get model-predicted estimates from MAXENT model
pxSLF <- dismo::predict(predictorsSLF, xmSLF, ext=extent(extent(IntroducedRangeMap_sp)), progress='text')
plot(pxSLF, col=parula(100))

#writeRaster
raster::writeRaster(pxSLF, filename=file.path("./Results/SLF_predicted_distribution_7_300km_rad.tif"), format="GTiff", overwrite=TRUE)


################################################################################333
