#SLF Individual-based Model in R

#For later, a) force dispersal, b) if cells are occupied, drive dispersal behavior)

#clear environment
rm(list=ls())

#set working directory to newly-created folder
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model")

#roads data
library(tigris)
library(ggplot2)
library(ggthemes)
library(rgeos)
library(sp)
library(mapproj)
library(rgdal)
library(raster)
library(sf)
library(gdalUtils)
library(pals)
library(viridis)

#####################################################################################################################################
#read in maxEnt raster for entire US
maxEnt_surface<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Results/SLF_predicted_distribution_1km_All_8-17-2020.tif")

#plot it
plot(maxEnt_surface, col=parula(100))

#crop raster to mid-Atlantic region
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
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
                                     grepl("tennessee", counties$ID)))

#check plot of sates with counties
plot(counties.sub)

#remove 3 counties not in states
removeCountyList<-c("oklahoma,delaware","indiana,delaware","indiana,ohio","iowa,delaware")
counties.sub.2<-subset(counties.sub, ! ID %in% removeCountyList)

plot(counties.sub.2)

#crop MaxEnt raster
maxEnt_crop<-crop(maxEnt_surface, extent(counties.sub.2))
plot(maxEnt_crop)
maxEnt_mask<-raster::mask(maxEnt_crop, counties.sub.2)

plot(maxEnt_mask, col=viridis(100))
plot(counties.sub.2, col="transparent",border=alpha("gray80",0.2),add=TRUE)

#look at dimensions and resolution
dim(maxEnt_mask) #1633 2793    1
res(maxEnt_mask) #0.008333333 0.008333333
range(na.omit(values(maxEnt_mask))) # 0.000224895 0.825288355

writeRaster(maxEnt_mask, "/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_Northeast_1km.tiff", overwrite=TRUE)

#######################################################################################################################################
#now downsample

#downsample to get into 4-km2 resolution
1633/4 #408
2793/4 #698
new.res<-0.008333333*4 #0.03333333

#try gdalwarp
gdalwarp(srcfile="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_Northeast_1km.tif",dstfile="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_Northeast_4km.tif",ts=c(698, 408),overwrite=TRUE,verbose=TRUE)

#read in new resampled raster
maxEnt_mask_proj<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_Northeast_4km.tif")

plot(maxEnt_mask_proj)
dim(maxEnt_mask_proj)
res(maxEnt_mask_proj)

#################################################################################################################3
#read in raster and manipulate image with imager

#load imager package
library(imager)

#maxEnt_1km.raster<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_Northeast_1km.tif")

maxEnt_4km.raster<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_Northeast_4km.tif")

plot(maxEnt_4km.raster, col=viridis(255))
res(maxEnt_4km.raster)
plot(counties.sub.2, col="transparent",border="white",add=TRUE)

#redefine NA as 999 in raster
maxEnt_4km.raster[is.na(maxEnt_4km.raster[])]<- 999

#pixel resolution
# rasterXRes<-xres(new.raster)
# rasterYRes<-yres(new.raster)
# #pixel aspect ratio
# res.aspect<-rasterXRes/rasterYRes
# 
# #number cells raster dimensions
# ras.width<-ncol(new.raster)
# ras.height<-nrow(new.raster)
# #raster aspect ratio
# ras.aspectRatio<-ras.height/ras.width
# 
# #apply gaussian filter
# ras.width.2= ras.width*ras.aspectRatio
# ras.height.2 =ras.height*ras.
# 
# aspect = 0.007278994/0.01664027
# 
# ncol(PA_roadsRast)
# nrow(PA_roadsRast)

#convert raster to grayscale image
rast.img<-as.cimg(maxEnt_4km.raster)
plot(rast.img)
plot(rast.img, rescale=TRUE)


#Save boats image
save.image(rast.img,file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_grayscale_4km.tiff")

