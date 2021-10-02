#script for getting vineyard and TOH distributions ready for MaxEnt modeling

#using data from https://github.com/UCDavisLibrary/ava

#clear environement
rm(list=ls())

#set working directory
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt")

#load packages
library(geojsonio)
library(sp)
library(raster)
library(rgdal)
library(pals)
library(sf)
library(pointdensityP)
library(KernSmooth)

#get USshp for cropping raster
#read in human.footprint data projected and masked
human.footprint<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/human_footprint_data/human.footprint.raster.mask.tif")
plot(human.footprint, col=parula(100))

#look at projection
crs(human.footprint)

extent(human.footprint)

#bring in shapefile
USAshp<-readOGR(dsn="./Data/GIS/",layer="USA_contiguous")
#plot(USAshp, col="green")
crs(USAshp)

extent(USAshp)

#reproject USAshp
USAshp.proj<-spTransform(USAshp, crs(human.footprint))

########################################################################################
#vineyards

# #load vineyard areas (polygons)
# pathToJSON<-"https://raw.githubusercontent.com/UCDavisLibrary/ava/master/avas.geojson"
# 
# vineyards.data <- geojson_read(pathToJSON,  what = "sp")
# 
# #rasterize polygons
# vineyard_r <- raster(extent(vineyards.data)) # +1 to increase the area
# res(vineyard_r) <- 0.01 # you can change the resolution here. 0.1 can be about 10x10 km if you are close to the equator.
# crs(vineyard_r) <- projection(vineyards.data) # transfer the coordinate system to the raster
# 
# 
# # Now you can add data to the cells in your raster to mark the ones that fall within your polygon.
# vineyard_r[vineyards.data,] <- 1 # this an easy way to subset a raster using a SpatialPolygons object
# 
# # check your raster
# plot(vineyard_r)
# 
# crs(vineyard_r)
# 
# vineyard_r_mask<-mask(vineyard_r, USAshp)
# plot(USAshp, col)
# plot(vineyard_r_mask, col="skyblue", add=TRUE)
# 
# #writeRaster
# writeRaster(vineyard_r_mask, filename="./Data/US_Vineyard_Data/USvineyards_raster.tif",overwrite=TRUE)
# 
# vineyard_raster<-raster("./Data/US_Vineyard_Data/USvineyards_raster.tif")
# 
# crs(vineyard_raster)
# 
# plot(vineyard_raster)
# 
# #reproject raster
# # r.proj<-projectRaster(from=vineyard_raster, to=human.footprint)
# # 
# # plot(r.proj)
# # 
# # unique(values(r.proj))
# 
# # #writeRaster
# # writeRaster(vineyard_raster, filename="./Data/US_Vineyard_Data/USvineyards_raster_withNAs.tif",overwrite=TRUE)
# # 
# # #load raster
# # vineyards.raster <- raster("./Data/US_Vineyard_Data/USvineyards_raster_withNAs.tif")
# 
# 
# vineyard.df<-na.omit(as.data.frame(vineyard_raster, xy=TRUE))
# 
# # sqrt(19868)
# # 
# x <- cbind(vineyard.df$x, vineyard.df$y)
# 
# #set dimensions of raster
# ras.width=4483
# ras.height=2660
# 
# #get kernel density values
# vineyard.est <- bkde2D(x, bandwidth = c(1, 1), gridsize = c(1000, 1000))
# 
# #get data.frame of estimated cell values
# # vineyard_BKD_df <- data.frame(lat = rep(vineyard.est$x2, each = ras.height), lon = rep(vineyard.est$x1, ras.width),count = c(vineyard.est$fhat))
# 
# vineyard_BKD_df <- data.frame(lat = rep(vineyard.est$x2, each = 1000), lon = rep(vineyard.est$x1, 1000),count = c(vineyard.est$fhat))
# 
# # create spatial points data frame
# vineyard.spg <- vineyard_BKD_df
# coordinates(vineyard.spg) <- ~ lon + lat
# # coerce to SpatialPixelsDataFrame
# gridded(vineyard.spg) <- TRUE
# # coerce to raster
# vineyard_rasterDF <- raster(vineyard.spg)
# vineyard_rasterDF
# 
# plot(vineyard_rasterDF, col=parula(100))
# 
# #now crop and mask
# crs(vineyard_rasterDF)<-projection(human.footprint)
# 
# crs(human.footprint)
# crs(USAshp.proj)
# vineyard_rasterDF.mask<-mask(vineyard_rasterDF, USAshp.proj)
# 
# plot(vineyard_rasterDF.mask)
# 
# writeRaster(vineyard_rasterDF.mask, filename="./Data/US_Vineyard_Data/USvineyards_raster_1km.tif")

################################################################################

#load vineyard areas (polygons)
pathToJSON<-"https://raw.githubusercontent.com/UCDavisLibrary/ava/master/avas.geojson"

vineyards.data <- geojson_read(pathToJSON,  what = "sp")

#rasterize polygons
vineyard_r <- raster(extent(vineyards.data)) # +1 to increase the area
res(vineyard_r) <- 0.01 # you can change the resolution here. 0.1 can be about 10x10 km if you are close to the equator.
crs(vineyard_r) <- projection(vineyards.data) # transfer the coordinate system to the raster

# Now you can add data to the cells in your raster to mark the ones that fall within your polygon.
vineyard_r[vineyards.data,] <- 1 # this an easy way to subset a raster using a SpatialPolygons object

# check your raster
plot(vineyard_r)

crs(vineyard_r)

#convert raster to data.frame
vineyard.df<-na.omit(as.data.frame(vineyard_r, xy=TRUE))

# get coordinates of each point
x <- cbind(vineyard.df$x, vineyard.df$y)

#set dimensions for raster (area of prediction)
ras.width=4483
ras.height=2660

USAbbox<-as.data.frame(USAshp@bbox)
minX<-ceiling(USAbbox$min[1]*1.3)
maxX<-ceiling(USAbbox$max[1]*0.7)
minY<-ceiling(USAbbox$min[2]*0.7)
maxY<-ceiling(USAbbox$max[2]*1.5)

#get kernel density values
vineyard.est <- bkde2D(x, bandwidth = c(1, 1), gridsize = c(ras.height, ras.width),
                  range.x=list(c(minX, maxX),c(minY, maxY)))

#get data.frame of output
vineyard_BKD_df <- data.frame(lat = rep(vineyard.est$x2, each = ras.height), lon = rep(vineyard.est$x1, ras.width),count = c(vineyard.est$fhat))

# create spatial points data frame
vineyard_spg <- vineyard_BKD_df
coordinates(vineyard_spg) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(vineyard_spg) <- TRUE
# coerce to raster
vineyard_rasterDF <- raster(vineyard_spg)

crs(vineyard_rasterDF)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

plot(vineyard_rasterDF, col=parula(100))

#now mask, while still in latlong
vineyard_rasterDF.crop<-crop(vineyard_rasterDF, extent(USAshp))
plot(vineyard_rasterDF.crop)
vineyard_rasterDF.mask<-mask(vineyard_rasterDF.crop, USAshp)

plot(vineyard_rasterDF.mask, col=parula(100))

vineyard_rasterDF.proj<-projectRaster(vineyard_rasterDF.mask, human.footprint)
vineyard_rasterDF.proj

plot(vineyard_rasterDF.proj)

writeRaster(vineyard_rasterDF.proj, filename="./Data/US_Vineyard_Data/USvineyards_raster_1km.tif",overwrite=TRUE)

################################

test.raster<-raster("./Data/US_Vineyard_Data/USvineyards_raster_1km.tif")
plot(test.raster, col=parula(100))




##########################################################################################
#TOH distribution

#read in TOH .csv data from https://www.eddmaps.org/distribution/viewmap.cfm?sub=3003

toh.data<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/Tree_of_Heaven_data/24695.csv", skip=3,header=TRUE)

#remove any rows without coords
toh.data.sub<-subset(toh.data, !is.na(Latitude))

#keep only positive detections
toh.data.df<-subset(toh.data.sub, OccStatus=="Positive")

#remove Latitude > 50
toh.data.df<-subset(toh.data.df, Latitude < 50)

#simplify data
toh.data.df<-toh.data.df[,c("objectid","Latitude","Longitude")]

# get coordinates of each point
x <- cbind(toh.data.df$Longitude, toh.data.df$Latitude)

#set dimensions for raster (area of prediction)
ras.width=4483
ras.height=2660

USAbbox<-as.data.frame(USAshp@bbox)
minX<-ceiling(USAbbox$min[1]*1.3)
maxX<-ceiling(USAbbox$max[1]*0.7)
minY<-ceiling(USAbbox$min[2]*0.7)
maxY<-ceiling(USAbbox$max[2]*1.5)

#get kernel density values
toh.est <- bkde2D(x, bandwidth = c(1, 1), gridsize = c(ras.height, ras.width),
                  range.x=list(c(minX, maxX),c(minY, maxY)))

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

plot(toh_rasterDF, col=parula(100))

#now mask, while still in latlong
toh_rasterDF.crop<-crop(toh_rasterDF, extent(USAshp))
plot(toh_rasterDF.crop)
toh_rasterDF.mask<-mask(toh_rasterDF.crop, USAshp)

plot(toh_rasterDF.mask, col=parula(100))

toh_rasterDF.proj<-projectRaster(toh_rasterDF.mask, human.footprint)
toh_rasterDF.proj

plot(toh_rasterDF.proj)

writeRaster(toh_rasterDF.proj, filename="./Data/Tree_of_Heaven_data/toh_raster_1km.tif",
            overwrite=TRUE)

################################

test.raster<-raster("./Data/Tree_of_Heaven_data/toh_raster_1km.tif")
plot(test.raster, col=parula(100))



#convert to SpatialPointsDataFrame
# coordinates(toh.data.df)<- ~Longitude+Latitude
# unique(toh.data.sub$Datum)
# 
# crs(toh.data.df)<-crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# 
# plot(toh.data.df)

#convert to UTM projection
#toh.data.proj<-spTransform(toh.data.df, crs(human.footprint))

#add count column 
#toh.data.proj@data$count<-1

#plot(toh.data.proj)

#crs(toh.data.proj)

#toh.data.proj.df<-as.data.frame(toh.data.proj, xy=TRUE)

#rasterize
r = raster(toh.data.proj, ncol = 4410, nrow = 2380)

toh.raster = rasterize(toh.data.proj,field="count", r, fun = "count")
toh.raster[is.na(toh.raster[])]<-0

plot(toh.raster, col=parula(100))

#now mask
toh.mask<-mask(toh.raster, USAshp.proj)

plot(toh.mask, col=parula(100))

#save raster
writeRaster(toh.mask, filename=file.path(paste("./Data/Tree_of_Heaven_data/toh_raster",".tif",sep="")), format="GTiff", overwrite=TRUE)

#read in toh raster

tohRaster<-raster(paste("./Data/Tree_of_Heaven_data/toh_raster",".tif",sep=""))

plot(tohRaster)

# # ras.width=4483-2100
# # ras.height=2660+1750
# 
# #get ratio
# myRatio=4410/2380
# 
# 
# ras.width=floor(1000*myRatio)
# ras.height=1000









########################################################################################
#working on other density visualizations


data("meuse.grid")
data("meuse")

#make density map
library(spatstat)

toh.ppp<-as.ppp(toh.data.proj@coords, W=c(xmin=-2294416,xmax=2116344,ymin=696723.9,ymax=3078804))

#get density
K1 <-density(toh.ppp, kernel="gaussian",sigma=100000) # Using the default bandwidth
plot(K1, main=NULL, las=1, col=parula(100))

#convert real-valued pixel image to raster
temp.ras<-raster(K1)

plot(temp.ras)

#define original projection
crs(temp.ras)<-projection(toh.data.proj)

#reproject raster
toh.density.ras.project<-projectRaster(from=temp.ras, to=human.footprint)

#crop and mask raster
toh.density.ras.crop<-crop(toh.density.ras.project, extent(human.footprint))
plot(toh.density.ras.crop)
toh.density.ras.mask<-mask(toh.density.ras.crop, human.footprint)
plot(toh.density.ras.mask, col=parula(100))


######################################################################################
#fooling around with stuff. . .

#try ordinary kriging https://rpubs.com/nabilabd/118172




library(ggplot2)
ggplot() +
  stat_contour(bins = 150, geom = "polygon", aes(x = lon, y = lat, z = count,fill = ..level..), data = BKD_df, alpha = 0.15)+ 
  scale_fill_continuous(name = "density", low = "green", high = "red")

#save raster
coordinates(BKD_df)<- ~lon+lat
BKD_raster<-raster(BKD_df)

r <- raster(extent(BKD_df)) # +1 to increase the area
res(r) <- 0.01 # you can change the resolution here. 0.1 can be about 10x10 km if you are close to the equator.
crs(r) <- projection(vineyards.data) # transfer the coordinate system to the raster


# Now you can add data to the cells in your raster to mark the ones that fall within your polygon.
r[vineyards.data,] <- 1 # this an easy way to subset a raster using a SpatialPolygons object



#now get entire area raster
vineyards.grid<-raster("./Data/US_Vineyard_Data/USvineyards_raster_1km.tif")

plot(vineyards.grid)

vineyards.grid.df<-as.data.frame(vineyards.grid, xy=TRUE)

library(ggplot2)
plot1 <- ggplot(aes(x, y), data=vineyards.df) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# this is clearly gridded over the region of interest
plot2 <- ggplot(aes(x, y),data=vineyards.grid.df) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

#convert vineyards.df to gridded sp
coordinates(vineyards.df)<- ~ x+y
class(vineyards.df)

str(vineyards.df)
gridded(vineyards.df)<-TRUE

diff(vineyards.df@sp@grid@cellsize)

#fit variogram (this is taking a while! Maybe, I should do this on the latlong projection, and then reproject raster to aea, afterwards?!)
library(gstat)
lzn.vgm <- variogram(USvineyards_raster_withNAs~1, data=vineyards.df) # calculates sample variogram values

lzn.fit <- fit.variogram(lzn.vgm, model=vgm("Sph")) # fit model

#plot variogram
plot(lzn.vgm, lzn.fit)


coordinates(vineyards.grid.df) <- ~ x + y # step 3 above
lzn.kriged <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=lzn.fit)




tif = file.path("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predicted_Distribution_MaxEnt/Data/US_Vineyard_Data/USvineyards_raster_withNAs.tif")
x = stars::read_stars(tif)
x[[1]] = round(x[[1]])

#export to points
vineyards.pts<-st_as_sf(x=x, as_points = TRUE, merge = FALSE)

vineyards.sp<- as_Spatial(vineyards.pts)

###################################
library(sp)
coordinates(vineyards.sp) <- ~LONGITUDE + LATITUDE
proj4string(x) <- CRS('+proj=longlat +datum=NAD83')
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
library(rgdal)
aq <- spTransform(x, TA)

cageo <- sp_data('counties.rds')
ca <- spTransform(cageo, TA)
r <- raster(ca)
res(r) <- 10  # 10 km if your CRS's units are in km
g <- as(r, 'SpatialGrid')

library(gstat)
names(vineyards.sp@data)
gs <- gstat(formula=values~1, locations=vineyards.sp)
v <- variogram(gs, width=20)
head(v)
plot(v)

fve <- fit.variogram(v, vgm(85, "Exp", 75, 20))
fve
##   model    psill    range
## 1   Nug 21.96600  0.00000
## 2   Exp 85.52957 72.31404
plot(variogramLine(fve, 400), type='l', ylim=c(0,120))
points(v[,2:3], pch=20, col='red')

#ordinary kriging
k <- gstat(formula=OZDLYAV~1, locations=aq, model=fve)
# predicted values
kp <- predict(k, g)
## [using ordinary kriging]
spplot(kp)



###################################




plot(vineyards.pts)
crs(vineyards.pts)

#save as a shapefile
writeOGR(vineyards.sp, dsn="./Data/US_Vineyard_Data/", layer="USvineyards", driver="ESRI Shapefile",overwrite_layer = TRUE) # this is in geographical projection

#read in shapefile
new.shp<-readOGR(paste("./Data/US_Vineyard_Data/","USvineyards",".shp",sep=""), layer= "USvineyards")

crs(new.shp)
plot(new.shp)

#convert new.shp to df
new.df<-as.data.frame(new.shp)
head(new.df)

new.df<-new.df[,c("coords.x1","coords.x2","US__NA_")]
colnames(new.df)<-c("x","y","value")

new.df<-na.omit(new.df)

library(fields)
data(ozone)

head(ozone)
fit <- Krig(x=c(new.df$x, new.df$y), y=new.df$value)  

summary( fit) # summary of fit 
set.panel( 2,2) 
plot(fit) # four diagnostic plots of fit  
set.panel()
surface( fit, type="C") # look at the surface 

# predict at data
predict( fit)

# predict on a grid ( grid chosen here by defaults)
out<- predict.surface( fit)
surface( out, type="C") # option "C" our favorite


#destination file
dst <- paste("./Data/US_Vineyard_Data/","USvineyards",".tiff",sep="")

new.zfield<- names(new.shp@data)[3]
#try gdal_grid (This is flipping the image, need to sort this out!)
gdalUtils::gdal_grid(src_datasource=paste("./Data/US_Climate_Monthly_Normals/Climate_shapefiles/",nameIn,".shp",sep=""), dst_filename = dst, zfield=new.zfield,outsize=c(4483,2660), a="invdist:power=2.0")

#read in raster and plot (GET THIS FOR ENTIRE USA)
new.raster<-raster(paste("./Data/US_Climate_Monthly_Normals/Climate_shapefiles/",nameIn,".tiff",sep=""))

new.raster.flip<-flip(new.raster, direction = "y")
plot(new.raster.flip, col=parula(100))

#now crop and mask
new.raster.crop<-crop(new.raster.flip, extent(human.footprint))
new.raster.mask<-mask(new.raster.crop, USAshp.proj)

plot(new.raster.mask, col=parula(100))


