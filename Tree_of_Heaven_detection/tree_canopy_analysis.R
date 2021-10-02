#Canopy imagery analysis in R

rm(list=ls())

setwd("/Users/zach/Dropbox (ZachTeam)/Bioverse_Labs/Projects/Canopy_analysis")

#load packages
library(itcSegment)
library(raster)
library(sp)
library(rgdal)
library(tiff)
library(raster)
library(ggplot2)
library(gridGraphics)
library(magick)
library(gridExtra)
library(rgeos)
library(knitr)
library(imager)
library(spatialEco)

##################################################################################################
new.img<-image_read("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Canopy_analysis/Data/raymond-gehman-an-aerial-view-of-a-road-passing-through-a-thick-forest_a-l-3996134-4990703.jpg")
plot(new.img)

img.ras<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Canopy_analysis/Data/raymond-gehman-an-aerial-view-of-a-road-passing-through-a-thick-forest_a-l-3996134-4990703.jpg")

plot(img.ras)

img.ras[img.ras > 140] <- NA

plot(img.ras)

tree_crowns<-itcIMG(img.ras, epsg=32632,searchWinSize = 33, TRESHSeed = 0.5,
                    TRESHCrown = 0.1, DIST = 70, th = 50, ischm = FALSE)


plot(new.img)
plot(tree_crowns, border="red", add=TRUE)











