# SLF Spread Model Explorer Server

#rm(list=ls())

#load packages
library(htmlwidgets)
library(leaflet)
library(shiny)
library(shinythemes)
library(shinyjs)
library(plyr)
library(dplyr)
library(jsonlite, pos=100)
library(ggplot2)
library(pals)
library(reshape2)
library(maps)
library(raster)
library(sp)
library(gstat)
library(Hmisc)
library(rgeos)
library(sf)

source("./Source/xyToLatLong.R")

#function to threshold raster by cell values
rasterThreshold <- function(r, min, max) {
  rr <- r[]
  rr[rr < min | rr > max] <- NA
  r[] <- rr
  r
}

#################
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
removeCountyList<-c("oklahoma,delaware","iowa,delaware")
counties.sub.2<-subset(counties.sub, ! ID %in% removeCountyList)

#reproject
counties.proj<-st_transform(counties.sub.2, "+proj=longlat +datum=WGS84")
plot(counties.proj)

###################################################################################
#plot with ggplot
#get a list of all counties where SLF were detected

#All Years
SLF_county_occurence<-read.csv("./Data/SLF_County_Level_occurrence_All_years.csv")
# SLF_county_occurence<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/SLF_Shiny_App/Data/SLF_County_Level_occurrence_All_years.csv")

####################################################################################
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

#######################################################################################
#2013
#subset data
SLF_county_occurence_2013<-subset(SLF_county_occurence, Year=="2014")
counties.proj.SLF_2013<-createPoly.SLF(dataIn=SLF_county_occurence_2013,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2013<-createPoly.no.SLF(dataIn=SLF_county_occurence_2013,countyDataIn = counties.sub.2)


#2014
#subset data
SLF_county_occurence_2014<-subset(SLF_county_occurence, Year=="2014")
counties.proj.SLF_2014<-createPoly.SLF(dataIn=SLF_county_occurence_2014,countyDataIn = counties.sub.2)
#plot(counties.proj.SLF_2014)
counties.proj.no.SLF_2014<-createPoly.no.SLF(dataIn=SLF_county_occurence_2014,countyDataIn = counties.sub.2)

#2015
#subset data
SLF_county_occurence_2015<-subset(SLF_county_occurence, Year=="2015")
counties.proj.SLF_2015<-createPoly.SLF(dataIn=SLF_county_occurence_2015,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2015<-createPoly.no.SLF(dataIn=SLF_county_occurence_2015,countyDataIn = counties.sub.2)

#2016
#subset data
SLF_county_occurence_2016<-subset(SLF_county_occurence, Year=="2016")
counties.proj.SLF_2016<-createPoly.SLF(dataIn=SLF_county_occurence_2016,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2016<-createPoly.no.SLF(dataIn=SLF_county_occurence_2016,countyDataIn = counties.sub.2)

#2017
#subset data
SLF_county_occurence_2017<-subset(SLF_county_occurence, Year=="2017")
counties.proj.SLF_2017<-createPoly.SLF(dataIn=SLF_county_occurence_2017,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2017<-createPoly.no.SLF(dataIn=SLF_county_occurence_2017,countyDataIn = counties.sub.2)

#2018
#subset data
SLF_county_occurence_2018<-subset(SLF_county_occurence, Year=="2018")
counties.proj.SLF_2018<-createPoly.SLF(dataIn=SLF_county_occurence_2018,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2018<-createPoly.no.SLF(dataIn=SLF_county_occurence_2018,countyDataIn = counties.sub.2)

#2019
#subset data
SLF_county_occurence_2019<-subset(SLF_county_occurence, Year=="2019")
counties.proj.SLF_2019<-createPoly.SLF(dataIn=SLF_county_occurence_2019,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2019<-createPoly.no.SLF(dataIn=SLF_county_occurence_2019,countyDataIn = counties.sub.2)

#2020
#subset data
SLF_county_occurence_2020<-subset(SLF_county_occurence, Year=="2020")
counties.proj.SLF_2020<-createPoly.SLF(dataIn=SLF_county_occurence_2020,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2020<-createPoly.no.SLF(dataIn=SLF_county_occurence_2020,countyDataIn = counties.sub.2)

#2021
#subset data
SLF_county_occurence_2021<-subset(SLF_county_occurence, Year=="2021")
counties.proj.SLF_2021<-createPoly.SLF(dataIn=SLF_county_occurence_2021,countyDataIn = counties.sub.2)
counties.proj.no.SLF_2021<-createPoly.no.SLF(dataIn=SLF_county_occurence_2021,countyDataIn = counties.sub.2)

######################################################################################
# #2020
# SLF_county_occurence_2020<-read.csv("./Data/SLF_County_Level_occurrence_9-23-2020.csv")
# #add Year column
# SLF_county_occurence_2020$Year<-"2020"
# 
# #2021
# SLF_county_occurence_2021<-read.csv("./Data/SLF_County_Level_occurrence_9-08-2021.csv")
# #add Year column
# SLF_county_occurence_2021$Year<-"2021"
# 
# #combine
# SLF_county_occurence<-rbind(SLF_county_occurence_2020, SLF_county_occurence_2021)
# 
# #get 2020 polygons
# SLF_county_occurence<-SLF_county_occurence_2020
# 
# #create column with "state,county" lowercase
# SLF_county_occurence$ID<-paste(tolower(SLF_county_occurence$State),tolower(SLF_county_occurence$County),sep=",")
# 
# #add factor level to counties.sub.2
# counties.sub.2$SLF_present<-ifelse(counties.sub.2$ID %in% SLF_county_occurence$ID,"Present","Absent")
# #make a factor
# counties.sub.2$SLF_present<-as.factor(counties.sub.2$SLF_present)
# 
# #add factor level to counties.sub.2
# counties$SLF_present<-ifelse(counties$ID %in% SLF_county_occurence$ID,"Present","Absent")
# #make a factor
# counties$SLF_present<-as.factor(counties$SLF_present)
# 
# #subset only counties with SLF present
# counties.SLF_2020<-subset(counties, SLF_present=="Present")
# #now reproject
# counties.proj.SLF_2020<-st_transform(counties.SLF_2020, "+proj=longlat +datum=WGS84")
# 
# #get counties without SLF
# counties.no.SLF_2020<-subset(counties, SLF_present=="Absent")
# 
# #now reproject
# counties.proj.no.SLF_2020<-st_transform(counties.no.SLF_2020, "+proj=longlat +datum=WGS84")
# #plot(counties.proj.no.SLF[2])

########################

# #get 2021 polygons
# SLF_county_occurence<-SLF_county_occurence_2021
# 
# #create column with "state,county" lowercase
# SLF_county_occurence$ID<-paste(tolower(SLF_county_occurence$State),tolower(SLF_county_occurence$County),sep=",")
# 
# #add factor level to counties.sub.2
# counties.sub.2$SLF_present<-ifelse(counties.sub.2$ID %in% SLF_county_occurence$ID,"Present","Absent")
# #make a factor
# counties.sub.2$SLF_present<-as.factor(counties.sub.2$SLF_present)
# 
# #add factor level to counties.sub.2
# counties$SLF_present<-ifelse(counties$ID %in% SLF_county_occurence$ID,"Present","Absent")
# #make a factor
# counties$SLF_present<-as.factor(counties$SLF_present)
# 
# #subset only counties with SLF present
# counties.SLF_2021<-subset(counties, SLF_present=="Present")
# #now reproject
# counties.proj.SLF_2021<-st_transform(counties.SLF_2021, "+proj=longlat +datum=WGS84")
# 
# #get counties without SLF
# counties.no.SLF_2021<-subset(counties, SLF_present=="Absent")
# 
# #now reproject
# counties.proj.no.SLF_2021<-st_transform(counties.no.SLF_2021, "+proj=longlat +datum=WGS84")
# #plot(counties.proj.no.SLF[2])


################
#get MAXENT layer

#MAXENT surface details

#read in MaxEnt raster surface
maxEnt_surface<-raster("./Data/MaxEnt_grayscale_4km.tiff", values=TRUE)

#make spatial polygons
countysp <- as_Spatial(counties.sub.2, IDs=counties.sub.2$GEOID)
#make data.frame
counties.df <- fortify(countysp)

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

################
#load raster data

folderList<-list.dirs(path="./Data/Simulation_Rasters",full.names=TRUE)[-1]

#make one giant raster stack
master.raster.list<-list()
for(i in 1:length(folderList)){
  
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

#create probability range data.frame
prob.df<-data.frame(Probability=seq(0,1,by=0.01))

#create lists for dropdown menus
growthRateList<-c("0.25","0.5","1","1.5")

#humanPopsList
humanPopsList<-c("0","3","5","7","10")

#create movement type list
moveTypeList<-c("Random","Non-Random")


##################################################################################################################
server <- function(input, output,session) {
  
    ####################################################################
    #Basemap 
  
    #get coordinates of all park units
    minLon=minLong
    maxLon=maxLong
    minLat=minLat
    maxLat=maxLat
    meanLon=mean(all.cells.df$Longitude, na.rm=TRUE)
    meanLat=mean(all.cells.df$Latitude, na.rm=TRUE)
    
    berksCounty_Lon = -75.927356
    berksCounty_Lat = 40.435244
    
    
    #output the map
    map <- leaflet() %>%
      addTiles(attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')%>%
      fitBounds(minLon, minLat,maxLon, maxLat)%>%
      setView(lng=berksCounty_Lon, lat=berksCounty_Lat, zoom=6)%>%
      addScaleBar(position = c("bottomleft"))
    
    #render leaflet map
    output$StudyAreaMap = renderLeaflet(map)
    
    observe({
      leafletProxy("StudyAreaMap") %>%
        clearTiles()%>%
        addTiles(group="Satellite", urlTemplate="https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiemFjaGxhZGluIiwiYSI6ImNrYnBvNGJjMTJiZ2YydnBmbDFxcmI5dW8ifQ.YwjylSgmx7Np-uY1WGjmdQ", options=tileOptions(minZoom=5,maxZoom=20, zIndex=0))%>%
        addTiles(group="Streets", urlTemplate="https://api.mapbox.com/styles/v1/mapbox/streets-v11/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiemFjaGxhZGluIiwiYSI6ImNrYnBvNGJjMTJiZ2YydnBmbDFxcmI5dW8ifQ.YwjylSgmx7Np-uY1WGjmdQ", options=tileOptions(minZoom=5,maxZoom=20, zIndex=0))%>%
        addTiles(group="Light", urlTemplate="https://api.mapbox.com/styles/v1/mapbox/light-v10/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiemFjaGxhZGluIiwiYSI6ImNrYnBvNGJjMTJiZ2YydnBmbDFxcmI5dW8ifQ.YwjylSgmx7Np-uY1WGjmdQ", options=tileOptions(minZoom=5,maxZoom=20, zIndex=0)) %>%
        addTiles(group="Dark", urlTemplate="https://api.mapbox.com/styles/v1/mapbox/dark-v10/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiemFjaGxhZGluIiwiYSI6ImNrYnBvNGJjMTJiZ2YydnBmbDFxcmI5dW8ifQ.YwjylSgmx7Np-uY1WGjmdQ",options=tileOptions(minZoom=5,maxZoom=20, zIndex=0)) %>%
        addLayersControl(baseGroups=c("Dark","Light","Streets","Satellite"), options=layersControlOptions(collapsed=F,autoZIndex=FALSE))
    })
    
observeEvent(input$YearSelect,{
  
  observe({
    #dynamically define counties.proj
    counties.proj.no.SLF<-reactive({
      
   if(as.character(input$YearSelect)=="2013"){
        counties.proj.no.SLF_2013
    } else{
      if(as.character(input$YearSelect)=="2014"){
        counties.proj.no.SLF_2014
      } else{
        if(as.character(input$YearSelect)=="2015"){
          counties.proj.no.SLF_2015
        } else{
          if(as.character(input$YearSelect)=="2016"){
            counties.proj.no.SLF_2016
          } else{
            if(as.character(input$YearSelect)=="2017"){
              counties.proj.no.SLF_2017
            } else{
              if(as.character(input$YearSelect)=="2018"){
                counties.proj.no.SLF_2018
              } else{
                if(as.character(input$YearSelect)=="2019"){
                  counties.proj.no.SLF_2019
                } else{
                  if(as.character(input$YearSelect)=="2020"){
                    counties.proj.no.SLF_2020
                  } else{
                    if(as.character(input$YearSelect) %in% c("2021","2022","2023","2024","2025")){
                      counties.proj.no.SLF_2021
                    }
                  }}}}}}}}
    })

      
    #dynamically define counties.proj
    counties.proj.SLF<-reactive({
      
  if(as.character(input$YearSelect)=="2013"){
        counties.proj.SLF_2013
    } else{
      if(as.character(input$YearSelect)=="2014"){
        counties.proj.SLF_2014
      } else{
        if(as.character(input$YearSelect)=="2015"){
          counties.proj.SLF_2015
        } else{
          if(as.character(input$YearSelect)=="2016"){
            counties.proj.SLF_2016
          } else{
            if(as.character(input$YearSelect)=="2017"){
              counties.proj.SLF_2017
            } else{
              if(as.character(input$YearSelect)=="2018"){
                counties.proj.SLF_2018
              } else{
                if(as.character(input$YearSelect)=="2019"){
                  counties.proj.SLF_2019
                } else{
                  if(as.character(input$YearSelect)=="2020"){
                    counties.proj.SLF_2020
                  } else{
                    if(as.character(input$YearSelect) %in% c("2021","2022","2023","2024","2025")){
                      counties.proj.SLF_2021
                    }
                  }}}}}}}}
    })
    

    #set up palSLF for legend
    palSLF <- try(colorFactor(
      c("red"),
      domain = counties.proj.SLF()$SLF_present,
      na.color = "transparent"
    ))
    
    #set up palLegend for raster legend
    palLegend <- try(colorNumeric(
      #palette = parula(100),
      palette = kovesi.linear_bmy_10_95_c78(100),
      domain = rev(c(0,1)),
      reverse=TRUE,
      na.color = "transparent"
    ))
    
  #turn off SLF counties and conuty boundaries and update map
        if(isTRUE(input$countyBoundaries) & isTRUE(input$SLFsurvey)){
          leafletProxy("StudyAreaMap") %>%
            clearShapes() %>%
            clearControls()%>%
            addPolygons(data=counties.proj, fillColor = "transparent", color="gray",opacity=0.5,weight=0.5) %>%
            addPolygons(data=counties.proj.SLF(), fillColor = "red", color="gray",opacity=0.4,weight=0.5, fillOpacity = 0.4)%>%
            addLegend("bottomright", pal = palLegend, values = c(0,1),opacity=0.9,
                      title = "Probability of SLF",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
            addLegend("bottomright", pal = palSLF, values=unique(counties.proj.SLF()$SLF_Present), opacity=0.5,
                      title = "SLF Counties",layerId="SLF_presence_legend")
        } else {
          
          if(isFALSE(input$countyBoundaries) & isTRUE(input$SLFsurvey)){
            leafletProxy("StudyAreaMap",data=counties.proj.SLF()) %>%
              clearShapes()%>%
              clearControls()%>%
              addPolygons(fillColor = "red", color="gray",opacity=0.4,weight=0.5, fillOpacity = 0.4)%>%
              addLegend("bottomright", pal = palLegend, values = c(0,1),opacity=0.9,
                        title = "Probability of SLF",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
              addLegend("bottomright", pal = palSLF, values=unique(counties.proj.SLF()$SLF_Present), opacity=0.5,
                        title = "SLF Counties",layerId="SLF_presence_legend")
          } else{
            if(isTRUE(input$countyBoundaries) & isFALSE(input$SLFsurvey)){
              leafletProxy("StudyAreaMap",data=counties.proj) %>%
                clearShapes()%>%
                clearControls()%>%
                addLegend("bottomright", pal = palLegend, values = c(0,1),opacity=0.9,
                          title = "Probability of SLF",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
                addPolygons(fillColor = "transparent", color="gray",opacity=0.5,weight=0.5)
            } else{
              leafletProxy("StudyAreaMap")%>%
                clearShapes()%>%
                clearControls()%>%
                addLegend("bottomright", pal = palLegend, values = c(0,1),opacity=0.9,
                          title = "Probability of SLF",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
            }}}
      })
            
})        
          
          
########################################################################################################
    #Present Raw data (Occupancy or Abundance)
      
  observe({
             
    #dynamically define counties.proj
    counties.proj.no.SLF<-reactive({
      
      if(as.character(input$YearSelect)=="2013"){
        counties.proj.no.SLF_2013
      } else{
        if(as.character(input$YearSelect)=="2014"){
          counties.proj.no.SLF_2014
        } else{
          if(as.character(input$YearSelect)=="2015"){
            counties.proj.no.SLF_2015
          } else{
            if(as.character(input$YearSelect)=="2016"){
              counties.proj.no.SLF_2016
            } else{
              if(as.character(input$YearSelect)=="2017"){
                counties.proj.no.SLF_2017
              } else{
                if(as.character(input$YearSelect)=="2018"){
                  counties.proj.no.SLF_2018
                } else{
                  if(as.character(input$YearSelect)=="2019"){
                    counties.proj.no.SLF_2019
                  } else{
                    if(as.character(input$YearSelect)=="2020"){
                      counties.proj.no.SLF_2020
                    } else{
                      if(as.character(input$YearSelect) %in% c("2021","2022","2023","2024","2025")){
                        counties.proj.no.SLF_2021
                      }
                    }}}}}}}}
    })
    
    
    #dynamically define counties.proj
    counties.proj.SLF<-reactive({
      
      if(as.character(input$YearSelect)=="2013"){
        counties.proj.SLF_2013
      } else{
        if(as.character(input$YearSelect)=="2014"){
          counties.proj.SLF_2014
        } else{
          if(as.character(input$YearSelect)=="2015"){
            counties.proj.SLF_2015
          } else{
            if(as.character(input$YearSelect)=="2016"){
              counties.proj.SLF_2016
            } else{
              if(as.character(input$YearSelect)=="2017"){
                counties.proj.SLF_2017
              } else{
                if(as.character(input$YearSelect)=="2018"){
                  counties.proj.SLF_2018
                } else{
                  if(as.character(input$YearSelect)=="2019"){
                    counties.proj.SLF_2019
                  } else{
                    if(as.character(input$YearSelect)=="2020"){
                      counties.proj.SLF_2020
                    } else{
                      if(as.character(input$YearSelect) %in% c("2021","2022","2023","2024","2025")){
                        counties.proj.SLF_2021
                      }
                    }}}}}}}}
    })
    
        
              #get growth rate
              new.growth.rate<-reactive({
                as.character(input$GrowthRate)
              })
              
              #get max years
              new.year<-reactive({
                switch(as.character(input$YearSelect),
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
              })
              
              #new movement value convert from input$MovementType
              new.movement.value<-reactive({
                switch(as.character(input$MovementType),
                       "Random" = "0.99",
                       "Non-Random" = "0.01"
                       )
              })
              
              new.human.movement<-reactive({
                switch(as.character(input$HumanMediated),
                       "None" = "0",
                       "Low" = "3",
                       "Medium" = "5",
                       "High" = "7",
                       "Very High"= "10"
                )
              })
              
              
              
              new.raster.name<-reactive({
                paste("HumanPops",new.human.movement(), "Years_13","GrowthRate",new.growth.rate(), "q2mean",new.movement.value(),"q2sd_0.05_Binary","Year",new.year(),sep="_")
              })
              
              #subset raster from raster stack
              new.raster<-reactive({
                
                my.raster<-master.raster.stack[[new.raster.name()]]
                
                #threshold by probability
                thresh.raster<-rasterThreshold(r=my.raster, min=min(input$ProbSelect), max=max(input$ProbSelect))
                
                raster.cells.df<-data.frame(rasterToPoints(na.omit(thresh.raster)))
                colnames(raster.cells.df)<-c("x","y","z")

                #add Lat/Long
                #now apply these functions to slf.simu data

                #get raster min_X, max_X, min_Y, and max_Y
                raster.cells.df$Longitude<-coord2long(inX=raster.cells.df$x,minX = min_X, maxX = max_X, minlong = minLong, maxlong = maxLong)
                raster.cells.df$Latitude<-coord2lat(inY=raster.cells.df$y, minY = min_Y, maxY = max_Y, minlat = minLat, maxlat = maxLat)

                new.raster.2<-rasterFromXYZ(xyz=data.frame(raster.cells.df$Longitude, raster.cells.df$Latitude, raster.cells.df$z),
                                            digits=3, crs="+proj=longlat +datum=WGS84")
               new.raster.2
               
               # projection(thresh.raster)<-"+proj=merc +datum=WGS84"
               # new.raster.2<-raster::projectRaster(thresh.raster, crs="+proj=longlat +datum=WGS84")
               # 
               # new.raster.2
               
              })
              
              showModal(modalDialog(title="Computing raster information. . .",size="s",footer = NULL, easyClose = TRUE))
              
              
              ############################################################################
              #render updated rasters in map
              palRas <- try(colorNumeric(
                #palette = parula(100),
                palette = kovesi.linear_bmy_10_95_c78(100),
                domain = c(0,1),
                reverse=FALSE,
                na.color = "transparent"
              ))
              
              
              leafletProxy("StudyAreaMap") %>%
                clearImages()%>%
                #clearControls()%>%
                addRasterImage(new.raster(), colors = palRas, opacity = 0.9,project=FALSE)
                # addLegend("bottomright", pal = palLegend, values = c(0,1),opacity=0.9,
                #           title = "Probability of SLF",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
              
              ############################################################################
              #get model performance metrics
                #get number of all US Counties with predicted SLF
                USpolygonList<-raster::extract(new.raster(), counties.proj)
                count.all.predicted.polygons<-length(USpolygonList)

                #get number of correctly predicted counties
                polygonList<-raster::extract(new.raster(), counties.proj.SLF())
                
                 #count of all SLF polygons (observed)
                count.all.slf.polygons<-length(polygonList)
                #count of counties with SLF correctly predicted (accurate)
                count.slf.polygons<-length(polygonList[!unlist(lapply(polygonList, is.null))])
                #count of counties with SLF missing (false negatives)
                count.null.polygonList<-length(polygonList[unlist(lapply(polygonList, is.null))])
                #count of counties where SLF do not occur, but were predicted (false positives)
                
                #proportion of cells within SLF counties
                pct.accurate<-round(count.slf.polygons/count.all.slf.polygons*100,0)
                
                #false negatives
                false.negatives<-100-pct.accurate
                
                #get count of polygons (with No SLF present, where predictions were made by model)
                false.county.predictions<-raster::extract(new.raster(), counties.proj.no.SLF())
                

                count.false.counties<-length(false.county.predictions[!unlist(lapply(false.county.predictions, is.null))])
                
                #false positives (intersections of new.raster.2 with mask.zero.rater.inverse)
                false.positives<-round(count.false.counties/count.all.predicted.polygons*100,0)
                
                #compute F1 score for model performance
                
                #precision = true positives/true+false positives
                mod.precision<-round(count.slf.polygons/(count.slf.polygons+count.false.counties),3)
                
                #recall = true positives/ all true positives
                mod.recall<-round(count.slf.polygons/count.all.slf.polygons,3)
                
                #F1 = 2*((precision*recall)/(precision+recall)); F1=1 is good performance, 0 is bad
                mod.F1<-round(2*((mod.precision*mod.recall)/(mod.precision+mod.recall)),3)
                
                #display model performance metrics
                output$percentAccurate<-renderText({ 
                              paste("Accuracy: ",pct.accurate,"%",sep="")
                           })
                  
                output$falseNegative<-renderText({ 
                               paste("False Negatives: ",false.negatives,"%",sep="")
                             })
                  
                output$falsePositive<-renderText({ 
                              paste("False Positives: ",false.positives,"%",sep="")
                             })

                output$modPrecision<-renderText({ 
                  paste("Precision: ",mod.precision, sep="")
                })
                
                output$modRecall<-renderText({ 
                  paste("Recall: ",mod.recall, sep="")
                })
                
                output$modF1<-renderText({ 
                  paste("F1: ",mod.F1, sep="")
                })
                
              
                removeModal()
              })
        #             })
        #           })
        #         })
        #       })
        #     })
        #   })
        # })
}

