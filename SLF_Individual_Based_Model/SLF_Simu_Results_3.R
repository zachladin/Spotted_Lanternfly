#Script for visualizing simulation results for SLF spread models

#clear environment
rm(list=ls())

#clear plots
dev.off()

#set working directory
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/")

#load packages
library(ggplot2)
library(ggthemes)
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
library(pals)
library(plyr)
library(geosphere)
library(ggmap)
library(dplyr)
library(scales)


#set my Google API key
myAPIkey = "YOUR KEY"
register_google(myAPIkey)

#source functions
source("./R_Models/R_Source/summaryFunction.R")
source("./R_Models/R_Source/processSimulationResults.R")
source("./R_Models/R_Source/xyToLatLong.R")
source("./R_Models/R_Source/getCummulativeSums.R")
source("./R_Models/R_Source/formatRasters.R")

##############################################################################
#processing simulation results

#set up county polygons and basemaps for making figures

#crop raster to mid-Atlantic region
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

#check projection
st_crs(counties)

#transform projection of counties
#counties.proj<-st_transform(counties, 3857)

# #see new projection
# st_crs(counties.proj)

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

#plot counties.sub.2
plot(counties.sub.2)

#plot with ggplot
#2020
SLF_county_occurence_2020<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/SLF_County_Level_occurrence_9-23-2020.csv")
#add Year column
SLF_county_occurence_2020$Year<-"2020"

#2021
SLF_county_occurence_2021<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/SLF_County_Level_occurrence_9-08-2021.csv")
#add Year column
SLF_county_occurence_2021$Year<-"2021"

#combine
SLF_county_occurence<-rbind(SLF_county_occurence_2020, SLF_county_occurence_2021)

#create column with "state,county" lowercase
SLF_county_occurence$ID<-paste(tolower(SLF_county_occurence$State),tolower(SLF_county_occurence$County),sep=",")


#add factor level to counties.sub.2
counties.sub.2$SLF_present<-ifelse(counties.sub.2$ID %in% SLF_county_occurence$ID,"Present","Absent")
#make a factor
counties.sub.2$SLF_present<-as.factor(counties.sub.2$SLF_present)

#plot with ggplot
ggplot()+
  geom_sf(data=counties.sub.2, aes(fill=SLF_present),color=alpha("gray20",0.4), inherit.aes = FALSE)+
  scale_fill_manual(values=c("gray80","tomato"))+
  facet_wrap(~Year)
##################################################################################################
#MAXENT surface details

#read in MaxEnt raster surface
maxEnt_surface<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_grayscale_4km.tiff", values=TRUE)

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
dim(maxEnt_surface)
plot(maxEnt_surface)

min_X<-0
max_X<-dim(maxEnt_surface)[2]
min_Y<-0
max_Y<-dim(maxEnt_surface)[1]

#get list of all possible (universe)
new.matrix<-matrix(0,nrow=nrow(maxEnt_surface), ncol=ncol(maxEnt_surface))
new.raster<-raster(nrows=nrow(new.matrix), ncols=ncol(new.matrix),xmn=0,ymn=0,xmx=ncol(new.matrix),ymx=nrow(new.matrix))
values(new.raster)<-0
plot(new.raster)
plot(maxEnt_surface, alpha=0.2, add=TRUE)
range(values(maxEnt_surface))
plot(maxEnt_surface)

#create a maxEnt_surface with only 0s and NAs
maxEnt_surface_zeros<-maxEnt_surface
maxEnt_surface_zeros[maxEnt_surface_zeros!=58137]<-0
maxEnt_surface_zeros[is.na(maxEnt_surface_zeros)]<-0
maxEnt_surface_zeros[maxEnt_surface_zeros==58137]<-NA
plot(maxEnt_surface_zeros)

unique(values(maxEnt_surface_zeros))

length(na.omit(maxEnt_surface_zeros))

###############################################################################
#create data.frame with list of all cell XY cords
all.cells.df<-data.frame(floor(rasterToPoints(na.omit(maxEnt_surface_zeros))))[,-3]

#add Lat/Long
#now apply these functions to slf.simu data
all.cells.df$Longitude<-coord2long(inX=all.cells.df$x,minX = min_X, maxX = max_X, minlong = minLong, maxlong = maxLong)
all.cells.df$Latitude<-coord2lat(inY=all.cells.df$y, minY = min_Y, maxY = max_Y, minlat = minLat, maxlat = maxLat)

#convert colnames
names(all.cells.df)[names(all.cells.df)=="x"]<-"X"
names(all.cells.df)[names(all.cells.df)=="y"]<-"Y"

###############################################################################
# #test function
# myfileDir<-"/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/Simulations_13_yr_growthRate_0.5_q2_mean_0.99_q2_sd_0.05"
# 
# processSimulationResults(fileDir=myfileDir, minX = min_X, maxX=max_X, minY=min_Y, maxY=max_Y, minlong=minLong, maxlong=maxLong,minlat = minLat, maxlat=maxLat, densityDependent=FALSE)

#with Density Dependence

myfileDir<-"/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/Simulations_Density_Dependent_13_yr_growthRate_0.5_q2_mean_0.99_q2_sd_0.05"

processSimulationResults(fileDir=myfileDir, minX = min_X, maxX=max_X, minY=min_Y, maxY=max_Y, minlong=minLong, maxlong=maxLong,minlat = minLat, maxlat=maxLat, densityDependent=TRUE)

###############################################################################
#read in compiled results

#read in simu.data
#simu.data<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/Simulations_13_yr_growthRate_0.5_q2_mean_0.99_q2_sd_0.05_Compiled/Combined_counts_Simulations_13_yr_growthRate_0.5_q2_mean_0.99_q2_sd_0.05.csv")

#wih Density Dependence
simu.data<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/HumanPops_0_Years_13_GrowthRate_0.5_q2_mean_0.99_q2_sd_0.05_Compiled/Combined_counts_HumanPops_0_Years_13_GrowthRate_0.5_q2mean_0.99_q2sd_0.05.csv")


#############################################################################################################
#use function to format and save raster stacks
  
formatRasters(dataIn=simu.data, folderName="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output_rasters/HumanPops_0_Years_13_GrowthRate_0.5_q2mean_0.99_q2sd_0.05")

#############################################################################################################
#save raster stacks (by cummulative years)

load("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output_rasters/13_yr_growthRate_1_q2_mean_0.99_q2_sd_0.05_rasters.Rdata")


#get summary of values of total visits in raster
raster.test<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output_rasters/13_yr_growthRate_1_q2_mean_0.99_q2_sd_0.05/13_yr_growthRate_1_q2_mean_0.99_q2_sd_0.05_Year_13.tif")

plot(raster.test, col=parula(100))

raster.test.binary<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output_rasters/13_yr_growthRate_1_q2_mean_0.99_q2_sd_0.05/13_yr_growthRate_1_q2_mean_0.99_q2_sd_0.05_Binary_Year_13.tif")

plot(raster.test.binary, col=parula(100))


#function to separate and save rasters by Year 0 to 13

processRasters<-function(dataIn, filename){
  
  new.raster.stack<-dataIn
  

  #raster subset by Years
  raster.stack.Y0<-stack(save.rasters[1:1000])
  raster.stack.Y0.mean<-mean(raster.stack.Y0)
  raster.stack.Y0.mean[raster.stack.Y0.mean[]==0] <- NA 
  plot(raster.stack.Y0.mean, col=parula(100))
  
  #Year 1
  raster.stack.Y1<-stack(save.rasters[1001:2000])
  raster.stack.Y1.mean<-mean(raster.stack.Y1)
  raster.stack.Y1.mean[raster.stack.Y1.mean[]==0] <- NA 
  plot(raster.stack.Y1.mean, col=parula(100))
  
  #Year 2
  raster.stack.Y2<-stack(save.rasters[2001:3000])
  raster.stack.Y2.mean<-mean(raster.stack.Y2)
  raster.stack.Y2.mean[raster.stack.Y2.mean[]==0] <- NA 
  plot(raster.stack.Y2.mean, col=parula(100))
  
  #Year 3
  raster.stack.Y3<-stack(save.rasters[3001:4000])
  raster.stack.Y3.mean<-mean(raster.stack.Y3)
  raster.stack.Y3.mean[raster.stack.Y3.mean[]==0] <- NA 
  plot(raster.stack.Y3.mean, col=parula(100))
  
  #Year 4
  raster.stack.Y4<-stack(save.rasters[4001:5000])
  raster.stack.Y4.mean<-mean(raster.stack.Y4)
  raster.stack.Y4.mean[raster.stack.Y4.mean[]==0] <- NA 
  plot(raster.stack.Y4.mean, col=parula(100))
  
  #Year 5
  raster.stack.Y5<-stack(save.rasters[5001:6000])
  raster.stack.Y5.mean<-mean(raster.stack.Y5)
  raster.stack.Y5.mean[raster.stack.Y5.mean[]==0] <- NA 
  plot(raster.stack.Y5.mean, col=parula(100))
  
  #Year 6
  raster.stack.Y6<-stack(save.rasters[6001:7000])
  raster.stack.Y6.mean<-mean(raster.stack.Y6)
  raster.stack.Y6.mean[raster.stack.Y6.mean[]==0] <- NA 
  plot(raster.stack.Y6.mean, col=parula(100))
  
  #Year 7
  raster.stack.Y7<-stack(save.rasters[7001:8000])
  raster.stack.Y7.mean<-mean(raster.stack.Y7)
  raster.stack.Y7.mean[raster.stack.Y7.mean[]==0] <- NA 
  plot(raster.stack.Y7.mean, col=parula(100))
  
  #Year 8
  raster.stack.Y8<-stack(save.rasters[8001:9000])
  raster.stack.Y8.mean<-mean(raster.stack.Y8)
  raster.stack.Y8.mean[raster.stack.Y8.mean[]==0] <- NA 
  plot(raster.stack.Y8.mean, col=parula(100))
  
  #Year 9
  raster.stack.Y9<-stack(save.rasters[9001:10000])
  raster.stack.Y9.mean<-mean(raster.stack.Y9)
  raster.stack.Y9.mean[raster.stack.Y9.mean[]==0] <- NA 
  plot(raster.stack.Y9.mean, col=parula(100))
  
  #Year 10
  raster.stack.Y10<-stack(save.rasters[10001:11000])
  raster.stack.Y10.mean<-mean(raster.stack.Y10)
  raster.stack.Y10.mean[raster.stack.Y10.mean[]==0] <- NA 
  plot(raster.stack.Y10.mean, col=parula(100))
  
  #Year 11
  raster.stack.Y11<-stack(save.rasters[11001:12000])
  raster.stack.Y11.mean<-mean(raster.stack.Y11)
  raster.stack.Y11.mean[raster.stack.Y11.mean[]==0] <- NA 
  plot(raster.stack.Y11.mean, col=parula(100))
  
  #Year 12
  raster.stack.Y12<-stack(save.rasters[12001:13000])
  raster.stack.Y12.mean<-mean(raster.stack.Y12)
  raster.stack.Y12.mean[raster.stack.Y12.mean[]==0] <- NA 
  plot(raster.stack.Y12.mean, col=parula(100))
  
  #Year 13
  raster.stack.Y13<-stack(save.rasters[13001:14000])
  raster.stack.Y13.mean<-mean(raster.stack.Y13)
  raster.stack.Y13.mean[raster.stack.Y13.mean[]==0] <- NA 
  plot(raster.stack.Y13.mean, col=parula(100))
  
  #compile mean rasters
  rasters.compile<-stack(raster.stack.Y0.mean,raster.stack.Y1.mean, raster.stack.Y2.mean, raster.stack.Y3.mean, raster.stack.Y4.mean, raster.stack.Y5.mean,raster.stack.Y6.mean, raster.stack.Y7.mean, raster.stack.Y8.mean, raster.stack.Y9.mean, raster.stack.Y10.mean,raster.stack.Y11.mean, raster.stack.Y12.mean, raster.stack.Y13.mean)
  
  names(rasters.compile)<-c("Year_0","Year_1","Year_2","Year_3","Year_4","Year_5","Year_6","Year_7","Year_8","Year_9","Year_10","Year_11","Year_12","Year_13")
  
  
  return(rasters.compile)
}


plot(rasters.compile,col=parula(100))

range(values(raster.stack.Y13.mean))
############################################################################################################################
#convert rasters to be plotted in ggplot

raster.compile.df.1<-as.data.frame(rasters.compile,xy=TRUE)

raster.compile.df.2<-reshape2::melt(raster.compile.df.1,id.vars=c("x","y"))

#round cell coords
raster.compile.df.2$x<-floor(raster.compile.df.2$x)
raster.compile.df.2$y<-floor(raster.compile.df.2$y)

#get Lat/Long coords
raster.compile.df.2$Longitude<-coord2long(inX=raster.compile.df.2$x, minX=min_X, maxX=max_X, minlong=minLong, maxlong=maxLong)
raster.compile.df.2$Latitude<-coord2lat(inY=raster.compile.df.2$y, minY=min_Y, maxY=max_Y, minlat=minLat, maxlat=maxLat)

#plot with ggplot
#test with plotting basemap and county polygons
SLF_observed<-ggplot() + 
  geom_point(data=raster.compile.df.2, aes(x=Longitude,y=Latitude, color=value), size=0.5)+
  scale_color_gradientn(colors=parula(100))+
  geom_sf(data=counties.sub.2, fill="transparent",color=alpha("gray50",0.4), inherit.aes = FALSE)+
  # coord_sf(crs = st_crs(3857),ylim=c(county.bbox[2]*0.99, county.bbox[4]*1.01), 
  #          xlim=c(county.bbox[1]*1.01, county.bbox[3]*0.99))+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"),
        legend.position = c(0.85,0.2),
        legend.background = element_blank())+
  labs(x="Longitude",y="Latitude")

SLF_observed

#facet over Year
SLF_observed_facet<-SLF_observed+facet_wrap(~Year)+theme(legend.position="right")
SLF_observed_facet

############################################################################################################################

all.raster.means<-mean(raster.stack)
plot(all.raster.means, col=parula(100))

#get summary of visted or not (binary 1 or 0)
raster.stack.binary<-stack(save.rasters.binary)

all.raster.binary.means<-mean(raster.stack.binary)
plot(all.raster.binary.means, col=parula(100))


###########################################################################################################3
#Model evaluation using survey data from PA

#plot observed SLF survey data (from PA)

SLF_data<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/SLF_survey_data/SLF_PA_survey_data.csv")

#simplify data
SLF_data.sub<-SLF_data[,c("Longitude","Latitude", "County","State","Location.ID","SurveyLocationKey","Host","Service.Date","Receive.Date","Complete.Date","Count","Life.Stage.Type","Band.Count")]

#create total count column (Band.Count+Count)
SLF_data.sub$Total.Count<-rowSums(SLF_data.sub[,c("Band.Count","Count")],na.rm=TRUE)

#create year column
SLF_data.sub$Year<-substr(SLF_data.sub$Service.Date, 1, 4)
unique(SLF_data.sub$Year)
SLF_data.sub$Year<-as.factor(SLF_data.sub$Year)

SLF_data.naYear<-subset(SLF_data.sub, is.na(Year))

#remove rows with Year is.na()
SLF_data.sub.2<-subset(SLF_data.sub, ! is.na(Year)) 

#double-check with SLF survey data folks from PA if NAs were, in fact, surveyed?
#I have confirmed that NAs are completed surveys with counts = 0
#SLF_data.sub.3<-subset(SLF_data.sub.2, c(! is.na(Count) & ! is.na(Band.Count)))

#add Presence column
SLF_data.sub.2$Presence<-ifelse(SLF_data.sub.2$Total.Count>0,1,0)

#remove location outliers
SLF_data.sub.2<-subset(SLF_data.sub.2, c(Longitude > -82 & Longitude < -74))

SLF.data.out<-unique(SLF_data.sub.2)

#test lat2XY
SLF.data.out$X<-long2coord(longIn=SLF.data.out$Longitude,minX=min_X, maxX=max_X, minlong=minLong, maxlong=maxLong)
SLF.data.out$Y<-lat2coord(latIn=SLF.data.out$Latitude,minY=min_Y, maxY=max_Y, minlat=minLat, maxlat=maxLat)

###########################################################################################################
#get cell presence absence from simu.data

#get yearListSurveys from survey data
yearListSurveys<-sort(unique(as.character(SLF.data.out$Year)))[-length(unique(as.character(SLF.data.out$Year)))]

#add Presence column (0 or 1)
simu.data$Presence<-ifelse(simu.data$Total_Visits>0,1,0)

#get yearList from simulation results
yearList<-unique(simu.data$Year)

#remove year 0 through n.remove
n.remove<- -3
yearListSimu<-yearList[-1:n.remove]

simu.data.save<-list()
for(i in 1:length(yearListSurveys)){
  
  #subset year data
  yearData<-subset(simu.data, Year==yearListSimu[i])
  print(paste("Year",yearListSimu[i], sep=" "))
  
  simulationList<-unique(yearData$Simulation_ID)
  
  data.merge.save<-list()
  #set this later to length of simulationList?
  for(j in 1:1000){
    
    print(paste("Year",yearListSimu[i], "Simulation", simulationList[j],sep=" "))
    
    year.simu.data<-subset(yearData, Simulation_ID==simulationList[j])
    
    #merge with all cells and fill in empty cells with 0s
    data.merge<-merge(year.simu.data, all.cells.df, by=c("X","Y","Latitude","Longitude"), all.y=TRUE)
    
    #fill in columns
    data.merge$Total_Visits<-ifelse(is.na(data.merge$Total_Visits), 0, data.merge$Total_Visits)
    data.merge$Year<-ifelse(is.na(data.merge$Year), year.simu.data$Year, data.merge$Year)
    data.merge$Simulation_ID<-ifelse(is.na(data.merge$Simulation_ID), year.simu.data$Simulation_ID, data.merge$Simulation_ID)
    data.merge$YearDate<-ifelse(is.na(data.merge$YearDate), year.simu.data$YearDate, data.merge$YearDate)
    data.merge$Presence<-ifelse(data.merge$Total_Visits>0,1,0)
    
    #convert SLF data subset by year to XY coords (have to figure out year since SLF survey data is from 2014-2019)

    SLF_data.year<-subset(SLF.data.out, Year==yearListSurveys[i])
    
    #aggregate and max count per cell per year
    SLF_data.year.agg<-aggregate(Presence~X+Y,data=SLF_data.year, FUN="max")
    
    colnames(SLF_data.year.agg)[3]<-"Survey_Presence"
    
    #merge with data.merge
    data.merge.2<-na.omit(merge(data.merge, SLF_data.year.agg, by=c("X","Y"),all.y=TRUE))
    names(data.merge.2)[names(data.merge.2)=="Presence"]<-"Simulated_Presence"
    
    #create column of correct positive detections
    data.merge.2$CorrectEstimate<-ifelse(data.merge.2$Simulated_Presence == data.merge.2$Survey_Presence, 1,0)
    
    #get proportion of correct estimates
    data.merge.2$CorrectProportion<-mean(data.merge.2$CorrectEstimate)
    #print(unique(round(data.merge.2$CorrectProportion,5)))
    
    #get number of false positives
    data.merge.2$False_Positives<-ifelse((data.merge.2$Survey_Presence-data.merge.2$Simulated_Presence) < 0,1,0)
    data.merge.2$False_Negatives<-ifelse((data.merge.2$Survey_Presence-data.merge.2$Simulated_Presence) > 0,1,0)
    
    #get False positive and negative proportions
    data.merge.2$False_Positive_prop<-mean(data.merge.2$False_Positives)
    data.merge.2$False_Negative_prop<-mean(data.merge.2$False_Negatives)
    
    cat(paste("Prop. correct: ",unique(round(data.merge.2$CorrectProportion,5)),"\n",
          "Prop. false positive: ", unique(round(data.merge.2$False_Positive_prop,5)),"\n",
          "Prop. false negative: ", unique(round(data.merge.2$False_Negative_prop,5)),"\n",
          sep=""))
    
    #simulation.evaluation.save
    model.evaluation.save<-unique(data.merge.2[,c("Year","Simulation_ID","CorrectProportion","False_Positive_prop","False_Negative_prop")])
    colnames(model.evaluation.save)[1]<-"Simulation_Year"
    
    #add Year
    model.evaluation.save$Survey_Year<-yearListSurveys[i]
    
    #re-organize columns
    model.evaluation.save<-model.evaluation.save[,c("Simulation_Year","Survey_Year","Simulation_ID","CorrectProportion","False_Positive_prop","False_Negative_prop")]
    
    data.merge.save<-rbind(data.merge.save, model.evaluation.save)
    
  }
  
  simu.data.save<-rbind(simu.data.save, data.merge.save)
}

#########################################################################################
#save results
write.csv(simu.data.save, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Validation_growthRate_1_q2_mean_0.99_q2_sd_0.05.csv", row.names=FALSE)


#########################################################################################
#compare version 1A, q2=0.3 (original run) vs. version 1 (with updated nextCellGroup_lookup function)

mod.eval.1A<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Validation_growthRate_1_q2_mean_0.3_q2_sd_0.05_A.csv")

#add type column
mod.eval.1A$Growth_Rate<-"1A"

mod.eval.1<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Validation_growthRate_1_q2_mean_0.3_q2_sd_0.05.csv")

#add type column
mod.eval.1$Growth_Rate<-"1"

#combine models
mod.eval.comb<-rbind(mod.eval.1A, mod.eval.1)

#make Growth_Rate a factor
mod.eval.comb$Growth_Rate<-factor(mod.eval.comb$Growth_Rate, levels=c("1A","1"))

#create Survey_Year_Growth_Rate column
mod.eval.comb$Year_Growth_Rate<-paste(mod.eval.comb$Survey_Year, mod.eval.comb$Growth_Rate, sep="_")

#get summary stats
mod.eval.summary<-summaryFunction(DataIn=mod.eval.comb, factor="Year_Growth_Rate", response="CorrectProportion")

#break out columns
Year.Growth<-read.table(text=as.character(mod.eval.summary$Year_Growth_Rate),sep="_")
colnames(Year.Growth)<-c("Survey_Year","Intrinsic_Growth_Rate")

#add back into summary stats
mod.eval.summary.out<-data.frame(Year.Growth, mod.eval.summary)

#make Intrinsic_Growth_Rate a factor
mod.eval.summary.out$Intrinsic_Growth_Rate<-factor(mod.eval.summary.out$Intrinsic_Growth_Rate, levels=c("1A","1"))



#plot with ggplot
model.evaluation.plot.1<-ggplot(data=mod.eval.summary.out, aes(x=Survey_Year, y=mean))+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD, color=Intrinsic_Growth_Rate),width=0,position=position_dodge(width=0.6))+
  geom_point(aes(color=Intrinsic_Growth_Rate),size=1,position=position_dodge(width=0.6))+
  scale_color_manual(values=c("royalblue2","tomato","goldenrod3"))+
  labs(x="Year",y="Proportion of accurate model predictions")+
  #ylim(0,1)+
  theme(
    legend.position=c(0.8,0.2)
  )

model.evaluation.plot.1

########################################################################
#also along random movement to best-choice gradient

#Fully-Random: q2=0.99
mod.eval_1<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Validation_growthRate_1_q2_mean_0.99_q2_sd_0.05.csv")

#add growthRate column
mod.eval_1$Growth_Rate<-"1"

#add q2 column
mod.eval_1$Movement_Type<-"Fully Random"

#High random: q2=0.7
mod.eval_2<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Validation_growthRate_1_q2_mean_0.7_q2_sd_0.05.csv")

#add growthRate column
mod.eval_2$Growth_Rate<-"1"

#add q2 column
mod.eval_2$Movement_Type<-"High Random"

#Low random: q2=0.3
mod.eval_3<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Validation_growthRate_1_q2_mean_0.3_q2_sd_0.05.csv")

#add growthRate column
mod.eval_3$Growth_Rate<-"1"

#add q2 column
mod.eval_3$Movement_Type<-"Low Random"

#Non-random: q2=0.01
mod.eval_4<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Model_Validation_growthRate_1_q2_mean_0.01_q2_sd_0.05.csv")

#add growthRate column
mod.eval_4$Growth_Rate<-"1"

#add q2 column
mod.eval_4$Movement_Type<-"Non Random"

###################################################################################################################
#combine models
mod.eval.comb<-rbind(rbind(rbind(mod.eval_1, mod.eval_2), mod.eval_3),mod.eval_4)

#make Growth_Rate a factor
mod.eval.comb$Growth_Rate<-as.factor(mod.eval.comb$Growth_Rate)

#make q2 a factor
mod.eval.comb$Movement_Type<-as.factor(mod.eval.comb$Movement_Type)

#create Survey_Year_Growth_Rate column
mod.eval.comb$Year_Growth_Rate_Movement<-paste(mod.eval.comb$Survey_Year, mod.eval.comb$Growth_Rate, mod.eval.comb$Movement_Type, sep="_")

#get summary stats
mod.eval.summary<-summaryFunction(DataIn=mod.eval.comb, factor="Year_Growth_Rate_Movement", response="CorrectProportion")

#break out columns
Year.Growth.Movement<-read.table(text=as.character(mod.eval.summary$Year_Growth_Rate_Movement),sep="_")
colnames(Year.Growth.Movement)<-c("Survey_Year","Intrinsic_Growth_Rate","Movement_Type")

#add back into summary stats
mod.eval.summary.out<-data.frame(Year.Growth.Movement, mod.eval.summary)

#make Intrinsic_Growth_Rate a factor
mod.eval.summary.out$Intrinsic_Growth_Rate<-as.factor(mod.eval.summary.out$Intrinsic_Growth_Rate)

#make q2 a factor
mod.eval.summary.out$Movement_Type<-as.factor(mod.eval.summary.out$Movement_Type)

#plot with ggplot
model.evaluation.plot.3<-ggplot(data=mod.eval.summary.out, aes(x=Survey_Year, y=mean, group=Movement_Type))+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD, color=Movement_Type),width=0,position=position_dodge(width=0.6))+
  geom_point(aes(color=Movement_Type),size=1,position=position_dodge(width=0.6))+
  scale_x_continuous(breaks=c(2014,2015,2016,2017,2018,2019))+
  scale_color_manual(values=c("royalblue2","tomato","goldenrod3","darkorchid1"))+
  labs(x="Year",y="Proportion of accurate model predictions")+
  ylim(0.5,1)+
  theme(
    legend.position=c(0.8,0.2),
    panel.background = element_rect(color="black",fill="transparent"),
    panel.border = element_rect(color="black", fill="transparent")
  )

model.evaluation.plot.3

ggsave(model.evaluation.plot.3, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Evaluation_figs/Model_accuracy_figure_2.png",width=6, height=6, dpi=300)

#########################################################################################
#look at model performance stats

#create Survey_Year_Growth_Rate column
new.model.eval.comb$Year_Growth_Rate<-paste(new.model.eval.comb$Survey_Year, new.model.eval.comb$Growth_Rate, sep="_")

set.seed=33

#randomly sample from simulated output
rndid <- with(new.model.eval.comb, ave(Survey_Year, FUN=function(x) {sample.int(length(x))}))
simu.data.sample<-new.model.eval.comb[rndid<100,]
#########################################################################################

model.eval.summary<-summaryFunction(DataIn=new.model.eval.comb, factor="Year_Growth_Rate", response="CorrectProportion")

#break out columns
Year.Growth<-read.table(text=as.character(model.eval.summary$Year_Growth_Rate),sep="_")
colnames(Year.Growth)<-c("Survey_Year","Intrinsic_Growth_Rate")

#add back into summary stats
model.eval.summary.out<-data.frame(Year.Growth, model.eval.summary)

#make Intrinsic_Growth_Rate a factor
model.eval.summary.out$Intrinsic_Growth_Rate<-factor(model.eval.summary.out$Intrinsic_Growth_Rate, levels=c("1","1.5","2"))

#########################################################################################
#plot with ggplot
model.evaluation.plot<-ggplot(data=model.eval.summary.out, aes(x=Survey_Year, y=mean))+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD, color=Intrinsic_Growth_Rate),width=0,position=position_dodge(width=0.6))+
  geom_point(aes(color=Intrinsic_Growth_Rate),size=1,position=position_dodge(width=0.6))+
  scale_color_manual(values=c("royalblue2","tomato","goldenrod3"))+
  labs(x="Year",y="Proportion of accurate model predictions")+
  ylim(0,1)+
  theme(
    legend.position=c(0.8,0.2)
  )

model.evaluation.plot

ggsave(model.evaluation.plot, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Model_Validation/Evaluation_figs/Model_accuracy_figure_1.png",width=5, height=5, dpi=300)
#########################################################################################
##################################################################################################
#get presence/absence from simu.data

#get yearList
yearList<-unique(simu.data$Year)

# #create all.cells.years.df
# all.cells.years.df<-list()
# for(i in 1:length(yearList)){
#   
#   print(yearList[i])
#   all.cells.df$Year<-yearList[i]
#   all.cells.years.df<-rbind(all.cells.years.df, all.cells.df)
# }


#get simulationList
simulationList<-unique(simu.data$Simulation_ID)

save.all.year.results<-list()
for(j in 1:length(yearList)){
  
  year.data<-subset(simu.data, Year==yearList[j])
  print(paste("Year",j))
  
  save.simu.results<-list()
  for(i in 1:length(simulationList)){
    
      print(paste("Simulation",i))
    
      simu.data.sub<-subset(year.data, c(Simulation_ID==simulationList[i]))
      
      #merge with all cells 
      simu.data.sub.merge<-merge(simu.data.sub, all.cells.df, by=c("X","Y","Longitude","Latitude"),all.y=TRUE)
      
      #fill in columns
      simu.data.sub.merge$Total_Visits<-ifelse(is.na(simu.data.sub.merge$Total_Visits), 0, simu.data.sub.merge$Total_Visits)
      simu.data.sub.merge$Year<-ifelse(is.na(simu.data.sub.merge$Year), year.data$Year, simu.data.sub.merge$Year)
      simu.data.sub.merge$Simulation_ID<-ifelse(is.na(simu.data.sub.merge$Simulation_ID), simulationList[i], na.omit(unique(as.character(simu.data.sub.merge$Simulation_ID))))
      simu.data.sub.merge$YearDate<-ifelse(is.na(simu.data.sub.merge$YearDate), year.data$YearDate, simu.data.sub.merge$YearDate)
      simu.data.sub.merge$Presence<-ifelse(simu.data.sub.merge$Total_Visits>0,1,0)
      
      save.simu.results<-rbind(save.simu.results, simu.data.sub.merge)
    
  }
  
  #get mean results with summary stats
  
  simu.mean.presence<-aggregate(Presence~Latitude+Longitude+Year, FUN=function(x) {mean(x, na.rm=TRUE)},data=save.simu.results)
  
  
  save.all.year.results<-rbind(save.all.year.results, save.simu.results)
}
  
#take the average pixel value across all simulations at each XY coordinate
simu.mean.presence<-aggregate(Presence~Latitude+Longitude+Year, FUN=function(x) {mean(x, na.rm=TRUE)},data=simu.data.merge)

unique(simu.mean.presence$Presence)

####################################################################################################################need functions #now plot 

#test with plotting basemap and county polygons
SLF_observed<-ggplot() + 
  geom_point(data=simu.mean.presence, aes(x=Longitude,y=Latitude, color=Presence), size=0.5)+
  scale_color_gradientn(colors=parula(100))+
  geom_sf(data=counties.sub.2, fill="transparent",color=alpha("gray50",0.4), inherit.aes = FALSE)+
  # coord_sf(crs = st_crs(3857),ylim=c(county.bbox[2]*0.99, county.bbox[4]*1.01), 
  #          xlim=c(county.bbox[1]*1.01, county.bbox[3]*0.99))+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"),
        legend.position = c(0.85,0.2),
        legend.background = element_blank())+
  labs(x="Longitude",y="Latitude")

SLF_observed

#facet over Year
SLF_observed_facet<-SLF_observed+facet_wrap(~Year)+theme(legend.position="right")
SLF_observed_facet


####################################################################################################

# # create matrix of longitude and latitude values
# latlon <- SLF_data_presence %>% 
#   select(Longitude, Latitude) %>%
#   as.matrix()
# 
# # convert to a simple features column 
# geometry <- latlon %>%
#   st_multipoint() %>% 
#   st_sfc() %>%
#   st_cast("POINT")
# # append the columns and attach the "longlat" geometry
# # in which the coordinates were originally defined
# slf_points <- st_sf(Total.Count = SLF_data_presence$Total.Count, geometry)
# st_crs(slf_points) <- st_crs("+proj=latlong +datum=WGS84")
# slf_points.proj<-st_transform(slf_points, 3857)
# 
# #add Year to slf_points.proj
# slf_points.proj$Year<-SLF_data_presence$Year

#plot observed data
#####################################################################################################
#now plot convex hull polygons of areas with observed SLF data

#get list of years from survey data
yearList<-sort(unique(as.character(SLF_data_presence$Year)))

#gather data (stacked cummulative results)
cumulative.data<-list()
for(i in 1:length(yearList)){
  
  new.yearList<-c(as.character(yearList[1:i]))
  
  sub.data<-subset(SLF_data_presence, Year %in% new.yearList)
  
  sub.data$CumulativeYear<-yearList[i]
  
  cumulative.data<-rbind(cumulative.data, sub.data)
}

#get convex hulls
save.hulls<-list()
for(i in 1:length(yearList)){
  
  new.yearList<-c(as.character(yearList[1:i]))
  
  sub.data<-subset(SLF_data_presence, Year %in% new.yearList)
  
  #add polygon centroids
  sub.data$center.Longitude <- mean(sub.data$Longitude)
  sub.data$center.Latitude <- mean(sub.data$Latitude)
  
  new.hull<- sub.data[chull(sub.data$Latitude, sub.data$Longitude),]
  
  new.hull.2 <- cbind(new.hull$Longitude, new.hull$Latitude)
  box.chull.coords <- new.hull.2
  
  #get hull area
  chull.poly <- Polygon(box.chull.coords, hole=F)
  
  chull.area <- geosphere::areaPolygon(chull.poly@coords)/1000000
  
  new.hull$Area_sq_km<-chull.area
  
  #add new column for cummulative year
  new.hull$CumulativeYear<-yearList[i]
  
  save.hulls<-rbind(save.hulls, new.hull)
  
}

# #convert cumulative.data to correct projection
# # create matrix of longitude and latitude values
# latlon <- save.hulls %>% 
#   select(Longitude, Latitude,CumulativeYear) %>%
#   as.list(as.matrix())
# #latlon$CumulativeYear<-as.factor(latlon$CumulativeYear)
# 
# # convert to a simple features column 
# geometry <- latlon %>%
#   st_multipolygon() %>% 
#   st_sfc() %>%
#   st_cast("POLYGON")%>%
#   st_cast("MULTIPOLYGON")
# 
# # append the columns and attach the "longlat" geometry
# # in which the coordinates were originally defined
# slf_hulls <- st_sf(Area_sq_km = save.hulls$Area_sq_km, geometry)
# st_crs(slf_hulls) <- st_crs("+proj=latlong +datum=WGS84")
# slf_hulls.proj<-st_transform(slf_hulls, 3857)
# 
# #convert sf_point to data.frame for plotting in ggplot
# slf_hulls.proj.coords<-as.data.frame(st_coordinates(slf_hulls.proj))
# slf_hulls.proj.coords$Area_sq_km<-save.hulls$Area_sq_km
# slf_hulls.proj.coords$Year<-as.factor(save.hulls$CumulativeYear)
# 
# slf_hulls.proj.coords<-unique(slf_hulls.proj.coords)
# 
# slf_hulls.proj.coords.sub<-unique(subset(slf_hulls.proj.coords, Year=="2014"))


SLF_observed_hulls<-ggplot() + 
  geom_point(data=cumulative.data, aes(x=Longitude,y=Latitude, color=Total.Count))+
  scale_color_gradientn(colors=parula(100))+
  geom_sf(data=counties.sub.2, fill="transparent",color=alpha("gray50",0.4), inherit.aes = FALSE)+
  # coord_sf(crs = st_crs(3857),ylim=c(county.bbox[2]*0.99, county.bbox[4]*1.01), 
  #          xlim=c(county.bbox[1]*1.01, county.bbox[3]*0.99))+
  geom_polygon(data=save.hulls,fill=alpha("red",0.5),color="red",aes(x=Longitude,y=Latitude))+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"),
        legend.position = c(0.85,0.2),
        legend.background = element_blank())+
  labs(x="Longitude",y="Latitude")

SLF_observed_hulls

#now facet over years
SLF_observed_hulls_facet<-SLF_observed_hulls+facet_wrap(~CumulativeYear)+theme(legend.position="right")
SLF_observed_hulls_facet

########################################################################################################
#compare change in area of convex hulls over time (observed)
slf.area.df<-unique(save.hulls[,c("CumulativeYear","Area_sq_km")])

save.diffs<-list()
for(i in 1:length(row.names(slf.area.df))-1){
  
  year.data<-slf.area.df[i:(i+1),]
  year.diff<-data.frame(Delta_Area=year.data[2,2]-year.data[1,2])
  
  #add percent change
  year.diff$Pct.change<-round((year.diff$Delta_Area/year.data[1,2])*100)
  
  save.diffs<-rbind(save.diffs, year.diff)
  
}

#add to slf.area.df
slf.area.df.2<-data.frame(slf.area.df, save.diffs)

#make Year numeric
unique(slf.area.df.2$CumulativeYear)

#plot percent change in area of observed spread
SLF_spread_plot<-ggplot(data=slf.area.df.2, aes(x=as.integer(CumulativeYear), y=Area_sq_km))+
  geom_area(fill=alpha("red",0.4),color="red")+
  scale_x_continuous(breaks=c(2014,2015,2016,2017,2018,2019),labels=c("2014","2015","2016","2017","2018","2019"))+
  labs(x="Year",y="Cumulative SLF area (km^2)")+
  theme(panel.border=element_rect(fill="transparent",color="black"),
        panel.background=element_rect(fill="white",color="black"))
SLF_spread_plot

########################################################################################################
#now look at simulation results




####################################################################################################################
#convert rasters to 0s and 1s and then average

#pseudo code for using monitoring locations as beacons

# samplingLocations<-data.frame()
# pair-wise comparisons between trap location (0 or 1) and count of visits 0 or >
# 

#write function: does current raster intersect with sampling_locations, if so, how many yes out of how many possible, track this ratio, closer to 1 == an accurate model

#if slf are not detected at any points, and simu are not detected at any points, then 1
#if slf are detected at all points, and simu are detected at all points, then 1
#if slf are not detected at all points, and simu are detected (or vice versa), the proportion of points where slf are detected, approaches 1 as better accuracy
#sum these 3 numbers and use this as an index


#add grid of sampling locations



#########################################################################################

#now look at results total visits per pixel (4km^2)

visitedRasterPlot<-ggplot()+
  #geom_tile(data=NE_surface.df, aes(x=Longitude, y=Latitude, fill=MaxEnt_grayscale_4km), alpha=0.4,show.legend = FALSE)+
  scale_fill_gradient(low="black",high="white")+
  new_scale_fill() +   # same as `new_scale("color")`
  geom_tile(data=simu.mean.presence, aes(x=Longitude, y=Latitude, fill=Presence),alpha=0.6, inherit.aes = TRUE)+
  scale_fill_gradientn(colors=plasma(100),labels=comma)+
  geom_sf(data=counties.sub.2, fill="transparent",color=alpha("gray50",0.1), inherit.aes = FALSE)+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"),
        legend.position = c(0.85,0.2),
        legend.background = element_blank())+
  labs(x="Longitude",y="Latitude")

visitedRasterPlot

#####################################################################################
#plot facet by Year

visitedRasterPlot_facet<-visitedRasterPlot+facet_wrap(~Year)+theme(legend.position="right")
visitedRasterPlot_facet

#########################################################################################
#now look at convex hulls

simuHullsPlot<-ggplot()+
  #geom_tile(data=NE_surface.df, aes(x=Longitude, y=Latitude, fill=MaxEnt_grayscale_4km), alpha=0.4,show.legend = FALSE)+
  scale_fill_gradient(low="black",high="white")+
  new_scale_fill() +   # same as `new_scale("color")`
  geom_tile(data=simu.data, aes(x=Longitude, y=Latitude, fill=Total_Visits),alpha=0.025, inherit.aes = TRUE)+
  geom_polygon(data=simu.convex.hulls, aes(x=Longitude,y=Latitude),color=alpha("orange",0.7),fill="transparent",size=0.06,show.legend=FALSE)+
  scale_fill_gradientn(colors=plasma(100),labels=comma)+
  geom_sf(data=counties.sub.2, fill="transparent",color=alpha("gray50",0.4), inherit.aes = FALSE)+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"),
        legend.position = c(0.85,0.2),
        legend.background = element_blank())+
  labs(x="Longitude",y="Latitude")

simuHullsPlot

#plot facet by Year

simuHullsPlot_facet<-simuHullsPlot+facet_wrap(~Year)+theme(legend.position="right")
simuHullsPlot_facet

#########################################################################################
#compare hulls areas to observed hull area
slf.area.simu.df<-unique(simu.convex.hulls[,c("CumulativeYear","Area_sq_km","Simulation_ID")])

#get simulation list
simulationList<-unique(slf.area.simu.df$Simulation_ID)

simu.diff.save<-list()
for(j in 1:length(simulationList)){
  
  simu.data.sub<-subset(slf.area.simu.df, Simulation_ID==simulationList[1])
  
  save.diffs.simu<-list()
  for(i in 1:length(row.names(slf.area.simu.df))-1){
    
    year.data<-slf.area.simu.df[i:(i+1),]
    year.diff<-data.frame(Delta_Area=year.data[2,2]-year.data[1,2])
    
    #add percent change
    year.diff$Pct.change<-round((year.diff$Delta_Area/year.data[1,2])*100)
    
    save.diffs.simu<-rbind(save.diffs.simu, year.diff)
    
  }
  
  
  
}


#add to slf.area.df
slf.area.simu.df.2<-data.frame(slf.area.simu.df, save.diffs.simu)
#add DataType column
slf.area.simu.df.2$DataType<-"Simulation"

slf.area.df.2$DataType<-"Observed"

#combine simulated and observed data
slf.area.all<-rbind(slf.area.df.2, slf.area.simu.df.2)
slf.area.all$DataType<-as.factor(slf.area.all$DataType)

SLF_spread_plot_compare<-ggplot(data=slf.area.all, aes(x=as.integer(CumulativeYear), y=Area_sq_km))+
  geom_area(aes(fill=DataType,color=DataType))+
  scale_fill_manual(values=c(alpha("red",0.4),alpha("royalblue",0.4)))+
  scale_color_manual(values=c("red","royalblue"))+
  scale_x_continuous(breaks=c(2014,2015,2016,2017,2018,2019),labels=c("2014","2015","2016","2017","2018","2019"))+
  labs(x="Year",y="Cumulative SLF area (km^2)")+
  theme(panel.border=element_rect(fill="transparent",color="black"),
        panel.background=element_rect(fill="white",color="black"))
SLF_spread_plot_compare

#save plot
ggsave(SLF_spread_plot_compare, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Figures/SLF_cumulative_area_compare.png", width=5, height=4, dpi=200)

#########################################################################################
#compute area of intersection of multiple polygons
