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
library(googleway)
library(jpeg)
library(RCurl)


########################################################################################################################
#This section is for predicting if TOH (or other spp.) are in an image using a deployed model on GCP
## your valid API key here
key <- "AIzaSyAaDHA8482JIVvEv8yFPPcpvUukZ1j69zo"

#signature
#mySig<-"3v8e7J3xj6oZzCGRStv4HtVeEGE="

#read in coordinate data
coords<-read.csv("./Analyses/Predict_TOH/Data/All_PA_coords.csv")

#sample every 20th point
coords.samp<- coords[seq(1, nrow(coords), 150), ]

#get subset of locations from road.sub.coords.df
myLocations<-coords.samp

#myLocationsA
myLocationsA<-head(myLocations, round(length(row.names(myLocations))/2))

myLocationsB<-tail(myLocations, (length(row.names(myLocations))) - round(length(row.names(myLocations))/2))

#subset myLocations
myLocationsSub<-myLocationsB[-1:-3469,]
myLocations<-myLocationsSub

#make directory to save streetview images
saveDir<-"/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/Downloaded_Images"
dir.create(saveDir)

#set up directions and pitches for google images
dirList<-c(seq(0,315, by=45))
#pitchList<-c(-90, -60, -30, 0, 30, 60, 90)
pitchList<-c(10)
fovList<-c(90)
#i=1

#myLocations<-myLocationsA

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
      
        
        #Sys.sleep(3)
  dev.off()
      }
    }
  
  #prediction step
  use_python("/Applications/Python 3.7", required=TRUE)
  #use_condaenv("anaconda")
  
  #source_python("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Scripts/Predict_AutoML.py")
  
  myNewDir<-paste(saveDir, new.loc.name, sep="/")
  imagesList<-dir(myNewDir)
  
  resultsSave<-list()
  for(z in 1:length(imagesList)){
    
    newImageName = imagesList[z]
    newImagePath<-paste(myNewDir, newImageName,sep="/")
    
    runCommand<- paste("python3", "/Users/zach/'Dropbox (ZachTeam)'/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Scripts/Predict_AutoML.py", shQuote(newImagePath, type="sh"),"my-project-1507338742705", "IOD7149055390125129728")
    #my-project-1507338742705-68bd0aa29b39.json
    
    #curl request (in order to get bounding box info as well)
    # curl -X POST -H "Content-Type: application/json" \
    # -H "Authorization: Bearer $(gcloud auth application-default print-access-token)" \
    # https://automl.googleapis.com/v1beta1/projects/643533147029/locations/us-central1/models/IOD7149055390125129728:predict \
    # -d @request.json
    
    
    #run prediction from AutoML
    predictResults<-try(system(runCommand, intern=TRUE))
    
    #harvest results from AutoML
    predictResultsTrim<-trimws(predictResults)
    
    #bounding box coords
    xminNorm = as.numeric(gsub("x: ", "", predictResultsTrim[6]))
    yminNorm = as.numeric(gsub("y: ", "", predictResultsTrim[7]))
    xmaxNorm = as.numeric(gsub("x: ", "", predictResultsTrim[10]))
    ymaxNorm = as.numeric(gsub("y: ", "", predictResultsTrim[11]))
    
    #confidence of prediction
    confidence = as.numeric(gsub("score: ", "", predictResultsTrim[14]))
    
    #clean up label
    label_1 = gsub("display_name: ", "", predictResultsTrim[16])
    label = gsub("[^[:alnum:][:blank:]+?&/\\-_]", "",label_1)
    
    message(paste("Saving model prediction results for location ", i))
    
    ifelse(predictResults =="", 
           {
             results.df<-data.frame(State="Pennsylvania",ImageName=newImageName,Longitude=longitude, Latitude=latitude, Label= NA, Confidence=NA)
             results.df$TOH_detected<-"Not_Detected"
           },
           {
             results.df<-data.frame(State="Pennsylvania",ImageName=newImageName,Longitude=longitude, Latitude=latitude, Label= label, Confidence=confidence)
             results.df$TOH_detected<-ifelse(results.df$Label=="Tree_of_Heaven","Detected","Not_Detected")
           })
           
    resultsSave<-rbind(resultsSave, results.df)
  }
  resultsOut<-rbind(resultsOut, resultsSave)
}

#write.csv(resultsOut, file=paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data", paste("TOH_predictions_AutoML_saved_B4","_", Sys.Date(),".csv",sep=""),sep="/"),row.names=FALSE)


#####################################################################################################################
#combine predictions from multiple files

#combine prediction files
pred1<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_A_2020-01-12.csv",header=TRUE)

pred2<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_B_2020-01-12.csv",header=TRUE)

pred3<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_B1_2020-01-12.csv")

pred4<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_B2_2020-01-12.csv", header=TRUE)

pred5<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_B3_2020-01-13.csv", header=TRUE)

pred6<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_B4_2020-01-13.csv", header=TRUE)

pred7<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_C_2020-01-12.csv", header=TRUE)

pred8<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_C2_2020-01-12.csv", header=TRUE)

pred9<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_C3_2020-01-13.csv", header=TRUE)

pred10<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_C4_2020-01-13.csv", header=TRUE)

fileList<-list(pred1, pred2, pred3, pred4, pred5, pred6, pred7, pred8, pred9, pred10)

combined.df <- do.call(rbind , fileList)

#save
write.csv(combined.df, file=paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data", paste("TOH_predictions_AutoML_saved_ALL","_", Sys.Date(),".csv",sep=""),sep="/"),row.names=FALSE)

#####################################################################################################################
#This section is for mapping and for exporting points where TOH were detected as a shp file

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
library(googleway)
library(jpeg)
library(RCurl)

## your valid API key here
register_google("AIzaSyAaDHA8482JIVvEv8yFPPcpvUukZ1j69zo")

#read in all data and map points
allPred<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Data/TOH_predictions_AutoML_saved_ALL_2020-01-13.csv",header=TRUE)

#add Location column (factor)
allPred$Location<-as.factor(paste(allPred$Longitude,allPred$Latitude, sep="_"))

#get max Confidence (% accuracy) per location
maxConf.df<-aggregate(Confidence~Location+Label+TOH_detected,data=allPred,FUN="max")

#get all unique (tested) locations
all.unique.points<-unique(allPred[,c("Longitude","Latitude","Location")])

#merge max confidence per location and all unique locations
pred.merge<-merge(all.unique.points, maxConf.df,  by=c("Location"), all.x=TRUE)

#substitute NAs with "Not_Detected
pred.merge$TOH_detected[is.na(pred.merge$TOH_detected)]<-"Not_Detected"

#subset all "Detected" points
#all.toh.detected<-subset(allPred, TOH_detected=="Detected")

#add factors for confindence levels of toh occurrence predictions
pred.merge$ConfLevel<-ifelse(pred.merge$Confidence > 0.9,">90%",
                                   ifelse(pred.merge$Confidence <= 0.9 & pred.merge$Confidence >= 0.5,"50-90%",
                                    ifelse(pred.merge$Confidence <= 0.5,"<50%",NA
                                      )
                                   )
                                )
#set ConfLevel to factor
pred.merge$ConfLevel<-as.factor(pred.merge$ConfLevel)

#set factor levels for Label
pred.merge$Label<-factor(pred.merge$Label, levels=c("Tree_of_Heaven","Staghorn_Sumac","Mimosa","Black_Walnut"))

#levels(all.toh.detected$ConfLevel)
#look at points on a map
xmean<-mean(pred.merge$Longitude)
ymean<-mean(pred.merge$Latitude)

map.center<-c(xmean, ymean)

myMap<-get_map(location=map.center, maptype = "toner-lite",zoom=7)
ggmap(myMap)

myColors<-c("tomato","gold")


myMapPoints<-ggmap(myMap, darken = c(0.8, "black"))+
  geom_point(data=pred.merge, aes(x=Longitude, y=Latitude, color=ConfLevel),size=1, alpha=0.5)+
  #scale_color_gradient(low="lightgray",high="royalblue")
  scale_color_manual(values = myColors, na.value="white")+
  labs(x="Longitude",y="Latitude")
myMapPoints

#facet species and confidence levels
mapFacet<-myMapPoints+facet_grid(ConfLevel~Label)
mapFacet

#get count of points (n) per group

#create LabelConf column
pred.merge$LabelConf<-as.factor(paste(pred.merge$Label, pred.merge$ConfLevel, sep="+"))

pred.n<-aggregate(.~LabelConf, FUN="length",data=pred.merge)[,1:2]
pred.n.2<-data.frame(read.table(text=as.character(pred.n$LabelConf), sep="+"), pred.n)
#rename columns
colnames(pred.n.2)<-c("Label","ConfLevel", "LabelConf", "Count")


#set factor levels for plot
pred.n.2$Label<-factor(pred.n.2$Label, levels=c("Tree_of_Heaven","Staghorn_Sumac","Mimosa","Black_Walnut"))
levels(pred.n.2$Label)

#plot number of detections by labeland accuracy category
barPlot_1<-ggplot(data=pred.n.2, aes(x=Label,y=Count))+
  geom_bar(stat="identity",aes(fill=ConfLevel), position=position_dodge(), alpha=0.8)+
  scale_fill_manual(values=myColors)
barPlot_1


#map without NAs
myMapPoints<-ggmap(myMap, darken = c(0.8, "black"))+
  geom_point(data=na.omit(pred.merge), aes(x=Longitude, y=Latitude, color=ConfLevel),size=1, alpha=0.6)+
  #scale_color_gradient(low="lightgray",high="royalblue")
  scale_color_manual(values = myColors, na.value="lightgray")+
  labs(x="Longitude",y="Latitude")
myMapPoints

#facet species and confidence levels
mapFacet<-myMapPoints+facet_grid(ConfLevel~Label)+
  theme(strip.text.x=element_text(face="bold"),strip.text.y=element_text(angle=0, face="bold"))
mapFacet

#save image for presentation
ggsave(mapFacet, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Maps/TOH_detected_1-14-2020.png",width=10, height=8, dpi=600)


#map without All Points sampled
myMapPointsAllPoints<-ggmap(myMap, darken = c(0.8, "black"))+
  geom_point(data=pred.merge, aes(x=Longitude, y=Latitude), color="royalblue",size=1, alpha=0.6)+
  #scale_color_gradient(low="lightgray",high="royalblue")
  #scale_color_manual(values = myColors, na.value="lightgray")+
  labs(x="Longitude",y="Latitude")
myMapPointsAllPoints

ggsave(myMapPointsAllPoints, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Predict_TOH/Maps/All_Points_Sampled_1-14-2020.png",width=8, height=8, dpi=600)

#############################################################################################
#convert TOH detection points to shp

#get just TOH detected points
TOH_detect<-subset(pred.merge, TOH_detected=="Detected")

#verify length of unique locations
length(unique(TOH_detect$Location))

xy <- TOH_detect[,c("Longitude","Latitude")]
spdf <- SpatialPointsDataFrame(coords = xy, data = TOH_detect,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#read in shapefile
shapefile(spdf, filename='/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/TOH_detected_all.shp')

#get just TOH detections >90% accuracy
spdf90<-subset(spdf, spdf@data$Confidence>=0.90)

#write shapefile
shapefile(spdf90, filename='/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/TOH_detected_>90.shp')



all.toh.detected.unique<-unique(all.toh.detected[,c("Longitude","Latitude")])
all.toh.detected.unique$TOH_detected<-"Detected"
all.toh.merge<-merge(all.unique.points, all.toh.detected.unique, by=c("Longitude","Latitude"), all.x=TRUE)

all.toh.merge$TOH_detected
all.toh.merge$TOH_detected[is.na(all.toh.merge$TOH_detected)] <- "Not_Detected"

myColors<-c("red","royalblue")

#look at points on a map
xmean<-mean(all.toh.merge$Longitude)
ymean<-mean(all.toh.merge$Latitude)

map.center<-c(xmean, ymean)

myMap<-get_map(location=map.center, maptype = "terrain-background",zoom=7)
ggmap(myMap)

myMapPoints<-ggmap(myMap)+
  geom_point(data=all.toh.merge, aes(x=Longitude, y=Latitude, color=TOH_detected),size=1, alpha=0.4)
myMapPoints





allPred$Location<-paste(allPred$Longitude, allPred$Latitude, sep="_")

#get unique number of locations 
uniqueCoords<-unique(allPred$Location)
length(uniqueCoords)

#number of points detected
countSpecies<-aggregate(Location~Label,data=allPred, FUN=length)

allDetect<-subset(allPred, TOH_Detected=="Detected")

myColors<-c("red","royalblue")

#look at points on a map
xmean<-mean(allPred$Longitude)
ymean<-mean(allPred$Latitude)

map.center<-c(xmean, ymean)

myMap<-get_map(location=map.center, maptype = "terrain-background",zoom=7)
ggmap(myMap)

myMapPoints<-ggmap(myMap)+
  geom_point(data=allPred, aes(x=Longitude, y=Latitude), color="red",size=1)
myMapPoints


#subset TOH
tohAllData<-subset(allPred, Label=="Tree_of_Heaven")
tohAllData$Location<-as.factor(tohAllData$Location)

tohAllData.sub<-tohAllData[,c("State","Longitude","Latitude","Label","TOH_detected")]

AllLocations<-unique(allPred[,c("State","Longitude","Latitude","TOH_detected")])
AllLocations$Location<-as.factor(AllLocations$Location)



toh.merge<-unique(merge(AllLocations,tohAllData.sub, by=c("State","Longitude","Latitude","TOH_detected"),all.x=TRUE))


#now map
myMapPoints<-ggmap(myMap)+
  geom_point(data=toh.merge, aes(x=Longitude, y=Latitude, color=TOH_detected),size=0.4,alpha=0.5)+
  scale_color_manual(values=myColors)+
  theme(panel.border=element_rect(color="black",fill="transparent"))+
  labs(x="Longitude",y="Latitude")
myMapPoints

mapFacet<-myMapPoints+facet_wrap(.~Label)
mapFacet











