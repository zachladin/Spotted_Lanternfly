#Function to process simulation results
processSimulationResults<-function(fileDir,minX,maxX,minlong,maxlong,minY,maxY,minlat,maxlat){

  fileList<-list.files(as.character(fileDir),full.names = TRUE)
  fileListShort<-list.files(as.character(fileDir),full.names = FALSE)
  
  #save simulation data
  save.simu.data<-list()
  #save.all.hulls<-list()
  for(i in 1:length(fileList)){
    
    print(paste("Simulation", i, sep="_"))
    
    new.SLF.data<-read.csv(fileList[i])
    
    #get cummulative sums of total visits for each year
    simu.cummulative.sums<-getCummulativeSums(visited.years.in=new.SLF.data)
    
    #make Year a factor
    simu.cummulative.sums$Year<-as.factor(simu.cummulative.sums$Year)
    
    #sort by Year
    simu.sort<-simu.cummulative.sums[order(simu.cummulative.sums$Year,decreasing=FALSE),]
    #levels(simu.sort$Year)
    
    #add simulation_ID column
    simu.sort$Simulation_ID<-paste("Simulation_",i,sep="")
    
    #now apply these functions to slf.simu data
    simu.sort$Longitude<-coord2long(inX=simu.sort$X,minX=minX,maxX=maxX,minlong=minlong, maxlong=maxlong)
    simu.sort$Latitude<-coord2lat(inY=simu.sort$Y, minY=minY, maxY=maxY,minlat=minlat, maxlat=maxlat)
    
    yearNumList<-c(as.character(seq(0,length(unique(simu.sort$Year))-1)))
    
    simu.sort$Year<-factor(simu.sort$Year, level=yearNumList)
    
    #unique(simu.sort$Year)
    
    #redefine factor levels
    yearDateList<-c(as.character(seq(2025-length(unique(simu.sort$Year))+1, 2025)))
    #levels(simu.sort$Year)<-yearDateList
    
    #add YearDate to simu.sort
    simu.sort$YearDate<-simu.sort$Year
    levels(simu.sort$YearDate)<-yearDateList
    
    message("Computing and saving results.")
    
    # save.hulls<-list()
    # for(k in 1:length(yearNumList)){
    #   
    #   new.yearList<-c(as.character(yearNumList[1:k]))
    #   
    #   sub.data<-subset(simu.sort, Year %in% new.yearList)
    #   
    #   #add polygon centroids
    #   sub.data$center.Longitude <- mean(sub.data$Longitude)
    #   sub.data$center.Latitude <- mean(sub.data$Latitude)
    #   
    #   new.hull<- sub.data[chull(sub.data$Longitude, sub.data$Latitude),]
    #   
    #   new.hull.2 <- cbind(new.hull$Longitude, new.hull$Latitude)
    #   box.chull.coords <- new.hull.2
    #   
    #   #get hull area
    #   chull.poly <- Polygon(box.chull.coords, hole=F)
    #   
    #   chull.area <- geosphere::areaPolygon(chull.poly@coords)/1000000
    #   
    #   new.hull$Area_sq_km<-chull.area
    #   
    #   #add new column for cummulative year
    #   new.hull$CumulativeYear<-yearNumList[k]
    #   
    #   new.hull$CumulativeYearDate<-yearDateList[k]
    #   
    #   # new.plot<-ggplot(sub.data, aes(Latitude, Longitude), colour="black", fill="black") + 
    #   #   geom_point() + 
    #   #   geom_density2d(alpha=.5) + 
    #   #   labs(x = "Latitude", y = "Longitude") + 
    #   #   geom_polygon(data=new.hull, alpha=.2)
    #   # print(new.plot)
    #   
    #   save.hulls<-rbind(save.hulls, new.hull)
    # }
    
    #save cummulative sums data
    save.simu.data<-rbind(save.simu.data, simu.sort)
    
    #save.all.hulls<-rbind(save.all.hulls, save.hulls)
    
  }
  
  #get info from file name
  fileNameInfo<-read.table(text=as.character(fileListShort[i]),sep="_")
  
  fileNameSave<-paste(fileNameInfo$V1, fileNameInfo$V2, fileNameInfo$V3, fileNameInfo$V4, fileNameInfo$V5, fileNameInfo$V6,
                      paste(fileNameInfo$V7,fileNameInfo$V8,sep=""),fileNameInfo$V9,paste(fileNameInfo$V10,fileNameInfo$V11,sep=""),fileNameInfo$V12,sep="_")
  
  
  saveDir<-paste(fileDir, "_Compiled",sep="")
  dir.create(saveDir)
  
  message("Combining and saving all simulation results")
  write.csv(save.simu.data, file=paste(saveDir, paste("Combined_counts_",fileNameSave,".csv",sep=""),sep="/"),row.names=FALSE)
  
  # message("Combining and saving all convex hull results")
  # write.csv(save.all.hulls, file=paste(saveDir, paste("Combined_hulls_",fileNameSave,".csv",sep=""),sep="/"),row.names=FALSE)
  
}
