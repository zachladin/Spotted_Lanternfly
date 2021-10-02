#function to format and combine rasters
formatRasters<-function(dataIn,folderName){
  
  extend_all = function(rasters){
    extent(Reduce(extend,rasters))
  }
  
  sum_all_rasters = function(rasters, extent){
    re = lapply(rasters, function(r){extend(r, extent, value=0)})
    Reduce("+",re)
  } 
  
  new.simu.data<-dataIn
  
  #go through each simulation and create a matrix of values and 0s (also save binary matrices)
  simulationList<-unique(new.simu.data$Simulation_ID)
  
  #get year list
  yearList<-unique(new.simu.data$Year)
  
  #for loop to compile rasters into stacks (this takes a while, so should save the resulting raster stacks!)
  save.rasters<-list()
  save.rasters.binary<-list()
  for(j in 2:length(yearList)){
    

    cummulativeYear<-yearList[j]
    
    print(paste("Year",cummulativeYear))
    
    save.rasters.year<-list()
    save.rasters.binary.year<-list()
    for(i in 1:length(simulationList)){
      
      print(paste("Year",cummulativeYear,"Simulation",i))
      
      new.simu.data.sub<-subset(new.simu.data, c(Simulation_ID==simulationList[i] & Year == cummulativeYear))
      
      #add matrix of 0s to new.simu.data
      new.simu.coords<-new.simu.data.sub[,c("X","Y","Total_Visits")]
      
      #make new raster
      new.simu.raster<-rasterFromXYZ(new.simu.coords,res=res(maxEnt_surface))
      
      #replace values (if not NA or > 0, then 0)
      new.simu.raster[is.na(new.simu.raster)] <- 0
      
      #mere simulation raster with maxEnt_surface_zeros
      rasters.merge<-merge(new.simu.raster, maxEnt_surface_zeros,tolerance=0.5, ext=floor(extend_all(list(new.simu.raster,maxEnt_surface_zeros))))
      
      #get info from newFolerNames
      fullNameInfo<-read.table(text=as.character(folderName),sep="/")
      
      nameInfo<-fullNameInfo[ncol(fullNameInfo)]
      
      rasters.merge@data@names<-paste("Simulation",i, nameInfo, sep="_")
      
      save.rasters.year<-append(save.rasters.year, rasters.merge)
      
    }
    
    message("Getting mean values among simulations.")
    #total visits
    raster.stack.year<-stack(save.rasters.year)
    raster.stack.year.mean<-mean(raster.stack.year)
    raster.stack.year.mean[raster.stack.year.mean[]==0] <- NA 
    
    #occurrence
    raster.stack.year.binary<-raster.stack.year
    raster.stack.year.binary[raster.stack.year.binary[]>0] <- 1 
    raster.stack.year.binary.mean<-mean(raster.stack.year.binary)
    raster.stack.year.binary.mean[raster.stack.year.binary.mean[]==0] <- NA 
    
    #create folder if needed
    dir.create(folderName)

    #total count filename
    rasterFilename<-paste(folderName,paste(nameInfo,"_Year_",cummulativeYear,".tiff",sep=""),sep="/")
    
    #binary filename
    rasterFilenameBinary<-paste(folderName,paste(nameInfo,"_Binary_Year_",cummulativeYear,".tiff",sep=""),sep="/")
    
    message("Saving rasters")
    writeRaster(raster.stack.year.mean, rasterFilename, overwrite=TRUE)
    writeRaster(raster.stack.year.binary.mean, rasterFilenameBinary, overwrite=TRUE)
    
  }
  
}

