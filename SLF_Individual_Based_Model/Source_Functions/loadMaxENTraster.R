#load MaxENT surface

loadMaxENTraster<-function(rasterIn, saveFileDir, preCompute=FALSE){
  
  maxEnt_surface<-raster(rasterIn, values=TRUE)
  
  #convert values >255 to NA
  maxEnt_surface[maxEnt_surface[]>255]<-NA

  maxEnt_surface.df<-na.omit(as.data.frame(maxEnt_surface, xy = TRUE))
  #round coords
  maxEnt_surface.df$x<-floor(maxEnt_surface.df$x)
  maxEnt_surface.df$y<-floor(maxEnt_surface.df$y)
  
  
  #compute Best Cells (takes a little time), if TRUE
  ifelse(isTRUE(preCompute),
    preComputeBestCells(dataIn=maxEnt_surface.df, rasterIn=maxEnt_surface, saveFilePath=saveFileDir),
    NA)
    
  ################################################################################################################################
  #read in NE_raster_bestCells.csv and use as lookup table
  NE_raster_bestCells<-read.csv(saveFileDir)
  
  #create CurrentCell_comb column 
  NE_raster_bestCells$CurrentCell_comb<-paste(NE_raster_bestCells$CurrentX, NE_raster_bestCells$CurrentY, sep="_")
  
  #create randCell_1, randCell_2, etc. . .
  NE_raster_bestCells$randCell_1<-paste(NE_raster_bestCells$X1, NE_raster_bestCells$Y1, sep="_")
  NE_raster_bestCells$randCell_2<-paste(NE_raster_bestCells$X2, NE_raster_bestCells$Y2, sep="_")
  NE_raster_bestCells$randCell_3<-paste(NE_raster_bestCells$X3, NE_raster_bestCells$Y3, sep="_")
  NE_raster_bestCells$randCell_4<-paste(NE_raster_bestCells$X4, NE_raster_bestCells$Y4, sep="_")
  NE_raster_bestCells$randCell_5<-paste(NE_raster_bestCells$X5, NE_raster_bestCells$Y5, sep="_")
  NE_raster_bestCells$randCell_6<-paste(NE_raster_bestCells$X6, NE_raster_bestCells$Y6, sep="_")
  NE_raster_bestCells$randCell_7<-paste(NE_raster_bestCells$X7, NE_raster_bestCells$Y7, sep="_")
  NE_raster_bestCells$randCell_8<-paste(NE_raster_bestCells$X8, NE_raster_bestCells$Y8, sep="_")
  
  #combine BestX and BestY
  NE_raster_bestCells$BestCell_comb<-paste(NE_raster_bestCells$BestX, NE_raster_bestCells$BestY, sep="_")
  
  return(NE_raster_bestCells)
  
}