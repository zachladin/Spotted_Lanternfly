#pre-compute best cells from a raster

preComputeBestCells<-function(dataIn,rasterIn,saveFilePath){
  #Pre-compute the Best next cell and all adjacent cells (for random choice) for every given cell
  
  new.df<-dataIn
  newRaster = rasterIn
  
  save.BestCells<-list()
  for(i in 1:nrow(new.df)){
    print(i)

    #get current cell
    new.cell<-new.df[i,]

    #getBestCell for each cell
    new.bestCell<-getAllCells(rasterIn=newRaster,cellIn=new.cell)

    #get all adjacent cells
    save.BestCells<-rbind(save.BestCells,new.bestCell)

  }

  #write .csv
  write.csv(save.BestCells, file=saveFilePath,row.names=FALSE)
  
}