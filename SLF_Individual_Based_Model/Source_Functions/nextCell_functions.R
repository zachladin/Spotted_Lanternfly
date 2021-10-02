#1) rank order (by raster values) 9-nearest neighborhood cells for a given current cell, and return best choice (highest value)
getBestCell<-function(myRaster, cellIn){
  currentX=as.integer(cellIn[[1]][1])
  currentY=as.integer(cellIn[[2]][1])
  
  Xplus1=currentX+1
  Xminus1=currentX-1
  Yplus1=currentY+1
  Yminus1=currentY-1
  
  my.x.coords<-c(currentX,Xplus1,Xminus1)
  my.y.coords<-c(currentY,Yplus1,Yminus1)
  
  #all coords
  all.combos<-expand.grid(p1 = my.x.coords, p2 = my.y.coords, stringsAsFactors = FALSE)
  colnames(all.combos)<-c("X","Y")
  
  cell.values<-data.frame("Value" = getValuesBlock(x=myRaster, row=Yminus1, nrows=3, col=Xminus1, ncols=3))
  
  #make d.f. with coords and cell.values
  cell.values.df<-data.frame(all.combos,"Value"= cell.values)
  
  cell.values.ranked<-cell.values.df[order(cell.values.df$Value, decreasing=TRUE),]
  
  bestCell<-cell.values.ranked[1,1:2]
  row.names(bestCell)<-NULL
  
  return(bestCell)
}

#choose a random cell (but don't remain in current cell)
getRandomCell<-function(myRaster, cellIn){
  currentX=as.integer(cellIn[[1]][1])
  currentY=as.integer(cellIn[[2]][1])
  
  Xplus1=currentX+1
  Xminus1=currentX-1
  Yplus1=currentY+1
  Yminus1=currentY-1
  
  my.x.coords<-c(currentX,Xplus1,Xminus1)
  my.y.coords<-c(currentY,Yplus1,Yminus1)
  
  #all coords
  all.combos<-expand.grid(p1 = my.x.coords, p2 = my.y.coords, stringsAsFactors = FALSE)
  colnames(all.combos)<-c("X","Y")
  
  cell.values<-data.frame("Value" = getValuesBlock(x=myRaster, row=Yminus1, nrows=3, col=Xminus1, ncols=3))
  
  #make d.f. with coords and cell.values
  cell.values.df<-data.frame(all.combos,"Value"= cell.values)[-1,]
  
  #select a random integer 1:nCells
  randCell=cell.values.df[sample(1:length(row.names(cell.values.df)), 1),1:2]
  row.names(randCell)<-NULL
  
  return(randCell)
}


#function to choose Cells
nextCellGroup<-function(rasterIn,currentCells,q1,q2){
  #require("Rcpp")
  
  nextCell<-function(x,y,currentCell){ 
    if(x < y) { 
      getRandomCell(myRaster=rasterIn, cellIn= currentCell)
    }  else {
      getBestCell(myRaster=rasterIn, cellIn = currentCell)
    }
  }
  
  #use mapply to map function to each row (vectorized) 
  nextGroupCells<-t(mapply(nextCell, x=q1, y=q2, currentCell=currentCells))
  
  nextGroupCells.df<-as.data.frame(nextGroupCells)
  row.names(nextGroupCells.df)<-NULL

  nextGroupCellsList<-as.numeric(as.character(unlist(split(nextGroupCells.df[,c("X","Y")], 1:nrow(nextGroupCells.df)))))
  
  nextCellsOut<-split(nextGroupCellsList, ceiling(seq_along(nextGroupCellsList)/2))
  
  return(nextCellsOut)
} 


getCellCount<-function(nextCellCoords,yearName, moveNum){
  
  #get counts per cell and save
  nextCell.df<-data.frame(matrix(unlist(myNextCells), nrow=length(myNextCells), byrow=T))
  colnames(nextCell.df)<-c("X","Y")
  
  #add columns
  nextCell.df$CellCoord<-paste(nextCell.df$X, nextCell.df$Y, sep="_")
  
  #add Individual column
  nextCell.df$Individual<-paste("Individual_",seq(1:length(row.names(nextCell.df))), sep="")
  
  #get a count of how many per cell
  cellCount<-as.data.frame(as.matrix(rev(table(nextCell.df$CellCoord))))
  cellCount$CellCoord<-row.names(cellCount)
  names(cellCount)[names(cellCount)=="V1"]<-"Count"
  
  #merge with nextCell.df
  cellCount.merge<-merge(nextCell.df, cellCount, by="CellCoord",all.x=TRUE)
  
  #add timesteps
  cellCount.merge$Year<-yearName
  cellCount.merge$Move<-moveNum
  
  cellCount.merge<-unique(cellCount.merge)
  
  # xyCoord<-read.table(text=as.character(cellCount$CellCoord), sep="_")
  # cellCount<-data.frame(xyCoord, cellCount)
  # colnames(cellCount)[1:2]<-c("X","Y")
  
  cellCount.merge$CellCoord<-NULL
  row.names(cellCount.merge)<-NULL
  
  return(cellCount.merge)
}

#get best cell for new introductions (mediated by human movement)
getBestPotentialCell<-function(myRaster, potentialCellList){
  
  newCells = unique(potentialCellList)
  
  compileCellValues<-list()
  for(y in 1:length(newCells)){
    
    newCell=unlist(newCells[y])
    newCell.df<-data.frame(newX=newCell[1], newY=newCell[2])
    
    cell.values<-data.frame("Value" = getValuesBlock(x=myRaster, row=newCell[2], nrows=1, col=newCell[[1]], ncols=1))
    
    #make d.f. with coords and cell.values
    cell.values.df<-data.frame(newCell.df,"Value"= cell.values)
    
    compileCellValues<-rbind(compileCellValues,cell.values.df)
  }
  
    cell.values.ranked<-compileCellValues[order(compileCellValues$Value, decreasing=TRUE),]
    
    bestPotentialCell.df<-cell.values.ranked[1,1:2]
    row.names(bestPotentialCell.df)<-NULL
    colnames(bestPotentialCell.df)<-NULL
    
  return(bestPotentialCell.df)
}

