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
