#getCellCount - count number of individuals present within a cell
getCellCount<-function(nextCellCoords,yearName){
  
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
  #cellCount.merge$Move<-moveNum
  
  cellCount.merge<-unique(cellCount.merge)
  
  # xyCoord<-read.table(text=as.character(cellCount$CellCoord), sep="_")
  # cellCount<-data.frame(xyCoord, cellCount)
  # colnames(cellCount)[1:2]<-c("X","Y")
  
  cellCount.merge$CellCoord<-NULL
  row.names(cellCount.merge)<-NULL
  
  return(cellCount.merge)
}
