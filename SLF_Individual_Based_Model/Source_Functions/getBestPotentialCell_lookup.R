#get best cell for new introductions (mediated by human movement) using lookup table
getBestPotentialCell_lookup<-function(lookupTableIn, potentialCellList){
  
  newCells = t(as.data.frame(unique(potentialCellList)))
  newCells_comb.df<-data.frame(newCells_comb=paste(newCells[,1], newCells[,2],sep="_"))
  
  #convert lookupTableIn to data.table format
  lookup.dt<-as.data.table(lookupTableIn)
  #subset by all current unique cells
  lookup.sub<-lookup.dt[c(CurrentCell_comb %in% newCells_comb.df$newCells_comb)]
  
  #rank order by Value
  cell.values.ranked<-as.data.frame(lookup.sub[order(lookup.sub$Value, decreasing=TRUE),])
  
  bestPotentialCell.df<-cell.values.ranked[1,1:2]
  row.names(bestPotentialCell.df)<-NULL
  colnames(bestPotentialCell.df)<-NULL
  
  return(bestPotentialCell.df)
}
