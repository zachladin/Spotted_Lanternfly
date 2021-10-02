#Source functions for SLF spread model


################################################################################################################################################
#Section 1: functions that get values from input raster

#nextCellGroup_disperse
nextCellGroup_disperse<-function(rasterIn, currentCells,q1,q2){
  #require("Rcpp")
  
  #nextCell_disperse
  nextCell_disperse<-function(x,y,currentCell){ 
    if(x < y) { 
      getRandomCell_disperse(cellIn = currentCell)
    }  else {
      getBestCell_disperse(myRaster=rasterIn, cellIn = currentCell)
    }
  }
  
  #use mapply to map function to each row (vectorized) 
  nextGroupCells<-t(mapply(nextCell_disperse, x=q1, y=q2, currentCell=currentCells))
  
  nextGroupCells.df<-as.data.frame(nextGroupCells)
  row.names(nextGroupCells.df)<-NULL
  
  nextGroupCellsList<-as.numeric(as.character(unlist(split(nextGroupCells.df[,c("X","Y")], 1:nrow(nextGroupCells.df)))))
  
  nextCellsOut<-split(nextGroupCellsList, ceiling(seq_along(nextGroupCellsList)/2))
  
  return(nextCellsOut)
} 


#getRandomCell_disperse - choose a random cell (but don't remain in current cell)
getRandomCell_disperse<-function(cellIn){
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
  
  #don't need to compute values, if just choosing randomly!
  #cell.values<-data.frame("Value" = getValuesBlock(x=myRaster, row=Yminus1, nrows=3, col=Xminus1, ncols=3))
  cell.values<-data.frame("Value" = NA)
  
  #make d.f. with coords and cell.values
  cell.values.df<-data.frame(all.combos,"Value"= cell.values)[-1,]
  
  #select a random integer 1:nCells
  randCell=cell.values.df[sample(1:length(row.names(cell.values.df)), 1),1:2]
  row.names(randCell)<-NULL
  
  return(randCell)
}


#getBestCell_disperse - for each cell in the input raster, compute
getBestCell_disperse<-function(myRaster, cellIn){
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
  
  cell.values<-data.frame("Value" = raster::getValuesBlock(x=myRaster, row=Yminus1, nrows=3, col=Xminus1, ncols=3))
  
  #make d.f. with coords and cell.values
  cell.values.df<-data.frame(all.combos,"Value"= cell.values)[-1,]
  
  #edge case, if cells are NAs (i.e., off the grid, draw randomly)
  cell.values.ranked<-tryCatch({
    cell.values.df[order(cell.values.df$Value, decreasing=TRUE),]
  }, error=function(cond2){
    cond2 = cell.values.df
    cond2
  })
  
  ifelse(! is.na(cell.values.ranked[1,]$Value),
         bestCell<-cell.values.ranked[1,1:2], bestCell<-data.frame(X=NA, Y=NA))
  
  row.names(bestCell)<-NULL
  
  return(bestCell)
}

################################################################################################################################################
#Section 2: functions that get values from input lookup table of pre-computed best Cells

################################################################################################################################################
#testing area

#function to choose Cells via lookup table
nextCellGroup_lookup<-function(lookupTableIn,currentCells,q1,q2){
  #require("Rcpp")
  
  #first make data.table
  currentCell.dt<-data.table(currentCells)
  
  currentCell.df<-data.frame(X=substr(currentCell.dt$currentCells,3,5), Y=substr(currentCell.dt$currentCells,8,10))
  
  currentCell_comb<-paste(currentCell.df[,1], currentCell.df[,2],sep="_")
  
  freq.cells<-data.frame(table(currentCell_comb))
  colnames(freq.cells)[1]<-"CurrentCell_comb"
  
  #convert lookupTableIn to data.table format
  lookup.dt<-as.data.table(lookupTableIn)
  #subset by all current unique cells
  lookup.sub<-lookup.dt[c(CurrentCell_comb %in% currentCell_comb)]
  
  #add column with freqs
  lookup.sub<-merge(lookup.sub, freq.cells, by="CurrentCell_comb", all=TRUE)
  
  sum(lookup.sub$Freq)
  
  #create rows based on freq.cells (THIS IS WHAT I NEED TO FIX)
  lookup.sub.2<-as.data.frame(lapply(lookup.sub, rep, lookup.sub$Freq))
  
  #generrate random integer for each row
  lookup.sub.2$randInt<-sample(seq(from=1, to=8),replace=TRUE,size=length(row.names(lookup.sub.2)))

  #add randIntX and randIntY
  lookup.sub.2$randIntX<-paste("X",lookup.sub.2$randInt,sep="")
  lookup.sub.2$randIntY<-paste("Y",lookup.sub.2$randInt,sep="")
  
  #add q1 and q2 to data.frame
  lookup.sub.2$q1<-q1
  lookup.sub.2$q2<-q2
  
  lookup.sub.2$randCellSelect<- ifelse(lookup.sub.2$randInt==1, lookup.sub.2[,"randCell_1"],
                                      ifelse(lookup.sub.2$randInt==2, lookup.sub.2[,"randCell_2"],
                                             ifelse(lookup.sub.2$randInt==3, lookup.sub.2[,"randCell_3"],
                                                    ifelse(lookup.sub.2$randInt==4, lookup.sub.2[,"randCell_4"],
                                                           ifelse(lookup.sub.2$randInt==5, lookup.sub.2[,"randCell_5"],
                                                                  ifelse(lookup.sub.2$randInt==6, lookup.sub.2[,"randCell_6"],
                                                                         ifelse(lookup.sub.2$randInt==7, lookup.sub.2[,"randCell_7"],
                                                                                lookup.sub.2[,"randCell_8"])))))))

  lookup.sub.2$nextCell_comb<- ifelse(lookup.sub.2$q1 < lookup.sub.2$q2 | is.na(lookup.sub.2$BestX),
                                        lookup.sub.2[,c("randCellSelect")],
                                        lookup.sub.2[,c("BestCell_comb")])
  
  #now separate nextCellX and nextCellY
  lookup.sub.2$nextCellX<-as.integer(substr(lookup.sub.2$nextCell_comb,1,3))
  lookup.sub.2$nextCellY<-as.integer(substr(lookup.sub.2$nextCell_comb,5,8))
  
  
  nextGroupCells.df<-as.data.frame(lookup.sub.2[,c("nextCellX","nextCellY")])
  

  #use mapply to map function to each row (vectorized) 
  row.names(nextGroupCells.df)<-NULL
  
  nextGroupCellsList<-as.numeric(as.character(unlist(split(nextGroupCells.df, 1:nrow(nextGroupCells.df)))))
  
  nextCellsOut<-split(nextGroupCellsList, ceiling(seq_along(nextGroupCellsList)/2))
  
  return(nextCellsOut)
} 





################################################################################################################################################


#function to choose Cells via lookup table
# nextCellGroup_lookup<-function(lookupTableIn,currentCells,q1,q2){
#   #require("Rcpp")
#   
#   nextCell_disperse_lookup<-function(x,y,currentCell){ 
#     if(x < y) { 
#       getRandomCell_disperse_lookup(cellIn= currentCell, myLookupTable=lookupTableIn)
#     }  else {
#       getBestCell_disperse_lookup(cellIn = currentCell, myLookupTable=lookupTableIn)
#     }
#   }
#   
#   #use mapply to map function to each row (vectorized) 
#   nextGroupCells<-t(mapply(nextCell_disperse_lookup, x=q1, y=q2, currentCell=currentCells))
#   
#   nextGroupCells.df<-as.data.frame(nextGroupCells)
#   row.names(nextGroupCells.df)<-NULL
#   
#   nextGroupCellsList<-as.numeric(as.character(unlist(split(nextGroupCells.df, 1:nrow(nextGroupCells.df)))))
#   
#   nextCellsOut<-split(nextGroupCellsList, ceiling(seq_along(nextGroupCellsList)/2))
#   
#   return(nextCellsOut)
# } 


#choose a random cell (but don't remain in current cell)
# getRandomCell_disperse_lookup<-function(cellIn,myLookupTable){
#   
#   currentX=as.integer(cellIn[[1]][1])
#   currentY=as.integer(cellIn[[2]][1])
#   
#   lookup.dt<-as.data.table(myLookupTable)
#   
#   lookup.sub<-lookup.dt[c(CurrentX==currentX & CurrentY==currentY)]
#   
#   my.rand.int<-sample(seq(from=1, to=8),size=1)
#   
#   myCols<-c(paste("X",my.rand.int,sep=""), paste("Y",my.rand.int,sep=""))
#   
#   myCells<-as.data.frame(lookup.sub[,.SD, .SDcols=myCols])
#   
#   row.names(myCells)<-NULL
#   
#   return(myCells)
# }


#getBestCelldisperse_lookup - for each cell in the input raster, compute
# getBestCell_disperse_lookup<-function(cellIn,myLookupTable){
# 
#   currentX=as.integer(cellIn[[1]][1])
#   currentY=as.integer(cellIn[[2]][1])
# 
#   lookup.dt<-as.data.table(myLookupTable)
#   
#   lookup.sub<-lookup.dt[c(CurrentX==currentX & CurrentY==currentY)]
#   
#   #if BestCells are na, pick randomly, otherwise, pick Best
#   if(! is.na(lookup.sub[,.SD, .SDcols="BestX"])){
#     myCells<-as.data.frame(lookup.sub[,c("BestX","BestY")])
#   } else{
#     my.rand.int<-sample(seq(from=1, to=8),size=1)
#     myCols<-c(paste("X",my.rand.int,sep=""), paste("Y",my.rand.int,sep=""))
#     myCells<-as.data.frame(lookup.sub[,.SD, .SDcols=myCols])
#   }
#   
#   row.names(myCells)<-NULL
#   
#   return(myCells)
# }

################################################################################################################################################
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
################################################################################################################################################
#function to generate new random cells
generateNewCell<-function(myCellCount.df.in, myLookupTable){
  
  #select random X and Y from within current occupied area, and add random value to each
  randStartX<-ceiling(runif(n=1, min=min(myCellCount.df[1],na.rm=TRUE), max=max(myCellCount.df[1],na.rm=TRUE))+floor(rnorm(n=1,mean=0, sd=30)))
  randStartY<-ceiling(runif(n=1, min=min(myCellCount.df[2],na.rm=TRUE), max=max(myCellCount.df[2],na.rm=TRUE))+floor(rnorm(n=1,mean=0, sd=30)))
  
  #comobine
  randXY<-paste(randStartX, randStartY, sep="_")
  
  # ifelse(! randXY  %in% myLookupTable$CurrentCell_comb,
  #        {
  #          randStartX<-ceiling(runif(n=1, min=min(myCellCount.df[1]), max=max(myCellCount.df[1]))+floor(rnorm(n=1,mean=0, sd=5)))
  #          randStartY<-ceiling(runif(n=1, min=min(myCellCount.df[2]), max=max(myCellCount.df[2]))+floor(rnorm(n=1,mean=0, sd=5)))
  #          randXY<-paste(randStartX, randStartY, sep="_")
  #          
  #        },
  #        randXY<-paste(randStartX, randStartY, sep="_")
  #        )
  
  while(! randXY  %in% myLookupTable$CurrentCell_comb){
      #select random X and Y from within current occupied area, and add random value to each
      randStartX<-ceiling(runif(n=1, min=min(myCellCount.df[1],na.rm=TRUE), max=max(myCellCount.df[1],na.rm=TRUE))+floor(rnorm(n=1,mean=0, sd=30)))

      randStartY<-ceiling(runif(n=1, min=min(myCellCount.df[2],na.rm=TRUE), max=max(myCellCount.df[2],na.rm=TRUE))+floor(rnorm(n=1,mean=0, sd=30)))

      #combine
      randXY<-paste(randStartX, randStartY, sep="_")
    }
  
  newRandCellcheck.df<-data.frame(newX=randStartX, newY=randStartY,newXY=randXY)
  
  return(newRandCellcheck.df)
}

################################################################################################################################################
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

################################################################################################################################################
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

################################################################################################################################################
#getAllCells - used to generate lookup table from MaxEnt model surface - only need to run once.
getAllCells<-function(rasterIn, cellIn){
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
  
  cell.values<-data.frame("Value" = raster::getValuesBlock(x=rasterIn, row=Yminus1, nrows=3, col=Xminus1, ncols=3))
  
  #make d.f. with coords and cell.values
  cell.values.df<-data.frame(all.combos,"Value"= cell.values)[-1,]
  
  #edge case, if cells are NAs (i.e., off the grid, draw randomly)
  cell.values.ranked<-tryCatch({
    cell.values.df[order(cell.values.df$Value, decreasing=TRUE),]
  }, error=function(cond2){
    cond2 = cell.values.df
    cond2
  })
  
  ifelse(! is.na(cell.values.ranked[1,]$Value),
         bestCell<-cell.values.ranked[1,1:2], bestCell<-data.frame(X=NA, Y=NA))
  
  #getBestCell for each cell
  new.bestCell<-data.frame(cellIn, bestCell)
  colnames(new.bestCell)<-c("CurrentX","CurrentY","Value","BestX","BestY")
  row.names(new.bestCell)<-NULL
  
  #cbind all adjacent cells (not including current cell)
  all.combos.list<-data.frame(t(unlist(all.combos[-1,])))
  row.names(all.combos.list)<-NULL
  all.combos.order<-all.combos.list[,c("X1","Y1","X2","Y2","X3","Y3","X4","Y4","X5","Y5","X6","Y6","X7","Y7","X8","Y8")]
  
  allCells.out<-data.frame(new.bestCell, all.combos.order)
  
  return(allCells.out)
}
