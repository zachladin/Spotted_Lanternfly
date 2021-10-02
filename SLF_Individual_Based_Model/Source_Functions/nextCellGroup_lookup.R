#nextCellGroup_lookup
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
  
  #create a CurrentCell_comb column
  lookup.dt$CurrentCell_comb<-paste(lookup.dt$CurrentX,lookup.dt$CurrentY,sep="_")
  
  #subset by all current unique cells
  lookup.sub<-lookup.dt[c(CurrentCell_comb %in% currentCell_comb)]
  
  #add column with freqs
  lookup.sub.merge<-merge(lookup.sub, freq.cells, by="CurrentCell_comb", all=TRUE)
  
  sum(lookup.sub.merge$Freq)
  
  #create rows based on freq.cells (THIS IS WHAT I NEED TO FIX)
  lookup.sub.2<-as.data.frame(lapply(lookup.sub.merge, rep, lookup.sub.merge$Freq))
  
  #generate random integer for each row
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

