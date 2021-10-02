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

