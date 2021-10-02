#get cummulative sums of Total visits to each cell by year
getCummulativeSums<-function(visited.years.in){
  
  visited.years.data<-unique(visited.years.in[c("X","Y","Year","Count","Iteration")])
  
  yearList<-unique(visited.years.data$Year)
  
  cum.totals.save<-list()
  for(i in 1:length(yearList)){
    print(paste("Year",i))
    visited.sub<-subset(visited.years.data, Year %in% yearList[1:i])
    
    sub.agg.sum<-aggregate(Count~X+Y, FUN="sum",data=visited.sub)
    names(sub.agg.sum)[names(sub.agg.sum)=="Count"]<-"Total_Visits"
    
    sub.agg.df<-sub.agg.sum
    # sub.agg.mean<-aggregate(Count~X+Y, FUN="mean",data=visited.sub)
    # names(sub.agg.mean)[names(sub.agg.mean)=="Count"]<-"Mean_Visits"
    # 
    # sub.agg.sd<-aggregate(Count~X+Y, FUN="sd",data=visited.sub)
    # names(sub.agg.sd)[names(sub.agg.sd)=="Count"]<-"SD_Visits"
    # 
    # sub.agg.df<-merge(merge(sub.agg.sum, sub.agg.mean, by=c("X","Y"),all.x=TRUE),sub.agg.sd, by=c("X","Y"),all.x=TRUE)
    # 
    # #add CV
    # sub.agg.df$CV<-sub.agg.df$SD_Visits/sub.agg.df$Mean_Visits
    
    sub.agg.df$Year<-yearList[i]
    
    cum.totals.save<-rbind(cum.totals.save,sub.agg.df)
  }
  return(cum.totals.save)
}
