#SLF Individual-based Model in R

#clear environment
rm(list=ls())

#set working directory to newly-created folder
setwd("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output")

#roads data
library(tigris)
library(ggplot2)
library(ggthemes)
library(rgeos)
library(sp)
library(mapproj)
library(rgdal)
library(raster)
library(sf)
library(gdalUtils)
library(raster)
library(combinat)
library(viridis)
library(gganimate)
library(ggnewscale)
library(pals)
library(gifski)
library(gapminder)
library(Rcpp)
library(data.table)
library(scales)
library(doParallel)
library(parallel)
library(foreach)
library(dclone)


#source functions
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/SLF_source_functions.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/summaryFunction.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/processSimulationResults.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/xyToLatLong.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/getCummulativeSums.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/formatRasters.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/preComputeBestCells.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/loadMaxENTraster.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/R_Source/getCounties.R")

#################################################################################################################
#get required elements from MaxENT raster surface and county polygons

#read in MaxEnt raster surface
maxEnt_surface<-"/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_grayscale_4km.tiff"

#NE_raster_bestCells<-loadMaxENTraster(rasterIn=maxEnt_surface, saveFileDir="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/NE_raster_bestCells.csv", preCompute = FALSE)

NE_raster_bestCells<-data.table::fread("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/NE_raster_bestCells.csv")

counties.df<-getCounties(SLF_counties="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/SLF_Shiny_App/Data/SLF_County_Level_occurrence_9-08-2021.csv")

#plot with ggplot
ggplot()+
  geom_sf(data=counties.df, aes(fill=SLF_present),color=alpha("gray20",0.4), inherit.aes = FALSE)+
  scale_fill_manual(values=c("gray80","tomato"))

#read in MaxEnt raster surface
maxEnt_surface<-raster("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/Data/Rasters/MaxEnt_grayscale_4km.tiff", values=TRUE)

plot(maxEnt_surface, col=parula(100))


#make spatial polygons
countysp <- as_Spatial(counties.df, IDs=counties.df$GEOID)
#make data.frame
counties.fortify.df <- fortify(countysp)

#get corners of map
minLong<-min(counties.fortify.df$long)
maxLong<-max(counties.fortify.df$long)
minLat<-min(counties.fortify.df$lat)
maxLat<-max(counties.fortify.df$lat)

#get bounds of maxENT_surface
min_X<-0
max_X<-dim(maxEnt_surface)[2]
min_Y<-0
max_Y<-dim(maxEnt_surface)[1]

#get list of all possible (universe)
new.matrix<-matrix(0,nrow=nrow(maxEnt_surface), ncol=ncol(maxEnt_surface))
new.raster<-raster(nrows=nrow(new.matrix), ncols=ncol(new.matrix),xmn=0,ymn=0,xmx=ncol(new.matrix),ymx=nrow(new.matrix))
values(new.raster)<-0

#create a maxEnt_surface with only 0s and NAs
maxEnt_surface_zeros<-maxEnt_surface
maxEnt_surface_zeros[maxEnt_surface_zeros!=58137]<-0
maxEnt_surface_zeros[is.na(maxEnt_surface_zeros)]<-0
maxEnt_surface_zeros[maxEnt_surface_zeros==58137]<-NA
plot(maxEnt_surface_zeros)

#create data.frame with list of all cell XY cords
all.cells.df<-data.frame(floor(rasterToPoints(na.omit(maxEnt_surface_zeros))))[,-3]

#add Lat/Long
#now apply these functions to slf.simu data
all.cells.df$Longitude<-coord2long(inX=all.cells.df$x,minX = min_X, maxX = max_X, minlong = minLong, maxlong = maxLong)
all.cells.df$Latitude<-coord2lat(inY=all.cells.df$y, minY = min_Y, maxY = max_Y, minlat = minLat, maxlat = maxLat)

#convert colnames
names(all.cells.df)[names(all.cells.df)=="x"]<-"X"
names(all.cells.df)[names(all.cells.df)=="y"]<-"Y"

#################################################################################################################
#Set up parallelization for running simulations on multiple cores

#stopCluster(cl)
detectCores()
cl<-parallel::makeCluster(6, setup_strategy = "sequential")
registerDoParallel(cl)

############################################################################################################33
#SET OVERALL PARAMETERS TO RUN THRUOGH FULL SIMULATIONS
#make q2MeanList to iterate over
q2MeanList<-c(0.01,0.99)
growthRateList<-c(0.25,0.5,1.0, 1.5)
HumanPopList<-c(0)

############################################################################################################33
#Nested Loop
for(h in 1:length(HumanPopList)){
  
  humanPops<-HumanPopList[h]
  
  for(z in 1:length(growthRateList)){
    
    growthRate<-growthRateList[z]

  for(k in 1:length(q2MeanList)){
    
    q2MeanIn<-q2MeanList[k]
    
    #define number of iterations 
    iterations=3
    
    #now run simulations
    
    #for(r in 1:iterations){
    foreach(r=1:iterations,.packages=c("data.table","raster","doParallel","parallel","foreach","dclone","coda","Matrix")) %dopar% {
      
      years=13
      PopSize = 1000
      n = PopSize
      ntm1 = n
      nList = list(n)
      startingCells = list(c(438, 197))
      SLF = rep(startingCells, length.out=n)
      q2Mean = q2MeanIn
      q2SD = 0.05
      q2 =rnorm(n,mean=q2Mean, sd=q2SD)
      visited = SLF
      visited2 = SLF
      timeSteps = years
      AdultSurvProb <- function(){runif(n=1,min=0.5,max=0.9)}
      growthRateDiscrete=growthRate
      K=10000000000000000 #carrying capacity
      cellCountSave<-data.frame(X=unique(SLF)[[1]][1],Y=unique(SLF)[[1]][2],Count=n, Individual=paste("Individual_",seq(1:n),sep=""),Year=0)
      
      
      iterationNum = paste("Iteration",r,sep="_")
      print(iterationNum)
      
      visited.years<-list()
      #define cellCountSave with starting cells
      for(i in 1:years){
        
        yearName = i
        print(paste("Iteration",r,"-","Year",yearName))
        
        #message
        message("Getting next cells for each agent.")
        
          #use nextCellGroup function
          myNextCells<-nextCellGroup_lookup(lookupTableIn = NE_raster_bestCells, 
                                     currentCells = SLF,
                                     q1=runif(n=n, min=0, max=1), 
                                     q2=q2)
          
          
          #need to map this over startingCells
          myCellCount<- getCellCount(nextCellCoords=myNextCells,yearName=yearName)
          
          
          #compile results
          cellCountSave<-unique(rbind(cellCountSave, myCellCount))
          #cellCountSave<-rbind(cellCountSave, myCellCount)
          
          #update startingCells
          myCellCount.df<-myCellCount[,c("X","Y")]
          colnames(myCellCount.df)<-NULL
          
          #get list of unique cells
          #startingCells<<-as.list(split(myCellCount.df, seq(nrow(myCellCount.df))))
          
          SLF = myNextCells
          #SLF=myCellCount.df
          
        #try logistic growth model
        ntm1 = round(n*AdultSurvProb())
        
        #message
        message("Computing population size at Nt+1.")
        
        #random additive factor to K (to add environmental stochasticity)
        Vrand<-runif(min=-0.5, max=0.5, n=1)
        #n=ifelse(ntm1<100,round(as.numeric((ntm1+(ntm1*growthRateDiscrete)*(1-(ntm1/K))))),100)
        n=round(as.numeric(ntm1*(1+((growthRateDiscrete/K)*(K-ntm1))+Vrand)))
        
        visited.years<-unique(rbind(visited.years, cellCountSave))
        
        SLF <- try(rep(SLF, length.out=round(n)))
        names(SLF)<-seq_along(1:n)
        
        #normalize between 0 to 1
        range01 <- function(x){(x-min(x))/(max(x)-min(x))}
        
        #mean of normalized (0 to 1) numbers per cell
        mean.normalized.density<-mean(range01(myCellCount$Count))
        
        randProb1<-runif(n=1, min=0, max=1)
        randProb2<-runif(n=1,min=mean.normalized.density, max=1)
        
        #message
        message("Generating potential human-mediated dispersal events.")
        
        #message
        message("Sampling random locations for satellite populations.")
        
        #randomly sample  cells from current cells
        ifelse(randProb1 < randProb2 & humanPops > 0,
               try({
                 #sample as a function of population size
                 nSample<-ifelse(nrow(myCellCount.df)<10000, nrow(myCellCount.df),10000)#should this be larger? #Use this to improve the model
                 
                 #sample rows from myCellCount.df
                 myCells.sample.df<-myCellCount.df[sample(nrow(myCellCount.df), floor(runif(min=1,max=nSample, n=1))), ]
                 
                 #define number of random iterations to gather new cells (will determine the number of new cells evaluated)
                 randNumIterations<-floor(runif(n=1,min=1, max=10))
                 
                 randNumNewPops<-floor(runif(n=1,min=1, max=humanPops*mean.normalized.density))
                 
                 compileNewCellsAll<-list()
                 bestCellsSave<-list()
                 for(m in 1:randNumNewPops){
                   
                   compileNewCells<-list()
                   for(z in 1:randNumIterations){
                     
                     #generate new random cell
                     newRandCellcheck.df<-generateNewCell(myCellCount.df.in=myCells.sample.df, myLookupTable=NE_raster_bestCells)
                     
                     randStartX<-newRandCellcheck.df$newX
                     randStartY<-newRandCellcheck.df$newY
                     
                     #get number of individuals that begin at new location
                     newIndList<-seq.int(from=1, to=60, by=1)
                     
                     #randomly generates number of new SLF to be born from a given randomly generated cell
                     newRandCell<-try(rep(c(randStartX, randStartY), length.out=sample(newIndList,size=1)))
    
                     newRandCellPairs<-as.list(split(newRandCell,ceiling(seq_along(newRandCell)/2)))
                     
                     #remove singletons
                     newRandCellPairsNamesKeep<-names(which(lapply(newRandCellPairs, length)>1))
                     
                     newRandCellPairsOut<-newRandCellPairs[newRandCellPairsNamesKeep]
                     
                     #compile new locations
                     compileNewCells<-append(compileNewCells, newRandCellPairsOut)
        
                   }
                   
                   bestPotentialCells<-getBestPotentialCell_lookup(lookupTableIn = NE_raster_bestCells,
                                                                   potentialCellList=compileNewCells)
                   
                   bestPotentialCells.out<-split(unlist(bestPotentialCells),1)
                   
                   bestCellsSave<-c(bestCellsSave, bestPotentialCells.out)
                 }
                  
                   #format to correct list of coords
                   names(bestCellsSave)<-seq_along(bestCellsSave)
                     
                   bestPotentialCellsKeep<-lapply(bestCellsSave, function(x) lapply(compileNewCells, function(y) intersect(x,y)))
                     
                   #now filter out numeric(0)s within list
                   bestPotentialCellsFilter<-unlist(lapply(bestPotentialCellsKeep, function(i) Filter(function(x) any(x != 0), i)), recursive=FALSE)
    
                   compileNewCellsAll<-bestPotentialCellsFilter
                   #remove any coordinates that don't have x and y values
                   compileNewCellsAllCheck<-compileNewCellsAll[lengths(compileNewCellsAll)>1]

                   SLF<-append(SLF, compileNewCellsAllCheck)
                 
               }),
               SLF
        )
        
        #get new length of SLF
        n=length(SLF)
        print(paste("Pop. Size N:",n))
        
        q2 =rnorm(n,mean=q2Mean, sd=q2SD)
        
      }
      
      #message
      message("Compiling and saving iteration results.")
      
      #compile results from iterations
      iteration.save<-visited.years
      iteration.save$Iteration<-iterationNum
      
      new.folder.dir<-paste("HumanPops_",humanPops,"_Years_",years,"_GrowthRate_", growthRateDiscrete,"_q2_mean_",q2Mean,"_q2_sd_",q2SD, sep="")
      
      dir.create(paste(getwd(), new.folder.dir, sep="/"))
      
      #perhaps save as .Rdata instead?
      write.csv(iteration.save, file=paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/", new.folder.dir, paste("HumanPops_",humanPops,"_Years_", years,"_GrowthRate_",growthRateDiscrete,"_q2_mean_",q2Mean,"_q2_sd_",q2SD,"_Iteration_",r,"_",Sys.time(),".csv",sep=""),sep="/"),row.names=FALSE)
      
    }
   }
  }
}


###################################################################################################################################
#now set up loops to process and generate rasters

#set my Google API key
myAPIkey = "YOUR KEY"
register_google(myAPIkey)

##############################################################################
#processing simulation results

#get list of files within Simulation_Output
simuFileList<-list.files("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output", full.names = TRUE)

#remove any folders with "_Compiled" in the name
simuFileListKeep<-simuFileList[! grepl("_Compiled", simuFileList)]

#get only files with HumanPops_0 and Years_14
simuFileListKeep1<-simuFileListKeep[grepl("20",simuFileListKeep)]
#simuFileListKeep2<-simuFileListKeep[grepl("HumanPops_0",simuFileListKeep)]

# simuFileListKeepAll<-c(simuFileListKeep1, simuFileListKeep2)
# simuFileListKeep<-simuFileListKeepAll

# #with GrowthRate_1.5
# simuFileListKeep1<-simuFileListKeep[grepl("1.5",simuFileListKeep)]
simuFileListKeep<-simuFileListKeep1
# 
# simuFileListKeep<-c("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/HumanPops_0_Years_13_GrowthRate_1.5_q2_mean_0.01_q2_sd_0.05")


#for(i in 1:length(simuFileListKeep)){
# foreach(i=1:length(simuFileListKeep),.packages=c("data.table","raster","doParallel","parallel","foreach","dclone","coda","Matrix")) %dopar% {
for(i in 1:length(simuFileListKeep)){ 
  newFileDir<-simuFileListKeep[i]

  #run function to process simulation results
  processSimulationResults(fileDir=newFileDir, minX = min_X, maxX=max_X, minY=min_Y, maxY=max_Y, minlong=minLong, maxlong=maxLong,minlat = minLat, maxlat=maxLat)
  
      }

######################################################################################################################
#now loop through and generate rasters

#get list of files within Simulation_Output
simuFileList<-list.files("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output", full.names = TRUE)

#remove any folders with "_Compiled" in the name
simuFileListCompiled<-simuFileList[grepl("_Compiled", simuFileList)]

simuFileListCompiledKeep1<-simuFileListCompiled[grepl("20", simuFileListCompiled)]

simuFileListCompiledKeep<-c(simuFileListCompiledKeep1[2],
                            simuFileListCompiledKeep1[4],
                            simuFileListCompiledKeep1[5],
                            simuFileListCompiledKeep1[6],
                            simuFileListCompiledKeep1[7],
                            simuFileListCompiledKeep1[8],
                            simuFileListCompiledKeep1[9],
                            simuFileListCompiledKeep1[10])

#for(i in 1:length(simuFileListCompiledKeep)){
foreach(i=1:length(simuFileListCompiledKeep),.packages=c("data.table","raster","doParallel","parallel","foreach","dclone","coda","Matrix")) %dopar% {
    
  new.simu.data<-read.csv(list.files(simuFileListCompiledKeep[i], full.names = TRUE))
  
  newFolderName<-gsub("Combined_counts_","",gsub(".csv","",list.files(simuFileListCompiledKeep[i], full.names = FALSE)))
  
  newFolderNameFull<-paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output_rasters",newFolderName,sep="/")
  
  formatRasters(dataIn=new.simu.data, folderName=newFolderNameFull)

}

#################################################################################################################
#use function to format and save raster stacks










###################################################################################################################################
#compile iteration output results
#write.csv(simulations.save, file="/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/Simulations_7yr_growthRate7.csv",row.names=FALSE)

#############################################################################################################
#read in saved data
visited.years<-read.csv("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/Simulation_Output/Simulations_15yr_growthRate2_Iteration_2020-08-21 02:50:58.csv")

#get sum of all visits to each unique cell
#visited.order<-unique(visited.years[,c("X","Y","Count")])


#try converting to data.tables, then merging to increase the speed this takes!

#get cummulative sums of Total visits to each cell by year
getCummulativeSums<-function(visited.years.in){
  
  visited.years.data<-visited.years.in
  
  yearList<-unique(visited.years.data$Year)
  
  cum.totals.save<-list()
  for(i in 1:length(yearList)){
    print(paste("Year",i))
    visited.sub<-subset(visited.years.data, Year %in% yearList[1:i])
    sub.agg<-aggregate(Count~X+Y, FUN="sum",data=visited.sub)
    names(sub.agg)[names(sub.agg)=="Count"]<-"Total_Visits"
    sub.agg$Year<-yearList[i]
    
    cum.totals.save<-rbind(cum.totals.save,sub.agg)
  }
  return(cum.totals.save)
}

#get cummulative sums of total visits for each year
visited.cum.sums<-getCummulativeSums(visited.years.in=visited.years)
  
# #take sum of count at any given cell across ALL years
# visited.sum<-aggregate(Count~X+Y, FUN="sum",data=visited.years)
# names(visited.sum)[names(visited.sum)=="Count"]<-"Total_Visits"

#merge all
#visited.merge.all<-merge(unique(visited.years), visited.sum, by=c("X","Y"),all=TRUE)
# visited.merge<-merge(unique(visited.years), visited.cum.sums, by=c("X","Y","Year"),all.x=TRUE)
# 
# visited.years.dt<-as.data.table(unique(visited.years))
# visited.cum.sums.dt<-as.data.table(visited.cum.sums)
# 
# visited.merge.dt<-merge(visited.years.dt, visited.cum.sums.dt, by=c("X","Y","Year"),all.x=TRUE)



#make Year a factor
visited.cum.sums$Year<-as.factor(visited.cum.sums$Year)

#sort by Year
visited.cum.sums.sort<-visited.cum.sums[order(visited.cum.sums$Year,decreasing=FALSE),]
levels(visited.cum.sums.sort$Year)

#plot visited cells with raster
visitedRasterPlot<-ggplot()+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"))+
  geom_tile(data=NE_surface.df, aes(x=x, y=y, fill=MaxEnt_grayscale_4km), alpha=0.4, show.legend = FALSE)+
  scale_fill_gradient(low="black",high="white")+
  new_scale_fill() +   # same as `new_scale("color")`
  geom_tile(data=subset(visited.cum.sums.sort, Year==max(as.integer(as.character(visited.cum.sums.sort$Year)))), aes(x=X, y=Y, fill=Total_Visits),alpha=0.9,inherit.aes = FALSE)+
  scale_fill_gradientn(colors=parula(100),labels=comma)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE,
              clip = "on")

#ggsave(visitedRasterPlot, file="/Users/zach/Desktop/test.png",width=7, height=7,units="in")
visitedRasterPlot


#plot visited cells with raster for each year
visitedRasterPlotYears<-ggplot()+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"))+
  geom_tile(data=NE_surface.df, aes(x=x, y=y, fill=MaxEnt_grayscale_4km), alpha=0.4, show.legend = FALSE)+
  scale_fill_gradient(low="black",high="white")+
  new_scale_fill() +   # same as `new_scale("color")`
  geom_tile(data=subset(visited.cum.sums.sort, Year!="0"), aes(x=X, y=Y, fill=Total_Visits),alpha=0.8,inherit.aes = FALSE)+
  # scale_fill_gradientn(colors=parula(100),limits=c(0,3100000000),breaks=c(0,1000000000,2000000000,3000000000), 
  #                      labels=c("0","1 billion","2 billion","3 billion"))+
  scale_fill_gradientn(colors=parula(100),labels=comma)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE,
              clip = "on")

#now facet by year
visitedRasterPlotYears<-visitedRasterPlotYears+facet_wrap(.~Year)
visitedRasterPlotYears

#make IndYearMove factor
# visited.merge$IndYearMove<-factor(paste(visited.merge$Individual, visited.merge$YearMove,sep="_"))

#get unique rows
#visited.merge.unique<-unique(visited.merge)


#unique(visited.merge.sort$Year)

#animate plot
visitedRasterPlot<-ggplot()+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"),
        plot.title=element_text(size=88),
        legend.key.size = unit(3.5, "cm"),
        legend.text = element_text(size=40),
        legend.title=element_text(size=50))+
  geom_tile(data=NE_surface.df, aes(x=x, y=y, fill=MaxEnt_grayscale_4km), alpha=0.4,show.legend = FALSE)+
  scale_fill_gradient(low="black",high="white")+
  new_scale_fill() +   # same as `new_scale("color")`
  geom_tile(data=visited.merge.sort, aes(x=X, y=Y, fill=Total_Visits),alpha=0.3, inherit.aes = TRUE)+
  scale_fill_gradientn(colors=parula(100),limits=c(0,3100000000),breaks=c(1000000000,2000000000,3000000000), 
                       labels=c("1 billion","2 billion","3 billion"))+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE,
              clip = "on")+
  # transition_time(YearMove) +
  # shadow_mark()+
  transition_states(Year, wrap=FALSE)+
  enter_fade()+
  ease_aes('cubic-in-out')+
  labs(title = paste("Year ","{closest_state}"))

gganimate::anim_save(animation=animate(visitedRasterPlot,start_pause=3, end_pause=10, duration=25, height = 2000, width =2000), renderer=gifski_renderer(), filename = paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/GIFs/", paste("Simulations_",years,"yr_growthRate",growthRateDiscrete,"_Iteration","_",Sys.time(),".gif",sep=""),sep=""))

#########################################################################################################################testing animation settings
simulation.data<-subset(visited.cum.sums.sort, Year != "0")

#simulation.data<-subset(visited.cum.sums.sort, Year %in% c("10","11","12"))
simulation.data$Year<-factor(simulation.data$Year, levels=as.character(sort(unique(as.integer(as.character(simulation.data$Year))))))
                             
levels(simulation.data$Year)

#add YearNum column
#simulation.data$YearNum<-as.numeric(as.character(simulation.data$Year))

#animate plot
visitedRasterPlot<-ggplot()+
  theme(panel.background=element_rect(fill="white"),
        panel.border=element_rect(color="black",fill="transparent"),
        plot.title=element_text(size=88),
        legend.key.size = unit(3.5, "cm"),
        legend.text = element_text(size=40),
        legend.title=element_text(size=50))+
  geom_tile(data=NE_surface.df, aes(x=x, y=y, fill=MaxEnt_grayscale_4km), alpha=0.4,show.legend = FALSE)+
  scale_fill_gradient(low="black",high="white")+
  new_scale_fill() +   # same as `new_scale("color")`
  geom_tile(data=simulation.data, aes(x=X, y=Y, fill=Total_Visits),alpha=0.9, inherit.aes = TRUE)+
  scale_fill_gradientn(colors=parula(100),labels=comma)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE,
              clip = "on")+
  #transition_states(Year, wrap=FALSE, transition_length = 1, state_length = 2)+
  transition_manual(Year)+
  enter_fade()+
  #exit_fade()+
  ease_aes('cubic-in-out')+
  labs(title = paste("Year ","{current_frame}"))

gganimate::anim_save(animation=animate(visitedRasterPlot,start_pause=1, end_pause=1, duration=20, height = 2500, width =2500), renderer=gifski_renderer(), filename = paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/SLF_Individual_Based_Model/R_Models/Results/GIFs/", paste("Simulations_",years,"yr_growthRate",growthRateDiscrete,"_Iteration","_",Sys.time(),".gif",sep=""),sep=""))

