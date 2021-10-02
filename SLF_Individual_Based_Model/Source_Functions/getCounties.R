#set up counties

getCounties<-function(SLF_counties){
  require(sf)
  require(maps)
  
  #crop raster to mid-Atlantic region
  counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
  
  #check projection
  st_crs(counties)
  
  #subset counties to only inclued Northeast/Mid-Atlantic region
  counties.sub <- subset(counties, c(grepl("maine", counties$ID) |
                                       grepl("vermont", counties$ID) |
                                       grepl("massachusetts", counties$ID) |
                                       grepl("new hampshire", counties$ID) |
                                       grepl("rhode island", counties$ID) |
                                       grepl("connecticut", counties$ID) |
                                       grepl("new york", counties$ID) |
                                       grepl("pennsylvania", counties$ID) |
                                       grepl("new jersey", counties$ID) |
                                       grepl("maryland", counties$ID) |
                                       grepl("delaware", counties$ID) |
                                       grepl("virginia", counties$ID) |
                                       grepl("west virginia", counties$ID) |
                                       grepl("ohio", counties$ID) |
                                       grepl("north carolina", counties$ID) |
                                       grepl("kentucky", counties$ID) |
                                       grepl("tennessee", counties$ID)))
  
  #remove 3 counties not in states
  removeCountyList<-c("oklahoma,delaware","indiana,delaware","indiana,ohio","iowa,delaware")
  counties.sub.2<-subset(counties.sub, ! ID %in% removeCountyList)
  
  #plot with ggplot
  #get a list of all counties where SLF were detected
  SLF_county_occurence<-read.csv(SLF_counties)
  
  #create column with "state,county" lowercase
  SLF_county_occurence$ID<-paste(tolower(SLF_county_occurence$State),tolower(SLF_county_occurence$County),sep=",")
  
  #add factor level to counties.sub.2
  counties.sub.2$SLF_present<-ifelse(counties.sub.2$ID %in% SLF_county_occurence$ID,"Present","Absent")
  #make a factor
  counties.sub.2$SLF_present<-as.factor(counties.sub.2$SLF_present)
  
  return(counties.sub.2)
  
}