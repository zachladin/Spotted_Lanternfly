library(shiny)
library(shinythemes)
library(shinydashboard)
library(webshot)
library(shinyjs)
library(htmltools)
library(ggmap)
library(ggplot2)
library(googleway)
library(jpeg)
library(imager)
library(magick)
library(adimpro)
library(reticulate)
library(raster)
library(leaflet)

# This package is required for Accessing APIS (HTTP or HTTPS URLS from Web)
library(httr)
#This package exposes some additional functions to convert json/text to data frame
library(rlist)
#This package exposes some additional functions to convert json/text to data frame
library(jsonlite)
#This library is used to manipulate data
library(dplyr)

myDir<-"/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App"

#load helper functions
source(paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App","helpers.R",sep="/"))

#fill in Species CommonName (to put each species in separate folders)
#SpeciesName = "Mimosa"

# #create folders
# dir.create(paste(myDir,"Saved_Images",sep="/")) #Saved_Images
# dir.create(paste(myDir,"Temp",sep="/")) #Temp folder to hold current screenshot)
# dir.create(paste(myDir,"Saved_Images",SpeciesName,sep="/")) #Species folder
# dir.create(paste(myDir,"Saved_Images",SpeciesName,paste(SpeciesName,"Screenshots",sep="_"),sep="/")) # Screenshots folder
# dir.create(paste(myDir,"Saved_Images",SpeciesName,paste(SpeciesName,"Streetview",sep="_"),sep="/")) # Streetview dowloads folder

##############################################################################
#function to capture screenshot and modify image

screenCapture<-function(directory,SpeciesName,location,n,imgDate){
  
  #get position
  #target_pos <-data.frame(lat=39.689349,lon=-75.760289)
  target_pos <-data.frame(lat=location[1],lon=location[2])
  
  #get Lat and Long coords separately
  LAT=target_pos$lat
  LONG=target_pos$lon
  
  #set save directories
  saveDirStreetview = paste(myDir,"Saved_Images",SpeciesName, paste(SpeciesName, "Streetview",sep="_"),sep="/")
  dir.create(saveDirStreetview)
  
  saveDirScreenshot<-paste(myDir,"Saved_Images",SpeciesName,paste(SpeciesName,"Screenshots",sep="_"),sep="/")
  
  #new.img<-load.image(paste(saveDir, paste("screenshot", "png",sep="."),sep="/"))
  
  new.img<-load.image(paste(myDir, "Temp",paste("screenshot", "jpg",sep="."),sep="/"))
  #plot(new.img)

  
  #***********  MESSAGE from Andrew Brenner  ****************#
  # BEWARE of the dimensions of img.crop below
  # x>1950 may need to be increased for longer addresses / larger address box
  #*****************      **************      ***************#
  
  
  #crop image
  img.crop<-imsub(new.img,x<=3150,x>1300, y>300, y<850)#First 30 columns and rows
  #plot(img.crop)

  #dim(img.crop)

  #save image
  jpeg(paste(saveDirScreenshot,paste(SpeciesName,"Screenshot",n,LAT, LONG,'.jpeg',sep="_"), sep="/"))
  
  par(mar = rep(0, 4),xpd=NA)
  plot(img.crop, axes=FALSE, xaxs="i",yaxs="i")

  dev.off()

  #read in image as image magick object
  new.img<-image_read(paste(saveDirScreenshot,paste(SpeciesName,"Screenshot",n,LAT, LONG,'.jpeg',sep="_"), sep="/"))

  new.img.crop<-image_trim(new.img,fuzz=20)

  #write image
  image_write(new.img.crop, paste(saveDirScreenshot,paste(SpeciesName,"Screenshot",n,LAT, LONG,"Date",imgDate,'.jpeg',sep="_"), sep="/"), quality=100,density=1200)
  
  unlink(paste(saveDirScreenshot,paste(SpeciesName,"Screenshot",n,LAT, LONG,'.jpeg',sep="_"), sep="/"))
  
}
################################################################################
#UI
ui <- dashboardPage(
  
  skin = "green",
  
  dashboardHeader(title = "Tree Street View Image Downloader", titleWidth = 450),
  
  dashboardSidebar(
      width = 250,
      useShinyjs(),
      #tags$style(appCSS),
      br(),
      
      #textInput("choose_location", "Choose a location:" , "Ex. Newark, DE"),
      h3("Select a location by clicking on the map."),
      
      hr(),
      
      h4("Longitude, Latitude:"),
      h4(textOutput("MapCoords"), style="color:orange"),
    
      hr(),
      
      selectInput("SpeciesIn", 
                  label = "Select species",
                  choices = c("Tree_of_Heaven", 
                              "Black_Walnut",
                              "Mimosa", 
                              "Staghorn_Sumac"),
                  selected = "Tree_of_Heaven"),
      
      br(),
  
      withBusyIndicatorUI(
      actionButton(
        "downloadImage",
        "Download Street View Images",
        class = "btn-primary"
        )
      ),
      
      br(),

      
      withBusyIndicatorUI(
        actionButton(
          "screenshot",
          "Save Screen Shot",
          class = "btn-primary"
        )
      ),
      
      
      textOutput("SpeciesNameText"),
      
      br(),
      hr()

      # textInput(
      #   inputId="SpeciesText", label="Enter Species Name:",
      #   verbatimTextOutput("value")),
    ),
      
    dashboardBody(
      column(width = 12,
      box(width = 12,
        google_mapOutput(outputId = "pano")
        )
      ),
      column(width =12,
      box(width = 12,
        google_mapOutput(outputId = "map")
      )
    ),
    br()
  )
  
  
)

#######################################################################################################################################
#server
server <- function(input, output, session) {
  
  if (interactive()) {
    
    #google API key
    set_key("AIzaSyAaDHA8482JIVvEv8yFPPcpvUukZ1j69zo")
    ## your valid API key here
    StreetViewKey <- "AIzaSyAaDHA8482JIVvEv8yFPPcpvUukZ1j69zo"
    
    #google map
    output$map <- renderGoogle_map({
      #set target_pos to Newark DE or get coords from map_click
      
      ifelse(is.null(input$map_map_click),
             {
               target_pos <-data.frame(lat=39.689349,lon=-75.760289)
             }, {
               target_pos=input$map_map_click
             })
      
      LAT=target_pos$lat
      LONG=target_pos$lon
      ZOOM=12

      google_map(location = c(LAT, LONG),
                 zoom = ZOOM,
                 split_view = "pano")
      
    })
  
##############################################################
    #get Species from selectInput
    SpeciesOut<<-reactive({
        input$SpeciesIn
      })
    
    
    #observe SpeciesIn to create required folders
    observeEvent(input$SpeciesIn,
      {
        SpeciesName<-isolate(SpeciesOut())
        
        speciesDir<-paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App","Saved_Images",SpeciesName,sep="/")
        try(dir.create(speciesDir))
        
        saveDirStreetview<-paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App","Saved_Images",SpeciesName,paste(SpeciesName,"Streetview",sep="_"),sep="/")
        try(dir.create(saveDirStreetview))
        
        saveDirScreenshot<-paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App","Saved_Images",SpeciesName,paste(SpeciesName,"Screenshots",sep="_"),sep="/")
        try(dir.create(saveDirScreenshot))
    })
    
    #############################################################

    #set up directions and pitches for google StreetView images
    dirList<-c(seq(0,360, by=45))
    #pitchList<-c(-90, -60, -30, 0, 30, 60, 90)
    pitchList<-c(10)
    fovList<-c(90)
    
    #observe event when "downloadStreetViewImages" button is pushed
    output$MapCoords <- renderText({ 
      print(c(-75.760289, 39.689349))
    })
    
    #observe map click to print new coords
    observeEvent(input$map_map_click,
                 {
                   coordsInfo<-input$map_map_click
                   
                   output$MapCoords <- renderText({ 
                     print(c(coordsInfo$lon, coordsInfo$lat))
                   })
                 })
    
                   
    observeEvent(input$downloadImage,
                 {
                   
                   SpeciesName<-isolate(SpeciesOut())
                   
                   withBusyIndicatorServer("downloadImage", {
                     Sys.sleep(7)
                   })
                   
                   ifelse(is.null(input$map_map_click),
                          {
                            target_pos <-data.frame(lat=39.689349,lon=-75.760289)
                          }, {
                            target_pos=input$map_map_click
                          })
                  LAT=target_pos$lat
                  LONG=target_pos$lon
                            
                  myLocations<-data.frame(Longitude=LONG, Latitude=LAT)
                  new.loc<-c(myLocations$Latitude, myLocations$Longitude)
                     
                  # observe({
                  #           SpeciesName<-input$SpeciesIn
                                 
                             saveDirStreetview<-paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App","Saved_Images",SpeciesName,paste(SpeciesName,"Streetview",sep="_"),sep="/")
                               
                  
                  n=ifelse(length(pitchList)>1,
                           length(list.files(saveDirStreetview))/(length(dirList)+length(pitchList)),
                           length(list.files(saveDirStreetview))/length(dirList))
                  # })
                  
                  n=n+1
                  
                     for(j in 1:length(dirList)){
                       new.dir<-dirList[j]
                       
                       for(k in 1:length(pitchList)){
                         new.pitch<-pitchList[k]
                         
                         for(z in 1:length(fovList)){
                           new.fov<-fovList[z]
                           
                           newStreetView<-google_streetview(location = new.loc,
                                                            size = c(640,640),
                                                            panorama_id = NULL,
                                                            output = "plot",
                                                            heading = new.dir,
                                                            fov = new.fov,
                                                            pitch = new.pitch,
                                                            response_check = FALSE,
                                                            key = StreetViewKey)
                           
                           #get image metadata
                           locCords<-c(new.loc[1],new.loc[2])
                           
                           #get metadata
                           myUrlnoSig<-paste("https://maps.googleapis.com/maps/api/streetview/metadata?size=640x640&location=",new.loc[1],",",new.loc[2],"&heading=",new.dir,"&pitch=",new.pitch,"&fov=",new.fov,"&key=",StreetViewKey,sep="")
                           
                           resp<-GET(myUrlnoSig)
                           
                           query<-list(page="2")
                           resp<-GET(myUrlnoSig,query=query)
                           http_type(resp)
                           
                           jsonRespText<-content(resp,as="text") 
                           jsonRespText
                           
                           imgMetaData<-read.table(text=jsonRespText, sep="\n")
                           
                           imgDate=tryCatch({
                             gsub(",","",gsub("   date : ", "",as.character(imgMetaData[3,])))
                           },error=function(cond2){
                             cond2="Unknown"
                             return(cond2)
                           })
                           
                           # observe({
                           #   SpeciesName<-input$SpeciesIn
                             
                           jpeg(paste(saveDirStreetview,paste(paste(SpeciesName,"_Image"),n,"dir",new.dir,"pitch",new.pitch,"fov",new.fov,myLocations$Latitude, myLocations$Longitude,"Date",imgDate,'.jpeg',sep="_"), sep="/"))
                           
                           

                           google_streetview(location = new.loc,
                                             size = c(640,640),
                                             panorama_id = NULL,
                                             output = "plot",
                                             heading = new.dir,
                                             fov = new.fov,
                                             pitch = new.pitch,
                                             response_check = FALSE,
                                             key = StreetViewKey)
                           
                           
                           
                           
                           dev.off()
                           
                           # })
                         }
                       }
                     }
                  
                                
                  print(environment(show))
                  use_python("/usr/local/bin/python3", required=TRUE)
                  
                  source_python(paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View", "Scripts/crop_image.py",sep="/"))
                  
                 
              })
    
    observeEvent(input$screenshot,
                 
                 {withBusyIndicatorServer("screenshot", {
                     Sys.sleep(4)
                   })
                   
                   SpeciesName<-isolate(SpeciesOut())
                   
                   ifelse(is.null(input$map_map_click),
                          {
                            target_pos <-data.frame(lat=39.689349,lon=-75.760289)
                          }, {
                            target_pos=input$map_map_click
                          })
                   
                   LAT=target_pos$lat
                   LONG=target_pos$lon
                   ZOOM=12
                   
                   myLocations<-data.frame(Longitude=LONG, Latitude=LAT)
                   new.loc.screen<-c(myLocations$Latitude, myLocations$Longitude)
                   new.dir.screen<-dirList[1]
                   new.pitch.screen<-pitchList[1]
                   new.fov.screen<-fovList[1]
                   
                   #run screencapture command from terminal
                   system("screencapture /Users/zach/'Dropbox (ZachTeam)'/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App/Temp/screenshot.jpg")
                   

                   #get imgDate from metadata
                   myUrlnoSig<-paste("https://maps.googleapis.com/maps/api/streetview/metadata?size=640x640&location=",new.loc.screen[1],",",new.loc.screen[2],"&heading=",new.dir.screen,"&pitch=",new.pitch.screen,"&fov=",new.fov.screen,"&key=",StreetViewKey,sep="")
                   
                   resp<-GET(myUrlnoSig)
                   
                   query<-list(page="2")
                   resp<-GET(myUrlnoSig,query=query)
                   http_type(resp)
                   
                   jsonRespText<-content(resp,as="text") 
                   jsonRespText
                   
                   imgMetaData<-read.table(text=jsonRespText, sep="\n")
                   
                   imgDate=tryCatch({
                     gsub(",","",gsub("   date : ", "",as.character(imgMetaData[3,])))
                   },error=function(cond2){
                     cond2="Unknown"
                     return(cond2)
                   })
                   
                   
                   screenCapDir<-paste("/Users/zach/Dropbox (ZachTeam)/Projects/Spotted_Lanternfly/Analyses/Street_View/Tree_Download_Shiny_App","Saved_Images",SpeciesName,paste(SpeciesName, "Screenshots",sep="_"),sep="/")

                    #run screenCapture function to process images
                    numScreenshots<-length(list.files(paste(screenCapDir,"/",sep="")))-1
                                    
                    #run screenCapture()               
                    screenCapture(directory =myDir , SpeciesName=SpeciesName, location = c(LAT, LONG), n=1+numScreenshots,imgDate=imgDate)
                                     
              })
     
   }
 }


shinyApp(ui, server)
  