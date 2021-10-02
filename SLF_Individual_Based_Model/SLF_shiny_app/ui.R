#SLF predicted spread Model Explorer

#set working directory

#rm(list=ls())

#load packages
library(htmlwidgets)
library(leaflet)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plyr)
library(dplyr)
library(jsonlite, pos=100)
library(ggplot2)
library(reshape2)
library(pals)
library(lubridate)
library(magrittr)
library(tidyr)

#load packages
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
library(pals)
library(plyr)
library(geosphere)
library(ggmap)
library(dplyr)
library(scales)

source("./Source/xyToLatLong.R")

# #load source functions

#read in raster data


#create date data.frame
year.df<-data.frame(Year=seq(2013,2025,1))

#create probability range data.frame
prob.df<-data.frame(Probability=seq(0,1,by=0.05))

#create lists for dropdown menus
growthRateList<-c("0.25","0.5","1","1.5")

#humanPopsList
humanPopsList<-c("None","Low","Medium","High","Very High")

#create movement type list
moveTypeList<-c("Random","Non-Random")

########################################################################################################################
# Define UI for application that draws a histogram
ui <- 
  library(shinydashboard)
  dashboardPage(
  
  dashboardHeader(
    
    title = div(img(src="DCNRlogo_2.png"), "Spotted Lanternfly - Predicted Spread Model Explorer",tags$head(includeCSS("./www/mainpageTest.css") )),titleWidth=600
  ),
  
  dashboardSidebar(width=200,

                   sidebarMenu(startExpanded=TRUE,
                               
                               hr(),

                               menuItem(text = "Model Performance"),
                   
                               #model performance metrics
                               textOutput("percentAccurate"),
                               tags$head(tags$style("#percentAccurate{color: violet;
                                 font-size: 15px;
                                 font-style: italic;
                                 padding-left: 16px;
                                 }"
                               )),
                               
                               textOutput("falseNegative"),
                               tags$head(tags$style("#falseNegative{color: deepskyblue;
                                 font-size: 15px;
                                 font-style: italic;
                                 padding-left: 16px;
                                 }"
                               )),
                               
                               textOutput("falsePositive"),
                               tags$head(tags$style("#falsePositive{color: limegreen;
                                 font-size: 15px;
                                 font-style: italic;
                                 padding-left: 16px;
                                 }"
                               )),
                               
                               textOutput("modPrecision"),
                               tags$head(tags$style("#modPrecision{color: yellow;
                                 font-size: 15px;
                                 font-style: italic;
                                 padding-left: 16px;
                                 }"
                               )),
                               
                               textOutput("modRecall"),
                               tags$head(tags$style("#modRecall{color: orange;
                                 font-size: 15px;
                                 font-style: italic;
                                 padding-left: 16px;
                                 }"
                               )),
                               
                               textOutput("modF1"),
                               tags$head(tags$style("#modF1{color: orangered;
                                 font-size: 15px;
                                 font-style: italic;
                                 padding-left: 16px;
                                 }"
                               )),
                               
                               hr(),
                               
                               #Select range of years
                               div(id="Year Controls",
                                   tags$div(title="SelectYears",
                                            sliderInput(inputId="YearSelect",
                                                        label="Years",
                                                        min = min(unique(year.df$Year)),
                                                        max = max(unique(year.df$Year)),
                                                        value=2021,
                                                        step=1,
                                                        sep="",
                                                        dragRange=FALSE
                                                        #animate = TRUE
                                            )
                                   )),
                               

                               #Select range of probabilities to display
                               div(id="Probability Controls",
                                   tags$div(title="SelectProb",
                                            sliderInput(inputId="ProbSelect",
                                                        label="Probability of Occurence",
                                                        min = min(unique(prob.df$Probability)),
                                                        max = max(unique(prob.df$Probability)),
                                                        value=c(min(unique(prob.df$Probability)),max(unique(prob.df$Probability))),
                                                        step=0.05,
                                                        dragRange=TRUE
                                                        #animate = TRUE
                                            )
                                   )),                                                         
                               
                               #hr(),

                               #Select Growth rate controls
                               div(id="GrowthRateControls",
                                   tags$div(title="SelectRate",
                                            selectInput(inputId="GrowthRate",
                                                        label="Population Growth Rate (r)", 
                                                        choices=sort(as.character(growthRateList)), 
                                                        multiple=FALSE,selected="0.25")
                                   )),
                               
                               
                              
                               #Select Movement Type controls (Random vs. Directed)
                               div(id="MovementControls",
                                   tags$div(title="SelectMovement",
                                            selectInput(inputId="MovementType",
                                                        label="Movement Type", 
                                                        choices=sort(as.character(moveTypeList)), 
                                                        multiple=FALSE)
                                                        )),
                               
                               #Select Human-mediated movement amoung
                               div(id="HumanMovement",
                                   tags$div(title="SelectHumanMovement",
                                            selectInput(inputId="HumanMediated",
                                                        label="Human-mediated Dispersal", 
                                                        choices=as.character(humanPopsList), 
                                                        multiple=FALSE,selected="Low")
                                   )),
                               
                               div(id="CheckboxControls", label="Options",
                                   tags$div(title="ControlButtons"),
                                   checkboxInput(inputId = "countyBoundaries",
                                                 label = "County Borders", 
                                                 value=TRUE),
                                   checkboxInput(inputId = "SLFsurvey",  
                                                 label = "SLF Counties", 
                                                 value=TRUE)
                                   
                               ),
                               
                               hr(),
                               
                     
                               
                               div(id="Info",label="ModelInformation", style="marign-left:10px",
                                tags$div(class="header", checked=NA,
                                        tags$a(href="https://rpubs.com/ZLadin/SLF_MAXENT", strong("Model Info Here"))))
                               
                               
                   )),
  
  dashboardBody(id="dashboardBody",
                tags$head( tags$meta(name = "viewport", content = "width=1600"),uiOutput("body")),
                #Main Body
                useShinyjs(),
                div(class="outer",
                    tags$head(includeCSS("./www/mapstyles.css") ),
                    leafletOutput("StudyAreaMap", width = "100%", height = "100%")
                      )
                    )
)
