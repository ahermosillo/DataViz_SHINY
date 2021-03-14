####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Load R packages
library(dplyr)
library(shiny)
library(shinythemes)

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

####Read in data and clean ####
adult.dat <- read.csv("www/SCAdult_testdata_20210115.csv", sep = ",", header = T) #Initial test dataset. It takes a moment to load. 
#374004 obs of 62 var

#Pair down to what we need and reformat date column
adults <- adult.dat %>%
  filter(Site == "Weir",  #1887 obs
         Event != "MOR") %>% #1867 obs
  filter(Sex != "U", Sex != "") %>% 
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
         Year = year(Date)) %>%
  select(Year,Date, Species, Origin, Sex, SpawnCond, Length_mm, Mass_g, BodyDepth_cm)

#str(adults) #Checking data types .. so far so good.

# Data Columns:
# Date - date of capture at the weir.
# Species - Onmy = Steelhead, Onki = Coho (our two salmonids.
# Origin - Natural = "wild", Hatchery = raise din captivity and was released.
# Sex - F = Female, M = Male.
# SpawnCond - Spawning Condition, How "ready" the fish is for reproduction. 
# Length_mm - Fish's fork length.
# Mass_g - Fish's mass.
# BodyDepth_cm - measures the girth of the fish.


#### Define UI ####
ui <- fluidPage(theme = shinytheme("flatly"),
        navbarPage(
            "What's happening at the Sott Creek Weir?",
            tabPanel(icon("home"),
             fluidRow(
              column(
                
                br(),
                p("Scott Creek is a small (70 km2) coastal watershed located just north of Davenport in Santa Cruz County, California. 
                  The Scott Creek watershed comprises approximately 23 km of anadromous waterway and supports populations of both 
                  Central California Coast coho salmon (Oncorhynchus kisutch, state and federally endangered) and Central California 
                  Coast steelhead trout ( anadromous O. mykiss; federally threatened). The Scott Creek LCMS, operated by the National 
                  Oceanic and Atmospheric Administration’s Southwest Fisheries Science Center (NOAA SWFSC) and University of California, 
                  Santa Cruz, has been providing data on status and trends of anadromous salmonids in the Scott Creek watershed since 2003. 
                  The Scott Creek LCMS is the only LCMS in the Santa Cruz Mountains Diversity Stratum and provides critical 
                  information to inform the management of ESA-listed salmonids. As a LCMS, we handle salmonids at almost every life 
                  stage including adults returning to spawn in their natal stream. We use a resistance board weir (see photo) to 
                  temporarily intercept fish which we then release upstream of the trap to continue their migration in the watershed.",
                  style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                br(),
                
                p("The goal of this shiny app was to visualize the cumulative number of adult fish caught each season at the Scott
                  Creek Weir. Variables included: species, origin (natural verses hatchery raised), and sex",
                  style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                tags$img(src="weirlookingDS.png",width=680,height=400,
                         style="display: block; margin-left: auto; margin-right: auto;"),
                br(),
                width=7),
              column(
                br(),
                tags$img(src="ScottCreek24.jpg", width = 500,height = 370,
                         style="display: block; margin-left: auto; margin-right: auto;"),
                br(),
                tags$img(src="steelhead.jpg", width = 500, height = 370,
                         style="display: block; margin-left: auto; margin-right: auto;"),
                
                width=5)),
             
                #hr(),
             #p(em("Developed by"),br("Name here"),style="text-align:center; font-family: times")
            ),
            tabPanel("Visualize Data",
                     sidebarPanel(
                         tags$h3("Variables to filter by"),
                        selectInput("dates",
                                   "Season",
                                   choices = 
                                     list(
                                          "2003-2004 Winter",
                                          "2004-2005 Winter",
                                          "2005-2006 Winter",
                                          "2006-2007 Winter",
                                          "2007-2008 Winter",
                                          "2007-2008 Winter",
                                          "2008-2009 Winter",
                                          "2009-2010 Winter",
                                          "2010-2011 Winter",
                                          "2011-2012 Winter",
                                          "2012-2013 Winter",
                                          "2013-2014 Winter",
                                          "2014-2015 Winter",
                                          "2015-2016 Winter",
                                          "2016-2017 Winter",
                                          "2017-2018 Winter",
                                          "2018-2019 Winter",
                                          "2019-2020 Winter"
                                          ), selected = "2014-2015 Winter"),
                         radioButtons("sp_name", 
                            "Select Species:", 
                            choices = list(
                              "Both",
                              "Coho" = "Onki", 
                              "Steelhead"="Onmy" )
                              ),
                         radioButtons("sex_name", 
                                      "Select Sex:", 
                                      choices = list("Both", "Male" = "M", "Female"="F")
                                      ),
                        radioButtons("origin_name", 
                                     "Select Origin:", 
                                     choices = list("Both", "Natural" = "Natural", "Hatchery"="Hatchery")
                                   ),
                        submitButton("Apply Filters")
                         
                     ), # sidebarPanel
                     mainPanel(
                         h3("Coho Salmon and Steelhead Trout Cumulative Sum Over Time"),
                         br(),
                         br(),
                         plotOutput("distPlot"),
                     ) # mainPanel
                     
            ) # Navbar 1, tabPanel
            #tabPanel("Plot 3 3", "This panel is intentionally left blank")
                    
                ) # navbarPage
) # fluidPage


#### Define server function  ####
server <- function(input, output) {
  
    #filter data based on conditions of the user. probably more efficient way of doing this
    dataplot<-reactive ({
    if (input$dates == "2003-2004 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2003-10-01" & Date < "2004-07-01")
    } else if (input$dates == "2004-2005 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2004-10-01" & Date < "2005-07-01")
    } else if (input$dates == "2005-2006 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2005-10-01" & Date < "2006-07-01")
    } else if (input$dates == "2006-2007 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2006-10-01" & Date < "2007-07-01")
    } else if (input$dates == "2007-2008 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2007-10-01" & Date < "2008-07-01")
    } else if (input$dates == "2008-2009 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2008-10-01" & Date < "2009-07-01")
    } else if (input$dates == "2009-2010 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2009-10-01" & Date < "2010-07-01")
    } else if (input$dates == "2010-2011 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2010-10-01" & Date < "2011-07-01")
    } else if (input$dates == "2011-2012 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2011-10-01" & Date < "2012-07-01")
    } else if (input$dates == "2012-2013 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2012-10-01" & Date < "2013-07-01")
    } else if (input$dates == "2013-2014 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2013-10-01" & Date < "2014-07-01")
    } else if (input$dates == "2014-2015 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2014-10-01" & Date < "2015-07-01")
    } else if (input$dates == "2015-2016 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2015-10-01" & Date < "2016-07-01")
    } else if (input$dates == "2016-2017 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2016-10-01" & Date < "2017-07-01")
    } else if (input$dates == "2017-2018 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2017-10-01" & Date < "2018-07-01")
    } else if (input$dates == "2018-2019 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2018-10-01" & Date < "2019-07-01")
    } else if (input$dates == "2019-2020 Winter"){
      filtered_dat <- adults %>% 
        filter(Date >= "2019-10-01" & Date < "2020-07-01")
    } else {filtered_dat<-adults}
    if (input$sex_name == "M") {
      filtered_dat<-filtered_dat %>% filter(Sex == "M")
    } else if (input$sex_name == "F"){
        filtered_dat<-filtered_dat %>% filter(Sex == "F")
    } else{filtered_dat<-filtered_dat
    }
      if (input$sp_name == "Onki") {
        filtered_dat<-filtered_dat %>% filter(Species == "Onki")
      } else if (input$sp_name == "Onmy"){
        filtered_dat<-filtered_dat %>% filter(Species == "Onmy")
      } else{filtered_dat<-filtered_dat
      }
      if (input$origin_name == "Natural") {
        filtered_dat<-filtered_dat %>% filter(Origin == "Natural")
      } else if (input$origin_name == "Hatchery"){
        filtered_dat<-filtered_dat %>% filter(Origin == "Hatchery")
      } else{filtered_dat<-filtered_dat
      }
        return(filtered_dat)
    }) #done filtering
    
    output$distPlot <- renderPlot({
      # make the plot 
        txt <- paste("Sex:", input$sex_name, "\nspecies:", input$sp_name,"\ndates:", input$dates )
      
        plot1<-ggplot(dataplot(), aes(x = Date)) +
        stat_bin(aes(y = cumsum(..count..)),geom = "step") + labs(x = "Date", y = "Cumulative Number of Fish") + 
        #annotate("text", x=min(Date), y=Inf, label = "Top-left", hjust = 0, vjust = 1)+  
          #couldn't figure out how to put a text box in upper left corner with info :(
        #ggtitle(paste("Sex:", input$sex_name, "species:", input$sp_name,"dates:", input$dates )) +
        theme_classic() +
        theme(legend.position ="bottom",#this puts legend on the bottom
        plot.title = (element_text(face="bold", vjust = 2, size = 20,hjust = 0.5)),#this makes the axis titles in bold,
        axis.line=element_line(color="black",size=1),#Makes the axis line black and  thicker
        text=element_text(size=18,face="bold",color="black"),
        axis.text.x = element_text(size =20, angle = 70, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 20)) 
        return(plot1)
    })

} # server


#### Create Shiny object ####
shinyApp(ui = ui, server = server)