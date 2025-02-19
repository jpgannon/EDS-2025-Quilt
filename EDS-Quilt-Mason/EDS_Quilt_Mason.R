library(shiny)
library(tidyverse)
library(lubridate)
#Data

# Package ID: knb-lter-hbr.59.14 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Daily Temperature Record, 1955 - present.
# Data set creator:    - USDA Forest Service, Northern Research Station 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/59/14/9723086870f14b48409869f6c06d6aa8" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "date",     
                 "STA",     
                 "MAX",     
                 "MIN",     
                 "AVE",     
                 "Flag"    ),
               check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$STA)!="factor") dt1$STA<- as.factor(dt1$STA)
if (class(dt1$MAX)=="factor") dt1$MAX <-as.numeric(levels(dt1$MAX))[as.integer(dt1$MAX) ]               
if (class(dt1$MAX)=="character") dt1$MAX <-as.numeric(dt1$MAX)
if (class(dt1$MIN)=="factor") dt1$MIN <-as.numeric(levels(dt1$MIN))[as.integer(dt1$MIN) ]               
if (class(dt1$MIN)=="character") dt1$MIN <-as.numeric(dt1$MIN)
if (class(dt1$AVE)=="factor") dt1$AVE <-as.numeric(levels(dt1$AVE))[as.integer(dt1$AVE) ]               
if (class(dt1$AVE)=="character") dt1$AVE <-as.numeric(dt1$AVE)
if (class(dt1$Flag)!="factor") dt1$Flag<- as.factor(dt1$Flag)

# Convert Missing Values to NA for non-dates

dt1$MAX <- ifelse((trimws(as.character(dt1$MAX))==trimws("NA")),NA,dt1$MAX)               
suppressWarnings(dt1$MAX <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$MAX))==as.character(as.numeric("NA"))),NA,dt1$MAX))
dt1$MIN <- ifelse((trimws(as.character(dt1$MIN))==trimws("NA")),NA,dt1$MIN)               
suppressWarnings(dt1$MIN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$MIN))==as.character(as.numeric("NA"))),NA,dt1$MIN))
dt1$AVE <- ifelse((trimws(as.character(dt1$AVE))==trimws("NA")),NA,dt1$AVE)               
suppressWarnings(dt1$AVE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$AVE))==as.character(as.numeric("NA"))),NA,dt1$AVE))
dt1$Flag <- as.factor(ifelse((trimws(as.character(dt1$Flag))==trimws("NA")),NA,as.character(dt1$Flag)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(date)
summary(STA)
summary(MAX)
summary(MIN)
summary(AVE)
summary(Flag) 
# Get more details on character variables

summary(as.factor(dt1$STA)) 
summary(as.factor(dt1$Flag))
detach(dt1)               


Temperature <- dt1 |>
  select(date, STA, AVE)
Temperature$STA <- as.character(Temperature$STA)

HQ_Temp <- filter(Temperature, STA == "HQ")
summary(HQ_Temp)

STA1_Temp <- filter(Temperature, STA == "STA1")
summary(STA1_Temp)

STA6_Temp <- filter(Temperature, STA == "STA6")
summary(STA6_Temp)

STA14_Temp <- filter(Temperature, STA == "STA14")
summary(STA14_Temp)

STA17_Temp <- filter(Temperature, STA == "STA17")
summary(STA17_Temp)

STA23_Temp <- filter(Temperature, STA == "STA23")
summary(STA23_Temp)

ALL_Stands_Temp <- Temperature |> 
  group_by(date) |>
  summarize(Avg_temp = mean(AVE))








# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
