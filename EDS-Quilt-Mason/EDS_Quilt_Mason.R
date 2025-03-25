library(shiny)
library(tidyverse)
library(lubridate)

#Shiny App

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Climate Quilt!"),
  
  tabsetPanel(
    tabPanel("Design", 
             h3("Design Your Quilt!"),
             p(  
               # Sidebar with a drop-down input for size of quilt
               sidebarLayout(
                 sidebarPanel(
                   selectInput("quiltsize",
                               "Choose Quilt Size",
                               choices = c("5x7 (Baby)",
                                           "6x9 (Crib)",
                                           "9x11 (Throw)",
                                           "12x15 (Twin)",
                                           "14x18 (Full)",
                                           "15x18 (Queen)",
                                           "18x18 (King)")),
                   helpText("Note: all squares will be 6 inches by 6 inches"),
                   
                   # Also in sidebar with drop-down for number of colors   
                   selectInput("colorquantity",
                               "Choose Amount of Colors",
                               choices = c("4", "8"))
                 ),
                 
                 # Show Images of color patches, making them selectable buttons
                 mainPanel(
                   h4("Choose a Color Scheme"),
                   fluidRow(
                     column(4, actionButton("color_bluegreen", label = HTML('<img src="bluegreenramp.png" style="width:100%; height:auto;">'))),
                     column(4, actionButton("color_greenred", label = HTML('<img src="greenredramp.png" style="width:100%; height:auto;">'))),
                   ),
                   fluidRow(
                     column(4, actionButton("color_redwhite", label = HTML('<img src="redwhiteramp.png" style="width:100%; height:auto;">'))),
                     column(4, actionButton("color_bluewhite", label = HTML('<img src="bluewhiteramp.png" style="width:100%; height:auto;">'))),
                   ),
                   fluidRow(
                     column(4, actionButton("color_brownwhite", label = HTML('<img src="brownwhiteramp.png" style="width:100%; height:auto;">'))),
                     column(4, actionButton("color_greenyellow", label = HTML('<img src="greenyellowramp.png" style="width:100%; height:auto;">'))),
                   ), 
                   fluidRow(
                     column(4, actionButton("color_redblue", label = HTML('<img src="redblueramp.png" style="width:100%; height:auto;">'))),
                     column(4, actionButton("color_redyellow", label = HTML('<img src="redyellowramp.png" style="width:100%; height:auto;">')))
                   ),
                   br(), 
                 )
               )
             )
    ),
    
    tabPanel("Data Setup", 
             h3("Choose Your Data!"),
             p(
               # Default Dataset Selection
               sidebarLayout(
                 sidebarPanel(
                   selectInput("defaultdataselect",
                               "Choose Your Data Type!", 
                               choices = c("Temperature" = "Temperature",
                                           "Water Chemistry" = "Water_Chemistry",
                                           "Soil Carbon" = "Soil_Carbon",
                                           "Soil Nitrogen" = "Soil_Nitrogen")),
                   
                   helpText("OR"),
                   
                   # Data Upload button
                   fileInput("fileupload", "Upload Your Own Data File!",
                             accept = ".csv"),
                   textOutput("dataInfo"),
                   
                   # Select Time Period of Data
                   helpText("Now select the time period your quilt will show! 
                            Date slider may take a moment to load, please wait!"),
                   
                   uiOutput("dateSliderUI")
                 ),
                 
                 mainPanel(
                   tableOutput("dataPreview")
                 )
               )
             )
    ),
    
    tabPanel("View & Share",
             h3("Preview Your Design & Share!"),
             p(
               sidebarLayout(
                 sidebarPanel(
                   #Download Button
                   downloadButton("downloadQuilt", "Download Quilt Pattern"),
                   #
                   actionButton("fabricWebsite", "Visit Fabric Website", style = "margin-top: 20px;"),
                   actionButton("shareButton", "Share Your Design!", 
                                style = "margin-bottom: 20px; display: block;",
                                onclick = "navigator.share({title: 'Check out this Quilt!', url: window.location.href})",
                                style = "margin-top: 20px;"),
                 ),
                 
                 mainPanel(
                   h3("Your Quilt Design"),
                   plotOutput("quiltPlot"),
                   h4("Fabric Requirements"),
                   tableOutput("fabricTable")
                 )
               )
             )
    ),
    
    tabPanel("User Guide",
             h3("How to Use App!"),
             p(
               tags$p("Tutorial on How to Use our App!"),
               tags$p("Tab 1: Design",
                      helpText("In this tab you will have the opportunity to choose your desired quilt size,
                               choose the color scheme for your quilt, as well as the amount of colors you 
                               want to display on your quilt. Use the top dropdown to select your size, the
                               second dropdown to select your color quantity, and select any of the color
                               ramp buttons to select your choice of color scheme.")),
               tags$p("Tab 2: Data Setup",
                      helpText("This is where you will select or upload the data and timeframe you would like 
                               your quilt to represent. Use the first dropdown to select the category of data 
                               your quilt will portray, which will use a random dataset found in the Hubbard Brook
                               Data Catalog to build your quilt design. Or, if you want to use a different 
                               type of data, you can upload your own dataset in .csv format! Note: IF you choose to 
                               upload your own data, you only need to include the date and whatever value
                               your data set tracks in your .csv file. Lastly, select both
                               a start date and end date using the interactive calendars,to specify the time 
                               frame of data that your quilt will show, whether that is multiple
                               days, weeks, months, or even years.")),
               tags$p("Tab 3: View & Share",
                      helpText("This tab allows you to preview your quilt design with the data you selected to show
                               as well as the color scheme and size you selected previously. Use the download button at
                               the top of the sidebar to save your design on to your device. You can be directed to a
                               craft store website, such as Joann's for example, where you can choose your colors and 
                               purchase the amount of fabric necessary, using the visit fabric website button. The share
                               your design button can be used to send your design to family and friends through email
                               or text message. At the bottom of the sidebar, all your fabric calculations will be done
                               for you, including seam allowance, so our app will tell you how much total fabric of each
                               color you will need! The right hand side of this page is where your design preview will
                               appear."))
             )
    )
  )
)

server <- function(input, output, session) {
  
  #Default Datasets
  options(HTTPUserAgent="EDI_CodeGen")
  
  
  inUrl11  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/59/14/9723086870f14b48409869f6c06d6aa8" 
  infile11 <- tempfile()
  try(download.file(inUrl11,infile11,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
  if (is.na(file.size(infile11))) download.file(inUrl11,infile11,method="auto")
  
  
  dt11 <-read.csv(infile11,header=F 
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
  
  unlink(infile11)
  
  # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  
  # attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
  tmpDateFormat<-"%Y-%m-%d"
  tmp1date<-as.Date(dt11$date,format=tmpDateFormat)
  # Keep the new dates only if they all converted correctly
  if(nrow(dt11[dt11$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt11$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
  
  if (class(dt11$STA)!="factor") dt11$STA<- as.factor(dt11$STA)
  if (class(dt11$MAX)=="factor") dt11$MAX <-as.numeric(levels(dt11$MAX))[as.integer(dt11$MAX) ]               
  if (class(dt11$MAX)=="character") dt11$MAX <-as.numeric(dt11$MAX)
  if (class(dt11$MIN)=="factor") dt11$MIN <-as.numeric(levels(dt11$MIN))[as.integer(dt11$MIN) ]               
  if (class(dt11$MIN)=="character") dt11$MIN <-as.numeric(dt11$MIN)
  if (class(dt11$AVE)=="factor") dt11$AVE <-as.numeric(levels(dt11$AVE))[as.integer(dt11$AVE) ]               
  if (class(dt11$AVE)=="character") dt11$AVE <-as.numeric(dt11$AVE)
  if (class(dt11$Flag)!="factor") dt11$Flag<- as.factor(dt11$Flag)
  
  # Convert Missing Values to NA for non-dates
  
  dt11$MAX <- ifelse((trimws(as.character(dt11$MAX))==trimws("NA")),NA,dt11$MAX)               
  suppressWarnings(dt11$MAX <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt11$MAX))==as.character(as.numeric("NA"))),NA,dt11$MAX))
  dt11$MIN <- ifelse((trimws(as.character(dt11$MIN))==trimws("NA")),NA,dt11$MIN)               
  suppressWarnings(dt11$MIN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt11$MIN))==as.character(as.numeric("NA"))),NA,dt11$MIN))
  dt11$AVE <- ifelse((trimws(as.character(dt11$AVE))==trimws("NA")),NA,dt11$AVE)               
  suppressWarnings(dt11$AVE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt11$AVE))==as.character(as.numeric("NA"))),NA,dt11$AVE))
  dt11$Flag <- as.factor(ifelse((trimws(as.character(dt11$Flag))==trimws("NA")),NA,as.character(dt11$Flag)))
  
  
  # Here is the structure of the input data frame:
  str(dt11)                            
  attach(dt11)                            
  # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
  
  summary(date)
  summary(STA)
  summary(MAX)
  summary(MIN)
  summary(AVE)
  summary(Flag) 
  # Get more details on character variables
  
  summary(as.factor(dt11$STA)) 
  summary(as.factor(dt11$Flag))
  detach(dt11)               
  
  
  Temperature <- dt11 |>
    select(date, AVE)|>
    relocate(date, AVE)|>
    group_by(date) |>
    summarize(Avg_temp = mean(AVE))
  
  Temperature <- Temperature |>
    rename('Date' = date)|>
    rename('Value' = Avg_temp)
  
  # Convert the 'date' column to Date type
  tmpDateFormat <- "%Y-%m-%d"
  tmp1date <- as.Date(Temperature$Date, format=tmpDateFormat)
  if (nrow(Temperature[Temperature$Date != "",]) == length(tmp1date[!is.na(tmp1date)])) {
    Temperature$Date <- tmp1date
  } else {
    print("Date conversion failed for Temperature$date. Please inspect the data and do the date conversion yourself.")
  }
  
  # Create reactive expression for filtering based on user dates
  filtered_data <- reactive({
    req(input$dataStartDate, input$dataEndDate)  # Ensure dates are selected
    data_filtered <- Temperature %>%
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) %>%
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(data_filtered)
  })
  
  #Water Chemistry
  # Package ID: knb-lter-hbr.208.11 Cataloging System:https://pasta.edirepository.org.
  # Data set title: Continuous precipitation and stream chemistry data, Hubbard Brook Ecosystem Study, 1963 â ongoing..
  # Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
  # Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
  # Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
  # Uncomment the following lines to have R clear previous work, or set a working directory
  # rm(list=ls())      
  
  # setwd("C:/users/my_name/my_dir")       
  
  
  
  options(HTTPUserAgent="EDI_CodeGen")
  
  
  inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/11/3b3cf7ea447cb875d7c7d68ebdfd24c7" 
  infile2 <- tempfile()
  try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
  if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
  
  
  dt2 <-read.csv(infile2,header=F 
                 ,skip=1
                 ,sep=","  
                 ,quot='"' 
                 , col.names=c(
                   "site",     
                   "date",     
                   "timeEST",     
                   "barcode",     
                   "pH",     
                   "DIC",     
                   "spCond",     
                   "temp",     
                   "ANC960",     
                   "ANCMet",     
                   "gageHt",     
                   "hydroGraph",     
                   "flowGageHt",     
                   "fieldCode",     
                   "notes",     
                   "uniqueID",     
                   "waterYr",     
                   "Ca",     
                   "Mg",     
                   "K",     
                   "Na",     
                   "TMAl",     
                   "OMAl",     
                   "Al_ICP",     
                   "Al_ferron",     
                   "NH4",     
                   "SO4",     
                   "NO3",     
                   "Cl",     
                   "PO4",     
                   "DOC",     
                   "TDN",     
                   "DON",     
                   "SiO2",     
                   "Mn",     
                   "Fe",     
                   "F",     
                   "cationCharge",     
                   "anionCharge",     
                   "ionError",     
                   "duplicate",     
                   "sampleType",     
                   "ionBalance",     
                   "canonical",     
                   "pHmetrohm"    ), check.names=TRUE)
  
  unlink(infile2)
  
  # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  
  if (class(dt2$site)!="factor") dt2$site<- as.factor(dt2$site)                                   
  # attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
  tmpDateFormat<-"%Y-%m-%d"
  tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
  # Keep the new dates only if they all converted correctly
  if(nrow(dt2[dt2$date != "",]) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
  
  if (class(dt2$barcode)!="factor") dt2$barcode<- as.factor(dt2$barcode)
  if (class(dt2$pH)=="factor") dt2$pH <-as.numeric(levels(dt2$pH))[as.integer(dt2$pH) ]               
  if (class(dt2$pH)=="character") dt2$pH <-as.numeric(dt2$pH)
  if (class(dt2$DIC)=="factor") dt2$DIC <-as.numeric(levels(dt2$DIC))[as.integer(dt2$DIC) ]               
  if (class(dt2$DIC)=="character") dt2$DIC <-as.numeric(dt2$DIC)
  if (class(dt2$spCond)=="factor") dt2$spCond <-as.numeric(levels(dt2$spCond))[as.integer(dt2$spCond) ]               
  if (class(dt2$spCond)=="character") dt2$spCond <-as.numeric(dt2$spCond)
  if (class(dt2$temp)=="factor") dt2$temp <-as.numeric(levels(dt2$temp))[as.integer(dt2$temp) ]               
  if (class(dt2$temp)=="character") dt2$temp <-as.numeric(dt2$temp)
  if (class(dt2$ANC960)=="factor") dt2$ANC960 <-as.numeric(levels(dt2$ANC960))[as.integer(dt2$ANC960) ]               
  if (class(dt2$ANC960)=="character") dt2$ANC960 <-as.numeric(dt2$ANC960)
  if (class(dt2$ANCMet)=="factor") dt2$ANCMet <-as.numeric(levels(dt2$ANCMet))[as.integer(dt2$ANCMet) ]               
  if (class(dt2$ANCMet)=="character") dt2$ANCMet <-as.numeric(dt2$ANCMet)
  if (class(dt2$gageHt)=="factor") dt2$gageHt <-as.numeric(levels(dt2$gageHt))[as.integer(dt2$gageHt) ]               
  if (class(dt2$gageHt)=="character") dt2$gageHt <-as.numeric(dt2$gageHt)
  if (class(dt2$hydroGraph)!="factor") dt2$hydroGraph<- as.factor(dt2$hydroGraph)
  if (class(dt2$flowGageHt)=="factor") dt2$flowGageHt <-as.numeric(levels(dt2$flowGageHt))[as.integer(dt2$flowGageHt) ]               
  if (class(dt2$flowGageHt)=="character") dt2$flowGageHt <-as.numeric(dt2$flowGageHt)
  if (class(dt2$fieldCode)!="factor") dt2$fieldCode<- as.factor(dt2$fieldCode)
  if (class(dt2$notes)!="factor") dt2$notes<- as.factor(dt2$notes)
  if (class(dt2$uniqueID)!="factor") dt2$uniqueID<- as.factor(dt2$uniqueID)
  if (class(dt2$Ca)=="factor") dt2$Ca <-as.numeric(levels(dt2$Ca))[as.integer(dt2$Ca) ]               
  if (class(dt2$Ca)=="character") dt2$Ca <-as.numeric(dt2$Ca)
  if (class(dt2$Mg)=="factor") dt2$Mg <-as.numeric(levels(dt2$Mg))[as.integer(dt2$Mg) ]               
  if (class(dt2$Mg)=="character") dt2$Mg <-as.numeric(dt2$Mg)
  if (class(dt2$K)=="factor") dt2$K <-as.numeric(levels(dt2$K))[as.integer(dt2$K) ]               
  if (class(dt2$K)=="character") dt2$K <-as.numeric(dt2$K)
  if (class(dt2$Na)=="factor") dt2$Na <-as.numeric(levels(dt2$Na))[as.integer(dt2$Na) ]               
  if (class(dt2$Na)=="character") dt2$Na <-as.numeric(dt2$Na)
  if (class(dt2$TMAl)=="factor") dt2$TMAl <-as.numeric(levels(dt2$TMAl))[as.integer(dt2$TMAl) ]               
  if (class(dt2$TMAl)=="character") dt2$TMAl <-as.numeric(dt2$TMAl)
  if (class(dt2$OMAl)=="factor") dt2$OMAl <-as.numeric(levels(dt2$OMAl))[as.integer(dt2$OMAl) ]               
  if (class(dt2$OMAl)=="character") dt2$OMAl <-as.numeric(dt2$OMAl)
  if (class(dt2$Al_ICP)=="factor") dt2$Al_ICP <-as.numeric(levels(dt2$Al_ICP))[as.integer(dt2$Al_ICP) ]               
  if (class(dt2$Al_ICP)=="character") dt2$Al_ICP <-as.numeric(dt2$Al_ICP)
  if (class(dt2$Al_ferron)=="factor") dt2$Al_ferron <-as.numeric(levels(dt2$Al_ferron))[as.integer(dt2$Al_ferron) ]               
  if (class(dt2$Al_ferron)=="character") dt2$Al_ferron <-as.numeric(dt2$Al_ferron)
  if (class(dt2$NH4)=="factor") dt2$NH4 <-as.numeric(levels(dt2$NH4))[as.integer(dt2$NH4) ]               
  if (class(dt2$NH4)=="character") dt2$NH4 <-as.numeric(dt2$NH4)
  if (class(dt2$SO4)=="factor") dt2$SO4 <-as.numeric(levels(dt2$SO4))[as.integer(dt2$SO4) ]               
  if (class(dt2$SO4)=="character") dt2$SO4 <-as.numeric(dt2$SO4)
  if (class(dt2$NO3)=="factor") dt2$NO3 <-as.numeric(levels(dt2$NO3))[as.integer(dt2$NO3) ]               
  if (class(dt2$NO3)=="character") dt2$NO3 <-as.numeric(dt2$NO3)
  if (class(dt2$Cl)=="factor") dt2$Cl <-as.numeric(levels(dt2$Cl))[as.integer(dt2$Cl) ]               
  if (class(dt2$Cl)=="character") dt2$Cl <-as.numeric(dt2$Cl)
  if (class(dt2$PO4)=="factor") dt2$PO4 <-as.numeric(levels(dt2$PO4))[as.integer(dt2$PO4) ]               
  if (class(dt2$PO4)=="character") dt2$PO4 <-as.numeric(dt2$PO4)
  if (class(dt2$DOC)=="factor") dt2$DOC <-as.numeric(levels(dt2$DOC))[as.integer(dt2$DOC) ]               
  if (class(dt2$DOC)=="character") dt2$DOC <-as.numeric(dt2$DOC)
  if (class(dt2$TDN)=="factor") dt2$TDN <-as.numeric(levels(dt2$TDN))[as.integer(dt2$TDN) ]               
  if (class(dt2$TDN)=="character") dt2$TDN <-as.numeric(dt2$TDN)
  if (class(dt2$DON)=="factor") dt2$DON <-as.numeric(levels(dt2$DON))[as.integer(dt2$DON) ]               
  if (class(dt2$DON)=="character") dt2$DON <-as.numeric(dt2$DON)
  if (class(dt2$SiO2)=="factor") dt2$SiO2 <-as.numeric(levels(dt2$SiO2))[as.integer(dt2$SiO2) ]               
  if (class(dt2$SiO2)=="character") dt2$SiO2 <-as.numeric(dt2$SiO2)
  if (class(dt2$Mn)=="factor") dt2$Mn <-as.numeric(levels(dt2$Mn))[as.integer(dt2$Mn) ]               
  if (class(dt2$Mn)=="character") dt2$Mn <-as.numeric(dt2$Mn)
  if (class(dt2$Fe)=="factor") dt2$Fe <-as.numeric(levels(dt2$Fe))[as.integer(dt2$Fe) ]               
  if (class(dt2$Fe)=="character") dt2$Fe <-as.numeric(dt2$Fe)
  if (class(dt2$F)=="factor") dt2$F <-as.numeric(levels(dt2$F))[as.integer(dt2$F) ]               
  if (class(dt2$F)=="character") dt2$F <-as.numeric(dt2$F)
  if (class(dt2$cationCharge)=="factor") dt2$cationCharge <-as.numeric(levels(dt2$cationCharge))[as.integer(dt2$cationCharge) ]               
  if (class(dt2$cationCharge)=="character") dt2$cationCharge <-as.numeric(dt2$cationCharge)
  if (class(dt2$anionCharge)=="factor") dt2$anionCharge <-as.numeric(levels(dt2$anionCharge))[as.integer(dt2$anionCharge) ]               
  if (class(dt2$anionCharge)=="character") dt2$anionCharge <-as.numeric(dt2$anionCharge)
  if (class(dt2$ionError)=="factor") dt2$ionError <-as.numeric(levels(dt2$ionError))[as.integer(dt2$ionError) ]               
  if (class(dt2$ionError)=="character") dt2$ionError <-as.numeric(dt2$ionError)
  if (class(dt2$duplicate)!="factor") dt2$duplicate<- as.factor(dt2$duplicate)
  if (class(dt2$sampleType)!="factor") dt2$sampleType<- as.factor(dt2$sampleType)
  if (class(dt2$ionBalance)=="factor") dt2$ionBalance <-as.numeric(levels(dt2$ionBalance))[as.integer(dt2$ionBalance) ]               
  if (class(dt2$ionBalance)=="character") dt2$ionBalance <-as.numeric(dt2$ionBalance)
  if (class(dt2$canonical)!="factor") dt2$canonical<- as.factor(dt2$canonical)
  if (class(dt2$pHmetrohm)!="factor") dt2$pHmetrohm<- as.factor(dt2$pHmetrohm)
  
  # Convert Missing Values to NA for non-dates
  
  dt2$site <- as.factor(ifelse((trimws(as.character(dt2$site))==trimws("NA")),NA,as.character(dt2$site)))
  dt2$barcode <- as.factor(ifelse((trimws(as.character(dt2$barcode))==trimws("NA")),NA,as.character(dt2$barcode)))
  dt2$pH <- ifelse((trimws(as.character(dt2$pH))==trimws("NA")),NA,dt2$pH)               
  suppressWarnings(dt2$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$pH))==as.character(as.numeric("NA"))),NA,dt2$pH))
  dt2$DIC <- ifelse((trimws(as.character(dt2$DIC))==trimws("NA")),NA,dt2$DIC)               
  suppressWarnings(dt2$DIC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DIC))==as.character(as.numeric("NA"))),NA,dt2$DIC))
  dt2$spCond <- ifelse((trimws(as.character(dt2$spCond))==trimws("NA")),NA,dt2$spCond)               
  suppressWarnings(dt2$spCond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$spCond))==as.character(as.numeric("NA"))),NA,dt2$spCond))
  dt2$temp <- ifelse((trimws(as.character(dt2$temp))==trimws("NA")),NA,dt2$temp)               
  suppressWarnings(dt2$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$temp))==as.character(as.numeric("NA"))),NA,dt2$temp))
  dt2$ANC960 <- ifelse((trimws(as.character(dt2$ANC960))==trimws("NA")),NA,dt2$ANC960)               
  suppressWarnings(dt2$ANC960 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANC960))==as.character(as.numeric("NA"))),NA,dt2$ANC960))
  dt2$ANCMet <- ifelse((trimws(as.character(dt2$ANCMet))==trimws("NA")),NA,dt2$ANCMet)               
  suppressWarnings(dt2$ANCMet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANCMet))==as.character(as.numeric("NA"))),NA,dt2$ANCMet))
  dt2$gageHt <- ifelse((trimws(as.character(dt2$gageHt))==trimws("NA")),NA,dt2$gageHt)               
  suppressWarnings(dt2$gageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$gageHt))==as.character(as.numeric("NA"))),NA,dt2$gageHt))
  dt2$hydroGraph <- as.factor(ifelse((trimws(as.character(dt2$hydroGraph))==trimws("NA")),NA,as.character(dt2$hydroGraph)))
  dt2$flowGageHt <- ifelse((trimws(as.character(dt2$flowGageHt))==trimws("NA")),NA,dt2$flowGageHt)               
  suppressWarnings(dt2$flowGageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$flowGageHt))==as.character(as.numeric("NA"))),NA,dt2$flowGageHt))
  dt2$fieldCode <- as.factor(ifelse((trimws(as.character(dt2$fieldCode))==trimws("NA")),NA,as.character(dt2$fieldCode)))
  dt2$notes <- as.factor(ifelse((trimws(as.character(dt2$notes))==trimws("NA")),NA,as.character(dt2$notes)))
  dt2$uniqueID <- as.factor(ifelse((trimws(as.character(dt2$uniqueID))==trimws("NA")),NA,as.character(dt2$uniqueID)))
  dt2$Ca <- ifelse((trimws(as.character(dt2$Ca))==trimws("NA")),NA,dt2$Ca)               
  suppressWarnings(dt2$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Ca))==as.character(as.numeric("NA"))),NA,dt2$Ca))
  dt2$Mg <- ifelse((trimws(as.character(dt2$Mg))==trimws("NA")),NA,dt2$Mg)               
  suppressWarnings(dt2$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mg))==as.character(as.numeric("NA"))),NA,dt2$Mg))
  dt2$K <- ifelse((trimws(as.character(dt2$K))==trimws("NA")),NA,dt2$K)               
  suppressWarnings(dt2$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$K))==as.character(as.numeric("NA"))),NA,dt2$K))
  dt2$Na <- ifelse((trimws(as.character(dt2$Na))==trimws("NA")),NA,dt2$Na)               
  suppressWarnings(dt2$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Na))==as.character(as.numeric("NA"))),NA,dt2$Na))
  dt2$TMAl <- ifelse((trimws(as.character(dt2$TMAl))==trimws("NA")),NA,dt2$TMAl)               
  suppressWarnings(dt2$TMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TMAl))==as.character(as.numeric("NA"))),NA,dt2$TMAl))
  dt2$OMAl <- ifelse((trimws(as.character(dt2$OMAl))==trimws("NA")),NA,dt2$OMAl)               
  suppressWarnings(dt2$OMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$OMAl))==as.character(as.numeric("NA"))),NA,dt2$OMAl))
  dt2$Al_ICP <- ifelse((trimws(as.character(dt2$Al_ICP))==trimws("NA")),NA,dt2$Al_ICP)               
  suppressWarnings(dt2$Al_ICP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Al_ICP))==as.character(as.numeric("NA"))),NA,dt2$Al_ICP))
  dt2$Al_ferron <- ifelse((trimws(as.character(dt2$Al_ferron))==trimws("NA")),NA,dt2$Al_ferron)               
  suppressWarnings(dt2$Al_ferron <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Al_ferron))==as.character(as.numeric("NA"))),NA,dt2$Al_ferron))
  dt2$NH4 <- ifelse((trimws(as.character(dt2$NH4))==trimws("NA")),NA,dt2$NH4)               
  suppressWarnings(dt2$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NH4))==as.character(as.numeric("NA"))),NA,dt2$NH4))
  dt2$SO4 <- ifelse((trimws(as.character(dt2$SO4))==trimws("NA")),NA,dt2$SO4)               
  suppressWarnings(dt2$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SO4))==as.character(as.numeric("NA"))),NA,dt2$SO4))
  dt2$NO3 <- ifelse((trimws(as.character(dt2$NO3))==trimws("NA")),NA,dt2$NO3)               
  suppressWarnings(dt2$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NO3))==as.character(as.numeric("NA"))),NA,dt2$NO3))
  dt2$Cl <- ifelse((trimws(as.character(dt2$Cl))==trimws("NA")),NA,dt2$Cl)               
  suppressWarnings(dt2$Cl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Cl))==as.character(as.numeric("NA"))),NA,dt2$Cl))
  dt2$PO4 <- ifelse((trimws(as.character(dt2$PO4))==trimws("NA")),NA,dt2$PO4)               
  suppressWarnings(dt2$PO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$PO4))==as.character(as.numeric("NA"))),NA,dt2$PO4))
  dt2$DOC <- ifelse((trimws(as.character(dt2$DOC))==trimws("NA")),NA,dt2$DOC)               
  suppressWarnings(dt2$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DOC))==as.character(as.numeric("NA"))),NA,dt2$DOC))
  dt2$TDN <- ifelse((trimws(as.character(dt2$TDN))==trimws("NA")),NA,dt2$TDN)               
  suppressWarnings(dt2$TDN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TDN))==as.character(as.numeric("NA"))),NA,dt2$TDN))
  dt2$DON <- ifelse((trimws(as.character(dt2$DON))==trimws("NA")),NA,dt2$DON)               
  suppressWarnings(dt2$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DON))==as.character(as.numeric("NA"))),NA,dt2$DON))
  dt2$SiO2 <- ifelse((trimws(as.character(dt2$SiO2))==trimws("NA")),NA,dt2$SiO2)               
  suppressWarnings(dt2$SiO2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SiO2))==as.character(as.numeric("NA"))),NA,dt2$SiO2))
  dt2$Mn <- ifelse((trimws(as.character(dt2$Mn))==trimws("NA")),NA,dt2$Mn)               
  suppressWarnings(dt2$Mn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mn))==as.character(as.numeric("NA"))),NA,dt2$Mn))
  dt2$Fe <- ifelse((trimws(as.character(dt2$Fe))==trimws("NA")),NA,dt2$Fe)               
  suppressWarnings(dt2$Fe <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Fe))==as.character(as.numeric("NA"))),NA,dt2$Fe))
  dt2$F <- ifelse((trimws(as.character(dt2$F))==trimws("NA")),NA,dt2$F)               
  suppressWarnings(dt2$F <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$F))==as.character(as.numeric("NA"))),NA,dt2$F))
  dt2$cationCharge <- ifelse((trimws(as.character(dt2$cationCharge))==trimws("NA")),NA,dt2$cationCharge)               
  suppressWarnings(dt2$cationCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$cationCharge))==as.character(as.numeric("NA"))),NA,dt2$cationCharge))
  dt2$anionCharge <- ifelse((trimws(as.character(dt2$anionCharge))==trimws("NA")),NA,dt2$anionCharge)               
  suppressWarnings(dt2$anionCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$anionCharge))==as.character(as.numeric("NA"))),NA,dt2$anionCharge))
  dt2$ionError <- ifelse((trimws(as.character(dt2$ionError))==trimws("NA")),NA,dt2$ionError)               
  suppressWarnings(dt2$ionError <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionError))==as.character(as.numeric("NA"))),NA,dt2$ionError))
  dt2$duplicate <- as.factor(ifelse((trimws(as.character(dt2$duplicate))==trimws("NA")),NA,as.character(dt2$duplicate)))
  dt2$sampleType <- as.factor(ifelse((trimws(as.character(dt2$sampleType))==trimws("NA")),NA,as.character(dt2$sampleType)))
  dt2$ionBalance <- ifelse((trimws(as.character(dt2$ionBalance))==trimws("NA")),NA,dt2$ionBalance)               
  suppressWarnings(dt2$ionBalance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionBalance))==as.character(as.numeric("NA"))),NA,dt2$ionBalance))
  dt2$canonical <- as.factor(ifelse((trimws(as.character(dt2$canonical))==trimws("NA")),NA,as.character(dt2$canonical)))
  dt2$pHmetrohm <- as.factor(ifelse((trimws(as.character(dt2$pHmetrohm))==trimws("NA")),NA,as.character(dt2$pHmetrohm)))
  
  
  #clean data
  
  Water_Chemistry <- dt2 |>
    select(date, pH)|>
    relocate(date, pH,)|>
    group_by(date)|>
    summarise(avg_pH = mean(pH))
  
  Water_Chemistry <- Water_Chemistry |>
    rename('Date' = date)|>
    rename('Value' = avg_pH)
  
  Water_Chemistry <- na.omit(Water_Chemistry)
  
  # Convert the 'date' column to Date type
  wtrDateFormat <- "%Y-%m-%d"
  wtr1date <- as.Date(Water_Chemistry$Date, format=wtrDateFormat)
  if (nrow(Water_Chemistry[Water_Chemistry$Date != "",]) == length(wtr1date[!is.na(wtr1date)])) {
    Water_Chemistry$Date <- wtr1date
  } else {
    print("Date conversion failed for Water_Chemistry$date. Please inspect the data and do the date conversion yourself.")
  }
  
  # Create reactive expression for filtering based on user dates
  filtered_wtr_data <- reactive({
    req(input$dataStartDate, input$dataEndDate)  # Ensure dates are selected
    wtr_data_filtered <- Water_Chemistry %>%
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) %>%
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(wtr_data_filtered)
  })
  
  #soils
  
  options(HTTPUserAgent="EDI_CodeGen")
  
  inUrl6  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/172/4/f25fc11474e2b787cecc67949ecd0028" 
  infile6 <- tempfile()
  try(download.file(inUrl6,infile6,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
  if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")
  
  
  dt6 <-read.csv(infile6,header=F 
                 ,skip=1
                 ,sep=","  
                 ,quot='"' 
                 , col.names=c(
                   "Site_ID",     
                   "Year",     
                   "Plot",     
                   "Horizon",     
                   "Watershed",     
                   "PerCentN",     
                   "PerCentC"    ), check.names=TRUE)
  
  unlink(infile6)
  
  # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  
  if (class(dt6$Site_ID)!="factor") dt6$Site_ID<- as.factor(dt6$Site_ID)
  if (class(dt6$Plot)!="factor") dt6$Plot<- as.factor(dt6$Plot)
  if (class(dt6$Horizon)!="factor") dt6$Horizon<- as.factor(dt6$Horizon)
  if (class(dt6$Watershed)!="factor") dt6$Watershed<- as.factor(dt6$Watershed)
  if (class(dt6$PerCentN)=="factor") dt6$PerCentN <-as.numeric(levels(dt6$PerCentN))[as.integer(dt6$PerCentN) ]               
  if (class(dt6$PerCentN)=="character") dt6$PerCentN <-as.numeric(dt6$PerCentN)
  if (class(dt6$PerCentC)=="factor") dt6$PerCentC <-as.numeric(levels(dt6$PerCentC))[as.integer(dt6$PerCentC) ]               
  if (class(dt6$PerCentC)=="character") dt6$PerCentC <-as.numeric(dt6$PerCentC)
  
  
  #data cleaning
  
  dt6$PerCentN[dt6$PerCentN < 0] <- NA
  dt6$PerCentC[dt6$PerCentC < 0] <- NA
  
  
  dt6 <- na.omit(dt6)
  
  Soil_Nitrogen <- dt6|>
    select(Year, PerCentN)|>
    group_by(Year)|>
    summarise(avg_N = mean(PerCentN))
  
  Soil_Nitrogen <- Soil_Nitrogen |>
    rename('Date' = Year)|>
    rename('Value' = avg_N)
  
  Soil_Nitrogen <- na.omit(Soil_Nitrogen)
  
  # Convert the 'date' column to Date type
  nitDateFormat <- "%Y-%m-%d"
  nit1date <- as.Date(Soil_Nitrogen$Date, format=nitDateFormat)
  if (nrow(Soil_Nitrogen[Soil_Nitrogen$Date != "",]) == length(nit1date[!is.na(nit1date)])) {
    Soil_Nitrogen$Date <- nit1date
  } else {
    print("Date conversion failed for Soil_Nitrogen$date. Please inspect the data and do the date conversion yourself.")
  }
  
  # Create reactive expression for filtering based on user dates
  filtered_nit_data <- reactive({
    req(input$dataStartDate, input$dataEndDate)  # Ensure dates are selected
    nit_data_filtered <- Soil_Nitrogen |>
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) |>
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(nit_data_filtered)
  })
  
  
  Soil_Carbon <- dt6 |>
    select(Year, PerCentC) |>
    group_by(Year)|>
    summarise(avg_C = mean(PerCentC))
  
  Soil_Carbon <- Soil_Carbon |>
    rename('Date' = Year)|>
    rename('Value' = avg_C)
  
  Soil_Carbon <- na.omit(Soil_Carbon)
  
  # Convert the 'date' column to Date type
  carDateFormat <- "%Y-%m-%d"
  car1date <- as.Date(Soil_Carbon$Date, format=carDateFormat)
  if (nrow(Soil_Carbon[Soil_Carbon$Date != "",]) == length(car1date[!is.na(car1date)])) {
    Soil_Carbon$Date <- car1date
  } else {
    print("Date conversion failed for Soil_Carbon$date. Please inspect the data and do the date conversion yourself.")
  }
  
  # Create reactive expression for filtering based on user dates
  filtered_car_data <- reactive({
    req(input$dataStartDate, input$dataEndDate)  # Ensure dates are selected
    car_data_filtered <- Soil_Carbon |>
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) |>
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(car_data_filtered)
  })
  
  # Reactive: Compute bins and colors once for consistency
  binningInfo <- reactive({
    req(input$quiltsize, selectedColor() != "None", input$colorquantity)
    
    # Get dataset
    quilt_data <- datasetInput()
    req(quilt_data)
    
    # Define the number of bins based on user selection
    bins <- as.numeric(input$colorquantity)
    
    # Calculate bin breaks using quantiles
    bin_breaks <- quantile(quilt_data$Value, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    
    # Create color palette based on the selected color scheme
    color_palette <- colorRampPalette(color_ramps[[selectedColor()]])(bins)
    
    # Return bin breaks and color palette for use
    list(bin_breaks = bin_breaks, color_palette = color_palette)
  })
  
  # Reactive expression to load dataset based on selection
  datasetInput <- reactive({
    if (!is.null(input$fileupload)) {
      df <- read.csv(input$fileupload$datapath)
    } else {
      df <- switch(input$defaultdataselect,
                   "Temperature" = Temperature,
                   "Water_Chemistry" = Water_Chemistry,
                   "Soil_Carbon" = Soil_Carbon,
                   "Soil_Nitrogen" = Soil_Nitrogen,
                   stop("Please select a dataset"))
    }
    
    # If there's no 'Date' column, check for 'Year' and create a 'Date' column
    if (!"Date" %in% colnames(df)) {
      if ("Year" %in% colnames(df)) {
        df$Date <- as.Date(paste0(df$Year, "-01-01"))  # Creating a Date from 'Year'
      } else {
        stop("Dataset does not contain a 'Date' or 'Year' column.")
      }
    } else {
      df$Date <- as.Date(df$Date)  # Ensure 'Date' column is in Date format
    }
    df <- df[!is.na(df$Value), ]
    return(df)
  })
  
  observe({
    df <- datasetInput()  # Get the dataset
    
    # Check if dataset is not NULL and output first few rows to console
    if (!is.null(df)) {
      cat("First few rows of df:\n")
      cat(paste0(capture.output(head(df)), collapse = "\n"), "\n")  # print head(df) to console
    }
  })
  
  # Reactive value to store selected color
  selectedColor <- reactiveVal("None")
  
  # Update selected color based on button click
  observeEvent(input$color_bluegreen, { selectedColor("Blue-Green")
    updateSelectInput(session, "color_ramp", selected = "Blue-Green")})
  observeEvent(input$color_greenred, { selectedColor("Green-Red")
    updateSelectInput(session, "color_ramp", selected = "Green-Red")})
  observeEvent(input$color_redwhite, { selectedColor("Red-White")
    updateSelectInput(session, "color_ramp", selected = "Red-White")})
  observeEvent(input$color_bluewhite, { selectedColor("Blue-White")
    updateSelectInput(session, "color_ramp", selected = "Blue-White")})
  observeEvent(input$color_brownwhite, { selectedColor("Brown-White")
    updateSelectInput(session, "color_ramp", selected = "Brown-White")})
  observeEvent(input$color_greenyellow, { selectedColor("Green-Yellow")
    updateSelectInput(session, "color_ramp", selected = "Green-Yellow")})
  observeEvent(input$color_redblue, { selectedColor("Red-Blue")
    updateSelectInput(session, "color_ramp", selected = "Red-Blue")})
  observeEvent(input$color_redyellow, { selectedColor("Red-Yellow") 
    updateSelectInput(session, "color_ramp", selected = "Red-Yellow")})
  
  observe({
    cat("Selected Color:", selectedColor(), "\n")
  })
  
  # Handle file upload and preview
  dataFile <- reactive({
    req(input$fileupload)
    read.csv(input$fileupload$datapath)
  })
  
  output$dataPreview <- renderTable({
    req(dataFile())
    head(dataFile())  # Show first few rows of uploaded file
  })
  
  output$dateSliderUI <- renderUI({
    data <- datasetInput()  # Use the reactive dataset
    
    # Check if the dataset contains a valid 'Date' column
    if (is.null(data) || !"Date" %in% colnames(data)) {
      return(NULL)  # Return NULL if the dataset doesn't have a 'Date' column
    }
    
    # Generate the date range slider using the 'Date' column
    sliderInput("dateRange", "Select Date Range:",
                min = min(data$Date, na.rm = TRUE),
                max = max(data$Date, na.rm = TRUE),
                value = c(min(data$Date, na.rm = TRUE), max(data$Date, na.rm = TRUE)),
                timeFormat = "%Y-%m-%d")
  })
  
  
  
  # Color palettes for ombre effect
  color_ramps <- list(
    "Blue-Green" = c("#0000FF", "#00FFFF", "#00FF00"),  
    "Green-Red" = c("#008000", "#FFFF00", "#FF0000"),  
    "Red-White" = c("#FF0000", "#FFA07A", "#FFFFFF"), 
    "Blue-White" = c("#0000FF", "#87CEFA", "#FFFFFF"),  
    "Brown-White" = c("#8B4513", "#D2B48C", "#FFFFFF"),  
    "Green-Yellow" = c("#006400", "#ADFF2F", "#FFFF00"),  
    "Red-Blue" = c("#FF0000", "#800080", "#0000FF"),  
    "Red-Yellow" = c("#FF0000", "#FF8C00", "#FFFF00")   
  )
  
  
  # Generate quilt design with ombre effect
  output$quiltPlot <- renderPlot({
    req(binningInfo())  # Ensure bins are available
    
    quilt_data <- datasetInput()
    req(quilt_data)
    
    # Get quilt size
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    total_squares <- quilt_size[1] * quilt_size[2]
    
    # Apply binning to the data
    bin_breaks <- binningInfo()$bin_breaks
    color_palette <- binningInfo()$color_palette
    
    # Bin the values based on quantiles
    quilt_data$category <- cut(quilt_data$Value, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE)
    quilt_data$color <- color_palette[as.numeric(quilt_data$category)]
    
    # Generate quilt grid
    quilt_grid <- expand.grid(x = 1:quilt_size[1], y = 1:quilt_size[2])
    quilt_grid$category <- rep(quilt_data$category, length.out = nrow(quilt_grid))
    quilt_grid$color <- color_palette[as.numeric(quilt_grid$category)]
    
    # Plot quilt design with legend
    ggplot(quilt_grid, aes(x = x, y = y, fill = color)) +
      geom_tile(color = "black", width = 1, height = 1) +  # Each quilt square
      scale_fill_manual(values = color_palette) +  # Use the custom color palette
      theme_void() +  # Remove axes and labels
      coord_fixed() +  # Keep squares equal in size
      labs(title = "Quilt Preview") +
      theme(legend.position = "right")  # Position the legend to the right of the quilt
  })
  
  #Fabric Calculation
  output$fabricTable <- renderTable({
    req(binningInfo())  # Ensure bins are available
    
    quilt_data <- datasetInput()
    req(quilt_data)
    
    # Assign colors based on bins
    bins <- as.numeric(input$colorquantity)
    color_palette <- binningInfo()$color_palette
    
    # Bin the values into categories based on quantiles
    bin_breaks <- binningInfo()$bin_breaks
    quilt_data$category <- cut(quilt_data$Value, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE)
    quilt_data$color <- color_palette[as.numeric(quilt_data$category)]
    
    # Select only the displayed quilt's data based on selected size
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    total_squares <- quilt_size[1] * quilt_size[2]
    quilt_data <- quilt_data[1:total_squares, ]
    
    # Count occurrences of each color
    fabric_counts <- quilt_data %>%
      group_by(color) %>%
      summarise(Squares = n()) %>%
      mutate(
        SquareSize = 6,  # Inches per square
        SeamAllowance = 0.25,  # Extra fabric for sewing
        FabricNeeded = Squares * (SquareSize + 2 * SeamAllowance)^2 / 144  # Convert to square feet
      ) %>%
      rename("Color" = color, "Fabric Needed (sq ft)" = FabricNeeded)
    
    return(fabric_counts)
  })
  
  
  # Download dummy quilt pattern
  output$downloadQuilt <- downloadHandler(
    filename = function() { "quilt_pattern.pdf" },
    content = function(file) {
      writeLines("This is your quilt pattern placeholder.", file)
    }
  )
  
  observeEvent(input$shareButton, {
    url <- session$clientData$url_hostname
    
    # Modify URL for different platforms
    twitter_url <- paste0("https://twitter.com/intent/tweet?text=Check%20out%20this%20Quilt!&url=", url)
    facebook_url <- paste0("https://www.facebook.com/sharer/sharer.php?u=", url)
    pinterest_url <- paste0("https://www.pinterest.com/pin/create/button/?url=", url, "&description=My%20Quilt%20Design")
    
    # Open a pop-up window with share options
    showModal(
      modalDialog(
        title = "Share Your Quilt!",
        tags$a(href = twitter_url, "Share on Twitter", target = "_blank", style = "display:block; margin-bottom: 10px;"),
        tags$a(href = facebook_url, "Share on Facebook", target = "_blank", style = "display:block; margin-bottom: 10px;"),
        tags$a(href = pinterest_url, "Share on Pinterest", target = "_blank", style = "display:block;"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })    
  
  # Open fabric website
  observeEvent(input$fabricWebsite, {
    browseURL("https://www.spoonflower.com")  #Online fabric Store that correlates Colors from R
  })
  
  # Share design (works on mobile)
  observeEvent(input$shareButton, {
    session$sendCustomMessage(type = "share", 
                              message = list(title = "Check out this Quilt!", 
                                             url = session$clientData$url_hostname))
  })
  
}

shinyApp(ui = ui, server = server)
