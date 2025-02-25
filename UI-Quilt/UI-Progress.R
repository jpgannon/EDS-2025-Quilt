
library(tidyverse)
library(shiny)

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
                                 choices = c("5",
                                             "6",
                                             "7",
                                             "8",
                                             "9",
                                             "10",
                                             "11",
                                             "12"))
                     ),
                   
                   # Show Images of color patches, making them selectable buttons
                   mainPanel(
                     plotOutput("distPlo")
                   )
                 ))),
      
      tabPanel("Data Setup", 
               h3("Choose Your Data!"),
               p(
                 #Default Dataset Selection
                 mainPanel(
                   selectInput("defaultdataselect",
                               "Choose Your Data Type!", 
                               choices = c("Temperature",
                                           "Water Chemistry",
                                           "Soils",
                                           "Vegetation",
                                           "Heterotrophs")),
                   
                   helpText("OR"),
                   
                   #Data Upload button
                   fileInput("fileupload", "Upload Your Own Data File!",
                           accept = ".csv"),
                   textOutput("dataInfo"),
                 
                   #Select Time Period of Data
                   helpText("Now select the time period your quilt will show!"),
                   dateInput("dataStartDate", "Select a Start Date:", 
                             value = Sys.Date(),
                             format = "mm/dd/yyyy"),
                   dateInput("dataEndDate", "Select an End Date:", 
                             value = Sys.Date(),
                             format = "mm/dd/yyyy"),
               )
                 
               )),
      
      tabPanel("View & Share",
               h3("Preview Your Design & Share!"),
               p(
                 sidebarLayout(
                   sidebarPanel(
                     
                     downloadButton("downloadQuilt,
                                    Download Quilt Pattern")
                     
                   )
                 )
               )),
      
      tabPanel("User Guide",
               h3("How to Use App!"),
               p(
                 helpText("Tutorial on How to Use our App!")
               ))
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
