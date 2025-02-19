
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
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
                     plotOutput("distPlot")
                   )
                 ))),
      
      tabPanel("Data Setup", 
               h3("Choose Your Data!"),
               p("example"
                 
               )),
      
      tabPanel("View & Share",
               h3("Preview Your Design & Share!"),
               p("This is example content for the third tab.")),
      
      tabPanel("User Guide",
               h3("How to Use App!"),
               p("This is example content for the third tab."))
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
