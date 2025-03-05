#QUILT PROJECT - BASIC FUNCTIONALITY - KELLIE WILLIAMS


#######################################################

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
                               choices = c("5", "6", "7", "8", "9", "10"))
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
                               choices = c("Temperature",
                                           "Water Chemistry",
                                           "Soils",
                                           "Vegetation",
                                           "Heterotrophs")),
                   
                   helpText("OR"),
                   
                   # Data Upload button
                   fileInput("fileupload", "Upload Your Own Data File!",
                             accept = ".csv"),
                   textOutput("dataInfo"),
                   
                   # Select Time Period of Data
                   helpText("Now select the time period your quilt will show!"),
                   dateInput("dataStartDate", "Select a Start Date:", 
                             value = Sys.Date(),
                             format = "mm/dd/yyyy"),
                   dateInput("dataEndDate", "Select an End Date:", 
                             value = Sys.Date(),
                             format = "mm/dd/yyyy")
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
                   helpText("You Need... X Amount for each color * Figure out How to Calculate *"),
                 ),
                 
                 mainPanel(
                   h3("Your Quilt Design"),
                   plotOutput("quiltPlot")
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
                               type of data, you can upload your own dataset in .csv format! Lastly, select both
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
  
  # Reactive value to store selected color
  selectedColor <- reactiveVal("None")
  
  # Update selected color based on button click
  observeEvent(input$color_bluegreen, { selectedColor("Blue-Green") })
  observeEvent(input$color_greenred, { selectedColor("Green-Red") })
  observeEvent(input$color_redwhite, { selectedColor("Red-White") })
  observeEvent(input$color_bluewhite, { selectedColor("Blue-White") })
  observeEvent(input$color_brownwhite, { selectedColor("Brown-White") })
  observeEvent(input$color_greenyellow, { selectedColor("Green-Yellow") })
  observeEvent(input$color_redblue, { selectedColor("Red-Blue") })
  observeEvent(input$color_redyellow, { selectedColor("Red-Yellow") })
  
  # Display selected color
  output$selectedColor <- renderText({ paste("Selected Color Scheme:", selectedColor()) })
  
  # Handle file upload and preview
  dataFile <- reactive({
    req(input$fileupload)
    read.csv(input$fileupload$datapath)
  })
  
  output$dataPreview <- renderTable({
    req(dataFile())
    head(dataFile())  # Show first few rows of uploaded file
  })
  
  # Color palettes for ombre effect
  color_schemes <- list(
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
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    
    selected_scheme <- selectedColor()
    
    # Generate ombre gradient based on selection
    if (selected_scheme %in% names(color_schemes)) {
      ombre_colors <- colorRampPalette(color_schemes[[selected_scheme]])(as.numeric(input$colorquantity))
    } else {
      ombre_colors <- rainbow(as.numeric(input$colorquantity))  # Default if no color selected
    }
    
    # Generate quilt grid
    quilt_data <- expand.grid(x = 1:quilt_size[1], y = 1:quilt_size[2])
    
    # Apply a gradient effect instead of random colors
    quilt_data$color <- rep(ombre_colors, length.out = nrow(quilt_data))
    
    # Plot quilt design with ombre effect
    ggplot(quilt_data, aes(x, y, fill = color)) +
      geom_tile(color = "black") +
      scale_fill_identity() +
      theme_void() +
      coord_fixed() +
      labs(title = "Quilt Preview")
  })
  
  # Download dummy quilt pattern
  output$downloadQuilt <- downloadHandler(
    filename = function() { "quilt_pattern.pdf" },
    content = function(file) {
      writeLines("This is your quilt pattern placeholder.", file)
    }
  )
  
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

