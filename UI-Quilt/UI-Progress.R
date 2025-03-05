
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

server <- function(input, output) {
  
  # Reactive values to store user selections
  selectedColorRamp <- reactiveVal("viridis")  # Default color scheme
  quiltRows <- reactiveVal(10)  # Default rows
  quiltCols <- reactiveVal(10)  # Default columns
  numColors <- reactiveVal(5)  # Default number of colors
  
  # Observe quilt size selection
  observeEvent(input$quiltsize, {
    size_map <- list(
      "5x7 (Baby)" = c(5, 7),
      "6x9 (Crib)" = c(6, 9),
      "9x11 (Throw)" = c(9, 11),
      "12x15 (Twin)" = c(12, 15),
      "14x18 (Full)" = c(14, 18),
      "15x18 (Queen)" = c(15, 18),
      "18x18 (King)" = c(18, 18)
    )
    quiltRows(size_map[[input$quiltsize]][1])
    quiltCols(size_map[[input$quiltsize]][2])
  })
  
  # Observe color quantity selection
  observeEvent(input$colorquantity, {
    numColors(as.numeric(input$colorquantity))
  })
  
  # Observe events for color ramp selection
  observeEvent(input$color_bluegreen, { selectedColorRamp("Blue-Green") })
  observeEvent(input$color_greenred, { selectedColorRamp("Green-Red") })
  observeEvent(input$color_redwhite, { selectedColorRamp("Red-White") })
  observeEvent(input$color_bluewhite, { selectedColorRamp("Blue-White") })
  observeEvent(input$color_brownwhite, { selectedColorRamp("Brown-White") })
  observeEvent(input$color_greenyellow, { selectedColorRamp("Green-Yellow") })
  observeEvent(input$color_redblue, { selectedColorRamp("Red-Blue") })
  observeEvent(input$color_redyellow, { selectedColorRamp("Red-Yellow") })
  
  # Generate heatmap
  output$quiltPlot <- renderPlot({
    rows <- quiltRows()
    cols <- quiltCols()
    
    # Generate data matrix based on selected quilt size
    data <- matrix(runif(rows * cols, min = 0, max = 1), nrow = rows)
    
    # Define color palettes
    color_palettes <- list(
      "Blue-Green" = scale_fill_gradientn(colors = colorRampPalette(c("#0192FF", "#3EAB52"))(numColors())),
      "Green-Red" = scale_fill_gradientn(colors = colorRampPalette(c("#3EAB52", "#E41B1B"))(numColors())),
      "Red-White" = scale_fill_gradientn(colors = colorRampPalette(c("#C91717", "#FFFFFF"))(numColors())),
      "Blue-White" = scale_fill_gradientn(colors = colorRampPalette(c("#0163BF", "#FFFFFF"))(numColors())),
      "Brown-White" = scale_fill_gradientn(colors = colorRampPalette(c("#5E3115", "#FFFFFF"))(numColors())),
      "Green-Yellow" = scale_fill_gradientn(colors = colorRampPalette(c("#066C00", "#FFEA06"))(numColors())),
      "Red-Blue" = scale_fill_gradientn(colors = colorRampPalette(c("#E72828", "#4EA3FF"))(numColors())),
      "Red-Yellow" = scale_fill_gradientn(colors = colorRampPalette(c("#FF2A2A", "#FFEF3B"))(numColors()))
    )
    
    
    heatmap_df <- expand.grid(x = 1:cols, y = 1:rows)
    heatmap_df$z <- as.vector(data)
    
    ggplot(heatmap_df, aes(x, y, fill = z)) +
      geom_tile(color = "black") +
      color_palettes[[selectedColorRamp()]] +  # Apply selected color scheme
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks = element_blank(),
        legend.position = "none"
      ) +
      labs(title = " ")
  })
}


shinyApp(ui = ui, server = server)
