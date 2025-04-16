library(tidyverse)
library(shiny)
library(lubridate)
library(ggplot2)
library(shinythemes)
library(shinyjs)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Environmental Data Quilt!"),
  
  theme = shinytheme("cerulean"),  # You can choose other themes like "cosmo" or "sandstone"
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; }  /* Light gray background */
      .well { background-color: white; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); } /* Side panel cards */
      h3 { color: #2c3e50; font-weight: bold; }  /* Darker headings */
      .btn-primary { background-color: #007bff; border-color: #007bff; }  /* Stylish buttons */
      .btn-primary:hover { background-color: #0056b3; } /* Button hover effect */
      .tab-content { padding-top: 20px; }
    "))
  ),
  
  tabsetPanel(
    
    tabPanel("📊 Data Setup", 
             h3("Choose Your Data!"),
             p(
               # Default Dataset Selection
               sidebarLayout(
                 sidebarPanel(
                   selectInput("defaultdataselect",
                               "Choose Your Data Type!", 
                               choices = c("Avg Temperature" = "Temperature",
                                           "Stream Chemistry (pH)" = "Stream_Chemistry",
                                           "Precipitation pH" = "Precipitation",
                                           "Soil Carbon" = "Soil_Carbon",
                                           "Soil Nitrogen" = "Soil_Nitrogen")),
                   
                   helpText("OR"),
                   
                   # Data Upload button
                   fileInput("fileupload", "Upload Your Own Data File!",
                             accept = ".csv"),
                   textOutput("dataInfo"),
                   
                   ###############                                      
                   selectInput(
                     inputId = "layout_mode",
                     label = "Data Display Mode",
                     choices = c("Chronological", "One Year per Row"),
                     selected = NULL
                   ),
                   
                   #################                   
                   
                   # Select Time Period of Data
                   helpText("Now select the time period your quilt will show! 
                            Date slider may take a moment to load, please wait!"),
                   
                   uiOutput("dateSliderUI")
                 ),
                 
                 mainPanel(
                   fluidRow(
                     column(12, plotOutput("dataPreview")),  # Existing plot
                     column(12, plotOutput("squaresPlot"))   # New plot below
                   )
                 )
               )
             )
    ),
    
    
    tabPanel("🎨 Design", 
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
                               "Choose Number of Colors",
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
    
    
    tabPanel("📷 View & Share",
             h3("Preview Your Design & Share!"),
             tags$script(HTML("
  Shiny.addCustomMessageHandler('openTab', function(url) {
    window.open(url, '_blank');
  });
")),
             p(
               sidebarLayout(
                 sidebarPanel(
                   #Download Button
                   downloadButton("downloadQuilt", "Download Quilt Pattern"),
                   #add border of chosen color
                   downloadButton("save_hex_colors", "Download Hex Colors"),
                   br(),
                   selectInput("border_color", "Choose Border Color:", 
                               choices = c("Black" = "black", "Gray" = "gray", "Blue" = "blue", "Red" = "red", "Yellow" = "yellow")),
                   sliderInput("border_size", "Border Size:", min = 0, max = 5, value = 10),
                   checkboxInput("add_border", "Add Border", value = FALSE),
                   checkboxInput("reverse_colors", "Reverse Color Scheme", value = FALSE),
                   checkboxInput("show_ids", "Show Color ID Number", value = FALSE),
                   tags$a(href = "https://www.spoonflower.com", 
                          target = "_blank", 
                          class = "btn btn-primary", 
                          "Visit Spoonflower Fabric Website"),
                   tags$a(href = "https://fabric.alisongale.com/", 
                          target = "_blank", 
                          class = "btn btn-primary", 
                          "Visit Hex Code to Fabric Website"),
                   actionButton("shareButton", "Share Your Design!", 
                                style = "margin-bottom: 20px; display: block;",
                                onclick = "navigator.share({title: 'Check out this Quilt!', url: window.location.href})",
                                style = "margin-top: 20px;"),
                   
                 ),
                 
                 mainPanel(
                   h3("Your Quilt Design"),
                   helpText("If no design appears, you must select a color scheme."),
                   helpText("Design shows data chronologically from top to bottom; Top is earliest data, bottom is most recent data."),
                   plotOutput("quiltPlot"),
                   h4("Fabric Requirements"),
                   tableOutput("fabricTable")
                 )
               )
             )
    ),
    
    tabPanel("📚 User Guide",
             h3("How to Use App!"),
             p(
               tags$p("Tutorial on How to Use our App!"),
               
               tags$p("Tab 1: Data Setup",
                      helpText("This is where you will select or upload your dataset of choice, and select the timeframe
                               you would like your quilt to represent. This tab also allows you to preview the dataset on
                               a line graph before moving forward to the quilt design. Use the first dropdown to select the category of data 
                               your quilt will portray, which will use a pre-loaded dataset found in the Hubbard Brook
                               Data Catalog to build your quilt design. Or, if you want to use a different 
                               type of data, you can upload your own dataset in .csv format! Note!: If you choose to 
                               upload your own data, you only need to include a date column titled 'Date' and a column containing
                               your data values titled 'Value' in your .csv file. Lastly, select both
                               a start date and end date using the interactive slider,to specify the time 
                               frame of data that your quilt will show. This can be helpful when working with larger datasets
                               that cover tens of years. To the right of the menu, a plot of your data in blue will appear over time,
                               and will change dynamically as you change the dataset and time period. Below this plot, is another
                               graph in red, that shows the data values for each square in the quilt design that appears
                               on the following tab, in order from top to bottom, or increasing index.")),
               
               tags$p("Tab 2: Design",
                      helpText("In this tab you will have the opportunity to choose your desired quilt size,
                               choose the color scheme for your quilt, as well as the amount of colors you 
                               want to display on your quilt. Use the top dropdown to select your size, where the 
                               numbers listed by each type represents the count of squares width by the count of square 
                               height. The second dropdown to select your color quantity, 4 for smaller quilt sizes, 
                               and 8 for larger ones. Select any of the color ramp buttons to select your choice of 
                               color scheme.")),
               
               tags$p("Tab 3: View & Share",
                      helpText("This tab allows you to preview your quilt design with the data you selected to show,
                               as well as the color scheme and size you selected previously. Use the download pattern button at
                               the top of the sidebar to save your design on to your device as a PDF. Use the download hex code button
                               to get a .csv file of your hex codes for your colors needed. Use the buttons to visit a couple different 
                               fabric websites. The Spoonflower link directs you to a craft store website, where you can choose your colors and purchase the 
                               amount of fabric necessary, and the Hex Fabric Match link is a website that will allow you to input a hex code and the website
                               will return fabric colors similar to the inputted hex code, where you can then buy from your favorite local craft store.
                               You also have the option to add a border
                               to your design, using the checkbox, as well as the dropdown to select your border color, if a border is 
                               desired. The slider also allows you to change the thickness of the border if you choose to add one. The 'share your design' button can be used to share your design on social media, on Pinterest, 
                               Twitter (X), or Facebook. The right hand side
                               of this page is where your quilt design preview will appear! The hex codes with color swatches are shown
                               on the right, in addition to the table below your design, containing your hex codes/colors needed
                               for your quilt, how many squares there are of each color, the total square feet of fabric needed 
                               for each color, as well as the range of your data values that fall into each color category.")),
               tags$p("This app was made by Hannah Crook, Mason Gooder, and Kellie Williams as a part of our Environmental Data Science
                      Capstone class, taught by Dr. JP Gannon at Virginia Tech."),
               tags$p("Thank you and we hope you enjoy our app!")
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Read the CSV files from GitHub
  
  #Temperature
  Temperature <- read_csv("Data/HBEF_air_temp_daily_1957-2024.csv")
  
  Temperature <- Temperature |>
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
    data_filtered <- Temperature |>
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) |>
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(data_filtered)
  })
  
  
  #Precipitation
  Precipitation <- read_csv("Data/HubbardBrook_weekly_precipitation_chemistry_1963-2024.csv")
  
  Precipitation <- Precipitation |>
    select(date, pH)|>
    relocate(date, pH,)|>
    group_by(date)|>
    summarise(avg_pH = mean(pH))
  
  Precipitation <- Precipitation |>
    rename('Date' = date)|>
    rename('Value' = avg_pH)
  
  Precipitation <- na.omit(Precipitation)
  
  # Convert the 'date' column to Date type
  preDateFormat <- "%Y-%m-%d"
  pre1date <- as.Date(Precipitation$Date, format=preDateFormat)
  if (nrow(Precipitation[Precipitation$Date != "",]) == length(pre1date[!is.na(pre1date)])) {
    Precipitation$Date <- pre1date
  } else {
    print("Date conversion failed for Precipitation$date. Please inspect the data and do the date conversion yourself.")
  }
  
  # Create reactive expression for filtering based on user dates
  filtered_pre_data <- reactive({
    req(input$dataStartDate, input$dataEndDate)  # Ensure dates are selected
    pre_data_filtered <- Precipitation |>
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) |>
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(pre_data_filtered)
  })
  
  #Stream Chemistry
  Stream_Chemistry <- read_csv("Data/HubbardBrook_weekly_stream_chemistry_1963-2024.csv")
  
  Stream_Chemistry <- Stream_Chemistry |>
    select(date, pH)|>
    relocate(date, pH,)|>
    group_by(date)|>
    summarise(avg_pH = mean(pH))
  
  Stream_Chemistry <- Stream_Chemistry |>
    rename('Date' = date)|>
    rename('Value' = avg_pH)
  
  Stream_Chemistry <- na.omit(Stream_Chemistry)
  
  # Convert the 'date' column to Date type
  strDateFormat <- "%Y-%m-%d"
  str1date <- as.Date(Stream_Chemistry$Date, format=strDateFormat)
  if (nrow(Stream_Chemistry[Stream_Chemistry$Date != "",]) == length(str1date[!is.na(str1date)])) {
    Stream_Chemistry$Date <- str1date
  } else {
    print("Date conversion failed for Stream_Chemistry$date. Please inspect the data and do the date conversion yourself.")
  }
  
  # Create reactive expression for filtering based on user dates
  filtered_str_data <- reactive({
    req(input$dataStartDate, input$dataEndDate)  # Ensure dates are selected
    str_data_filtered <- Stream_Chemistry |>
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) |>
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(str_data_filtered)
  })
  
  #Soil Composition
  Soil <- read_csv("Data/HubbardBrook_ForestFloor_CN_W6.csv")
  
  Soil_Nitrogen <- Soil |>
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
  
  
  Soil_Carbon <- Soil |>
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
  
  # Reactive expression to load dataset based on selection
  datasetInput <- reactive({
    if (!is.null(input$fileupload)) {
      df <- read.csv(input$fileupload$datapath)
    } else {
      df <- switch(input$defaultdataselect,
                   "Temperature" = Temperature,
                   "Stream_Chemistry" = Stream_Chemistry,
                   "Precipitation" = Precipitation,
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
    df <- datasetInput()
    if ("Date" %in% colnames(df)) {
      minDate <- min(df$Date, na.rm = TRUE)
      maxDate <- max(df$Date, na.rm = TRUE)
      
      # Only update min and max, keep user's selection
      updateSliderInput(session, "dateRange",
                        min = minDate,
                        max = maxDate)
    }
  })
  
  # Reactive expression for filtering the data based on the selected date range
  filteredData <- reactive({
    df <- datasetInput()
    
    if (!is.null(input$dateRange)) {
      df <- df[df$Date >= input$dateRange[1] & df$Date <= input$dateRange[2], ]
    }
    
    df <- df[order(df$Date), ]
    return(df)
  })
  
  binnedData <- reactive({
    df <- filteredData()
    req(nrow(df) > 0, input$quiltsize)
    
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    num_squares <- prod(quilt_size)
    
    if (input$layout_mode == "One Year per Row") {
      df$Year <- year(df$Date)
      years_to_plot <- sort(unique(df$Year), decreasing = TRUE)[1:quilt_size[2]]  # most recent N years
      df <- df |> filter(Year %in% years_to_plot)
      
      binned_df <- df |>
        group_by(Year) |>
        arrange(Date) |>
        mutate(bin_index = ntile(row_number(), quilt_size[1])) |>
        group_by(Year, bin_index) |>
        summarize(Date = min(Date), AvgValue = mean(Value, na.rm = TRUE), .groups = "drop") |>
        mutate(Row = match(Year, sort(years_to_plot)))  # map to quilt row (1 = top)
      
    } else {
      df$bin_index <- cut(df$Date, breaks = num_squares, labels = FALSE)
      binned_df <- df |>
        group_by(bin_index) |>
        summarize(Date = min(Date), AvgValue = mean(Value, na.rm = TRUE), .groups = "drop")
    }
    
    return(binned_df)
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
    "Blue-Green" = c("#3333CC", "#3EC0C1", "#008B00"),  
    "Green-Red" = c("#008000", "#FDDA0D", "#D2042D"),  
    "Red-White" = c("#990000", "#FF6666", "#FFFFFF"), 
    "Blue-White" = c("#3333CC", "#3399FF", "#FFFFFF"),  
    "Brown-White" = c("#663300", "#996633", "#FFFFFF"),  
    "Green-Yellow" = c("#006600", "#66CC33", "#FDDA0D"),  
    "Red-Blue" = c("#D2042D", "#9900CC", "#3333CC"),  
    "Red-Yellow" = c("#D2042D", "#FF6633", "#FDDA0D")   
  )
  
  #Plot for data preview
  output$dataPreview <- renderPlot({
    req(filteredData())  # Ensure data is available
    
    df <- filteredData()
    
    # Ensure 'Value' column exists and is numeric
    req("Value" %in% colnames(df))
    df$Value <- as.numeric(df$Value)
    
    # Basic scatter plot (customize based on your dataset)
    ggplot(df, aes(x = Date, y = Value)) +
      geom_point(color = "blue", size = 2, alpha = 0.7) +
      geom_line(color = "blue", alpha = 0.5) +
      labs(title = "Preview Data Plot",
           x = "Date",
           y = "Value") +
      theme_minimal()
  })
  
  # New plot for the data values corresponding to the quilt squares
  output$squaresPlot <- renderPlot({
    binned_df <- binnedData()
    num_squares <- nrow(binned_df)
    
    ggplot(data.frame(Index = 1:num_squares, Value = binned_df$AvgValue),
           aes(x = Index, y = Value)) +
      geom_point(color = "red", size = 2, alpha = 0.7) +
      geom_line(color = "red") +
      labs(title = paste("Data Values for", num_squares, "Quilt Squares"),
           x = "Square Index",
           y = "Data Value") +
      theme_minimal()
  })
  
  plotQuilt <- reactiveVal(NULL)
  
  # Generate quilt design on 3rd tab with ombre effect
  output$quiltPlot <- renderPlot({
    req(selectedColor() != "None", input$colorquantity, input$quiltsize)
    
    binned_df <- binnedData()
    req(nrow(binned_df) > 0)
    
    bins <- min(as.numeric(input$colorquantity), length(unique(binned_df$AvgValue)))
    bin_breaks <- quantile(binned_df$AvgValue, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    binned_df$category <- cut(binned_df$AvgValue, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE)
    
    color_palette <- colorRampPalette(color_ramps[[selectedColor()]])(bins)
    if (input$reverse_colors) {
      color_palette <- rev(color_palette)
    }
    binned_df$color <- color_palette[as.numeric(binned_df$category)]
    
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    if (input$layout_mode == "One Year per Row") {
      quilt_data <- binned_df
      quilt_data$x <- rep(1:quilt_size[1], times = quilt_size[2])[1:nrow(quilt_data)]
      quilt_data$y <- rep(quilt_size[2]:1, each = quilt_size[1])[1:nrow(quilt_data)]
    } else {
      quilt_data <- expand.grid(x = 1:quilt_size[1], y = quilt_size[2]:1)
      quilt_data$category <- rep(binned_df$category, length.out = nrow(quilt_data))
      quilt_data$color <- rep(binned_df$color, length.out = nrow(quilt_data))
    }
    
    quilt_data$category <- factor(quilt_data$category, levels = 1:bins)
    
    border_col <- if (input$add_border) input$border_color else NA
    border_size <- if (input$add_border) input$border_size else 0
    border_offset <- if (input$quiltsize %in% c("14x18 (Full)", "15x18 (Queen)", "18x18 (King)")) {
      border_size * 0.0375
    } else {
      0.02
    }
    
    quilt_border <- data.frame(
      xmin = 0.5 - border_offset, xmax = quilt_size[1] + 0.5 + border_offset,
      ymin = 0.5 - border_offset, ymax = quilt_size[2] + 0.5 + border_offset
    )
    
    legend_labels <- if (input$show_ids) {
      paste0(1:bins, ": ", color_palette)
    } else {
      color_palette
    }
    
    quiltPlot <- ggplot(quilt_data, aes(x, y, fill = factor(category))) +
      geom_tile(color = "black") +
      scale_fill_manual(values = color_palette, labels = legend_labels) +
      theme_void() +
      coord_fixed() +
      labs(title = "Quilt Preview", fill = if (input$show_ids) "Color ID Number and Hex Code" else "Color Hex Code")
    
    if (input$add_border) {
      quiltPlot <- quiltPlot + geom_rect(data = quilt_border, inherit.aes = FALSE,
                                         aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                         color = border_col, fill = NA, linewidth = border_size)
    }
    
    if (input$show_ids) {
      quiltPlot <- quiltPlot + 
        geom_text(aes(label = as.numeric(category)), 
                  color = "black", 
                  size = 3, 
                  alpha = 0.6,
                  fontface = "bold")
    }
    
    plotQuilt(quiltPlot)
    quiltPlot
  })
  
  # Fabric Calculation
  output$fabricTable <- renderTable({
    req(input$quiltsize, selectedColor() != "None", input$colorquantity)
    
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    binned_df <- binnedData()
    req(binned_df)
    
    # Ensure AvgValue is numeric
    binned_df$AvgValue <- as.numeric(binned_df$AvgValue)
    binned_df <- binned_df[!is.na(binned_df$AvgValue), ]
    
    # Binning
    bins <- as.numeric(input$colorquantity)
    if (bins <= 0) stop("Number of bins must be greater than zero.") 
    
    bin_breaks <- quantile(binned_df$AvgValue, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    binned_df$category <- cut(binned_df$AvgValue, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE)
    
    # Generate color palette
    color_palette <- colorRampPalette(color_ramps[[selectedColor()]])(bins)
    if (input$reverse_colors) {
      color_palette <- rev(color_palette)
    }
    
    binned_df$color <- color_palette[binned_df$category]
    
    # Quilt grid
    quilt_data <- expand.grid(x = 1:quilt_size[1], y = 1:quilt_size[2])
    quilt_data$category <- rep(binned_df$category, length.out = nrow(quilt_data))
    quilt_data$color <- rep(binned_df$color, length.out = nrow(quilt_data))
    
    # Count squares per color
    fabric_counts <- quilt_data |>
      group_by(color) |>
      summarise(Squares = n(), .groups = 'drop') |>
      mutate(
        SquareSize = 6,
        SeamAllowance = 0.25,
        FabricNeededSqFt = Squares * (SquareSize + 2 * SeamAllowance)^2 / 144,
        FabricNeededYards = FabricNeededSqFt / 9
      )
    
    # Data range info
    bin_ranges <- data.frame(
      category = 1:bins,
      MinValue = bin_breaks[-length(bin_breaks)],
      MaxValue = bin_breaks[-1]
    )
    
    fabric_counts <- fabric_counts |>
      mutate(category = match(color, color_palette)) |>
      left_join(bin_ranges, by = "category") |>
      mutate(`Data Range` = paste0(round(MinValue, 2), " - ", round(MaxValue, 2))) |>
      rename("Color" = color, "Fabric Needed (Yards)" = FabricNeededYards) |>
      select(Color, Squares, `Fabric Needed (Yards)`, `Data Range`) |>
      arrange(desc(`Data Range`))
    
    # Ensure at least one row
    if (nrow(fabric_counts) == 0) {
      return(data.frame(
        Color = NA, Squares = NA, `Fabric Needed (Yards)` = NA, `Data Range` = "No data"
      ))
    }
    
    return(fabric_counts)
  })
  
  
  quiltColors <- reactive({
    req(selectedColor(), input$colorquantity)
    
    bins <- as.numeric(input$colorquantity)
    color_palette <- colorRampPalette(color_ramps[[selectedColor()]])(bins)
    
    return(color_palette)
  })
  
  # Download handler to save hex colors
  output$save_hex_colors <- downloadHandler(
    filename = function() {
      paste0("quilt_colors_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(quiltColors(), file, row.names = FALSE, col.names = FALSE)
    }
  )  
  
  
  output$downloadQuilt <- downloadHandler(
    filename = function() { "quilt_pattern.pdf" },
    content = function(file) {
      req(plotQuilt())  # Ensure the plot is available
      
      pdf(file)
      print(plotQuilt())  # Retrieve and print stored plot
      dev.off()
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
  
  
  # Open Spoonflower link
  observeEvent(input$spoonflower, {
    session$sendCustomMessage("openTab", "https://www.spoonflower.com")
  })
  
  # Open Hex Code to Fabric Website
  observeEvent(input$hexfabricmatch, {
    session$sendCustomMessage("openTab", "https://fabric.alisongale.com/")
  })
  
  
  # Share design (works on mobile)
  observeEvent(input$shareButton, {
    session$sendCustomMessage(type = "share", 
                              message = list(title = "Check out this Quilt!", 
                                             url = session$clientData$url_hostname))
  })
  
}
shinyApp(ui = ui, server = server)
