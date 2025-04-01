library(tidyverse)
library(shiny)
library(lubridate)
library(ggplot2)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Environmental Data Quilt!"),
  
  theme = shinytheme("cerulean"),  # You can choose other themes like "cerulean", "cosmo", "sandstone"
  
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
    
    tabPanel("📷 View & Share",
             h3("Preview Your Design & Share!"),
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
                   checkboxInput("add_border", "Add Border", value = TRUE),
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
    
    tabPanel("📚 User Guide",
             h3("How to Use App!"),
             p(
               tags$p("Tutorial on How to Use our App!"),
               tags$p("Tab 1: Design",
                      helpText("In this tab you will have the opportunity to choose your desired quilt size,
                               choose the color scheme for your quilt, as well as the amount of colors you 
                               want to display on your quilt. Use the top dropdown to select your size, where the 
                               numbers listed by each type represents the count of squares width by the count of square 
                               height. The second dropdown to select your color quantity, 4 for smaller quilt sizes, 
                               and 8 for larger ones. Select any of the color ramp buttons to select your choice of 
                               color scheme.")),
               tags$p("Tab 2: Data Setup",
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
               tags$p("Tab 3: View & Share",
                      helpText("This tab allows you to preview your quilt design with the data you selected to show,
                               as well as the color scheme and size you selected previously. Use the download pattern button at
                               the top of the sidebar to save your design on to your device as a PDF. Use the download hex code button
                               to get a .csv file of your hex codes for your colors needed. The 'visit fabric website' button 
                               directs you to either a craft store website, where you can choose your colors and purchase the 
                               amount of fabric necessary, or a website that will allow you to input a hex code and the website
                               will return fabric colors similar to the inputted hex code. You also have the option to add a border
                               to your design, using the checkbox, as well as the dropdown to select your border color, if a border is 
                               desired. The 'share your design' button can be used to share your design on social media, on Pinterest, 
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
    data_filtered <- Temperature %>%
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) %>%
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
    pre_data_filtered <- Precipitation %>%
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) %>%
      select(Date, Value)  # Filter to include date, station, and average temperature
    return(pre_data_filtered)
  })
  
  #Stream Chemistry
  Stream_Chemistry <- read_csv("Data/HubbardBrook_weekly_Stream_Chemistry_1963-2024.csv")
  
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
    str_data_filtered <- Stream_Chemistry %>%
      filter(Date >= input$dataStartDate & Date <= input$dataEndDate) %>%
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
    
    # Check if the Date column exists and get the min/max date
    if ("Date" %in% colnames(df)) {
      minDate <- min(df$Date, na.rm = TRUE)
      maxDate <- max(df$Date, na.rm = TRUE)
      
      # Initialize the slider with full date range if not already set
      if (is.null(input$dateRange)) {
        updateSliderInput(session, "dateRange", 
                          min = minDate, 
                          max = maxDate, 
                          value = c(minDate, maxDate))  # Set initial range to full range
      }
    }
  })
  
  # Reactive expression for filtering the data based on the selected date range
  filteredData <- reactive({
    df <- datasetInput()
    
    # Get the selected date range from the slider
    if (!is.null(input$dateRange)) {
      df <- df[df$Date >= input$dateRange[1] & df$Date <= input$dateRange[2], ]
    }
    
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
    req(input$quiltsize)  # Ensure the quilt size input is selected
    
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    num_squares <- prod(quilt_size)  # Calculate total number of squares
    
    # Use the filtered dataset to get data values
    df <- filteredData()
    req(df)
    
    # Ensure that there are enough data points to match the number of squares
    req(nrow(df) >= num_squares)
    
    # Take the first `num_squares` data points (or modify this logic as needed)
    quilt_data <- head(df$Value, num_squares)
    
    # Create a simple line plot for the quilt data values
    ggplot(data.frame(Index = 1:num_squares, Value = quilt_data), aes(x = Index, y = Value)) +
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
    cat("Entered renderPlot function\n")
    
    
    req(selectedColor() != "None", input$colorquantity, input$quiltsize)
    cat("Inputs received: Color Scheme: ", selectedColor(), "Quantity: ", input$colorquantity, "Size: ", input$quiltsize, "\n")
    
    # Retrieve dataset
    df <- filteredData()
    req(df)
    
    cat("Dataset:\n")
    print(head(df))
    
    # Ensure 'Value' column exists and is numeric
    if (!"Value" %in% colnames(df)) {
      stop("Dataset does not contain a 'Value' column.")
    }
    
    df$Value <- as.numeric(df$Value)
    df <- df[!is.na(df$Value), ]
    
    # Check for NAs introduced by coercion
    if (any(is.na(df$Value))) {
      stop("Dataset contains non-numeric values in the 'Value' column.")
    }
    
    req(color_ramps[[selectedColor()]])  # Validate the color scheme
    
    # Define quilt size
    quilt_size <- switch(input$quiltsize,
                         "5x7 (Baby)" = c(5, 7),
                         "6x9 (Crib)" = c(6, 9),
                         "9x11 (Throw)" = c(9, 11),
                         "12x15 (Twin)" = c(12, 15),
                         "14x18 (Full)" = c(14, 18),
                         "15x18 (Queen)" = c(15, 18),
                         "18x18 (King)" = c(18, 18))
    
    # Ensure bins > 0
    unique_values <- length(unique(df$Value))
    bins <- min(as.numeric(input$colorquantity), unique_values)  # Limit bins to unique values
    
    cat("Unique Values: ", unique_values, "\n")
    cat("Bins for Quilt Size ", input$quiltsize, ": ", bins, "\n")
    
    # Define bin breaks using quantiles
    bin_breaks <- quantile(df$Value, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    
    cat("Bin Breaks for Quilt Size ", input$quiltsize, ":\n")
    print(bin_breaks)  # Print the breakpoints
    
    # Bin the values into categories based on quantiles
    df$category <- cut(df$Value, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE)
    
    # Print category distribution
    cat("Categories for Quilt Size ", input$quiltsize, ":\n")
    print(unique(df$category))  # Check how many values fall into each bin
    
    # Ensure we don't have any empty categories (redistribute data if needed)
    while (any(table(df$category) == 0)) {
      empty_bins <- which(table(df$category) == 0)  # Identify empty bins
      
      # Redistribute values into empty bins by adjusting bin breaks
      for (bin in empty_bins) {
        # Find the closest value that would fill the empty bin
        nearest_value <- min(df$Value[df$category == bin], na.rm = TRUE)
        df$category[df$Value == nearest_value] <- bin  # Assign that value to the empty bin
      }
    }
    
    df$category <- factor(df$category, levels = 1:bins)
    
    # Assign colors based on the number of bins and selected color ramp
    color_palette <- colorRampPalette(color_ramps[[selectedColor()]])(bins)
    
    cat("Color Palette: ", color_palette, "\n")
    
    # Map data to colors
    df$color <- color_palette[as.numeric(df$category)]
    
    cat("Categories and Assigned Colors:\n")
    print(unique(df[, c("category", "color")]))
    
    # Generate quilt grid
    quilt_data <- expand.grid(x = 1:quilt_size[1], y = 1:quilt_size[2])
    
    # Map data values to categories and assign the corresponding color
    quilt_data$category <- rep(df$category, length.out = nrow(quilt_data))  # Repeat categories evenly
    
    # Apply colors based on the categories
    quilt_data$color <- color_palette[as.numeric(quilt_data$category)]  # Use color corresponding to category
    
    # Debugging: Check final color mapping
    cat("Final Quilt Data Colors:\n")
    print(table(quilt_data$color))
    
    # Define border parameters
    border_col <- if (input$add_border) input$border_color else NA
    border_size <- if (input$add_border) input$border_size else 0  # Border thickness
    
    # Define outer rectangle (entire quilt border)
    quilt_border <- data.frame(
      xmin = 0.5, xmax = quilt_size[1] + 0.5,
      ymin = 0.5, ymax = quilt_size[2] + 0.5
    )
    
    # Plot quilt design
    quiltPlot <- ggplot(quilt_data, aes(x, y, fill = color)) +
      geom_tile(color = "black") +
      scale_fill_manual(values = color_palette) +
      theme_void() +
      coord_fixed() +
      labs(title = "Quilt Preview")
    
    if (input$add_border) {
      quiltPlot <- quiltPlot + geom_rect(data = quilt_border, inherit.aes = FALSE,
                                         aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                         color = border_col, fill = NA, linewidth = border_size)
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
    
    # Load dataset
    df <- filteredData()
    req(df)
    
    # Ensure Value column exists
    if (!"Value" %in% colnames(df)) {
      stop("Dataset does not contain a 'Value' column.")
    }
    
    # Convert to numeric
    df$Value <- as.numeric(df$Value)
    df <- df[!is.na(df$Value), ]
    
    # Binning step
    bins <- as.numeric(input$colorquantity)
    if (bins <= 0) stop("Number of bins must be greater than zero.") 
    
    bin_breaks <- quantile(df$Value, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    
    df$category <- cut(df$Value, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE)
    
    # Generate color palette
    color_palette <- colorRampPalette(color_ramps[[selectedColor()]])(bins)
    
    # Generate quilt grid
    quilt_data <- expand.grid(x = 1:quilt_size[1], y = 1:quilt_size[2])
    quilt_data$category <- rep(df$category, length.out = nrow(quilt_data))
    quilt_data$color <- color_palette[as.numeric(quilt_data$category)]
    
    # Create fabric count table
    fabric_counts <- quilt_data |>
      group_by(color) |>
      summarise(Squares = n(), .groups = 'drop') |>
      mutate(
        SquareSize = 6,  
        SeamAllowance = 0.25,  
        FabricNeeded = Squares * (SquareSize + 2 * SeamAllowance)^2 / 144  
      )
    
    # Generate Data Range column
    bin_ranges <- data.frame(
      category = 1:bins,
      MinValue = bin_breaks[-length(bin_breaks)],
      MaxValue = bin_breaks[-1]
    )
    
    # Ensure color and category are correctly matched
    fabric_counts <- fabric_counts |>
      mutate(category = match(color, color_palette)) |>  
      left_join(bin_ranges, by = "category") |>
      mutate(`Data Range` = paste0(round(MinValue, 2), " - ", round(MaxValue, 2))) |>
      rename("Color" = color, "Fabric Needed (sq ft)" = FabricNeeded) |>  # Ensure the Color column exists before selecting
      select(Color, Squares, `Fabric Needed (sq ft)`, `Data Range`)
    
    # Convert to character to prevent errors
    fabric_counts$`Data Range` <- as.character(fabric_counts$`Data Range`)
    
    # Ensure at least one row exists
    if (nrow(fabric_counts) == 0) {
      return(data.frame(
        Color = NA, Squares = NA, `Fabric Needed (sq ft)` = NA, `Data Range` = "No data"
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
  
  
  # Open fabric website
  observeEvent(input$fabricWebsite, {
    showModal(modalDialog(
      title = "Choose a Fabric Website",
      "Select which fabric website you want to visit:",
      easyClose = TRUE,
      footer = tagList(
        actionButton("goSpoonflower", "Go to Spoonflower"),
        actionButton("goAlisonGale", "Go to Hex Code Matcher")
      )
    ))
  })
  # Open Spoonflower link
  observeEvent(input$goSpoonflower, {
    removeModal()
    browseURL("https://www.spoonflower.com")  # Opens Spoonflower in a new browser tab
  })
  # Open Alison Gale link
  observeEvent(input$goAlisonGale, {
    removeModal()
    browseURL("https://fabric.alisongale.com/")  # Opens Alison Gale in a new browser tab
  })
  
  
  # Share design (works on mobile)
  observeEvent(input$shareButton, {
    session$sendCustomMessage(type = "share", 
                              message = list(title = "Check out this Quilt!", 
                                             url = session$clientData$url_hostname))
  })
  
}

shinyApp(ui = ui, server = server)
