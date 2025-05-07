# Environmental Data Quilt!

## Overview
This app allows you to take environmental data and turn it into a quilt pattern! Our app provides a unique medium to visualize environmental data. Choose one of the pre-loaded datasets in our app, or upload your own, determine how you would like your data to be displayed on your quilt, chronologically or one year is represented by one row, refine the date range of data that your quilt will include, select how large of a quilt you would like to make, pick a quantity of colors and color scheme for your quilt, and boom! You will be presented with an output where you can view your environmental data quilt design, telling you how many yards of each fabric color you will need to construct your quilt!

## Purpose
Our Environmental Data Quilt App aims to transform complex environmental datasets into tangible, artistic quilt creations. By merging data science with textile arts, our app seeks to make environmental data more accessible and engaging to a broader audience, helping to foster a deeper understanding of and connection to environmental issues. Our app can also be used to reveal trends and patterns that might be overlooked in traditional graphs and charts.

## Link to Live Online App
You can access our live app online at this link: https://wwsdsa-hannah-crook.shinyapps.io/quilt/

## Dataset References
All pre-loaded datasets in our app were sourced from the Hubbard Brook Data Catalog, at the following links:
- Average Air Temperature: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.59.14 ​
- Stream pH: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.208.12​
- Precipitation pH: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.208.12​
- Soil Carbon & Nitrogen Biomass: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.67.25​

## Key Functions & Features
- Use Pre-loaded or Custom Datasets:
    - Choose from several built-in datasets sourced from the Hubbard Brook Experimental Forest, or upload your own .csv file to personalize your quilt with data that matters to you.
- Flexible Date Range Selection:
    - Narrow or expand the timeframe of the dataset to reflect seasonal patterns, long-term trends, or specific historical periods in your quilt design.
- Multiple Layout Options:
    - Select how the data is arranged on the quilt:
        - Chronological layout: Tiles flow through time continuously.
        - Year-row layout: Each row in the quilt represents one year of data.
- Customizable Color Schemes:
    - Tailor the appearance of your quilt by selecting the number of distinct colors and choosing from several color palettes. Options are designed to balance visual aesthetics with clear data representation.
- Quilt Size & Block Configuration:
    - Specify the overall quilt dimensions and the number of data blocks, allowing for small wall hangings or full-size bed quilts.
- Fabric Calculator:
    - Automatically generates yardage requirements for each fabric color based on your quilt design, helping bridge the gap between digital visualization and physical construction.
- Real-Time Preview:
    - View a dynamic rendering of your quilt pattern as you make selections. Instantly see how different datasets, date ranges, and styles affect the final design.
## How Does Our App Work?
The app is built in R using the Shiny framework and processes environmental data to create a visual quilt representation. Behind the scenes, the app uses tidyverse functions for data manipulation, lubridate for handling dates, and ggplot2 to generate both preview and final quilt plots.

Data Selection & Filtering: 
When the app starts, it loads several pre-processed datasets from the Hubbard Brook Data Catalog. Alternatively, users can upload their own .csv file, which must contain a Date column and a Value column.

Date Range Selection:
A dynamic date slider is rendered using the min and max dates from the selected dataset. Users adjust this to define which time period will be visualized in the quilt. A filteredData() reactive expression applies this filter in real time.

Binning Logic Based on Quilt Size and Layout:
Once the date range and quilt size are defined, the app calculates how many “squares” (tiles) are needed for the quilt. The data is then grouped into that many time bins using cut() for the chronological layout or ntile() within each year for the "one year per row" option:

Color Assignment and Quilting:
Based on user-selected color palette and number of bins (4 or 8), the app uses colorRampPalette() to create color gradients. Data bins are assigned to color categories using quantile() breaks. A quilt preview is rendered as a tiled ggplot, where each tile's fill color reflects the binned data value:

The app creates:

A time-series plot of the raw data (dataPreview)

A quilt plot (quiltPlot) using geom_tile()

A fabric yardage table (fabricTable) that calculates square footage per color, converts to yards, and matches hex codes to data ranges

Download & Sharing:
Users can export their quilt as a PDF and save the hex codes as a CSV. There are also sharing links to external websites and a built-in share button for social media.

In short, our app turns a filtered slice of time-series data into a structured grid, maps those values to a color ramp, and renders a quilt preview that bridges scientific insight with artistic expression. The design is modular and extensible, making it easy to add new datasets, color schemes, or layout logic.
## How to Run Our App Locally/Install Packages Needed

## Known Issues
