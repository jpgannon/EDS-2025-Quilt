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
  select(date, AVE)|>
  relocate(date, AVE)|>
  group_by(date) |>
  summarize(Avg_temp = mean(AVE))

# Convert the 'date' column to Date type
tmpDateFormat <- "%Y-%m-%d"
tmp1date <- as.Date(Temperature$date, format=tmpDateFormat)
if (nrow(Temperature[Temperature$date != "",]) == length(tmp1date[!is.na(tmp1date)])) {
  Temperature$date <- tmp1date
} else {
  print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")
}

# Create reactive expression for filtering based on user dates
filtered_data <- reactive({
  req(input$dataStartDate, input$dataEndDate)  # Ensure dates are selected
  data_filtered <- Temperature %>%
    filter(date >= input$dataStartDate & date <= input$dataEndDate) %>%
    select(date, AVE)  # Filter to include date, station, and average temperature
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

tibble(Water_Chemistry)


#Vegetation (LAI)

# Package ID: knb-lter-hbr.295.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Leaf Area Index (LAI) Throughfall Plots.
# Data set creator:  Timothy Fahey - Cornell University 
# Data set creator:  Natalie Cleavitt - Cornell University 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/295/2/62c65377dae651d9cb97ef71be6af675" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Plot",     
                 "Year",     
                 "Trap",     
                 "LAI"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Plot)!="factor") dt3$Plot<- as.factor(dt3$Plot)
if (class(dt3$Trap)!="factor") dt3$Trap<- as.factor(dt3$Trap)
if (class(dt3$LAI)=="factor") dt3$LAI <-as.numeric(levels(dt3$LAI))[as.integer(dt3$LAI) ]               
if (class(dt3$LAI)=="character") dt3$LAI <-as.numeric(dt3$LAI)

# Convert Missing Values to NA for non-dates

dt3$LAI <- ifelse((trimws(as.character(dt3$LAI))==trimws("-999.99")),NA,dt3$LAI)               
suppressWarnings(dt3$LAI <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt3$LAI))==as.character(as.numeric("-999.99"))),NA,dt3$LAI))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Plot)
summary(Year)
summary(Trap)
summary(LAI) 
# Get more details on character variables

summary(as.factor(dt3$Plot)) 
summary(as.factor(dt3$Trap))
detach(dt3)             



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



Soil_Carbon <- dt6 |>
  select(Year, PerCentC) |>
  group_by(Year)|>
  summarise(avg_C = mean(PerCentC))


#Bird Abundance

options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/81/8/043d7527d229aa84ae1ddd8d1193ccbb" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "BirdSpecies",     
                 "y.1969",     
                 "y.1970",     
                 "y.1971",     
                 "y.1972",     
                 "y.1973",     
                 "y.1974",     
                 "y.1975",     
                 "y.1976",     
                 "y.1977",     
                 "y.1978",     
                 "y.1979",     
                 "y.1980",     
                 "y.1981",     
                 "y.1982",     
                 "y.1983",     
                 "y.1984",     
                 "y.1985",     
                 "y.1986",     
                 "y.1987",     
                 "y.1988",     
                 "y.1989",     
                 "y.1990",     
                 "y.1991",     
                 "y.1992",     
                 "y.1993",     
                 "y.1994",     
                 "y.1995",     
                 "y.1996",     
                 "y.1997",     
                 "y.1998",     
                 "y.1999",     
                 "y.2000",     
                 "y.2001",     
                 "y.2002",     
                 "y.2003",     
                 "y.2004",     
                 "y.2005",     
                 "y.2006",     
                 "y.2007",     
                 "y.2008",     
                 "y.2009",     
                 "y.2010",     
                 "y.2011",     
                 "y.2012",     
                 "y.2013",     
                 "y.2014",     
                 "y.2015",     
                 "y.2016",     
                 "y.2017",     
                 "y.2018"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$BirdSpecies)!="factor") dt1$BirdSpecies<- as.factor(dt1$BirdSpecies)
if (class(dt1$y.1969)=="factor") dt1$y.1969 <-as.numeric(levels(dt1$y.1969))[as.integer(dt1$y.1969) ]               
if (class(dt1$y.1969)=="character") dt1$y.1969 <-as.numeric(dt1$y.1969)
if (class(dt1$y.1970)=="factor") dt1$y.1970 <-as.numeric(levels(dt1$y.1970))[as.integer(dt1$y.1970) ]               
if (class(dt1$y.1970)=="character") dt1$y.1970 <-as.numeric(dt1$y.1970)
if (class(dt1$y.1971)=="factor") dt1$y.1971 <-as.numeric(levels(dt1$y.1971))[as.integer(dt1$y.1971) ]               
if (class(dt1$y.1971)=="character") dt1$y.1971 <-as.numeric(dt1$y.1971)
if (class(dt1$y.1972)=="factor") dt1$y.1972 <-as.numeric(levels(dt1$y.1972))[as.integer(dt1$y.1972) ]               
if (class(dt1$y.1972)=="character") dt1$y.1972 <-as.numeric(dt1$y.1972)
if (class(dt1$y.1973)=="factor") dt1$y.1973 <-as.numeric(levels(dt1$y.1973))[as.integer(dt1$y.1973) ]               
if (class(dt1$y.1973)=="character") dt1$y.1973 <-as.numeric(dt1$y.1973)
if (class(dt1$y.1974)=="factor") dt1$y.1974 <-as.numeric(levels(dt1$y.1974))[as.integer(dt1$y.1974) ]               
if (class(dt1$y.1974)=="character") dt1$y.1974 <-as.numeric(dt1$y.1974)
if (class(dt1$y.1975)=="factor") dt1$y.1975 <-as.numeric(levels(dt1$y.1975))[as.integer(dt1$y.1975) ]               
if (class(dt1$y.1975)=="character") dt1$y.1975 <-as.numeric(dt1$y.1975)
if (class(dt1$y.1976)=="factor") dt1$y.1976 <-as.numeric(levels(dt1$y.1976))[as.integer(dt1$y.1976) ]               
if (class(dt1$y.1976)=="character") dt1$y.1976 <-as.numeric(dt1$y.1976)
if (class(dt1$y.1977)=="factor") dt1$y.1977 <-as.numeric(levels(dt1$y.1977))[as.integer(dt1$y.1977) ]               
if (class(dt1$y.1977)=="character") dt1$y.1977 <-as.numeric(dt1$y.1977)
if (class(dt1$y.1978)=="factor") dt1$y.1978 <-as.numeric(levels(dt1$y.1978))[as.integer(dt1$y.1978) ]               
if (class(dt1$y.1978)=="character") dt1$y.1978 <-as.numeric(dt1$y.1978)
if (class(dt1$y.1979)=="factor") dt1$y.1979 <-as.numeric(levels(dt1$y.1979))[as.integer(dt1$y.1979) ]               
if (class(dt1$y.1979)=="character") dt1$y.1979 <-as.numeric(dt1$y.1979)
if (class(dt1$y.1980)=="factor") dt1$y.1980 <-as.numeric(levels(dt1$y.1980))[as.integer(dt1$y.1980) ]               
if (class(dt1$y.1980)=="character") dt1$y.1980 <-as.numeric(dt1$y.1980)
if (class(dt1$y.1981)=="factor") dt1$y.1981 <-as.numeric(levels(dt1$y.1981))[as.integer(dt1$y.1981) ]               
if (class(dt1$y.1981)=="character") dt1$y.1981 <-as.numeric(dt1$y.1981)
if (class(dt1$y.1982)=="factor") dt1$y.1982 <-as.numeric(levels(dt1$y.1982))[as.integer(dt1$y.1982) ]               
if (class(dt1$y.1982)=="character") dt1$y.1982 <-as.numeric(dt1$y.1982)
if (class(dt1$y.1983)=="factor") dt1$y.1983 <-as.numeric(levels(dt1$y.1983))[as.integer(dt1$y.1983) ]               
if (class(dt1$y.1983)=="character") dt1$y.1983 <-as.numeric(dt1$y.1983)
if (class(dt1$y.1984)=="factor") dt1$y.1984 <-as.numeric(levels(dt1$y.1984))[as.integer(dt1$y.1984) ]               
if (class(dt1$y.1984)=="character") dt1$y.1984 <-as.numeric(dt1$y.1984)
if (class(dt1$y.1985)=="factor") dt1$y.1985 <-as.numeric(levels(dt1$y.1985))[as.integer(dt1$y.1985) ]               
if (class(dt1$y.1985)=="character") dt1$y.1985 <-as.numeric(dt1$y.1985)
if (class(dt1$y.1986)=="factor") dt1$y.1986 <-as.numeric(levels(dt1$y.1986))[as.integer(dt1$y.1986) ]               
if (class(dt1$y.1986)=="character") dt1$y.1986 <-as.numeric(dt1$y.1986)
if (class(dt1$y.1987)=="factor") dt1$y.1987 <-as.numeric(levels(dt1$y.1987))[as.integer(dt1$y.1987) ]               
if (class(dt1$y.1987)=="character") dt1$y.1987 <-as.numeric(dt1$y.1987)
if (class(dt1$y.1988)=="factor") dt1$y.1988 <-as.numeric(levels(dt1$y.1988))[as.integer(dt1$y.1988) ]               
if (class(dt1$y.1988)=="character") dt1$y.1988 <-as.numeric(dt1$y.1988)
if (class(dt1$y.1989)=="factor") dt1$y.1989 <-as.numeric(levels(dt1$y.1989))[as.integer(dt1$y.1989) ]               
if (class(dt1$y.1989)=="character") dt1$y.1989 <-as.numeric(dt1$y.1989)
if (class(dt1$y.1990)=="factor") dt1$y.1990 <-as.numeric(levels(dt1$y.1990))[as.integer(dt1$y.1990) ]               
if (class(dt1$y.1990)=="character") dt1$y.1990 <-as.numeric(dt1$y.1990)
if (class(dt1$y.1991)=="factor") dt1$y.1991 <-as.numeric(levels(dt1$y.1991))[as.integer(dt1$y.1991) ]               
if (class(dt1$y.1991)=="character") dt1$y.1991 <-as.numeric(dt1$y.1991)
if (class(dt1$y.1992)=="factor") dt1$y.1992 <-as.numeric(levels(dt1$y.1992))[as.integer(dt1$y.1992) ]               
if (class(dt1$y.1992)=="character") dt1$y.1992 <-as.numeric(dt1$y.1992)
if (class(dt1$y.1993)=="factor") dt1$y.1993 <-as.numeric(levels(dt1$y.1993))[as.integer(dt1$y.1993) ]               
if (class(dt1$y.1993)=="character") dt1$y.1993 <-as.numeric(dt1$y.1993)
if (class(dt1$y.1994)=="factor") dt1$y.1994 <-as.numeric(levels(dt1$y.1994))[as.integer(dt1$y.1994) ]               
if (class(dt1$y.1994)=="character") dt1$y.1994 <-as.numeric(dt1$y.1994)
if (class(dt1$y.1995)=="factor") dt1$y.1995 <-as.numeric(levels(dt1$y.1995))[as.integer(dt1$y.1995) ]               
if (class(dt1$y.1995)=="character") dt1$y.1995 <-as.numeric(dt1$y.1995)
if (class(dt1$y.1996)=="factor") dt1$y.1996 <-as.numeric(levels(dt1$y.1996))[as.integer(dt1$y.1996) ]               
if (class(dt1$y.1996)=="character") dt1$y.1996 <-as.numeric(dt1$y.1996)
if (class(dt1$y.1997)=="factor") dt1$y.1997 <-as.numeric(levels(dt1$y.1997))[as.integer(dt1$y.1997) ]               
if (class(dt1$y.1997)=="character") dt1$y.1997 <-as.numeric(dt1$y.1997)
if (class(dt1$y.1998)=="factor") dt1$y.1998 <-as.numeric(levels(dt1$y.1998))[as.integer(dt1$y.1998) ]               
if (class(dt1$y.1998)=="character") dt1$y.1998 <-as.numeric(dt1$y.1998)
if (class(dt1$y.1999)=="factor") dt1$y.1999 <-as.numeric(levels(dt1$y.1999))[as.integer(dt1$y.1999) ]               
if (class(dt1$y.1999)=="character") dt1$y.1999 <-as.numeric(dt1$y.1999)
if (class(dt1$y.2000)=="factor") dt1$y.2000 <-as.numeric(levels(dt1$y.2000))[as.integer(dt1$y.2000) ]               
if (class(dt1$y.2000)=="character") dt1$y.2000 <-as.numeric(dt1$y.2000)
if (class(dt1$y.2001)=="factor") dt1$y.2001 <-as.numeric(levels(dt1$y.2001))[as.integer(dt1$y.2001) ]               
if (class(dt1$y.2001)=="character") dt1$y.2001 <-as.numeric(dt1$y.2001)
if (class(dt1$y.2002)=="factor") dt1$y.2002 <-as.numeric(levels(dt1$y.2002))[as.integer(dt1$y.2002) ]               
if (class(dt1$y.2002)=="character") dt1$y.2002 <-as.numeric(dt1$y.2002)
if (class(dt1$y.2003)=="factor") dt1$y.2003 <-as.numeric(levels(dt1$y.2003))[as.integer(dt1$y.2003) ]               
if (class(dt1$y.2003)=="character") dt1$y.2003 <-as.numeric(dt1$y.2003)
if (class(dt1$y.2004)=="factor") dt1$y.2004 <-as.numeric(levels(dt1$y.2004))[as.integer(dt1$y.2004) ]               
if (class(dt1$y.2004)=="character") dt1$y.2004 <-as.numeric(dt1$y.2004)
if (class(dt1$y.2005)=="factor") dt1$y.2005 <-as.numeric(levels(dt1$y.2005))[as.integer(dt1$y.2005) ]               
if (class(dt1$y.2005)=="character") dt1$y.2005 <-as.numeric(dt1$y.2005)
if (class(dt1$y.2006)=="factor") dt1$y.2006 <-as.numeric(levels(dt1$y.2006))[as.integer(dt1$y.2006) ]               
if (class(dt1$y.2006)=="character") dt1$y.2006 <-as.numeric(dt1$y.2006)
if (class(dt1$y.2007)=="factor") dt1$y.2007 <-as.numeric(levels(dt1$y.2007))[as.integer(dt1$y.2007) ]               
if (class(dt1$y.2007)=="character") dt1$y.2007 <-as.numeric(dt1$y.2007)
if (class(dt1$y.2008)=="factor") dt1$y.2008 <-as.numeric(levels(dt1$y.2008))[as.integer(dt1$y.2008) ]               
if (class(dt1$y.2008)=="character") dt1$y.2008 <-as.numeric(dt1$y.2008)
if (class(dt1$y.2009)=="factor") dt1$y.2009 <-as.numeric(levels(dt1$y.2009))[as.integer(dt1$y.2009) ]               
if (class(dt1$y.2009)=="character") dt1$y.2009 <-as.numeric(dt1$y.2009)
if (class(dt1$y.2010)=="factor") dt1$y.2010 <-as.numeric(levels(dt1$y.2010))[as.integer(dt1$y.2010) ]               
if (class(dt1$y.2010)=="character") dt1$y.2010 <-as.numeric(dt1$y.2010)
if (class(dt1$y.2011)=="factor") dt1$y.2011 <-as.numeric(levels(dt1$y.2011))[as.integer(dt1$y.2011) ]               
if (class(dt1$y.2011)=="character") dt1$y.2011 <-as.numeric(dt1$y.2011)
if (class(dt1$y.2012)=="factor") dt1$y.2012 <-as.numeric(levels(dt1$y.2012))[as.integer(dt1$y.2012) ]               
if (class(dt1$y.2012)=="character") dt1$y.2012 <-as.numeric(dt1$y.2012)
if (class(dt1$y.2013)=="factor") dt1$y.2013 <-as.numeric(levels(dt1$y.2013))[as.integer(dt1$y.2013) ]               
if (class(dt1$y.2013)=="character") dt1$y.2013 <-as.numeric(dt1$y.2013)
if (class(dt1$y.2014)=="factor") dt1$y.2014 <-as.numeric(levels(dt1$y.2014))[as.integer(dt1$y.2014) ]               
if (class(dt1$y.2014)=="character") dt1$y.2014 <-as.numeric(dt1$y.2014)
if (class(dt1$y.2015)=="factor") dt1$y.2015 <-as.numeric(levels(dt1$y.2015))[as.integer(dt1$y.2015) ]               
if (class(dt1$y.2015)=="character") dt1$y.2015 <-as.numeric(dt1$y.2015)
if (class(dt1$y.2016)=="factor") dt1$y.2016 <-as.numeric(levels(dt1$y.2016))[as.integer(dt1$y.2016) ]               
if (class(dt1$y.2016)=="character") dt1$y.2016 <-as.numeric(dt1$y.2016)
if (class(dt1$y.2017)=="factor") dt1$y.2017 <-as.numeric(levels(dt1$y.2017))[as.integer(dt1$y.2017) ]               
if (class(dt1$y.2017)=="character") dt1$y.2017 <-as.numeric(dt1$y.2017)
if (class(dt1$y.2018)=="factor") dt1$y.2018 <-as.numeric(levels(dt1$y.2018))[as.integer(dt1$y.2018) ]               
if (class(dt1$y.2018)=="character") dt1$y.2018 <-as.numeric(dt1$y.2018)

# Convert Missing Values to NA for non-dates

dt1$y.1969 <- ifelse((trimws(as.character(dt1$y.1969))==trimws("t")),NA,dt1$y.1969)               
suppressWarnings(dt1$y.1969 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1969))==as.character(as.numeric("t"))),NA,dt1$y.1969))
dt1$y.1970 <- ifelse((trimws(as.character(dt1$y.1970))==trimws("t")),NA,dt1$y.1970)               
suppressWarnings(dt1$y.1970 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1970))==as.character(as.numeric("t"))),NA,dt1$y.1970))
dt1$y.1971 <- ifelse((trimws(as.character(dt1$y.1971))==trimws("t")),NA,dt1$y.1971)               
suppressWarnings(dt1$y.1971 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1971))==as.character(as.numeric("t"))),NA,dt1$y.1971))
dt1$y.1972 <- ifelse((trimws(as.character(dt1$y.1972))==trimws("t")),NA,dt1$y.1972)               
suppressWarnings(dt1$y.1972 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1972))==as.character(as.numeric("t"))),NA,dt1$y.1972))
dt1$y.1973 <- ifelse((trimws(as.character(dt1$y.1973))==trimws("t")),NA,dt1$y.1973)               
suppressWarnings(dt1$y.1973 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1973))==as.character(as.numeric("t"))),NA,dt1$y.1973))
dt1$y.1974 <- ifelse((trimws(as.character(dt1$y.1974))==trimws("t")),NA,dt1$y.1974)               
suppressWarnings(dt1$y.1974 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1974))==as.character(as.numeric("t"))),NA,dt1$y.1974))
dt1$y.1975 <- ifelse((trimws(as.character(dt1$y.1975))==trimws("t")),NA,dt1$y.1975)               
suppressWarnings(dt1$y.1975 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1975))==as.character(as.numeric("t"))),NA,dt1$y.1975))
dt1$y.1976 <- ifelse((trimws(as.character(dt1$y.1976))==trimws("t")),NA,dt1$y.1976)               
suppressWarnings(dt1$y.1976 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1976))==as.character(as.numeric("t"))),NA,dt1$y.1976))
dt1$y.1977 <- ifelse((trimws(as.character(dt1$y.1977))==trimws("t")),NA,dt1$y.1977)               
suppressWarnings(dt1$y.1977 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1977))==as.character(as.numeric("t"))),NA,dt1$y.1977))
dt1$y.1978 <- ifelse((trimws(as.character(dt1$y.1978))==trimws("t")),NA,dt1$y.1978)               
suppressWarnings(dt1$y.1978 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1978))==as.character(as.numeric("t"))),NA,dt1$y.1978))
dt1$y.1979 <- ifelse((trimws(as.character(dt1$y.1979))==trimws("t")),NA,dt1$y.1979)               
suppressWarnings(dt1$y.1979 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1979))==as.character(as.numeric("t"))),NA,dt1$y.1979))
dt1$y.1980 <- ifelse((trimws(as.character(dt1$y.1980))==trimws("t")),NA,dt1$y.1980)               
suppressWarnings(dt1$y.1980 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1980))==as.character(as.numeric("t"))),NA,dt1$y.1980))
dt1$y.1981 <- ifelse((trimws(as.character(dt1$y.1981))==trimws("t")),NA,dt1$y.1981)               
suppressWarnings(dt1$y.1981 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1981))==as.character(as.numeric("t"))),NA,dt1$y.1981))
dt1$y.1982 <- ifelse((trimws(as.character(dt1$y.1982))==trimws("t")),NA,dt1$y.1982)               
suppressWarnings(dt1$y.1982 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1982))==as.character(as.numeric("t"))),NA,dt1$y.1982))
dt1$y.1983 <- ifelse((trimws(as.character(dt1$y.1983))==trimws("t")),NA,dt1$y.1983)               
suppressWarnings(dt1$y.1983 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1983))==as.character(as.numeric("t"))),NA,dt1$y.1983))
dt1$y.1984 <- ifelse((trimws(as.character(dt1$y.1984))==trimws("t")),NA,dt1$y.1984)               
suppressWarnings(dt1$y.1984 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1984))==as.character(as.numeric("t"))),NA,dt1$y.1984))
dt1$y.1985 <- ifelse((trimws(as.character(dt1$y.1985))==trimws("t")),NA,dt1$y.1985)               
suppressWarnings(dt1$y.1985 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1985))==as.character(as.numeric("t"))),NA,dt1$y.1985))
dt1$y.1986 <- ifelse((trimws(as.character(dt1$y.1986))==trimws("t")),NA,dt1$y.1986)               
suppressWarnings(dt1$y.1986 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1986))==as.character(as.numeric("t"))),NA,dt1$y.1986))
dt1$y.1987 <- ifelse((trimws(as.character(dt1$y.1987))==trimws("t")),NA,dt1$y.1987)               
suppressWarnings(dt1$y.1987 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1987))==as.character(as.numeric("t"))),NA,dt1$y.1987))
dt1$y.1988 <- ifelse((trimws(as.character(dt1$y.1988))==trimws("t")),NA,dt1$y.1988)               
suppressWarnings(dt1$y.1988 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1988))==as.character(as.numeric("t"))),NA,dt1$y.1988))
dt1$y.1989 <- ifelse((trimws(as.character(dt1$y.1989))==trimws("t")),NA,dt1$y.1989)               
suppressWarnings(dt1$y.1989 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1989))==as.character(as.numeric("t"))),NA,dt1$y.1989))
dt1$y.1990 <- ifelse((trimws(as.character(dt1$y.1990))==trimws("t")),NA,dt1$y.1990)               
suppressWarnings(dt1$y.1990 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1990))==as.character(as.numeric("t"))),NA,dt1$y.1990))
dt1$y.1991 <- ifelse((trimws(as.character(dt1$y.1991))==trimws("t")),NA,dt1$y.1991)               
suppressWarnings(dt1$y.1991 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1991))==as.character(as.numeric("t"))),NA,dt1$y.1991))
dt1$y.1992 <- ifelse((trimws(as.character(dt1$y.1992))==trimws("t")),NA,dt1$y.1992)               
suppressWarnings(dt1$y.1992 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1992))==as.character(as.numeric("t"))),NA,dt1$y.1992))
dt1$y.1993 <- ifelse((trimws(as.character(dt1$y.1993))==trimws("t")),NA,dt1$y.1993)               
suppressWarnings(dt1$y.1993 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1993))==as.character(as.numeric("t"))),NA,dt1$y.1993))
dt1$y.1994 <- ifelse((trimws(as.character(dt1$y.1994))==trimws("t")),NA,dt1$y.1994)               
suppressWarnings(dt1$y.1994 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1994))==as.character(as.numeric("t"))),NA,dt1$y.1994))
dt1$y.1995 <- ifelse((trimws(as.character(dt1$y.1995))==trimws("t")),NA,dt1$y.1995)               
suppressWarnings(dt1$y.1995 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1995))==as.character(as.numeric("t"))),NA,dt1$y.1995))
dt1$y.1996 <- ifelse((trimws(as.character(dt1$y.1996))==trimws("t")),NA,dt1$y.1996)               
suppressWarnings(dt1$y.1996 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1996))==as.character(as.numeric("t"))),NA,dt1$y.1996))
dt1$y.1997 <- ifelse((trimws(as.character(dt1$y.1997))==trimws("t")),NA,dt1$y.1997)               
suppressWarnings(dt1$y.1997 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1997))==as.character(as.numeric("t"))),NA,dt1$y.1997))
dt1$y.1998 <- ifelse((trimws(as.character(dt1$y.1998))==trimws("t")),NA,dt1$y.1998)               
suppressWarnings(dt1$y.1998 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1998))==as.character(as.numeric("t"))),NA,dt1$y.1998))
dt1$y.1999 <- ifelse((trimws(as.character(dt1$y.1999))==trimws("t")),NA,dt1$y.1999)               
suppressWarnings(dt1$y.1999 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.1999))==as.character(as.numeric("t"))),NA,dt1$y.1999))
dt1$y.2000 <- ifelse((trimws(as.character(dt1$y.2000))==trimws("t")),NA,dt1$y.2000)               
suppressWarnings(dt1$y.2000 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2000))==as.character(as.numeric("t"))),NA,dt1$y.2000))
dt1$y.2001 <- ifelse((trimws(as.character(dt1$y.2001))==trimws("t")),NA,dt1$y.2001)               
suppressWarnings(dt1$y.2001 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2001))==as.character(as.numeric("t"))),NA,dt1$y.2001))
dt1$y.2002 <- ifelse((trimws(as.character(dt1$y.2002))==trimws("t")),NA,dt1$y.2002)               
suppressWarnings(dt1$y.2002 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2002))==as.character(as.numeric("t"))),NA,dt1$y.2002))
dt1$y.2003 <- ifelse((trimws(as.character(dt1$y.2003))==trimws("t")),NA,dt1$y.2003)               
suppressWarnings(dt1$y.2003 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2003))==as.character(as.numeric("t"))),NA,dt1$y.2003))
dt1$y.2004 <- ifelse((trimws(as.character(dt1$y.2004))==trimws("t")),NA,dt1$y.2004)               
suppressWarnings(dt1$y.2004 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2004))==as.character(as.numeric("t"))),NA,dt1$y.2004))
dt1$y.2005 <- ifelse((trimws(as.character(dt1$y.2005))==trimws("t")),NA,dt1$y.2005)               
suppressWarnings(dt1$y.2005 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2005))==as.character(as.numeric("t"))),NA,dt1$y.2005))
dt1$y.2006 <- ifelse((trimws(as.character(dt1$y.2006))==trimws("t")),NA,dt1$y.2006)               
suppressWarnings(dt1$y.2006 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2006))==as.character(as.numeric("t"))),NA,dt1$y.2006))
dt1$y.2007 <- ifelse((trimws(as.character(dt1$y.2007))==trimws("t")),NA,dt1$y.2007)               
suppressWarnings(dt1$y.2007 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2007))==as.character(as.numeric("t"))),NA,dt1$y.2007))
dt1$y.2008 <- ifelse((trimws(as.character(dt1$y.2008))==trimws("t")),NA,dt1$y.2008)               
suppressWarnings(dt1$y.2008 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2008))==as.character(as.numeric("t"))),NA,dt1$y.2008))
dt1$y.2009 <- ifelse((trimws(as.character(dt1$y.2009))==trimws("t")),NA,dt1$y.2009)               
suppressWarnings(dt1$y.2009 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2009))==as.character(as.numeric("t"))),NA,dt1$y.2009))
dt1$y.2010 <- ifelse((trimws(as.character(dt1$y.2010))==trimws("t")),NA,dt1$y.2010)               
suppressWarnings(dt1$y.2010 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2010))==as.character(as.numeric("t"))),NA,dt1$y.2010))
dt1$y.2011 <- ifelse((trimws(as.character(dt1$y.2011))==trimws("t")),NA,dt1$y.2011)               
suppressWarnings(dt1$y.2011 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2011))==as.character(as.numeric("t"))),NA,dt1$y.2011))
dt1$y.2012 <- ifelse((trimws(as.character(dt1$y.2012))==trimws("t")),NA,dt1$y.2012)               
suppressWarnings(dt1$y.2012 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2012))==as.character(as.numeric("t"))),NA,dt1$y.2012))
dt1$y.2013 <- ifelse((trimws(as.character(dt1$y.2013))==trimws("t")),NA,dt1$y.2013)               
suppressWarnings(dt1$y.2013 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2013))==as.character(as.numeric("t"))),NA,dt1$y.2013))
dt1$y.2014 <- ifelse((trimws(as.character(dt1$y.2014))==trimws("t")),NA,dt1$y.2014)               
suppressWarnings(dt1$y.2014 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2014))==as.character(as.numeric("t"))),NA,dt1$y.2014))
dt1$y.2015 <- ifelse((trimws(as.character(dt1$y.2015))==trimws("t")),NA,dt1$y.2015)               
suppressWarnings(dt1$y.2015 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2015))==as.character(as.numeric("t"))),NA,dt1$y.2015))
dt1$y.2016 <- ifelse((trimws(as.character(dt1$y.2016))==trimws("t")),NA,dt1$y.2016)               
suppressWarnings(dt1$y.2016 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2016))==as.character(as.numeric("t"))),NA,dt1$y.2016))
dt1$y.2017 <- ifelse((trimws(as.character(dt1$y.2017))==trimws("t")),NA,dt1$y.2017)               
suppressWarnings(dt1$y.2017 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2017))==as.character(as.numeric("t"))),NA,dt1$y.2017))
dt1$y.2018 <- ifelse((trimws(as.character(dt1$y.2018))==trimws("t")),NA,dt1$y.2018)               
suppressWarnings(dt1$y.2018 <- ifelse(!is.na(as.numeric("t")) & (trimws(as.character(dt1$y.2018))==as.character(as.numeric("t"))),NA,dt1$y.2018))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(BirdSpecies)
summary(y.1969)
summary(y.1970)
summary(y.1971)
summary(y.1972)
summary(y.1973)
summary(y.1974)
summary(y.1975)
summary(y.1976)
summary(y.1977)
summary(y.1978)
summary(y.1979)
summary(y.1980)
summary(y.1981)
summary(y.1982)
summary(y.1983)
summary(y.1984)
summary(y.1985)
summary(y.1986)
summary(y.1987)
summary(y.1988)
summary(y.1989)
summary(y.1990)
summary(y.1991)
summary(y.1992)
summary(y.1993)
summary(y.1994)
summary(y.1995)
summary(y.1996)
summary(y.1997)
summary(y.1998)
summary(y.1999)
summary(y.2000)
summary(y.2001)
summary(y.2002)
summary(y.2003)
summary(y.2004)
summary(y.2005)
summary(y.2006)
summary(y.2007)
summary(y.2008)
summary(y.2009)
summary(y.2010)
summary(y.2011)
summary(y.2012)
summary(y.2013)
summary(y.2014)
summary(y.2015)
summary(y.2016)
summary(y.2017)
summary(y.2018) 
# Get more details on character variables

summary(as.factor(dt1$BirdSpecies))
detach(dt1) 


#Data cleaning

Bird_Count <- dt1 |>
  pivot_longer(
    cols = y.1969:y.2018,
    names_to = "year",
    values_to = "count"
  )

Bird_Count <- na.omit(Bird_Count)

birdcount1 <- Bird_Count |>
  pivot_wider(
    names_from = BirdSpecies,
    values_from = count
  )




