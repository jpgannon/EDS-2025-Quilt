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
  select(date, STA, AVE)|>
  relocate(date, AVE, STA)
Temperature$STA <- as.character(Temperature$STA)

tibble(Temperature)

ALL_Stands_Temp <- Temperature |> 
  group_by(date) |>
  summarize(Avg_temp = mean(AVE))




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


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/11/c51dd121e9403a6547fc96aa5b58317f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
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
                 "precipCatch",     
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

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$barcode)!="factor") dt1$barcode<- as.factor(dt1$barcode)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$DIC)=="factor") dt1$DIC <-as.numeric(levels(dt1$DIC))[as.integer(dt1$DIC) ]               
if (class(dt1$DIC)=="character") dt1$DIC <-as.numeric(dt1$DIC)
if (class(dt1$spCond)=="factor") dt1$spCond <-as.numeric(levels(dt1$spCond))[as.integer(dt1$spCond) ]               
if (class(dt1$spCond)=="character") dt1$spCond <-as.numeric(dt1$spCond)
if (class(dt1$temp)=="factor") dt1$temp <-as.numeric(levels(dt1$temp))[as.integer(dt1$temp) ]               
if (class(dt1$temp)=="character") dt1$temp <-as.numeric(dt1$temp)
if (class(dt1$ANC960)=="factor") dt1$ANC960 <-as.numeric(levels(dt1$ANC960))[as.integer(dt1$ANC960) ]               
if (class(dt1$ANC960)=="character") dt1$ANC960 <-as.numeric(dt1$ANC960)
if (class(dt1$ANCMet)=="factor") dt1$ANCMet <-as.numeric(levels(dt1$ANCMet))[as.integer(dt1$ANCMet) ]               
if (class(dt1$ANCMet)=="character") dt1$ANCMet <-as.numeric(dt1$ANCMet)
if (class(dt1$precipCatch)=="factor") dt1$precipCatch <-as.numeric(levels(dt1$precipCatch))[as.integer(dt1$precipCatch) ]               
if (class(dt1$precipCatch)=="character") dt1$precipCatch <-as.numeric(dt1$precipCatch)
if (class(dt1$fieldCode)!="factor") dt1$fieldCode<- as.factor(dt1$fieldCode)
if (class(dt1$notes)!="factor") dt1$notes<- as.factor(dt1$notes)
if (class(dt1$uniqueID)!="factor") dt1$uniqueID<- as.factor(dt1$uniqueID)
if (class(dt1$Ca)=="factor") dt1$Ca <-as.numeric(levels(dt1$Ca))[as.integer(dt1$Ca) ]               
if (class(dt1$Ca)=="character") dt1$Ca <-as.numeric(dt1$Ca)
if (class(dt1$Mg)=="factor") dt1$Mg <-as.numeric(levels(dt1$Mg))[as.integer(dt1$Mg) ]               
if (class(dt1$Mg)=="character") dt1$Mg <-as.numeric(dt1$Mg)
if (class(dt1$K)=="factor") dt1$K <-as.numeric(levels(dt1$K))[as.integer(dt1$K) ]               
if (class(dt1$K)=="character") dt1$K <-as.numeric(dt1$K)
if (class(dt1$Na)=="factor") dt1$Na <-as.numeric(levels(dt1$Na))[as.integer(dt1$Na) ]               
if (class(dt1$Na)=="character") dt1$Na <-as.numeric(dt1$Na)
if (class(dt1$TMAl)=="factor") dt1$TMAl <-as.numeric(levels(dt1$TMAl))[as.integer(dt1$TMAl) ]               
if (class(dt1$TMAl)=="character") dt1$TMAl <-as.numeric(dt1$TMAl)
if (class(dt1$OMAl)=="factor") dt1$OMAl <-as.numeric(levels(dt1$OMAl))[as.integer(dt1$OMAl) ]               
if (class(dt1$OMAl)=="character") dt1$OMAl <-as.numeric(dt1$OMAl)
if (class(dt1$Al_ICP)=="factor") dt1$Al_ICP <-as.numeric(levels(dt1$Al_ICP))[as.integer(dt1$Al_ICP) ]               
if (class(dt1$Al_ICP)=="character") dt1$Al_ICP <-as.numeric(dt1$Al_ICP)
if (class(dt1$Al_ferron)=="factor") dt1$Al_ferron <-as.numeric(levels(dt1$Al_ferron))[as.integer(dt1$Al_ferron) ]               
if (class(dt1$Al_ferron)=="character") dt1$Al_ferron <-as.numeric(dt1$Al_ferron)
if (class(dt1$NH4)=="factor") dt1$NH4 <-as.numeric(levels(dt1$NH4))[as.integer(dt1$NH4) ]               
if (class(dt1$NH4)=="character") dt1$NH4 <-as.numeric(dt1$NH4)
if (class(dt1$SO4)=="factor") dt1$SO4 <-as.numeric(levels(dt1$SO4))[as.integer(dt1$SO4) ]               
if (class(dt1$SO4)=="character") dt1$SO4 <-as.numeric(dt1$SO4)
if (class(dt1$NO3)=="factor") dt1$NO3 <-as.numeric(levels(dt1$NO3))[as.integer(dt1$NO3) ]               
if (class(dt1$NO3)=="character") dt1$NO3 <-as.numeric(dt1$NO3)
if (class(dt1$Cl)=="factor") dt1$Cl <-as.numeric(levels(dt1$Cl))[as.integer(dt1$Cl) ]               
if (class(dt1$Cl)=="character") dt1$Cl <-as.numeric(dt1$Cl)
if (class(dt1$PO4)=="factor") dt1$PO4 <-as.numeric(levels(dt1$PO4))[as.integer(dt1$PO4) ]               
if (class(dt1$PO4)=="character") dt1$PO4 <-as.numeric(dt1$PO4)
if (class(dt1$DOC)=="factor") dt1$DOC <-as.numeric(levels(dt1$DOC))[as.integer(dt1$DOC) ]               
if (class(dt1$DOC)=="character") dt1$DOC <-as.numeric(dt1$DOC)
if (class(dt1$TDN)=="factor") dt1$TDN <-as.numeric(levels(dt1$TDN))[as.integer(dt1$TDN) ]               
if (class(dt1$TDN)=="character") dt1$TDN <-as.numeric(dt1$TDN)
if (class(dt1$DON)=="factor") dt1$DON <-as.numeric(levels(dt1$DON))[as.integer(dt1$DON) ]               
if (class(dt1$DON)=="character") dt1$DON <-as.numeric(dt1$DON)
if (class(dt1$SiO2)=="factor") dt1$SiO2 <-as.numeric(levels(dt1$SiO2))[as.integer(dt1$SiO2) ]               
if (class(dt1$SiO2)=="character") dt1$SiO2 <-as.numeric(dt1$SiO2)
if (class(dt1$Mn)=="factor") dt1$Mn <-as.numeric(levels(dt1$Mn))[as.integer(dt1$Mn) ]               
if (class(dt1$Mn)=="character") dt1$Mn <-as.numeric(dt1$Mn)
if (class(dt1$Fe)=="factor") dt1$Fe <-as.numeric(levels(dt1$Fe))[as.integer(dt1$Fe) ]               
if (class(dt1$Fe)=="character") dt1$Fe <-as.numeric(dt1$Fe)
if (class(dt1$F)=="factor") dt1$F <-as.numeric(levels(dt1$F))[as.integer(dt1$F) ]               
if (class(dt1$F)=="character") dt1$F <-as.numeric(dt1$F)
if (class(dt1$cationCharge)=="factor") dt1$cationCharge <-as.numeric(levels(dt1$cationCharge))[as.integer(dt1$cationCharge) ]               
if (class(dt1$cationCharge)=="character") dt1$cationCharge <-as.numeric(dt1$cationCharge)
if (class(dt1$anionCharge)=="factor") dt1$anionCharge <-as.numeric(levels(dt1$anionCharge))[as.integer(dt1$anionCharge) ]               
if (class(dt1$anionCharge)=="character") dt1$anionCharge <-as.numeric(dt1$anionCharge)
if (class(dt1$ionError)=="factor") dt1$ionError <-as.numeric(levels(dt1$ionError))[as.integer(dt1$ionError) ]               
if (class(dt1$ionError)=="character") dt1$ionError <-as.numeric(dt1$ionError)
if (class(dt1$duplicate)!="factor") dt1$duplicate<- as.factor(dt1$duplicate)
if (class(dt1$sampleType)!="factor") dt1$sampleType<- as.factor(dt1$sampleType)
if (class(dt1$ionBalance)=="factor") dt1$ionBalance <-as.numeric(levels(dt1$ionBalance))[as.integer(dt1$ionBalance) ]               
if (class(dt1$ionBalance)=="character") dt1$ionBalance <-as.numeric(dt1$ionBalance)
if (class(dt1$canonical)!="factor") dt1$canonical<- as.factor(dt1$canonical)
if (class(dt1$pHmetrohm)=="factor") dt1$pHmetrohm <-as.numeric(levels(dt1$pHmetrohm))[as.integer(dt1$pHmetrohm) ]               
if (class(dt1$pHmetrohm)=="character") dt1$pHmetrohm <-as.numeric(dt1$pHmetrohm)

# Convert Missing Values to NA for non-dates

dt1$site <- as.factor(ifelse((trimws(as.character(dt1$site))==trimws("NA")),NA,as.character(dt1$site)))
dt1$barcode <- as.factor(ifelse((trimws(as.character(dt1$barcode))==trimws("NA")),NA,as.character(dt1$barcode)))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NA")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NA"))),NA,dt1$pH))
dt1$DIC <- ifelse((trimws(as.character(dt1$DIC))==trimws("NA")),NA,dt1$DIC)               
suppressWarnings(dt1$DIC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC))==as.character(as.numeric("NA"))),NA,dt1$DIC))
dt1$spCond <- ifelse((trimws(as.character(dt1$spCond))==trimws("NA")),NA,dt1$spCond)               
suppressWarnings(dt1$spCond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$spCond))==as.character(as.numeric("NA"))),NA,dt1$spCond))
dt1$temp <- ifelse((trimws(as.character(dt1$temp))==trimws("NA")),NA,dt1$temp)               
suppressWarnings(dt1$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$temp))==as.character(as.numeric("NA"))),NA,dt1$temp))
dt1$ANC960 <- ifelse((trimws(as.character(dt1$ANC960))==trimws("NA")),NA,dt1$ANC960)               
suppressWarnings(dt1$ANC960 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ANC960))==as.character(as.numeric("NA"))),NA,dt1$ANC960))
dt1$ANCMet <- ifelse((trimws(as.character(dt1$ANCMet))==trimws("NA")),NA,dt1$ANCMet)               
suppressWarnings(dt1$ANCMet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ANCMet))==as.character(as.numeric("NA"))),NA,dt1$ANCMet))
dt1$precipCatch <- ifelse((trimws(as.character(dt1$precipCatch))==trimws("NA")),NA,dt1$precipCatch)               
suppressWarnings(dt1$precipCatch <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$precipCatch))==as.character(as.numeric("NA"))),NA,dt1$precipCatch))
dt1$fieldCode <- as.factor(ifelse((trimws(as.character(dt1$fieldCode))==trimws("NA")),NA,as.character(dt1$fieldCode)))
dt1$notes <- as.factor(ifelse((trimws(as.character(dt1$notes))==trimws("NA")),NA,as.character(dt1$notes)))
dt1$uniqueID <- as.factor(ifelse((trimws(as.character(dt1$uniqueID))==trimws("NA")),NA,as.character(dt1$uniqueID)))
dt1$Ca <- ifelse((trimws(as.character(dt1$Ca))==trimws("NA")),NA,dt1$Ca)               
suppressWarnings(dt1$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Ca))==as.character(as.numeric("NA"))),NA,dt1$Ca))
dt1$Mg <- ifelse((trimws(as.character(dt1$Mg))==trimws("NA")),NA,dt1$Mg)               
suppressWarnings(dt1$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mg))==as.character(as.numeric("NA"))),NA,dt1$Mg))
dt1$K <- ifelse((trimws(as.character(dt1$K))==trimws("NA")),NA,dt1$K)               
suppressWarnings(dt1$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$K))==as.character(as.numeric("NA"))),NA,dt1$K))
dt1$Na <- ifelse((trimws(as.character(dt1$Na))==trimws("NA")),NA,dt1$Na)               
suppressWarnings(dt1$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Na))==as.character(as.numeric("NA"))),NA,dt1$Na))
dt1$TMAl <- ifelse((trimws(as.character(dt1$TMAl))==trimws("NA")),NA,dt1$TMAl)               
suppressWarnings(dt1$TMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TMAl))==as.character(as.numeric("NA"))),NA,dt1$TMAl))
dt1$OMAl <- ifelse((trimws(as.character(dt1$OMAl))==trimws("NA")),NA,dt1$OMAl)               
suppressWarnings(dt1$OMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$OMAl))==as.character(as.numeric("NA"))),NA,dt1$OMAl))
dt1$Al_ICP <- ifelse((trimws(as.character(dt1$Al_ICP))==trimws("NA")),NA,dt1$Al_ICP)               
suppressWarnings(dt1$Al_ICP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Al_ICP))==as.character(as.numeric("NA"))),NA,dt1$Al_ICP))
dt1$Al_ferron <- ifelse((trimws(as.character(dt1$Al_ferron))==trimws("NA")),NA,dt1$Al_ferron)               
suppressWarnings(dt1$Al_ferron <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Al_ferron))==as.character(as.numeric("NA"))),NA,dt1$Al_ferron))
dt1$NH4 <- ifelse((trimws(as.character(dt1$NH4))==trimws("NA")),NA,dt1$NH4)               
suppressWarnings(dt1$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$NH4))==as.character(as.numeric("NA"))),NA,dt1$NH4))
dt1$SO4 <- ifelse((trimws(as.character(dt1$SO4))==trimws("NA")),NA,dt1$SO4)               
suppressWarnings(dt1$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SO4))==as.character(as.numeric("NA"))),NA,dt1$SO4))
dt1$NO3 <- ifelse((trimws(as.character(dt1$NO3))==trimws("NA")),NA,dt1$NO3)               
suppressWarnings(dt1$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$NO3))==as.character(as.numeric("NA"))),NA,dt1$NO3))
dt1$Cl <- ifelse((trimws(as.character(dt1$Cl))==trimws("NA")),NA,dt1$Cl)               
suppressWarnings(dt1$Cl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cl))==as.character(as.numeric("NA"))),NA,dt1$Cl))
dt1$PO4 <- ifelse((trimws(as.character(dt1$PO4))==trimws("NA")),NA,dt1$PO4)               
suppressWarnings(dt1$PO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$PO4))==as.character(as.numeric("NA"))),NA,dt1$PO4))
dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("NA")),NA,dt1$DOC)               
suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("NA"))),NA,dt1$DOC))
dt1$TDN <- ifelse((trimws(as.character(dt1$TDN))==trimws("NA")),NA,dt1$TDN)               
suppressWarnings(dt1$TDN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TDN))==as.character(as.numeric("NA"))),NA,dt1$TDN))
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("NA")),NA,dt1$DON)               
suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("NA"))),NA,dt1$DON))
dt1$SiO2 <- ifelse((trimws(as.character(dt1$SiO2))==trimws("NA")),NA,dt1$SiO2)               
suppressWarnings(dt1$SiO2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SiO2))==as.character(as.numeric("NA"))),NA,dt1$SiO2))
dt1$Mn <- ifelse((trimws(as.character(dt1$Mn))==trimws("NA")),NA,dt1$Mn)               
suppressWarnings(dt1$Mn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mn))==as.character(as.numeric("NA"))),NA,dt1$Mn))
dt1$Fe <- ifelse((trimws(as.character(dt1$Fe))==trimws("NA")),NA,dt1$Fe)               
suppressWarnings(dt1$Fe <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Fe))==as.character(as.numeric("NA"))),NA,dt1$Fe))
dt1$F <- ifelse((trimws(as.character(dt1$F))==trimws("NA")),NA,dt1$F)               
suppressWarnings(dt1$F <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$F))==as.character(as.numeric("NA"))),NA,dt1$F))
dt1$cationCharge <- ifelse((trimws(as.character(dt1$cationCharge))==trimws("NA")),NA,dt1$cationCharge)               
suppressWarnings(dt1$cationCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$cationCharge))==as.character(as.numeric("NA"))),NA,dt1$cationCharge))
dt1$anionCharge <- ifelse((trimws(as.character(dt1$anionCharge))==trimws("NA")),NA,dt1$anionCharge)               
suppressWarnings(dt1$anionCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$anionCharge))==as.character(as.numeric("NA"))),NA,dt1$anionCharge))
dt1$ionError <- ifelse((trimws(as.character(dt1$ionError))==trimws("NA")),NA,dt1$ionError)               
suppressWarnings(dt1$ionError <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ionError))==as.character(as.numeric("NA"))),NA,dt1$ionError))
dt1$duplicate <- as.factor(ifelse((trimws(as.character(dt1$duplicate))==trimws("NA")),NA,as.character(dt1$duplicate)))
dt1$sampleType <- as.factor(ifelse((trimws(as.character(dt1$sampleType))==trimws("NA")),NA,as.character(dt1$sampleType)))
dt1$ionBalance <- ifelse((trimws(as.character(dt1$ionBalance))==trimws("NA")),NA,dt1$ionBalance)               
suppressWarnings(dt1$ionBalance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ionBalance))==as.character(as.numeric("NA"))),NA,dt1$ionBalance))
dt1$canonical <- as.factor(ifelse((trimws(as.character(dt1$canonical))==trimws("NA")),NA,as.character(dt1$canonical)))
dt1$pHmetrohm <- ifelse((trimws(as.character(dt1$pHmetrohm))==trimws("NA")),NA,dt1$pHmetrohm)               
suppressWarnings(dt1$pHmetrohm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pHmetrohm))==as.character(as.numeric("NA"))),NA,dt1$pHmetrohm))


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

wtr_chem <- dt2 |>
  select(date,timeEST, site, pH)|>
  relocate(date, timeEST, pH, site)
  
wtr_chem$site <- as.character(wtr_chem$site)
wtr_chem$pH <- as.double(wtr_chem$pH)


#wtr_chem$timeEST <- as.integer(wtr_chem$timeEST)|>
  #mutate(datetime = make_datetime(date, timeEST))

tibble(wtr_chem)

ALL_Stands_Temp <- Temperature |> 
  group_by(date) |>
  summarize(Avg_temp = mean(AVE))
