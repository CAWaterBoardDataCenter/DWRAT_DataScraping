#Install and load libraries
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function

#Import DAT stuff
DAT_File <- read.delim(here("InputData/data_update_to_11.30.2022.txt"), sep = "\t")
DAT_Fields <- read.csv(here("InputData/DAT_Fields.csv"))

#Set DAT_File column names
colnames(DAT_File) <- colnames(DAT_Fields)

#Notes about DAT_File fields----
  #22 Runoff fields, always equal 1
  #6 date-time fields, pre-filled from 1/1/1990 through 9/30/2023
  #15 precipitation fields
  #8 temperature fields
  #We will replace a subset of the data corresponding to our timeframe of interest

#Whittle the DAT_File to a subset corresponding to our timeframe of interest, "DAT_Shell"----
#Add a Timestep column
DAT_File$TimeStep <- make_date(year = DAT_File$Year, 
                              month = DAT_File$month,
                              day = DAT_File$day)

#Filter to the timeframe of interest
#Set the start date by grabbing the first day of the current month
#Start_Date <- lubridate::floor_date(Sys.Date(), unit = "month")
Start_Date <- as.Date("2023-02-01")
#The end date is the current date + 5 days in the future; we grab 6 days of forecast data from CNRFC
End_Date <- Sys.Date() + 5
DAT_Shell <- subset(DAT_File, TimeStep >= Start_Date & TimeStep <= End_Date)
DAT_Shell

#Set TimeStep as the 7th column in DAT_Shell
DAT_Shell <- DAT_Shell %>% relocate(TimeStep, .after = s)

#Set all the DAT_Shell temperature and precipitation fields to blank
for (i in 1:length(DAT_Shell)){
  if (colnames(DAT_Shell[i]) %>% str_detect("PREC")){
    DAT_Shell[i] = ""
  } else if (colnames(DAT_Shell[i]) %>% str_detect("TM")){
    DAT_Shell[i] = ""
  }else {
    DAT_Shell[i] = DAT_Shell[i]
  }
}

#Run the Data Scraping and Processing Scripts----
source(here("Scripts/RAWS_Scraper.R"))
source(here("Scripts/CNRFC_Scraper.R"))
source(here("Scripts/CIMIS_Scraper.R"))

#Import Observed and Forecast Data----
Files = list.files(path = here("ProcessedData"), pattern="*.csv")
for (i in 1:length(Files)) assign(Files[i], read.csv(file = paste0(here("ProcessedData"), "/", Files[i])))

#Replace Missing Values with PRISM Data----
#RAWS
for (i in 1:nrow(RAWS_PRECIP4.csv))

#CIMIS

#DOWNSIZER
  
#CNRFC

#Set DAT_SHELL values----


#Filling in all Temp and Precipitation fields
DAT_Shell$PRECIP7 <- PRECIP7.csv$Precipitation
DAT_Shell$PRECIP9 <- PRECIP9.csv$Precipitation
DAT_Shell$PRECIP4 <- PRECIP4.csv$Precipitation
Dat_Shell$TMAX5 <- TEMP5.csv$Tmax
Dat_Shell$TMIN5 <- TEMP5.csv$Tmin
Dat_Shell$TMAX7 <- TEMP7.csv$Tmax
Dat_Shell$TMIN7 <- TEMP7.csv$Tmin
Dat_Shell$TMAX8  <-TEMP8.csv$Tmax
Dat_Shell$TMIN8 <- TEMP8.csv$Tmin
