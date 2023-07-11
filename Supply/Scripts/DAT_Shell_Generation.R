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
#Add a Date column
DAT_File$Date <- make_date(year = DAT_File$Year, 
                              month = DAT_File$month,
                              day = DAT_File$day)

#Filter to the timeframe of interest
#Set the start date by grabbing the first day of the current month
#Start_Date <- lubridate::floor_date(Sys.Date(), unit = "month")
#Start_Date <- as.Date("2023-01-11")
#The end date is the current date + 5 days in the future; we grab 6 days of forecast data from CNRFC
#End_Date <- Sys.Date() + 5
DAT_Shell <- subset(DAT_File, Date >= StartDate & Date <= End_Date) #Adjust as needed
DAT_Shell

#Set Date as the 7th column in DAT_Shell
DAT_Shell <- DAT_Shell %>% relocate(Date, .after = s)

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

#write to CSV
write.csv(DAT_Shell, here("InputData/Dat_Shell.csv"), row.names = FALSE)
