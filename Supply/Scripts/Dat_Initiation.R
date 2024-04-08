#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function

# PRMS Section----
#The purpose of this section is to generate 3 starter files required for creating
# the Dat PRMS file; while the observed metereology data will change and is downloaded
# by other scripts, for the remainder of the 2023-2024 water year, these files will 
# remain the same and will only have to be generated once:
    # 1) Pre-2024 Dat PRMS File: Consists of all data from 1/1/1990 - 9/30/2023; 
        # generated from an older Dat PRMS file

    # 2) SPI Dat PRMS File: Consists of SPI data representing the worst-case scenario from 11/1/2023 - 9/30/2024; unfortunately 
        # the data for October 2023 was not preserved but it's not necessary since even if were to rerun October 2023, we would use
        # observed data that has since become available 

    # 3) Similar Water Year File: Consists of 2021 water year (WY) data with 2023-2024 WY dates; separate analysis
    # by Aakash Prashar determined that the 2021 WY was the most similar to 2024 from 1990-2023. 

## Pre-2024 Dat PRMS File----
#Import an older PRMS Dat file that has all the 1/1/1990 - 9/30/2023 data intact
Dat_PRMS_Old <- read.delim(file = "InputData/Dat_Final_PRMS_Forecast_End_Date_2024-01-31.dat",
                                sep = "\t", skip = 6, header = FALSE)
Dat_Fields_PRMS <- read.csv(here("InputData/Dat_Fields_PRMS.csv"))

#Restore column names
colnames(Dat_PRMS_Old) <- colnames(Dat_Fields_PRMS)

#Add a Date Field
Dat_PRMS_Old$Date <- make_date(year = Dat_PRMS_Old$Year, 
                               month = Dat_PRMS_Old$month,
                               day = Dat_PRMS_Old$day)

#Set Date as the 7th column in Dat_PRMS_Old
Date_column <- Dat_PRMS_Old$Date
Dat_PRMS_Old$Date = NULL
Dat_PRMS_Old = cbind(Dat_PRMS_Old[, 1:6], Date = Date_column, Dat_PRMS_Old[, 7:59])

#Whittle to just 1/1/1990 to 9/30/2023
Dat_PRMS_Old = subset(Dat_PRMS_Old, 
                     Date >= as.Date("1990-01-01", format = "%Y-%m-%d") &
                       Date <= as.Date("2023-09-30", format = "%Y-%m-%d"))

#Export Dat_PRMS_Old as a CSV
write.csv(x = Dat_PRMS_Old, file = "InputData/Dat_PRMS_Pre_WY2024.csv", row.names = F)

## Reverse Engineer the SPI 2023-2024 data for 11/1/2023 - 9/30/2024----
#Import the 10/25/2023 PRMS Dat file which has SPI data intact from 11/1/2023 - 9/30/2024
# October 2023 data consists of forecast CNRFC and observed data; SPI data begins on 11/1/2023
Dat_PRMS_SPI <- read.delim(file = "InputData/data_update_to_2023_10_25_PA.dat",
                                sep = "\t", skip = 6, header = FALSE)

#Restore column names
colnames(Dat_PRMS_SPI) <- colnames(Dat_Fields_PRMS)

#Add a Date Field
Dat_PRMS_SPI$Date <- make_date(year = Dat_PRMS_SPI$Year, 
                                    month = Dat_PRMS_SPI$month,
                                    day = Dat_PRMS_SPI$day)

#Whittle to 11/1/2023 - 9/30/2024
SPI_2023_24 = subset(Dat_PRMS_SPI, 
                     Date >= as.Date("2023-11-01", format = "%Y-%m-%d") &
                       Date < as.Date("2024-09-30", format = "%Y-%m-%d"))

#Export SPI_2023-2024 as a csv
write.csv(x = SPI_2023_24, file = "InputData/SPI_2023_24.csv",row.names = FALSE)


## Create WY_2024 dataset----
# We determined in another script that the 2021 water year is most similar
# to the 2024 water year

#Obtain the 2021 water year data
WY_2021 = subset(Dat_PRMS_Old, 
                 Date >= as.Date("2020-10-01", format = "%Y-%m-%d") &
                 Date <= as.Date("2021-09-30", format = "%Y-%m-%d"))
WY_2021

# Push forward all the dates by 3 years
WY_2024 = WY_2021
WY_2024$Date = WY_2021$Date+years(3)
WY_2024$Year = WY_2021$Year + 3

#Export CSVs to the InputData folder----
write.csv(x = WY_2024, file = "InputData/PRMS_WY_2024_Similar_Year.csv", row.names = FALSE)



