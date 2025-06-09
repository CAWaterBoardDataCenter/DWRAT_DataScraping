#----PURPOSE----
# This script was created to recover and isolate the original SPI forecast data 
# embedded within a finalized SRP .dat file, after the standalone SPI source data
# had been lost. Recognizing that any data from April 1, 2024 onward in the file was
# forecast using SPI—rather than observed—the script extracts this segment into a
# separate CSV (SPI_SRP_WY_2023_2024.csv). It also isolates all historical data 
# through September 30, 2023 into another file (DAT_SRP_1947_to_WY2023.csv),
# preserving a clean, pre-forecast blueprint dataset.However, this script has been 
# superseded and is now obsolete. 

# Last Updated By: Payman Alemi

#Install and load libraries and custom functions----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function
library(readxl) #for read_xlsx function


# Rely on the shared functions from the Demand and Supply scripts
source("../Supply/Scripts/Shared_Functions_Supply.R")
source("../Demand/Scripts/Shared_Functions_Demand.R")

# Import the SRP Dat file used for the 3/14/2024 model run from SDA SharePoint folder 
  # Documents\DWRAT\SDU_Runs\Hydrology\2024-03-14

Dat_SRP_Path = makeSharePointPath(filePathFragment = "DWRAT\\SDU_Runs\\Hydrology\\2024-03-14\\Dat_SRP_Final_Forecast_2024-03-19.dat")

# Manipulate the Dat SRP File----
# Read the data as a single column
Dat_SRP_Body <- read_lines(file = Dat_SRP_Path, skip = 5)

# Define a regular expression pattern to split the data based on one or more spaces
pattern <- "\\s+"

# Split Dat_SRP_Body into separate columns based on the pattern
Dat_SRP_Body <- str_split(Dat_SRP_Body, pattern)

# Convert the list of vectors into a dataframe
Dat_SRP_Body <- as.data.frame(do.call(rbind, Dat_SRP_Body))
Dat_SRP_Body$V13 = NULL #Drop the 13th column

#Import Dat SRP Field Names
Dat_SRP_Fields_Path = makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints\\Dat_SRP_FieldNames.csv")
Dat_SRP_Fields = read.csv(file = Dat_SRP_Fields_Path, header = F) %>% unlist()

#Set the column names of Dat_SRP_Body to the values in Dat_SRP_Fields
colnames(Dat_SRP_Body) = Dat_SRP_Fields

# Generate a Date column for Dat_SRP_Body

  ## Concatenate the month, day, and year columns and set in the YYYY-MM-DD date format
  Date = as.Date(x = paste0(Dat_SRP_Body$month, "/", Dat_SRP_Body$day, "/", Dat_SRP_Body$year), 
                 format = "%m/%d/%Y")
  
  ## Insert Date as the 7th column in Dat_SRP_Body
  Dat_SRP_Body <- cbind(Dat_SRP_Body[, 1:6],Date, Dat_SRP_Body[,7:ncol(Dat_SRP_Body)])

# DAT_SRP_Body needs to subdivided into 2 separate standalone CSVs
  # (1) A file consisting of data from 10/1/1947 - 9/30/2023; that's our pre-2023_WY_SRP_DAT file
    #Save to DAT SRP Blueprints folder on SharePoint
  
  # (2) A file consisting of data from 3/1/2024 - 9/30/2024; that's our SPI WY 2023-2024 DAT file or Dat SRP Forecast file for short
    #Save to DAT SRP Blueprints folder on SharePoint

## Create the DAT SRP SPI Forecast file----
SPI_WY_2023_2024 = Dat_SRP_Body %>% filter(Date >= "2024-04-01" & Date <= "2024-09-30")

write.csv(x = SPI_WY_2023_2024, file = paste0(makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints\\"), 
                                              "SPI_SRP_WY_2023_2024.csv"), row.names = FALSE)

## Create the Pre-2023 WY DAT SRP file----
DAT_SRP_1947_to_WY2023 = Dat_SRP_Body %>% filter(Date >= "1947-10-01" & Date <= "2023-09-30")

write.csv(x = DAT_SRP_1947_to_WY2023, file = paste0(makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints\\"), 
                                                    "DAT_SRP_1947_to_WY2023.csv"), row.names = F)

