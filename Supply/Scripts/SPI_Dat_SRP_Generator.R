#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function
library(readxl) #for read_xlsx function


# Rely on the shared functions from the Demand and Supply scripts
source("../Supply/Scripts/Shared_Functions_Supply.R")
source("../Demand/Scripts/Shared_Functions_Demand.R")

# Import Latest Dat SRP File
Dat_SRP_Path = makeSharePointPath(filePathFragment = "DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints\\Dat_SRP_Body.dat")


# Read the data as a single column
Dat_SRP_Full <- read_lines(Dat_SRP_Path)

# Define a regular expression pattern to split the data based on one or more spaces
pattern <- "\\s+"

# Split the data into separate columns based on the pattern
split_data <- str_split(Dat_SRP_Full, pattern)

# Convert the list of vectors into a dataframe
Dat_SRP_Full <- as.data.frame(do.call(rbind, split_data))
Dat_SRP_Full$V13 = NULL

#Import Dat SRP Field Names
Dat_SRP_Fields_Path = makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints\\Dat_SRP_FieldNames.csv")
Dat_SRP_Fields = read.csv(file = Dat_SRP_Fields_Path, header = F) %>% unlist()

colnames(Dat_SRP_Full) = Dat_SRP_Fields

#DAT_SRP_Full needs to subdivided into 2 separate standalone CSVs
  #A file consisting of data from 1/1/1990 - 9/30/2023; that's our pre-2023WY_SRP_DAT file
    #Save to DAT SRP Blueprints folder on SharePoint
  #A file consisting of data from 4/1/2024 - 9/30/2024; that's our SPI WY 2023-2024 DAT file or Dat SRP Forecast file for short
    #Save to DAT SRP Blueprints folder on SharePoint

#Constructing a new SRP Dat File
  #Run the first portion of Master_Script_PRMS.R where you define StartDate, EndDate, and you download the SRP raw data
  #Import Pre-2023 WY SRP DAT file
  #Import SPI WY 2023-2024 SRP DAT File
  #Import SRP_Processed.csv

  #Have some logic that overrides overlapping portion of SPI file with observed data if observed data exists. 
  #Never use SPI data if you have observed data available.
  #combine the 3 datasets (Pre-2023 WY SRP DAT File, SPI data, and observed data) to generate final SRP DAT file
  # for a specific month
  #Save the SRP_Dat file for a specific month to the ProcessedData folder with a timestamp. The timestamp is the EndDate;
  #EndDate is the last day of the observed data range. 

# Import Processed SRP data
SRP_Processed = read.csv(file = "ProcessedData/SRP_Processed.csv")