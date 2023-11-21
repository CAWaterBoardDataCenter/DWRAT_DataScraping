#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function

#Import PRMS Dat stuff
Dat_File_PRMS <- read.delim(here("InputData/PRMS_2023-10-25_PA.txt"), sep = "\t")
Dat_Fields_PRMS <- read.csv(here("InputData/Dat_Fields_PRMS.csv"))

#Set Dat_File column names
colnames(Dat_File_PRMS) <- colnames(Dat_Fields_PRMS)

#Notes about Dat_File_PRMS fields----
  #22 Runoff fields, always equal 1
  #6 date-time fields, pre-filled from 1/1/1990 through 9/30/2024
  #15 precipitation fields
  #8 temperature fields
  #We will replace a subset of the data corresponding to our timeframe of interest

# Whittle the Dat_File_PRMS to a subset corresponding to our timeframe of interest, "Dat_Shell_PRMS"----
#Add a Date column
Dat_File_PRMS$Date <- make_date(year = Dat_File_PRMS$Year, 
                              month = Dat_File_PRMS$month,
                              day = Dat_File_PRMS$day)

## Filter to the timeframe of interest----
# Set the start date by grabbing the first day of the current month
  # Start_Date <- lubridate::floor_date(Sys.Date(), unit = "month")
  # Start_Date <- as.Date("2023-01-11")

# The end date is the current date + 5 days in the future; we grab 6 days of forecast data from CNRFC
  #End_Date <- Sys.Date() + 5
Dat_Shell_PRMS <- subset(Dat_File_PRMS, Date >= StartDate$date & Date <= End_Date) #Adjust as needed
Dat_Shell_PRMS

#Set Date as the 7th column in Dat_Shell_PRMS
Dat_Shell_PRMS <- Dat_Shell_PRMS %>% relocate(Date, .after = s)

#Set all the Dat_Shell_PRMS temperature and precipitation fields to blank
for (i in 1:length(Dat_Shell_PRMS)){
  if (colnames(Dat_Shell_PRMS[i]) %>% str_detect("PREC")){
    Dat_Shell_PRMS[i] = ""
  } else if (colnames(Dat_Shell_PRMS[i]) %>% str_detect("TM")){
    Dat_Shell_PRMS[i] = ""
  }else {
    Dat_Shell_PRMS[i] = Dat_Shell_PRMS[i]
  }
}

#write to CSV
write.csv(Dat_Shell_PRMS, here("InputData/Dat_Shell_PRMS.csv"), row.names = FALSE)

#Clean up variables----
# #Add Dat_Shell_PRMS to vars_to_keep
# vars_to_keep = c(vars_to_keep, "Dat_Shell_PRMS")
# 
# # List all variables in the global environment
# all_vars <- ls()
# 
# # Identify which variables to remove
# vars_to_remove <- setdiff(all_vars, vars_to_keep)
# 
# # Remove variables except those in vars_to_keep
# rm(list = vars_to_remove)

print("Dat_Shell_Generation.R has finished running")

