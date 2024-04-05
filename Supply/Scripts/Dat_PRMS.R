#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function


#Import the PRMS DAT file used for the last model run
Dat_PRMS_Path <- list.files("InputData", pattern = "PRMS.*\\.dat$", full.names = TRUE) %>% sort() %>% tail(1)
print(Dat_PRMS_Path)
Dat_PRMS_Original <- read.delim(file = Dat_PRMS_Path, sep = "\t", skip = 6, header = FALSE)
Dat_Fields_PRMS <- read.csv(here("InputData/Dat_Fields_PRMS.csv"))

#Set Dat_File column names
colnames(Dat_PRMS_Original) <- colnames(Dat_Fields_PRMS)

#Notes about Dat_PRMS_Original fields----
#22 Runoff fields, always equal 1
#6 date-time fields, pre-filled from 1/1/1990 through 9/30/2024
#15 precipitation fields
#8 temperature fields
#We will replace a subset of the data corresponding to our timeframe of interest

# Whittle the Dat_PRMS_Original to a subset corresponding to our timeframe of interest, "Dat_Shell_PRMS"----
#Add a Date column
Dat_PRMS_Original$Date <- make_date(year = Dat_PRMS_Original$Year, 
                                month = Dat_PRMS_Original$month,
                                day = Dat_PRMS_Original$day)

Dat_Shell_PRMS <- subset(Dat_PRMS_Original, Date >= StartDate$date & Date <= EndDate$date)
#Dat_Shell_PRMS <- subset(Dat_PRMS_Original, Date >= StartDate$date & Date <= End_Date) #Adjust as needed
Dat_Shell_PRMS

#Set Date as the 7th column in Dat_Shell_PRMS
Dat_Shell_PRMS <- Dat_Shell_PRMS %>% relocate(Date, .after = s)

# Import the necessary starter files for the  PRMS DAT File----
# DAT_Shell_PRMS serves as the shell of the PRMS DAT File, meteorological data (already pre-filled with PRISM and forecast data) contained in these files:
# RAWS_Processed.csv, CIMIS_Processed.csv, NOAA_Processed_[DATE].csv 

## Meteorological Data Sources ----
RAWS <- read.csv(here("ProcessedData/RAWS_Processed.csv"))

NOAA <- list.files("ProcessedData", pattern = "NOAA_API_Processed_", full.names = TRUE) %>%
  sort() %>% tail(1) %>% read.csv() # Formerly Downsizer

CIMIS <- read.csv(here("ProcessedData/CIMIS_Processed.csv"))

#Convert dates in all 3 data sources to date type in YYYY-MM-DD format
RAWS$Date = as.Date(x = RAWS$Date, format = "%m/%d/%Y")
CIMIS$Date = as.Date(x = CIMIS$Date, format = "%Y-%m-%d")
NOAA$Date = as.Date(x = NOAA$Date, format = "%Y-%m-%d")

# Create Dat_Final_PRMS----

# Merge meteorological data sources into one dataframe
Meteorological <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), 
              list(RAWS, NOAA, CIMIS))

#Write the full Meteorological dataset to a CSV
write.csv(x = Meteorological, 
          file = paste0("ProcessedData/Meteorological_",EndDate$date, ".csv" ),
          row.names = F)

# Merge Dat_Shell_PRMS with Meteorological data----

# Remove columns 1-6 of DAT_Shell_PRMS (individual datetime fields: year, month, etc) along with columns 39-60 (runoff columns)
Dat_Shell_PRMS2 <- Dat_Shell_PRMS[,-c(1:6, 39:60)]
head(Dat_Shell_PRMS2)

#Inner Join the Meteorological dataframe to Dat_Shell_PRMS2 by the Date field
Dat_Shell_PRMS3 <- inner_join(x = Meteorological, 
                              y =select(Dat_Shell_PRMS2, Date), 
                              by = "Date")
head(Dat_Shell_PRMS3)

# Import the individual datetime fields (year, month, etc) from Dat_Shell_PRMS
# And import the 22 runoff columns from Dat_Shell_PRMS
Dat_Shell_PRMS4 <- cbind(Dat_Shell_PRMS[(1:6)], Dat_Shell_PRMS3,Dat_Shell_PRMS[,39:60])
head(Dat_Shell_PRMS4)

# 'anti_join' keeps all records from Dat_PRMS_Original except the ones that 
#  match Dat_Shell_PRMS4
Dat_PRMS_Final <- anti_join(x = Dat_PRMS_Original, y = Dat_Shell_PRMS4, by = "Date")

# bind_row adds the dates that we removed in the previous step but this time those
# dates have the meteorological data that we have downloaded; this completes the data
# substitution for the PRMS Dat file
Dat_PRMS_Final <- bind_rows(Dat_PRMS_Final, Dat_Shell_PRMS4)
Dat_PRMS_Final


#Set Date as the 7th column in Dat_Shell_PRMS and sort by Date
Dat_PRMS_Final <- Dat_PRMS_Final %>% relocate(Date, .after = s) %>%
  arrange(Date)

#Remove the date column from Dat_Final_PRMS
Dat_PRMS_Final$Date = NULL

# Restore original column order to DAT_PRMS_Final----
# Get the column names from both dataframes
dat_final_names <- names(Dat_PRMS_Final)
col_order_final <- names(Dat_Fields_PRMS)

# Find the mapping between column positions
mapping <- match(col_order_final, dat_final_names)

# Reorder columns using indexing
Dat_PRMS_Final <- Dat_PRMS_Final[, mapping]

#Add the header to the Dat_PRMS_Final file----
Dat_PRMS_Header = read.csv(file = "InputData/Dat_PRMS_header.csv", header = F)

## Add 58 empty columns to DAT_PRMS_Header----
empty_cols <- data.frame(matrix("", nrow = 6, ncol = 58))
Dat_PRMS_Header = cbind(Dat_PRMS_Header, empty_cols)
colnames(Dat_PRMS_Header) = colnames(Dat_Fields_PRMS)

# Combine Dat_PRMS_Header with Dat_PRMS_Final
Dat_PRMS_Final = rbind(Dat_PRMS_Header, Dat_PRMS_Final)

#Write the Dat_Final_PRMS file to the ProcessedData folder
write.table(x = Dat_PRMS_Final, 
            file = paste0("ProcessedData/Dat_PRMS_Final_Forecast_End_Date_", EndDate$date, ".dat"),
            sep = "\t", row.names =  F, quote =  F, col.names = F)

  #This is one-time code used to generate the Dat PRMS file for the observed meteorological data
  #from 4/1/2023 - 1/31/2024
# write.table(x = Dat_PRMS_Final,
#             file =  "ProcessedData/Dat_PRMS_Final_Observed_2023-04-01_2024-01-31.dat",
#             sep = "\t", row.names = F, quote = F, col.names = F)

# Climate_Scenarios_Path = "C:\\RR_PRMS\\PRMS\\input\\climate_scenarios\\"
# write.table(x = Dat_Final_PRMS, file = paste0(Climate_Scenarios_Path, "DAT_Final_PRMS_Forecast_End_Date_", End_Date, ".dat"),
#             sep = "\t", row.names = F, quote = F, col.names = F)


#Clean up variables----
#Add Dat_PRMS_Final to vars_to_keep
#vars_to_keep = c(vars_to_keep, "Dat_PRMS_Final")

# List all variables in the global environment
#all_vars <- ls()

# Identify which variables to remove
#vars_to_remove <- setdiff(all_vars, vars_to_keep)

# Remove variables except those in vars_to_keep
#rm(list = vars_to_remove)

print("Dat PRMS script has finished running!")
