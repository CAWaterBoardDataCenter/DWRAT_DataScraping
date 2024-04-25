#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function
library(readxl) #for read_xlsx function

# Rely on the shared functions from the Demand scripts
source("../Supply/Scripts/Shared_Functions.R")

# Check Dat_PRMS for accuracy of April 2024 - September 2024 SPI data----

# Import 04/01/2024 - 09/30/2024 PRMS SPI Data
DAT_Predictions_PRMS <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Forecast_Values.dat") %>%
  read_delim("\t", col_names = FALSE, show_col_types = FALSE) %>%
  set_names(names(read_csv("InputData/DAT_Fields_PRMS.csv", show_col_types = FALSE)))

#Add a date column to the 7th position
DAT_Predictions_PRMS <- add_date_column(dat = DAT_Predictions_PRMS, col_position = 7) 

# Import 1990-pre2023 Water Year PRMS Data
DAT_Initial_PRMS <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_PRMS_1990_to_WY2023.dat") %>%
  read_delim("\t", col_names = FALSE, show_col_types = FALSE) %>%
  set_names(names(read_csv("InputData/DAT_Fields_PRMS.csv", show_col_types = FALSE)))

# Add a date column to the 7th position
DAT_Initial_PRMS <- add_date_column(dat = DAT_Initial_PRMS, col_position = 7) 

# Import SPI_Manashi.xlsx, sheet "Final"
SPI_Indices_Path <- makeSharePointPath(filePathFragment = "DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\SPI_Manashi.xlsx")
SPI_Indices = read_xlsx(path = SPI_Indices_Path, sheet = "Final")

#Dat_Predictions:
# April 2024 of Dat_Initial_PRMS should match April 2021 of Dat_Predictions_PRMS
# May 2024 "" should match May 2001 ""
# June 2024 "" should match June 2002 ""
# July 2024 "" should match July 1998""
# August 2024 ""should match August 1996"
# September 2024 "" should match September 2002"


# Extract the month-year combinations from rows 4 to 9 of SPI_Indices
months_years <- data.frame(
  Month = SPI_Indices$Month[4:9],
  Year = SPI_Indices$Year[4:9]
)

# Convert month and year to character type for comparison
months_years$Month <- as.character(months_years$Month)
months_years$Year <- as.character(months_years$Year)

# Generate conditions dynamically based on months_years dataframe
conditions <- paste0("(Year == ", months_years$Year, " & month == ", months_years$Month, ")")

# Combine conditions using OR operator
combined_conditions <- paste(conditions, collapse = " | ")

# Filter DAT_Initial_PRMS and DAT_Prediction_PRMS
DAT_Initial_PRMS_Subset <- filter(DAT_Initial_PRMS, eval(parse(text = combined_conditions)))
DAT_Initial_PRMS_Subset = DAT_Initial_PRMS_Subset[,1:38]
DAT_Initial_PRMS_Subset = DAT_Initial_PRMS_Subset %>% arrange(month)

DAT_Predictions_PRMS_Subset = DAT_Predictions_PRMS[,1:38]

#Compare two dataframes
identical(DAT_Initial_PRMS_Subset[,8:38], DAT_Predictions_PRMS_Subset[,8:38])
# Both dataframes are identical; hence the data was copied and pasted correctly!