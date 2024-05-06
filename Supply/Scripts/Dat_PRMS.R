#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function

# Rely on the shared functions from the Demand scripts
source("../Demand/Scripts/Shared_Functions_Demand.R")



# Use the DAT component files located on SharePoint


# Metadata that appears at the beginning of the file
DAT_Metadata <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Metadata.dat") %>%
  read_lines()



# Predicted values for the rest of the water year
DAT_Predictions <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Forecast_Values.dat") %>%
  read_delim("\t", col_names = FALSE, show_col_types = FALSE) %>%
  set_names(names(read_csv("InputData/DAT_Fields_PRMS.csv", show_col_types = FALSE)))



# Read in the DAT file that contains data from 1990 up to the current water year
DAT_Initial <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_PRMS_1990_to_WY2023.dat") %>%
  read_delim("\t", col_names = FALSE, show_col_types = FALSE) %>%
  set_names(names(DAT_Predictions))



#Notes about Dat_PRMS_Original fields----
#22 Runoff fields, always equal 1
#6 date-time fields, from 1/1/1990 through 9/30 of the previous WY
#15 precipitation fields
#8 temperature fields
# We will append data for the current water year 

# Next, read in the meteorological data that has been downloaded
## Meteorological Data Sources ----
RAWS <- read_csv("ProcessedData/RAWS_Processed.csv", show_col_types = FALSE)


NOAA <- read_csv("ProcessedData/NOAA_API_Processed.csv", show_col_types = FALSE) # Formerly Downsizer

CIMIS <- read_csv("ProcessedData/CIMIS_Processed.csv", show_col_types = FALSE)



# Ensure that the "Date" column is correctly interpreted as a "date" type in each dataset
RAWS$Date = as.Date(x = RAWS$Date, format = "%m/%d/%Y")
CIMIS$Date = as.Date(x = CIMIS$Date, format = "%Y-%m-%d")
NOAA$Date = as.Date(x = NOAA$Date, format = "%Y-%m-%d")



# Create Dat_Final_PRMS----

# Merge meteorological data sources into one dataframe
Meteorological <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), 
              list(RAWS, NOAA, CIMIS))

#Write the full Meteorological dataset to a CSV
write_csv(x = Meteorological, 
          file = paste0("ProcessedData/Meteorological_",EndDate$date, ".csv" ))



# Append 'Meteorological' to 'DAT_Initial'


# 'DAT_Initial' will need a date field for this join to be possible
DAT_Initial <- DAT_Initial %>%
  mutate(Date = as.Date(paste0(Year, "/", month, "/", day), format = "%Y/%m/%d"))



# Similarly, 'Meteorological' will require additional columns in order to match 'DAT_Initial'
Meteorological <- Meteorological %>%
  mutate(Year = year(Date), month = month(Date), day = day(Date), h = 0, m = 0, s = 0)


# 22 runoff columns are required as well
Meteorological[, paste0("Runoff", 1:22)] <- 1



# Perform an error check as well
# Check if 'Meteorological' contains any dates that appear in 'DAT_Initial'
# If that is the case, output a message and remove those rows from 'DAT_Initial'
if (DAT_Initial %>% filter(Date %in% Meteorological$Date) %>% nrow() > 0) {
  
  print(c("The scraped meteorological dataset contains rows for dates that appear in the DAT skeleton file.", 
          "The data for those dates in 'DAT_Initial' will be replaced with the data in the meteorological dataset."))
  
  
  # Remove those rows in 'DAT_Initial'
  DAT_Initial <- DAT_Initial %>%
    filter(!(Date %in% Meteorological$Date))
  
}



# Once these preparations are complete, the two data frames can be merged
DAT_Merged <- DAT_Initial %>%
  bind_rows(Meteorological) %>%
  arrange(Date)



# Water Year Forecast data----
# The next step is to append forecasted data for the rest of the water year


# Filter 'DAT_Predictions' to dates that do not appear in 'DAT_Merged'
DAT_Predictions <- DAT_Predictions %>%
  mutate(Date = as.Date(paste0(Year, "/", month, "/", day), format = "%Y/%m/%d")) %>%
  filter(Date > max(DAT_Merged$Date))



if (nrow(DAT_Predictions) > 0) {
  
  DAT_Merged <- bind_rows(DAT_Merged, DAT_Predictions) %>%
    arrange(Date)
  
}



# Check for errors----

# Check to make sure that no dates are missing data between the beginning and end of 'DAT_Merged'
dateSeq <- seq(from = min(DAT_Merged$Date), to = max(DAT_Merged$Date), by = "day")



if (dateSeq[!(dateSeq %in% DAT_Merged$Date)] %>% length() > 0) {
  
  print("DAT_Merged is missing data for the following date(s):")
  print(dateSeq[!(dateSeq %in% DAT_Merged$Date)])
  stop("Please correct this error before proceeding")
  
}



# Make sure there are no NA values or missing values in the dataset
stopifnot(!anyNA(DAT_Merged))
stopifnot(sum(grepl("\\-99", DAT_Merged)) == 0)



# Round the numeric values in 'DAT_Merged'
# (Keeping at most one decimal place)
DAT_Merged <- DAT_Merged %>%
  mutate(across(where(is.numeric), ~ round(., 1)))



# Output the final DAT file----


# Temporarily write 'DAT_Merged' to a file
DAT_Merged %>%
  select(-Date) %>%
  write_delim(paste0("ProcessedData/Dat_PRMS_Final_End_Date_", EndDate$date, ".dat"),
              delim = "\t", col_names = FALSE)



# Read back in this file
# Then, append 'DAT_Metadata' to the beginning
DAT_Merged_Tab <- c(DAT_Metadata,
  read_lines(paste0("ProcessedData/Dat_PRMS_Final_End_Date_", EndDate$date, ".dat")))



# Double-check that the same number of tabs appears in every row of the vector
stopifnot(DAT_Merged_Tab %>% str_count("\t") %>% unique() %>% length() == 1)
stopifnot(DAT_Merged_Tab %>% str_count("\t") %>% unique() == 58)



# Write this vector to a file
write.table(DAT_Merged_Tab,
            paste0("ProcessedData/Dat_PRMS_Final_End_Date_", EndDate$date, ".dat"),
            sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)



# Remove variables from the environment
remove(DAT_Initial, DAT_Merged, DAT_Predictions, 
       DAT_Merged_Tab, DAT_Metadata,
       Meteorological, CIMIS, NOAA, RAWS,
       dateSeq,
       getGIS, getXLSX, makeSharePointPath)



print("Dat PRMS script has finished running!")
