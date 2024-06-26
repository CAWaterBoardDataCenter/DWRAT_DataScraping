#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function

# Rely on the shared functions from the Demand scripts
source("../Demand/Scripts/Shared_Functions_Demand.R")



# Use the Dat component files located on SharePoint


# Metadata that appears at the beginning of the file
DAT_Metadata <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Metadata.dat") %>%
  read_lines()



# SBI predicted values for the rest of the water year
# (Used when October-February data for the water year is not yet available)
DAT_Predictions <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Forecast_Values.dat") %>%
  read_delim("\t", col_names = FALSE, show_col_types = FALSE) %>%
  set_names(makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Headers.txt") %>%
              read_lines())



# Read in the DAT file that contains data from 1990 up to the current water year
DAT_Initial <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_PRMS_1990_to_WY2023.dat") %>%
  read_delim("\t", col_names = FALSE, show_col_types = FALSE) %>%
  set_names(makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Headers.txt") %>%
              read_lines())



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
          file = paste0("ProcessedData/Meteorological_", EndDate$date, ".csv" ))



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

# QAQC steps----

## Identify negative precipitation values----

# Identify precipitation columns
precip_columns <- names(DAT_Merged)[grepl("PRECIP", names(DAT_Merged))]

# Create negative precipitation flag columns
Dat_Merged_Precip_Flags <- DAT_Merged %>%
  mutate(across(all_of(precip_columns), ~. < 0, .names = "{.col}_flag"))
    # across applies the mutate function to multiple columns; relies on helper
    # functions like all_of(), any_of(), starts_with()
    # across takes 3 arguments, must be defined and run inside mutate, else will fail
        # 1) dataset to apply it to, all_of(precip_columns)
        # 2) conditional statement, ~. < 0; the tilde allows you to create an 
        # an anonymous that's not defined explicitly
        # 3) column names to produces, ".names argument)

# Compute row sums of flag columns
row_sums <- Dat_Merged_Precip_Flags %>%
  select(ends_with("_flag")) %>%
  rowSums()


# Filter Dat_Merged_Flags based on row sums exceeding 0
negative_precip_dates <- Dat_Merged_Precip_Flags %>%
  filter(row_sums > 0)

print(negative_precip_dates) # returns 0 records on 6/25/2024

## Identify extreme temperature values----
  
  # 1) TMIN > TMAX

# Identify temperature columns
temperature_columns <- names(DAT_Merged)[grepl("TM", names(DAT_Merged))]

Dat_Merged_Temp <- DAT_Merged[, c("Date", temperature_columns)]

# Create TDIFF columns
for (i in 1:8) {
  tmax_col <- temperature_columns[i]
  tmin_col <- temperature_columns[i + 8]
  tdiff_col <- paste0("TDIFF", i)
  Dat_Merged_Temp[[tdiff_col]] = Dat_Merged_Temp[[tmax_col]] - Dat_Merged_Temp[[tmin_col]]
}

# Filter rows where any TDIFF is negative
negative_tdiff_rows <- rowSums(Dat_Merged_Temp[, paste0("TDIFF", 1:8)] < 0) > 0
tmin_exceedance_dates <- Dat_Merged_Temp[negative_tdiff_rows, ]

# Print or use tmin_exceedance_dates as needed
print(tmin_exceedance_dates)

  # 2) TMIN < average(TMIN) - 5 * standard deviations
  # 3) TMIN > average(TMIN) + 5 * standard deviations
  # 4) TMAX < average(TMAX) - 5 * standard deviations
  # 5) TMAX > average(TMAX) + 5 * standard deviations


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



# Water Year Forecast data 2----


# This procedure will only be used if precipitation data from October 
# to February is available for the current water year

# Based on a previously generated linear regression model, the most
# similar water year to the current water year (WY2024) was identified

# That year's data will be substituted in for the remainder of WY2024


# Unused Procedure:

# Using linear regression, develop a model that predicts the total
# precipitation for a water year using the October - February precipitation total

# Then, find the year whose total is most similar to the 
# predicted total for the current water year

# Substitute that chosen year's precipitation and temperature values for 
# the rest of the current water year

# (This procedure is repeated for each precipitation column)



# Check that 'EndDate' is within the proper bounds for this procedure
if (EndDate$date >= paste0(EndDate$year, "-03-01") & 
    EndDate$date < paste0(EndDate$year, "-09-30")) {
  
  
  # The commented-out code was meant to automate the process of 
  # identifying and substituting data from another WY
  # A manual selection was made instead at the end
  # However, this commented-out code should remain in case the procedure is later changed
  
  
  
  # Define a vector with the names of the precipitation columns in 'DAT_Merged'
  # precipNames <- names(DAT_Merged) %>%
  #   str_subset("_PRECIP")
  # 
  # 
  # 
  # # Iterate through each precipitation column
  # for (i in 1:length(precipNames)) {
  #   
  #   cat(paste0("Modeling ", precipNames[i], "...\n"))
  #   
  #   
  #   
  #   # Define a variable with monthly precipitation values
  #   monthlyDF <- DAT_Merged %>%
  #     mutate(WATER_YEAR = if_else(month < 10, Year, Year + 1)) %>%
  #     group_by(WATER_YEAR, month) %>%
  #     summarize(MONTHLY_PRECIP = sum(!!sym(precipNames[i])), .groups = "drop")
  #   
  #   
  #   
  #   # Calculate the October-to-February and WY total precipitation
  #   modelDF <- monthlyDF %>%
  #     group_by(WATER_YEAR) %>%
  #     summarize(OCT_TO_FEB_PRECIP = sum(MONTHLY_PRECIP[month > 9 | month < 3]),
  #               WY_PRECIP = sum(MONTHLY_PRECIP), .groups = "drop")
  #   
  #   
  #   
  #   # Randomly partition 'modelDF' into calibration and validation datasets
  #   # (Use a seed for reproducibility)
  #   set.seed(10)
  #   
  #   
  #   
  #   # Randomly assign water years to the calibration and validation datasets
  #   caliIndices <- sample(nrow(modelDF) - 1, round(2/3 * (nrow(modelDF) - 1)))
  #   valiIndices <- base::setdiff(1:(nrow(modelDF) - 1), caliIndices)
  #   
  #   
  #   
  #   # Error Check
  #   stopifnot(length(caliIndices) + length(valiIndices) + 1 == nrow(modelDF))
  #   
  #   
  #   
  #   # Define the datasets
  #   caliDF <- modelDF[caliIndices, ]
  #   valiDF <- modelDF[valiIndices, ]
  #   
  #   
  #   
  #   # Construct the regression model
  #   linModel <- lm(WY_PRECIP ~ OCT_TO_FEB_PRECIP, caliDF)
  #   
  #   
  #   
  #   # Calculate the R^2 value for the validation dataset
  #   valiDF <- valiDF %>%
  #     mutate(PREDICTED_VALUES = predict(linModel, valiDF),
  #            RESIDUALS = WY_PRECIP - PREDICTED_VALUES)
  #   
  #   
  #   
  #   # Calculate the components for the SSR and SST as well
  #   valiDF <- valiDF %>%
  #     mutate(SSR = (RESIDUALS)^2,
  #            SST = (WY_PRECIP - mean(valiDF$WY_PRECIP))^2)
  #   
  #   
  #   
  #   # After that, get the R^2 squared value for the validation dataset
  #   valiRSq <- 1 - sum(valiDF$SSR) / sum(valiDF$SST)
  #   
  #   
  #   
  #   # Next, find the most similar water year 
  #   # Get the difference between the predicted total for the current water
  #   # year and the actual total precipitation of the other water years
  #   # Choose the one with a minimal difference (but not the current water year)
  #   similarYear <- modelDF %>%
  #     mutate(PREDICTED_TOTAL = predict(linModel, modelDF)) %>%
  #     mutate(ERROR = abs(WY_PRECIP - PREDICTED_TOTAL[WATER_YEAR == last(WATER_YEAR)]) / PREDICTED_TOTAL[WATER_YEAR == last(WATER_YEAR)]) %>%
  #     filter(WATER_YEAR != last(WATER_YEAR)) %>%
  #     filter(ERROR == min(ERROR))
  #   
  #   
  #   
  #   # There is a small chance that more than one water year has the minimum "ERROR" value
  #   # However, only one year's data will be used
  #   if (nrow(similarYear) > 1) {
  #     
  #     cat(paste0("More than one water year (", 
  #                similarYear$WATER_YEAR %>% paste0(collapse = "; "),
  #                ") has a total precipitation ",
  #                "similar to the predicted total for the current water year\n",
  #                "The first one will be arbitrarily chosen\n"))
  #     
  #     
  #     similarYear <- similarYear[1, ]
  #     
  #   }
  #   
  #   
  #   
  #   # Output information about the fit and results to the console
  #   cat(sprintf("\nLinear Fit: y = %.3f * x + %.3f\nCalibration R^2 is %.3f\nValidation R^2 is %.3f\n\n", 
  #               linModel$coefficients[2], 
  #               linModel$coefficients[1],
  #               summary(linModel)$r.squared,
  #               valiRSq))
  #   
  #   cat(paste0("Most similar water year: ", similarYear$WATER_YEAR[1], "\n\n\n"))
  #   
  #   
  #   
  #   # For the current water year, substitute data for the rest of the 
  #   # water year (after 'EndDate' to September 30th)
  #   # Use data from the chosen similar water year
  #   DAT_Merged[DAT_Merged$Date > EndDate$date & 
  #                DAT_Merged$Date <= paste0(EndDate$year, "-09-30"), ][precipNames[i]] <- DAT_Merged[DAT_Merged$Date <= paste0(similarYear$WATER_YEAR, "-09-30") &
  #                DAT_Merged$Date > paste0(similarYear$WATER_YEAR, "-",
  #                                         EndDate$month, "-", EndDate$day), ][precipNames[i]]
  #   
  #   
  #   
  #   
  #   
  # }
  # 
  
  
  
  # This is a manual assignment
  # Based on the regression model generated on 5/17/2024,
  # data from WY2020 should be substituted into the remaining WY2024 range
  DAT_Merged[DAT_Merged$Date > EndDate$date & 
               DAT_Merged$Date <= paste0(EndDate$year, "-09-30"), ][base::setdiff(names(DAT_Merged), c("Year", "month", "day", "Date"))] <- DAT_Merged[DAT_Merged$Date <= "2020-09-30" &
                                                                                                    DAT_Merged$Date > paste0("2020-", EndDate$month, "-", EndDate$day), ][base::setdiff(names(DAT_Merged), c("Year", "month", "day", "Date"))]
  
}




# For each precipitation column, 
names(Meteorological)








# Round the numeric values in 'DAT_Merged'
# (Keeping at most one decimal place)
DAT_Merged <- DAT_Merged %>%
  mutate(across(where(is.numeric), ~ round(., 1)))



# Output the final DAT file----


# Temporarily write 'DAT_Merged' to a file
DAT_Merged %>%
  select(-Date) %>%
  write_delim(paste0("ProcessedData/Dat_PRMS_Observed_EndDate_", EndDate$date, ".dat"),
              delim = "\t", col_names = FALSE)



# Read back in this file
# Then, append 'DAT_Metadata' to the beginning
DAT_Merged_Tab <- c(DAT_Metadata,
  read_lines(paste0("ProcessedData/Dat_PRMS_Observed_EndDate_", EndDate$date, ".dat")))



# Double-check that the same number of tabs appears in every row of the vector
stopifnot(DAT_Merged_Tab %>% str_count("\t") %>% unique() %>% length() == 1)
stopifnot(DAT_Merged_Tab %>% str_count("\t") %>% unique() == 58)



# Write this vector to a file
write.table(DAT_Merged_Tab,
            paste0("ProcessedData/Dat_PRMS_Observed_EndDate_", EndDate$date, ".dat"),
            sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)



# Remove variables from the environment
remove(DAT_Initial, DAT_Merged, DAT_Predictions, 
       DAT_Merged_Tab, DAT_Metadata,
       Meteorological, CIMIS, NOAA, RAWS,
       dateSeq,
       getGIS, getXLSX, makeSharePointPath)



print("Dat PRMS script has finished running!")
