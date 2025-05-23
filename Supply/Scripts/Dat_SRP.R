# Save the SRP_Dat file for a specific month to the ProcessedData folder with a timestamp. The timestamp is the EndDate;
# EndDate is the last day of the observed data range. 


# Start Timer
# Start timer
start_time <- Sys.time()

# Load libraries and custom functions----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function
library(readxl) #for read_xlsx function

# Rely on the shared functions from the Demand and Supply scripts
source("../Supply/Scripts/Shared_Functions_Supply.R")
source("../Demand/Scripts/Shared_Functions_Demand.R")

# Import the precursor files----

# Import Pre-CWY (CWY = Current Water Year) SRP CSV file
SRP_Blueprints_Path = makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints\\")

PreCWY_SRP = read.csv(file = paste0(SRP_Blueprints_Path, "DAT_SRP_1947_to_WY2024.csv")) %>%
  mutate(Date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"))

#Convert Date field from character to date format 
PreCWY_SRP$Date = as.Date(PreCWY_SRP$Date, format = "%Y-%m-%d")

# Import SPI Forecast SRP CSV file
SPI_Forecast_SRP = read.csv(paste0(SRP_Blueprints_Path, "SPI_SRP_WY2025.csv")) %>%
  mutate(Date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"))

# Convert 1st 6 columns to integer data type to match PreCWY_SRP
SPI_Forecast_SRP = SPI_Forecast_SRP %>% 
  mutate_at(
    .vars = vars(1:6), #selects the 1st 6 columns
    .funs = as.integer #converts the selected columns to integers
  )

# Convert Date field from character to date format
SPI_Forecast_SRP$Date = as.Date(SPI_Forecast_SRP$Date, format = "%Y-%m-%d")

# Import Processed SRP data
SRP_Processed = read.csv(file = "ProcessedData/SRP_Processed.csv")

#Rename the columns in SRP_Processed
SRP_Processed = SRP_Processed %>% rename(
  "precip01" = "CIMIS_083_ppt",
  "tmin01" = "CIMIS_083_tmin",
  "tmax01" = "CIMIS_083_tmax",
  "precip02" = "CIMIS_103_ppt",
  "tmin02" =  "CIMIS_103_tmin",
  "tmax02" = "CIMIS_103_tmax",
)

# Convert Date field from character to date format
SRP_Processed$Date = as.Date(SRP_Processed$Date, format = "%Y-%m-%d")

# Rearrange SRP_Processed column order to match the other 2 dataframes
SRP_Processed <- SRP_Processed %>%
  select(Date, precip01, precip02, tmax01, tmax02, tmin01, tmin02)

# Add the year, month, day, hour, min, sec columns as the first 6 columns in SRP_Processed
SRP_Processed <- SRP_Processed %>%
  mutate(year = as.integer(format(Date, "%Y")),
         month = as.integer(format(Date, "%m")),
         day = as.integer(format(Date, "%d")),
         hour =as.integer(0),
         min = as.integer(0),
         sec = as.integer(0)) %>%
  select(year, month, day, hour, min, sec, everything())

# Merge the 3 datasets----
## Error Check for PreCWY_SRP and SRP_Preprocessed----
# Perform an error check to ensure that no rows from SRP_Processed overlap with PreCWY_SRP
if (PreCWY_SRP %>% filter(Date %in% SRP_Processed$Date) %>% nrow() > 0) {
  
  print(c("The scraped SRP meteorological dataset contains rows for dates that appear in the PreCWY_SRP dat file.", 
          "The data for those dates in 'PreCWY_SRP' will be replaced with the data in the meteorological dataset."))
  
  # Remove those rows from 'PreCWY_SRP'
  PreCWY_SRP <- PreCWY_SRP %>%
    filter(!(Date %in% SRP_Processed$Date))

  # Check for continuity of dataset--the earliest date in SRP_Processed should be 1 day AFTER the
   # latest date in PreCWY_SRP
} else if (as.Date(min(SRP_Processed$Date), "%Y-%m-%d") != as.Date(max(PreCWY_SRP$Date) + 1, "%Y-%m-%d")) {
  print("The earliest date in SRP_Processed is not exactly 1 day after the latest date in PreCWY_SRP.")
  
} else {
print("No errors exist because SRP_Preprocessed and PreCWY_SRP have no 
      overlapping records AND no date gap.")
}
  
## Merge PreCWY_SRP and SRP_Pre-processed and arrange in ascending order by Date-----
Dat_SRP_Merged = bind_rows(PreCWY_SRP, SRP_Processed) %>%
  arrange(Date)

## FLAGGING BLOCK -----
if (includeFlagging) {

### Identify negative precipitation values---

# Identify precipitation columns
precip_columns <- names(Dat_SRP_Merged)[grepl("precip", names(Dat_SRP_Merged))]

# Create negative precipitation flag columns
Dat_SRP_Merged_Precip_Flags = Dat_SRP_Merged %>%
  mutate(across(all_of(precip_columns), ~. <0, .names = "{.col}_flag"))
    # across applies the mutate function to multiple columns; relies on helper
    # functions like all_of(), any_of(), starts_with()
    # across takes 3 arguments, must be defined and run inside mutate, else will fail
        # 1) dataset to apply it to, all_of (precip_columns)
        # 2) conditional statement, ~. <0, the tilde allows you to create an
        # anonymous function that's not defined explicity
        # 3) column names to produce, ".names argument)

    # Compute row sums of flag columns and add as a new column
    Dat_SRP_Merged_Precip_Flags <- Dat_SRP_Merged_Precip_Flags %>%
            mutate(row_sums = select(., ends_with("_flag")) %>% 
            rowSums())
  
  # Filter Dat_SRP_Merged_Precip_Flags based on row_sums exceeding 0
  negative_precip_dates <- Dat_SRP_Merged_Precip_Flags %>%
    filter(row_sums > 0)

##  Identify extreme temperature values ----
  
  ### 1) TMIN > TMAX----
  
  # Identify temperature columns
  temperature_columns <- names(Dat_SRP_Merged)[grepl("tm", names(Dat_SRP_Merged))]
  
  Dat_Merged_SRP_Temp <- Dat_SRP_Merged[, c("Date", temperature_columns)]
  
  #Create TDIFF columns
  for (i in 1:2){
    tmax_col <- temperature_columns[i]
    tmin_col <- temperature_columns[i+2]
    tdiff_col <- paste0("TDIFF", i)
    Dat_Merged_SRP_Temp[[tdiff_col]] = Dat_Merged_SRP_Temp[[tmax_col]] - 
      Dat_Merged_SRP_Temp[[tmin_col]]
  }
  
  # Filter rows where any TDIFF is negative
  negative_tdiff_rows <- rowSums(Dat_Merged_SRP_Temp[, paste0("TDIFF", 1:2)] < 0) > 0
  tmin_exceedance_dates <- Dat_Merged_SRP_Temp[negative_tdiff_rows,]
  
  # Print or use tmin_exceedance_dates as needed
  print(tmin_exceedance_dates) # returns 0 records on 6/27/2024
  
  ### 2) TMIN < average(TMIN) - 5 * standard deviations AND ----
  # 3) TMIN > average(TMIN) + 5 * standard deviations
  
  Dat_Merged_SRP_Temp <- Dat_SRP_Merged[, c("Date", temperature_columns)]
  
  # Initialize a vector to store flag column names
  flag_columns <- character(length = 2) # Assuming 2 TMIN columns
  
  # Loop through each TMIN column
  for (i in 1:2) {
    tmax_col <- temperature_columns[i]
    tmin_col <- temperature_columns[i+2]
    
    # Calculate average and standard deviation for TMIN_i
    avg_tmin_i <- mean(Dat_SRP_Merged[[tmin_col]], na.rm = TRUE)
    sd_tmin_i <- sd(Dat_Merged_SRP_Temp[[tmin_col]], na.rm = TRUE)
    
    # Create flag column names
    flag_tmin_i_lt <- paste0("flag_", tmin_col, "_lt")
    flag_tmin_i_gt <- paste0("flag_", tmin_col," _gt")
    flag_columns[i] <- flag_tmin_i_lt  # Store one flag column name for each TMIN column   
    
    # Create flag columns where TMIN_i < avg(TMIN_i) - 5 * sd(TMIN_i)
    Dat_Merged_SRP_Temp[[flag_tmin_i_lt]] <- Dat_Merged_SRP_Temp[[tmin_col]] < 
                                              avg_tmin_i - 5 * sd_tmin_i
    
    # Create flag columns where TMIN_i > avg(TMIN_i) + 5 * sd(TMIN_i)
    Dat_Merged_SRP_Temp[[flag_tmin_i_gt]] <- Dat_Merged_SRP_Temp[[tmin_col]] > 
                                              avg_tmin_i + 5 * sd_tmin_i
  }
  
  # Filter to rows where any flag column is TRUE
  tmin_absurd <- Dat_Merged_SRP_Temp %>%
    filter(rowSums(select(., starts_with("flag"))) > 0 )
  
  #Print or use tmin_absurd as needed
  print(tmin_absurd) # REturns 0 records on 6/27/2024
  
  ### 4) TMAX < average(TMAX) - 5 * standard deviations----
  # 5) TMAX > average(TMAX) + 5 * standard deviations
  
  # Assuming temperature_columns contains both TMAX and TMIN columns
  Dat_Merged_SRP_Temp <- Dat_SRP_Merged[, c("Date", temperature_columns)]
  
  # Initialize a vector to store flag column names
  flag_columns <- character(length = 2)  # Assuming 2 TMAX columns
  
  # Loop through each TMAX column
  for (i in 1:2) {
    tmax_col <- temperature_columns[i]  # Adjust the index to select TMAX columns
    
    # Calculate average and standard deviation for TMAX_i
    avg_tmax_i <- mean(Dat_Merged_SRP_Temp[[tmax_col]], na.rm = TRUE)
    sd_tmax_i <- sd(Dat_Merged_SRP_Temp[[tmax_col]], na.rm = TRUE)
    
    # Create flag column names
    flag_tmax_i_lt <- paste0("flag_", tmax_col, "_lt")
    flag_tmax_i_gt <- paste0("flag_", tmax_col, "_gt")
    flag_columns[i] <- flag_tmax_i_lt  # Store one flag column name for each TMAX column
    
    # Create flag columns where TMAX_i < avg(TMAX_i) - 5 * sd(TMAX_i)
    Dat_Merged_SRP_Temp[[flag_tmax_i_lt]] <- Dat_Merged_SRP_Temp[[tmax_col]] < avg_tmax_i - 5 * sd_tmax_i
    
    # Create flag columns where TMAX_i > avg(TMAX_i) + 5 * sd(TMAX_i)
    Dat_Merged_SRP_Temp[[flag_tmax_i_gt]] <- Dat_Merged_SRP_Temp[[tmax_col]] > avg_tmax_i + 5 * sd_tmax_i
  }
  
  # Filter to rows where any flag column is TRUE
  tmax_absurd <- Dat_Merged_SRP_Temp %>%
    filter(rowSums(select(., starts_with("flag"))) > 0) 
  
  # Print or use tmax_absurd as needed
  print(tmax_absurd) # returns 0 records on 6/27/2024
  
  ### Export QAQC Flags to Excel spreadsheet----
    library(openxlsx)
    spreadsheet_name <- paste0("Dat_SRP_QAQC_Flags_", Sys.Date(), ".xlsx")
    folder_path <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints") 
    
    file_path = file.path(folder_path, spreadsheet_name)
    print(file_path)
    
    # Create a new workbook
    wb = createWorkbook()
    
    # Add each dataframe as a sheet
    addWorksheet(wb, "Negative Precipitation")
    writeData(wb, sheet = "Negative Precipitation", x= negative_precip_dates)
    
    addWorksheet(wb,"TMIN Exceedance")
    writeData(wb, sheet = "TMIN Exceedance", x = tmin_exceedance_dates)
    
    addWorksheet(wb,"TMAX absurd")
    writeData(wb, sheet = "TMAX absurd", x = tmax_absurd)
    
    addWorksheet(wb,"TMIN absurd")
    writeData(wb, sheet = "TMIN absurd", x = tmin_absurd)
    
    # Save the workbook
    saveWorkbook(wb, file = file_path, overwrite = TRUE)
}

# Error check for Dat_SRP_Merged and SPI_Forecast_SRP----
if (SPI_Forecast_SRP %>% filter(Date  %in% Dat_SRP_Merged$Date) %>% nrow() > 0) {
  
  print(c("The scraped SRP meteorological dataset contains rows for dates that appear 
          in the SPI_Forecast_SRP dat file."))

  # Remove overlapping records from SPI_Forecast_SRP
  SPI_Forecast_SRP = SPI_Forecast_SRP %>%
    filter(!(Date %in% Dat_SRP_Merged$Date))
  
# Check for continuity of dataset--the latest date in SRP_Processed should be 1 day BEFORE the 
  # earliest date in SPI_Forecast_SRP.
  
} else if (as.Date(max(Dat_SRP_Merged$Date),"%Y-%m-%d") != as.Date(min(SPI_Forecast_SRP$Date) -1, 
                                                                   "%Y-%m-%d")){
  print(c("ERROR! Review your data! The latest date in Dat_SRP_Merged is not exactly 1 day before the 
  earliest date in SPI_Forecast_SRP."))

} else { 
  print(c("No errors exist because SPI_Forecast_SRP and Dat_SRP_Merged have no overlapping records and no date gap"))
    
}

## Merge Dat_SRP_Merged and SPI_SRP_Forecast and arrange in ascending order by Date
Dat_SRP_Merged = bind_rows(Dat_SRP_Merged, SPI_Forecast_SRP) %>%
  arrange(Date)


# Final Error Checks ----
# Look for any NA values
stopifnot(!anyNA(Dat_SRP_Merged))

#Look for any rows containing -99 (indicates missing data)
rows_with_minus_99 <- apply(Dat_SRP_Merged == -99, 1, any)
rows_with_minus_99_values <- Dat_SRP_Merged[rows_with_minus_99, ]
print(rows_with_minus_99_values)



# Water Year Forecast data 2----

# This procedure will only be used if precipitation data from October 
# to February is available for the current water year

# Based on a previously generated linear regression model, the most
# similar water year to the current water year (WY2024) was identified

# That year's data will be substituted in for the remainder of WY2024


# Check that 'EndDate' is within the proper bounds for this procedure
if (EndDate$date >= paste0(EndDate$year, "-03-01") & 
    EndDate$date < paste0(EndDate$year, "-09-30")) {
  
  
  
  # This water year's data will be substituted into the remaining dates for the modeled water year
  waterYearSub <- 2020
  
  
  
  warning(paste0("Substituting data from ", EndDate$date + 1, " to ", EndDate$year, "-09-30 ",
                 "with corresponding values from ", waterYearSub))
  
  # This is a manual assignment
  # Based on the regression model generated on 5/17/2024,
  # data from WY2020 should be substituted into the remaining WY2024 range
  Dat_SRP_Merged[Dat_SRP_Merged$Date > EndDate$date & 
                   Dat_SRP_Merged$Date <= paste0(EndDate$year, "-09-30"), ][base::setdiff(names(Dat_SRP_Merged), c("year", "month", "day", "Date"))] <- Dat_SRP_Merged[Dat_SRP_Merged$Date <= paste0(waterYearSub, "-09-30") &
                                                                                                                                                                         Dat_SRP_Merged$Date > paste0(waterYearSub, "-", EndDate$month, "-", EndDate$day), ][base::setdiff(names(Dat_SRP_Merged), c("year", "month", "day", "Date"))]
  
}


# Dat Formatting Check----
# Ensure all numeric columns have at least 4 decimal places
Dat_SRP_Merged[, c("precip01", "precip02", "tmax01", "tmax02", "tmin01", "tmin02")] <- 
  format(Dat_SRP_Merged[, c("precip01", "precip02", "tmax01", "tmax02", "tmin01", "tmin02")], nsmall = 4)

#Drop the Date column from Dat_SRP_Merged
Dat_SRP_Merged$Date = NULL

# Save the spacing between columns required by the SRP Dat file into a vector
spacing_vector <- c(" ", "  ", "  ", "  ", " ", "    ", "     ", "    ", "    ", "    ", "    ", "    ")

# Get the first 12 column indices of Dat_SRP_Merged
all_column_indices <- seq_along(Dat_SRP_Merged)[1:12]

# Create a list with concatenated columns
concatenated_columns <- lapply(all_column_indices, function(i) {
  paste(Dat_SRP_Merged[[i]], spacing_vector[i])
})

# Rename Dat_SRP_Merged to Dat_SRP_Final
Dat_SRP_Final = Dat_SRP_Merged

# Unite all the columns into a single column
Dat_SRP_Final$Concatenated_Column <- do.call(paste, c(concatenated_columns, sep = ""))

#Remove all columns except for Concatenated_Column
Dat_SRP_Final = Dat_SRP_Final[, "Concatenated_Column"] %>% as.data.frame()
names(Dat_SRP_Final) = "Dat_SRP_Final"

# Combine Dat_SRP_Final with Dat_SRP_Heading
Dat_SRP_Heading = read.csv(file = paste0(SRP_Blueprints_Path, "Dat_SRP_Heading.dat"),
                           header = F)

#Unite all the columns in Dat_SRP_Heading into a single column
Dat_SRP_Heading = unite(Dat_SRP_Heading, Concatenated_Column, V1, V2, V3, sep = "")

# Rename the single column in Dat_SRP_Heading to "Dat_SRP_Final"
names(Dat_SRP_Heading) = "Dat_SRP_Final"
Dat_SRP_Final = rbind(Dat_SRP_Heading, Dat_SRP_Final)

# Export Dat_SRP_Final to the ProcessedData folder 
  # Include the final observed date, EndDate as the suffix to the file name

write.table(x = Dat_SRP_Final,
            file = paste0("ProcessedData/Dat_SRP_", modeler_name, "_Observed_EndDate_", EndDate$date, ".dat"),
            sep = "/t", row.names =  F, quote =  F, col.names = F)


# Calculate Run Time and Print Completion Statement

# End timer
  end_time <- Sys.time()
  
  # Calculate and print the duration
  duration <- end_time - start_time
  cat("The 'Dat_SRP.R' script has finished running!\nRun-time:", duration, "seconds", "\n")
