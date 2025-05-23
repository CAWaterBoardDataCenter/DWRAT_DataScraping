#Install and load libraries----

# Start timer
start_time <- Sys.time()

library(dplyr)
library(tidyverse)
library(lubridate) #for make_date function
library(data.table) #for fread function

# Rely on the shared functions from the Demand scripts
source("../Demand/Scripts/Shared_Functions_Demand.R")

# Safety Check for includeFlagging variable
if (!exists("includeFlagging")) {
  includeFlagging <- FALSE
}

if (!exists("includeRemediation")) {
  includeRemediation <- FALSE
}


# Use the Dat component files located on SharePoint


# Metadata that appears at the beginning of the file
DAT_Metadata <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Metadata.dat") %>%
  read_lines()



# Read in the DAT file that contains data from 1990 up to the current water year
DAT_Initial <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_PRMS_1990_to_WY2024.dat") %>%
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
  
  
  
  # Also remove any dates that appear after the dates in 'Meteorological'
  DAT_Initial <- DAT_Initial %>%
    filter(Date < min(Meteorological$Date))
  
}



# Once these preparations are complete, the two data frames can be merged
DAT_Merged <- DAT_Initial %>%
  bind_rows(Meteorological) %>%
  arrange(Date)

DAT_Merged <- DAT_Merged %>% relocate(Date, .after = 6)

# QAQC steps----

## FLAGGING BLOCK ----
if (includeFlagging) {
  ### Identify negative precipitation values----
  
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
  # an anonymous function that's not defined explicitly
  # 3) column names to produce, ".names argument)
  
  # Compute row sums of flag columns
  row_sums <- Dat_Merged_Precip_Flags %>%
    select(ends_with("_flag")) %>%
    rowSums()
  
  
  # Filter Dat_Merged_Flags based on row sums exceeding 0
  negative_precip_dates <- Dat_Merged_Precip_Flags %>%
    filter(row_sums > 0)
  
  #stopifnot(length(negative_precip_dates) == 0)
  print(negative_precip_dates) # returns 0 records on 6/25/2024
  
  ### Identify extreme temperature values----
  
  #### 1) TMIN > TMAX ----
  
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
  
  #### 2) TMIN < average(TMIN) - 5 * standard deviations AND ----
  # 3) TMIN > average(TMIN) + 5 * standard deviations
  
  Dat_Merged_Temp <- DAT_Merged[, c("Date", temperature_columns)]
  
  # Initialize a vector to store flag column names
  
  # Loop through each TMIN column
  for (i in 1:8) {
    tmin_col <- temperature_columns[i + 8]
    
    # Calculate average and standard deviation for TMIN_i
    avg_tmin_i <- mean(Dat_Merged_Temp[[tmin_col]], na.rm = TRUE)
    sd_tmin_i <- sd(Dat_Merged_Temp[[tmin_col]], na.rm = TRUE)
    
    # Create flag column names
    flag_tmin_i_lt <- paste0("flag_", tmin_col, "_lt")
    flag_tmin_i_gt <- paste0("flag_", tmin_col, "_gt")
    
    # Create flag columns where TMIN_i < avg(TMIN_i) - 3.5* sd(TMIN_i)
    Dat_Merged_Temp[[flag_tmin_i_lt]] <- Dat_Merged_Temp[[tmin_col]] < avg_tmin_i - 3.5 * sd_tmin_i
    
    # Create flag columns where TMIN_i > avg(TMIN_i) + 3.5 * sd(TMIN_i)
    Dat_Merged_Temp[[flag_tmin_i_gt]] <- Dat_Merged_Temp[[tmin_col]] > avg_tmin_i + 3.5 * sd_tmin_i
  }
  
  # Filter to rows where any flag column is TRUE
  tmin_absurd <- Dat_Merged_Temp %>%
    filter(rowSums(select(., starts_with("flag"))) > 0)
  
  # Print or use tmin_absurd as needed
  print(tmin_absurd) # Returns 1 record on 6/26/2024
  # Returns 37 records on 7/1/2024 for 3.5 sd
  
  
  
  #### 4) TMAX < average(TMAX) - 5 * standard deviations ----
  ### 5) TMAX > average(TMAX) + 5 * standard deviations
  # Assuming temperature_columns contains both TMAX and TMIN columns
  Dat_Merged_Temp <- DAT_Merged[, c("Date", temperature_columns)]
  
  
  # Loop through each TMAX column
  for (i in 1:8) {
    tmax_col <- temperature_columns[i]  # Adjust the index to select TMAX columns
    
    # Calculate average and standard deviation for TMAX_i
    avg_tmax_i <- mean(Dat_Merged_Temp[[tmax_col]], na.rm = TRUE)
    sd_tmax_i <- sd(Dat_Merged_Temp[[tmax_col]], na.rm = TRUE)
    
    # Create flag column names
    flag_tmax_i_lt <- paste0("flag_", tmax_col, "_lt")
    flag_tmax_i_gt <- paste0("flag_", tmax_col, "_gt")
    
    # Create flag columns where TMAX_i < avg(TMAX_i) - 3.5 * sd(TMAX_i)
    Dat_Merged_Temp[[flag_tmax_i_lt]] <- Dat_Merged_Temp[[tmax_col]] < avg_tmax_i - 3.5 * sd_tmax_i
    
    # Create flag columns where TMAX_i > avg(TMAX_i) + 3.5 * sd(TMAX_i)
    Dat_Merged_Temp[[flag_tmax_i_gt]] <- Dat_Merged_Temp[[tmax_col]] > avg_tmax_i + 3.5 * sd_tmax_i
  }
  
  # Filter to rows where any flag column is TRUE
  tmax_absurd <- Dat_Merged_Temp %>%
    filter(rowSums(select(., starts_with("flag"))) > 0)
  
  # Print or use tmax_absurd as needed
  print(tmax_absurd) #Returns 0 records on 6/26/2024 for sd * 5
  # Returns 3 records on 7/1/2024 for sd * 3.5
}

## REMEDIATION BLOCK ---- 
if(includeRemediation) {
  # Import PRISM data to replace absurd values
  # To be super conservative, set the start and end dates to match the full Dat PRMS timeframe
  Prism_Processed = read.csv("ProcessedData/Prism_Processed.csv")
  
  ### Negative Precipitation Records ----
  
  # Check if negative_precip_dates has any records
  if (nrow(negative_precip_dates) > 0) {
    # Convert the Date column to Date type if they are not already
    Prism_Processed$Date <- as.Date(Prism_Processed$Date, format = "%Y-%m-%d")
    negative_precip_dates$Date <- as.Date(negative_precip_dates$Date, format = "%Y-%m-%d")
    
    # Perform an inner join to combine the dataframes on the Date column
    precip_corrections <- inner_join(negative_precip_dates, Prism_Processed, by = "Date", suffix = c("_neg", ""))
    
    # Replace the precip columns using the precip_columns vector
    for (i in 1:length(precip_columns)) {
      precip_corrections[[precip_columns[i]]] <- precip_corrections[[paste0("PP_PRECIP", i)]]
    }
    
    # Remove unnecessary PP_PRECIP columns from the precip_corrections dataframe
    precip_corrections <- precip_corrections %>%
      select(-starts_with("PP_PRECIP")) %>%
      select(-starts_with("PT"))
    
    # Display the first few rows of the updated dataframe
    head(precip_corrections)
  } else {
    print("negative_precip_dates has no records--there are no negative precipitation values to correct!")
  }
  
  ### Tmin Exceedance Records ---- 
  if (nrow(tmin_exceedance_dates) > 0) {
    
    # Ensure that the Date fields have the same data type and format
    tmin_exceedance_dates$Date = as.Date(tmin_exceedance_dates$Date, format = "%Y-%m-%d")
    Prism_Processed$Date = as.Date(Prism_Processed$Date,  format = "%Y-%m-%d")
    
    # Perform an inner join to combine tmin_exceedance_date and Prism_Processed on the Date column
    tmin_exceedance_corrections <- inner_join(x = tmin_exceedance_dates, 
                                              y = Prism_Processed,
                                              by = "Date")
    
    # Whenever TDIFF_i < 0, replace TMAX_i and TMIN_i with PT_TMAX_i and PT_TMIN_i, respectively
    for (i in 1:8) {
      # Define column names dynamically
      tdiff_col <- paste0("TDIFF", i)
      tmax_col <- temperature_columns[i]
      tmin_col <- temperature_columns[i + 8]
      pt_tmax_col <- paste0("PT_TMAX", i)
      pt_tmin_col <- paste0("PT_TMIN", i)
      
      # Replace observed values with PRISM data where TDIFF < 0
      tmin_exceedance_corrections[[tmax_col]][tmin_exceedance_corrections[[tdiff_col]] < 0] <- tmin_exceedance_corrections[[pt_tmax_col]][tmin_exceedance_corrections[[tdiff_col]] < 0]
      tmin_exceedance_corrections[[tmin_col]][tmin_exceedance_corrections[[tdiff_col]] < 0] <- tmin_exceedance_corrections[[pt_tmin_col]][tmin_exceedance_corrections[[tdiff_col]] < 0]
    }
  } else {
    print("tmin_exceedance_dates has no records--there are no instances where tmin exceeds tmax for any station during the entire timeframe.")
    
    # Create an empty dataframe with the same structure as tmin_exceedance_corrections
    tmin_exceedance_corrections <- data.frame(Date = as.Date(character()), matrix(NA, nrow = 0, ncol = length(temperature_columns)))
    colnames(tmin_exceedance_corrections)[-1] <- temperature_columns
  }
  
  #### Correct Dat_PRMS----
  
  # Left Join DAT_Merged to Tmin_Exceedance_Corrections
  Dat_Merged_Update <- left_join (x = DAT_Merged,
                                  y = tmin_exceedance_corrections,
                                  by = "Date",
                                  suffix = c("", "_new")
  )
  
  # Replace values in the temperature columns using coalesce
  Dat_Merged_Update <- Dat_Merged_Update %>%
    mutate(across(all_of(temperature_columns),
                  ~  coalesce(get(paste0(cur_column(),"_new")),.)))
  
  # Keep only the 60 columns that originally existed in DAT_Initial
  Dat_Merged_Update = Dat_Merged_Update[,names(DAT_Initial)]
  
  ###  Tmin_absurd ----
  
  if (nrow(tmin_absurd) > 0) {
    
    # Ensure that the Date fields for tmin_absurd and Prism_Processed have the same type and format
    tmin_absurd$Date <- as.Date(tmin_absurd$Date, format = "%Y-%m-%d")
    Prism_Processed$Date <- as.Date(Prism_Processed$Date, format = "%Y-%m-%d")
    
    # Perform an inner join to combine tmin_absurd and Prism_Processed on the Date column
    tmin_absurd <- inner_join(
      x = tmin_absurd, 
      y = Prism_Processed,
      by = "Date"
    )
    
    # Define for loop for the 8 observed and PRISM TMIN stations
    for (i in 1:8) {
      # Define column names dynamically
      tmin_col <- temperature_columns[i + 8]
      tmax_col <- temperature_columns[i]
      pt_tmin_col <- paste0("PT_TMIN", i)
      pt_tmax_col <-paste0("PT_TMAX", i)
      
      # Replace tmin_absurd values with the corresponding PRISM data on the same date
      tmin_absurd[[tmin_col]] <- tmin_absurd[[pt_tmin_col]]
      tmin_absurd[[tmax_col]] <-tmin_absurd[[pt_tmax_col]]
    }
    
  } else {
    print("tmin_absurd has no records--there are no absurd minimum temperature for any station during the entire timeframe.")
  }
  
  #### Correct DAT_PRMS----
  
  # Left Join Dat_Merged_Update to tmin_absurd
  Dat_Merged_Update <- left_join (x = Dat_Merged_Update,
                                  y = tmin_absurd,
                                  by = "Date",
                                  suffix = c("", "_new"))
  
  # Replace values in the temperature columns using coalesce
  Dat_Merged_Update <- Dat_Merged_Update %>%
    mutate(across(all_of(temperature_columns),
                  ~  coalesce(get(paste0(cur_column(),"_new")),.)))
  
  # Restore original columns to Dat_Merged_Update
  Dat_Merged_Update = Dat_Merged_Update[, names(DAT_Initial)]
  
  ###  Tmax_absurd ----
  
  if (nrow(tmax_absurd) > 0) {
    
    # Ensure that the Date fields for tmin_absurd and Prism_Processed have the same type and format
    tmax_absurd$Date <- as.Date(tmax_absurd$Date, format = "%Y-%m-%d")
    Prism_Processed$Date <- as.Date(Prism_Processed$Date, format = "%Y-%m-%d")
    
    # Perform an inner join to combine tmin_absurd and Prism_Processed on the Date column
    tmax_absurd <- inner_join(
      x = tmax_absurd, 
      y = Prism_Processed,
      by = "Date"
    )
    
    # Define for loop for the 8 observed and PRISM TMAX stations
    for (i in 1:8) {
      # Define column names dynamically
      tmin_col <- temperature_columns[i + 8]
      tmax_col <- temperature_columns[i]
      pt_tmin_col <- paste0("PT_TMIN", i)
      pt_tmax_col <-paste0("PT_TMAX", i)
      
      # Replace tmax_absurd values with the corresponding PRISM data on the same date
      tmax_absurd[[tmin_col]] <- tmax_absurd[[pt_tmin_col]]
      tmax_absurd[[tmax_col]] <-tmax_absurd[[pt_tmax_col]]
    }
    
  } else {
    print("tmax_absurd has no records--there are no absurd maximum temperature for any station during the entire timeframe.")
  }
  
  #### Correct DAT_PRMS----
  
  # Left Join Dat_Merged_Update to tmax_absurd
  Dat_Merged_Update <- left_join (x = Dat_Merged_Update,
                                  y = tmax_absurd,
                                  by = "Date",
                                  suffix = c("", "_new"))
  
  # Replace values in the temperature columns using coalesce
  Dat_Merged_Update <- Dat_Merged_Update %>%
    mutate(across(all_of(temperature_columns),
                  ~  coalesce(get(paste0(cur_column(),"_new")),.)))
  
  # Restore original columns to Dat_Merged_Update
  Dat_Merged_Update = Dat_Merged_Update[, names(DAT_Initial)]
  
  Dat_Merged_Update <- Dat_Merged_Update %>%  relocate(Date, .after = 6)
  
  spreadsheet_name = paste0("Dat_PRMS_Remediation_", Sys.Date(), ".xlsx")
  folder_path <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints") 
  
  file_path = file.path(folder_path, spreadsheet_name)
  print(file_path)
  
  library(openxlsx)
  
  wb = createWorkbook()
  
  addWorksheet(wb, "Dat_PRMS_Remediated")
  
  writeData(wb, sheet = "Dat_PRMS_Remediated", x= Dat_Merged_Update)
  
  saveWorkbook(wb, file = file_path, overwrite = TRUE)
  
  ## Export QAQC Flags to Excel spreadsheet----
  library(openxlsx)
  
  # Define the full file_path for the spreadsheet
  spreadsheet_name = paste0("Dat_PRMS_QAQC_Flags_", Sys.Date(), ".xlsx")
  folder_path <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints") 
  
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



# Check for errors in DAT_Merged----

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



# Substitute Temperature with PRISM Data ####

# Substitute all temperature columns with corresponding PRISM data

# Read in the PRISM data
prismDF <- read_csv("ProcessedData/Prism_Processed.csv", show_col_types = FALSE)

# Make sure both 'prismDF' and 'DAT_Merged' are sorted by date
DAT_Merged <- DAT_Merged %>%
  arrange(Date)

prismDF <- prismDF %>%
  arrange(Date)


# Make sure every date in 'prismDF' appears in 'DAT_Merged'
# Also, there should be no repeats in either variable
stopifnot(sum(prismDF$Date %in% DAT_Merged$Date) == nrow(prismDF))
stopifnot(length(prismDF$Date) == length(unique(prismDF$Date)))
stopifnot(length(DAT_Merged$Date) == length(unique(DAT_Merged$Date)))

# Iterate through the columns in 'temperature_columns'
for (i in 1:length(temperature_columns)) {
  
  # Find the corresponding temperature column in 'prismDF'
  prismCol <- temperature_columns[i] %>%
    str_remove("^.+_") %>%
    paste0(., "$") %>%
    grep(names(prismDF), value = TRUE)
  
  
  
  stopifnot(length(prismCol) == 1)
  
  
  
  # Replace data in 'DAT_Merged' with data from this column of 'prismDF'
  DAT_Merged[[temperature_columns[i]]][DAT_Merged$Date %in% prismDF$Date] <- prismDF[[prismCol]]
  
}



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
remove(DAT_Initial, DAT_Merged, 
       DAT_Merged_Tab, DAT_Metadata,
       Meteorological, CIMIS, NOAA, RAWS,
       dateSeq,
       getGIS, getXLSX, makeSharePointPath)


# Calculate Run Time and Print Completion Statement

# End timer
end_time <- Sys.time()

# Calculate and print the duration
duration <- end_time - start_time
cat("The 'Dat_PRMS.R' script has finished running!\nRun-time:", duration, "seconds", "\n")



