# This script adjusts the data downloaded by 'NOAA_API_Scraper.R' to be compatible with the DAT format
# It also adds missing and forecasted data to the dataset

require(tidyverse)
require(readxl)


cat("Starting 'NOAA_API_Scraper.R'...\n")


mainProcedure <- function (StartDate, EndDate, includeForecast) {
  
  # There are three mains steps to perform in this script:
  # (1) Adjust the formatting of the NOAA CSV to mimic other DAT-related data tables
  # (2) Fill in missing entries with PRISM data
  # (3) Add forecasted data from CNRFC (depending on the value of 'includeForecast')
  
  
  
  # Step 1
  fileAdjustment()
  
  
  
  # Step 2
  prismFill(StartDate, EndDate)
  
  
  
  # Step 3
  if (includeForecast == TRUE) {
    cnrfcAdd()
  }
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



fileAdjustment <- function () {
  
  # Adjust the format of the output NOAA API CSV to match that of 
  # "RAWS_Processed.csv" and other files used in the DAT process
  
  
  
  # First, get the filename for the NOAA CSV (downloaded by "NOAA_API_Scraper.R")
  noaaPath <- "WebData/NOAA_API_Data.csv"
  
  
  
  # First read in the NOAA CSV
  noaaDF <- read_csv(noaaPath, show_col_types = FALSE)
  
  
  
  # In the RAWS format, stations would have distinct columns
  # (with separate columns for temperature/precipitation)
  # There is only one row per date in that format
  
  # Currently, 'noaaDF' contains distinct rows for different pairs of stations and dates
  # Stations' precipitation and temperature data are stored in separate columns 
  
  
  
  # In the RAWS format, different stations' columns are identified with a DAT field name
  # The corresponding names for each station are located in "RR_PRMS_StationList (2023-09-05).xlsx"
  stationDF <- read_xlsx("InputData/RR_PRMS_StationList (2023-09-05).xlsx")
  
  
  
  # The first row of 'stationDF' actually contains the headers
  stationDF <- stationDF[-1, ] %>% 
    set_names(stationDF[1, ] %>% unlist() %>% as.vector()) %>%
    filter(Source == "NOAA")
  
  
  
  # Define a data frame for the alternative format
  # The number of columns is equal to the number of rows in 'stationDF' (plus one for the "Date" column)
  # The number of rows is equal to the number of unique dates in 'noaaDF'
  newDF <- matrix(NA_real_, ncol = nrow(stationDF) + 1, nrow = length(unique(noaaDF$DATE))) %>%
    as.data.frame() %>%
    set_names(c("Date", stationDF$`DAT_File Field Name`))
  
  
  
  # Assign the dates in 'noaaDF' to 'newDF'
  newDF$Date <- noaaDF$DATE %>%
    unique() %>% sort()
  
  
  
  # Iterate through 'noaaDF' and input its values into 'newDF'
  for (i in 1:nrow(noaaDF)) {
    
    # Each row contains precipitation, minimum temperature, and maximum temperature data
    # Assign each of the values in this row of 'noaaDF' to the corresponding column
    
    
    
    # Create a temporary variable containing rows in 'stationDF' whose Station ID
    # match the ID contained in this row of 'noaaDF'
    stationSubset <- stationDF %>%
      filter(`Full Station ID` == noaaDF$STATION[i])
    
    
    # Error Check
    stopifnot(nrow(stationSubset) > 0)
    
    
    
    # Then, get the row index
    # The row index will be the value in 'newDF' that corresponds to this row's date
    rowIndex <- which(newDF$Date == noaaDF$DATE[i])
    
    
    
    # Check if 'stationSubset' contains a precipitation column
    # (That means that precipitation data will be extracted from 'noaaDF')
    if (sum(grepl("PRECIP", stationSubset$Rank)) > 0) {
      
      # Find the location of this station's precipitation column in 'newDF'
      colIndex <- findIndex("PRECIP", noaaDF$STATION[i], stationDF, newDF)
      
      
      
      # Update 'newDF' with this iteration's precipitation value
      # The precipitation data was downloaded as inches
      # Therefore, it must also be converted into millimeters
      # (25.4 mm per in)
      newDF[rowIndex, colIndex] <- noaaDF$PRCP[i] * 25.4
      
    }
    
    
    
    # Following similar steps, check if 'stationSubset' contains a max temp column
    # (That means that tmax data will be extracted from 'noaaDF')
    if (sum(grepl("TMAX", stationSubset$Rank)) > 0) {
      
      # Find the location of this station's max temp column in 'newDF'
      colIndex <- findIndex("TMAX", noaaDF$STATION[i], stationDF, newDF)
      
      
      
      # Update 'newDF' with this iteration's maximum temperature value
      # The temperature data was downloaded as Fahrenheit
      # Therefore, it must also be converted into Celsius
      # deg-C = (deg-F - 32) * 5/9
      newDF[rowIndex, colIndex] <- (noaaDF$TMAX[i] - 32) * 5/9
      
    }
    
    
    
    # Finally, check if 'stationSubset' contains a min temp column
    # (That means that tmin data will be extracted from 'noaaDF')
    if (sum(grepl("TMIN", stationSubset$Rank)) > 0) {
      
      # Find the location of this station's min temp column in 'newDF'
      colIndex <- findIndex("TMIN", noaaDF$STATION[i], stationDF, newDF)
      
      
      
      # Update 'newDF' with this iteration's minimum temperature value
      # The temperature data was downloaded as Fahrenheit
      # Therefore, it must also be converted into Celsius
      # deg-C = (deg-F - 32) * 5/9
      newDF[rowIndex, colIndex] <- (noaaDF$TMIN[i] - 32) * 5/9
      
    }
    
  } # End of for loop through 'noaaDF'
  
  
  
  # After that, sort the columns in 'newDF' 
  # (but with "Date" as the first column)
  newDF <- newDF %>%
    select(sort(colnames(newDF))) %>%
    relocate(Date)
  
  
  
  # Finally, replace "NA" entries in 'newDF' with -999
  newDF[, 2:ncol(newDF)] <- newDF[, 2:ncol(newDF)] %>%
    map_dfc(~ replace_na(., -999))
  
  
  
  # Save this updated CSV to the "ProcessedData" folder
  # (Use 'noaaPath' as a base for the output file string)
  write_csv(x = newDF, file = "ProcessedData/NOAA_API_Processed.csv")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



findIndex <- function (colType, stationID, stationDF, newDF) {
  
  # This function returns a column index from 'newDF'
  # Using 'colType' and 'stationID' a partial column name is extracted from 'stationDF'
  # This partial name is turned into a full NOAA column name
  # After that, the column names of 'stationDF' are checked for that full name string
  
  
  # 'colType' should be either "PRECIP", "TMAX", or "TMIN"
  
  
  # Create the target column name using the extracted string from 'stationDF'
  # ('stationID' and 'colType' help located this string)
  colStr <- stationDF %>%
    filter(`Full Station ID` == stationID) %>%
    filter(grepl(colType, Rank)) %>%
    select(Rank) %>% unlist() %>% as.vector() %>%
    paste0("NOAA_", .)
  
  
  
  # Find the index of this column in 'newDF'
  colIndex <- which(names(newDF) == colStr)
  
  
  
  # Check that exactly one match was found
  stopifnot(length(colIndex) == 1)
  
  
  
  # Return this index
  return(colIndex)
  
}



prismFill <- function (StartDate, EndDate) {
  
  # If the downloaded NOAA dataset does not have date up to 'EndDate', later scripts will fail
  # Substitute missing entries with data from PRISM
  
  
  
  # Using 'StartDate' and 'EndDate', create a vector of dates
  # Each date in this vector is expected to appear in the NOAA CSV file
  dateVec <- seq(from = StartDate$date, to = EndDate$date, by = "day")
  
  
  
  # Read in the processed NOAA CSV next
  noaaDF <- read_csv("ProcessedData/NOAA_API_Processed.csv", show_col_types = FALSE)
  
  
  
  # Create a vector of missing dates in 'noaaDF'
  missingDates <- dateVec[!(dateVec %in% noaaDF$Date)]
  
  
  
  # If there are missing dates, add empty rows to 'noaaDF' for each date
  if (length(missingDates) > 0) {
    
    # Convert 'missingDates' into a data frame with column name "Date"
    # Bind it to 'noaaDF' (and then sort the data frame by date)
    noaaDF <- bind_rows(noaaDF,
                        data.frame(Date = missingDates)) %>%
      arrange(Date)
    
  }
  
  
  
  # The next step is to fill in missing values with PRISM data
  
  
  
  # Read in "Prism_Processed.csv"
  prismDF <- read_csv("ProcessedData/Prism_Processed.csv", show_col_types = FALSE)
  
  
  
  # Use a nested loop to check every entry in 'noaaDF'
  for (i in 1:nrow(noaaDF)) {
    
    for (j in 2:ncol(noaaDF)) {
      
      
      # Skip entries where 'noaaDF' is not NA and it's not -999
      if (!is.na(noaaDF[i, j]) && noaaDF[i, j] != -999) {
        next
      }
      
      
      
      # Find the corresponding column and row in 'prismDF'
      # Then, assign that value to 'noaaDF'
      
      
      
      # Extract from the column name of 'noaaDF' the variable identifier
      # (e.g., "TMIN1" or "PRECIP15")
      # Then, add a "$" to the end of that string
      # (In regexes, that means that a matching string ends there)
      varStr <- names(noaaDF)[j] %>% str_extract("_.+$") %>%
        paste0(., "$")
      
      
      
      # Find the matching column index in 'prismDF' using 'varStr'
      colIndex <- grep(varStr, names(prismDF))
      
      
      
      # The matching row index in 'prismDF' will be the one with 
      # the same date as row 'i' of 'noaaDF'
      rowIndex <- which(prismDF$Date == noaaDF$Date[i])
      
      
      
      # Check for issues before proceeding
      stopifnot(length(colIndex) == 1)
      stopifnot(length(rowIndex) == 1)
      
      
      
      # Update entry i, j of 'noaaDF' using 'prismDF'
      noaaDF[i, j] <- prismDF[rowIndex, colIndex]
      
    } # End of 'j' loop
    
  } # End of 'i' loop
  
  
  
  # Save the updated 'noaaDF'
  write_csv(noaaDF, "ProcessedData/NOAA_API_Processed.csv")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



cnrfcAdd <- function () {
  
  # Add forecast data from "CNRFC_Processed.csv" to the processed NOAA CSV
  
  
  # Read in both files
  noaaDF <- read_csv("ProcessedData/NOAA_API_Processed.csv", show_col_types = FALSE)
  
  
  
  cnrfcDF <- read_csv("ProcessedData/CNRFC_Processed.csv", show_col_types = FALSE)
  
  
  
  # Get a vector of dates in 'cnrfcDF' that are not in 'noaaDF'
  # These should be all forecasted (i.e., future) dates
  newDates <- cnrfcDF$Date[!(cnrfcDF$Date %in% noaaDF$Date)]
  
  
  
  # Add rows to 'noaaDF' for these future dates
  noaaDF <- bind_rows(noaaDF,
            data.frame(Date = newDates)) %>%
    arrange(Date)
  
  
  
  # Use a nested loop to check every entry in 'noaaDF'
  for (i in 1:nrow(noaaDF)) {
    
    for (j in 2:ncol(noaaDF)) {
      
      
      # Skip entries where 'noaaDF' is not NA
      if (!is.na(noaaDF[i, j])) {
        next
      }
      
      
      
      # Find the corresponding column and row in 'cnrfcDF'
      # Then, assign that value to 'noaaDF'
      
      
      
      # Extract from the column name of 'noaaDF' the variable identifier
      # (e.g., "TMIN1" or "PRECIP15")
      # Then, add a "_" to the end of that string and remove the initial "_"
      # (In 'cnrfcDF' this string without the underscore is at the start of the column name)
      varStr <- names(noaaDF)[j] %>% str_extract("_.+$") %>%
        paste0(., "_") %>%
        str_remove("^_")
      
      
      
      # Find the matching column index in 'cnrfcDF' using 'varStr'
      colIndex <- grep(varStr, names(cnrfcDF))
      
      
      
      # The matching row index in 'cnrfcDF' will be the one with 
      # the same date as row 'i' of 'noaaDF'
      rowIndex <- which(cnrfcDF$Date == noaaDF$Date[i])
      
      
      
      # Check for issues before proceeding
      stopifnot(length(colIndex) == 1)
      stopifnot(length(rowIndex) == 1)
      
      
      
      # Update entry i, j of 'noaaDF' using 'cnrfcDF'
      noaaDF[i, j] <- cnrfcDF[rowIndex, colIndex]
      
    } # End of 'j' loop
    
  } # End of 'i' loop
  
  
  
  # Save the updated 'noaaDF'
  write_csv(noaaDF, "ProcessedData/NOAA_API_Processed.csv")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



mainProcedure(StartDate, EndDate, includeForecast)


cat("Done!\n")


remove(mainProcedure, fileAdjustment, findIndex, prismFill, cnrfcAdd)
