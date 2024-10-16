# This script adjusts the data downloaded by 'NOAA_API_Scraper.R' to be compatible with the DAT format
# It also adds missing and forecasted data to the dataset

require(tidyverse)
require(readxl)


cat("Starting 'NOAA_Processor.R'...\n")


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
  # Right now, it will only contain a column of dates
  # Eventually, the number of columns will equal the number of rows in 'stationDF' 
  # (plus one for the "Date" column)
  # The number of rows is equal to the number of unique dates in 'noaaDF'
  newDF <- noaaDF %>%
    select(DATE) %>%
    unique() %>% arrange() %>%
    rename(Date = DATE)
  
  
  
  # Iterate through the NOAA stations
  for (i in 1:nrow(stationDF)) {
    
    # Filter 'noaaDF' to only data from this station
    subsetDF <- noaaDF %>%
      filter(STATION == stationDF$`Full Station ID`[i])
    
    
    
    # Then, based on the station data type (precipitation, tmax, or tmin),
    # take a subset of columns in 'subsetDF'
    # Keep only that field and the date column (renamed as "Date" to match 'newDF')
    if (grepl("PRECIP", stationDF$`DAT_File Field Name`[i])) {
      
      subsetDF <- subsetDF %>%
        select(DATE, PRCP) %>%
        rename(!! stationDF$`DAT_File Field Name`[i] := PRCP,
               Date = DATE) %>%
        unique()
      
    } else if (grepl("TMAX", stationDF$`DAT_File Field Name`[i])) {
      
      subsetDF <- subsetDF %>%
        select(DATE, TMAX) %>%
        rename(!! stationDF$`DAT_File Field Name`[i] := TMAX,
               Date = DATE) %>%
        unique()
      
    } else if (grepl("TMIN", stationDF$`DAT_File Field Name`[i])) {
      
      subsetDF <- subsetDF %>%
        select(DATE, TMIN) %>%
        rename(!! stationDF$`DAT_File Field Name`[i] := TMIN,
               Date = DATE) %>%
        unique()
      
    } else {
      
      stop(paste0("Unknown DAT field name: ", stationDF$`DAT_File Field Name`[i]))
      
    }
    
    
    
    # Add that station's column to 'newDF' through a join with 'subsetDF'
    newDF <- newDF %>%
      left_join(subsetDF, by = "Date", relationship = "one-to-one")
    
  }

  
  
  # Another important step is to convert the units of the dataset
  # The data was downloaded in standard units, but metric units should be used
  # Convert precipitation from "in" to "mm" (25.4 mm per in)
  # Convert temperature from "Fahrenheit" to "Celsius" (deg-C = (deg-F - 32) * 5/9)
  newDF[, grep("PRECIP", names(newDF))] <- newDF[, grep("PRECIP", names(newDF))] * 25.4
  newDF[, grep("_T", names(newDF))] <- (newDF[, grep("_T", names(newDF))] - 32) * 5/9
  
  
  
  # After that, sort the columns in 'newDF' 
  # (with "Date" as the first column)
  newDF <- newDF %>%
    select(sort(colnames(newDF))) %>%
    relocate(Date) %>%
    arrange(Date)
  
  
  
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
  
  # If the downloaded NOAA dataset does not have data up to 'EndDate', later scripts will fail
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
  
  
  
  print(paste0("Replacing ", sum(noaaDF == -999, na.rm = TRUE), " elements (out of ",
               nrow(noaaDF) * ncol(noaaDF), " records) with PRISM data"))
  
  
  
  # Read in "Prism_Processed.csv"
  prismDF <- read_csv("ProcessedData/Prism_Processed.csv", show_col_types = FALSE)
  
  
  
  # # Make sure both data frames are properly sorted by date
  # noaaDF <- noaaDF %>%
  #   arrange(Date)
  # 
  # 
  # 
  # prismDF <- prismDF %>%
  #   arrange(Date)
  # 
  # 
  # 
  # # Check each column of 'noaaDF'
  # # (The first column is "Date", so that can be skipped)
  # for (j in 2:ncol(noaaDF)) {
  #   
  #   # Get the dates where this iteration's column is -999
  #   emptyDates <- noaaDF %>%
  #     filter(!! sym(names(noaaDF)[j]) == -999) %>%
  #     select(Date)
  #   
  #   
  #   
  #   # If there are no missing entries, skip the rest of this loop
  #   if (nrow(emptyDates) == 0) {
  #     next
  #   }
  #   
  #   
  #   
  #   # Otherwise, get the PRISM data that corresponds to this station and these dates
  #   prismSubset <- prismDF %>%
  #     filter(Date %in% emptyDates$Date) %>%
  #     select(Date,
  #            names(noaaDF)[j] %>%
  #              str_extract("_.+$") %>%
  #              paste0(., "$") %>%
  #              grep(names(prismDF), value = TRUE))
  #   
  #   
  #   
  #   # It should only contain two columns of data
  #   stopifnot(ncol(prismSubset) == 2)
  #   
  #   
  #   
  #   # Substitute the missing data in this column of 'noaaDF' with 'prismSubset'
  #   # This code works because both data frames are sorted
  #   noaaDF[noaaDF$Date %in% emptyDates$Date, j] <- prismSubset[prismSubset$Date %in% emptyDates$Date, 2]
  #   
  # }
  
  
  
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
