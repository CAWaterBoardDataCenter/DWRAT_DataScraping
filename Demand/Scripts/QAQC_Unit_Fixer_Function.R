# Prepare a function that will be used in "Scripts/Priority_Date_Preprocessing.R"
# This function will update "Statistics_Final.csv" with corrections related to unit conversion errors


# Dependencies
require(tidyverse)
require(readxl)
require(openxlsx)
require(data.table)


# Functions
unitFixer <- function (inputDF) {
  
  # Given the water use report dataset ('inputDF'), perform corrections on specified values
  
  # Two spreadsheets will be used to update 'inputDF':
  #  (1) "InputData/Expected_Demand_Units_QAQC_[DATE].xlsx"
  #  (2) "InputData/Expected_Demand_Units_QAQC_Median_Based_[DATE].xlsx"
  # ("[DATE]" in these filenames should be the most recent copies available)
  
  
  
  # To assist with the analysis, ensure that 'inputDF' is sorted
  # (What mainly matters is that the months are properly ordered)
  inputDF <- inputDF %>%
    arrange(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE)
  
  
  
  # Read in those two spreadsheets
  unitsQAQC <- read_xlsx("InputData/Expected_Demand_Units_QAQC_20230921.xlsx", sheet = "Corrected Data")
  unitsQAQC_Med <- read_xlsx("InputData/Expected_Demand_Units_QAQC_Median_Based_20230922.xlsx", sheet = "Filtered Data") %>%
    rename(QAQC_Action_Taken = QAQC_Action)
  
  
  # Filter out entries where no action is required
  unitsQAQC <- unitsQAQC %>%
    filter(!grepl("^[Nn]one", QAQC_Action_Taken))
  
  unitsQAQC_Med <- unitsQAQC_Med %>%
    filter(!grepl("^[Nn]one", QAQC_Action_Taken))
  
  
  # In a separate function, iterate through 'unitsQAQC' and 'unitsQAQC_Med'
  # Then make changes to 'inputDF'
  inputDF <- iterateQAQC(inputDF, unitsQAQC)
  
  
  inputDF <- iterateQAQC(inputDF, unitsQAQC_Med)
  
  
  
  # Return 'inputDF' after these changes
  return(inputDF)
  
}



iterateQAQC <- function (inputDF, unitsQAQC) {
  
  
  
  
  # Iterate through the different actions specified in 'unitsQAQC'
  for (i in 1:nrow(unitsQAQC)) {
    
    
    
    # For this first issue, all values for this right and year will be set to 0
    if (unitsQAQC$QAQC_Action_Taken[i] == "Change monthly values to 0") {
    
      
      inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                inputDF$YEAR == unitsQAQC$YEAR[i] &
                inputDF$DIVERSION_TYPE %in% c("DIRECT", "STORAGE") &
                inputDF$AMOUNT > 0, ]$AMOUNT <- 0
      
      
      # For these notes, the actual units are "gallons"
      # They need to be converted into AF
    } else if (grepl("^Convert from gallons ", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Use another function to choose the actions that will be modified
      toConvert <- chooseUseType(unitsQAQC$QAQC_Action_Taken[i])
      
      
      
      # There are 325,851 gallons in 1 AF
      inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                inputDF$YEAR == unitsQAQC$YEAR[i] &
                inputDF$DIVERSION_TYPE %in% toConvert &
                inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                          inputDF$YEAR == unitsQAQC$YEAR[i] &
                                                          inputDF$DIVERSION_TYPE %in% toConvert &
                                                          inputDF$AMOUNT > 0, ]$AMOUNT / 325851
      
      # For these notes, the actual units are "gpd"
      # They need to be converted into AF
    } else if (grepl("^Convert from gpd ", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Use another function to choose the actions that will be modified
      toConvert <- chooseUseType(unitsQAQC$QAQC_Action_Taken[i])
      
      
      
      # There are 325,851 gallons in 1 AF and 365 days in a year
      inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                inputDF$YEAR == unitsQAQC$YEAR[i] &
                inputDF$DIVERSION_TYPE %in% toConvert &
                inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                          inputDF$YEAR == unitsQAQC$YEAR[i] &
                                                          inputDF$DIVERSION_TYPE %in% toConvert &
                                                          inputDF$AMOUNT > 0, ]$AMOUNT / 325851 * 365
      
      
      # For these notes, the actual units are "gpm"
      # They need to be converted into AF
    } else if (grepl("^Convert from gpm ", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Use another function to choose the actions that will be modified
      toConvert <- chooseUseType(unitsQAQC$QAQC_Action_Taken[i])
      
      
      
      # There are:
      #   325,851 gallons in 1 AF
      #   365 days in a year
      #   24 hours in a day
      #   60 minutes in an hour
      inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                inputDF$YEAR == unitsQAQC$YEAR[i] &
                inputDF$DIVERSION_TYPE %in% toConvert &
                inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                          inputDF$YEAR == unitsQAQC$YEAR[i] &
                                                          inputDF$DIVERSION_TYPE %in% toConvert &
                                                          inputDF$AMOUNT > 0, ]$AMOUNT / 325851 * 60 * 24 * 365
      
      
      # The next action involves dividing all values for a right and year by a number
    } else if (grepl("^Divide monthly reported values by [0-9]+$", unitsQAQC$QAQC_Action_Taken[i])) {
      
      # Extract the number to use in the division
      divNum <- unitsQAQC$QAQC_Action_Taken[i] %>%
        str_extract("[0-9]+$") %>% as.numeric()
      
      
      # Error Check
      # 'divNum' should not be NA, and it should not be 0
      stopifnot(!is.na(divNum) & divNum != 0)
      
      
      # Divide all non-zero "AMOUNT" values by 'divNum'
      inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                inputDF$YEAR == unitsQAQC$YEAR[i] &
                inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                          inputDF$YEAR == unitsQAQC$YEAR[i] &
                                                          inputDF$AMOUNT > 0, ]$AMOUNT / divNum
      
      
      # The next action is for multiplying a specific entry by a number
    } else if (grepl("^Multiply [ADFJMNOS][a-z]+ [0-9]{4} [DS][irectoag]+ by [0-9]+$", unitsQAQC$QAQC_Action_Taken[i])) {
      
      # Extract the number to use in the multiplication
      mulNum <- unitsQAQC$QAQC_Action_Taken[i] %>%
        str_extract("[0-9]+$") %>% as.numeric()
      
      
      # Error Check
      # 'mulNum' should not be NA
      stopifnot(!is.na(mulNum))
      
      
      # Similarly, get a month, year, and use type from 'unitsQAQC'
      recordData <- unitsQAQC$QAQC_Action_Taken[i] %>%
        str_remove("^Multiply ") %>%
        str_remove(" by [0-9]+$") %>%
        str_split("\\s+") %>% unlist()

      
      # Error Checks
      # The first element should be a month
      stopifnot(recordData[1] %in% month.name)
      
      
      # The second element should be a year (i.e., a four-digit number)
      stopifnot(grepl("^[0-9]{4}$", recordData[2]))
      
      
      # The third element should be either "Direct" or "Storage"
      stopifnot(recordData[3] %in% c("Direct", "Storage"))
      
      
      # Multiply the specified "AMOUNT" value by 'mulNum'
      inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                inputDF$YEAR == as.numeric(recordData[2]) &
                inputDF$MONTH == which(recordData[1] == month.name) &
                inputDF$DIVERSION_TYPE == toupper(recordData[3]), ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                                                        inputDF$YEAR == as.numeric(recordData[2]) &
                                                                                        inputDF$MONTH == which(recordData[1] == month.name) &
                                                                                        inputDF$DIVERSION_TYPE == toupper(recordData[3]), ]$AMOUNT * mulNum
      
      
      # Another possible QA/QC action is replacing a value or values with a specified number
    } else if (grepl("^Replace [ADFJMNOS][/A-Za-z]+ [0-9]{4} [DS][irectoag]+ with [0-9\\.]+", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # There might be multiple changes intended in this action
      # They would be separated by a semicolon and a space ("; ")
      actionVec <- unitsQAQC$QAQC_Action_Taken[i] %>% str_split("; ") %>% unlist()
      
      
      # Iterate through the different actions in 'actionVec'
      for (j in 1:length(actionVec)) {
        
        # Extract the month(s), year, diversion type, and value
        recordData <- actionVec[j] %>%
          str_remove("^Replace ") %>% str_remove("with ") %>%
          str_split("\\s+") %>% unlist()
        
        
        # Element 1 will be one or more months (separated by "/")
        recordMonth <- recordData[1] %>% str_split("/") %>% unlist()
          
        
        # Error Checks
        
        # All values in 'recordMonth' should be a month
        stopifnot(sum(recordMonth %in% month.name) == length(recordMonth))
        
        
        # The second element of 'recordData' should be a year (four-digit number)
        stopifnot(grepl("^[0-9]{4}$", recordData[2]))
        
        
        # The third element should be either "Direct" or "Storage"
        stopifnot(recordData[3] %in% c("Direct", "Storage"))

      
        # The fourth element should be a number
        stopifnot(!is.na(as.numeric(recordData[4])))
        
        
        # Update 'inputDF' accordingly (one month at a time)
        for (k in 1:length(recordMonth)) {
          
          inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                    inputDF$YEAR == as.numeric(recordData[2]) &
                    inputDF$MONTH == which(recordMonth[k] == month.name) &
                    inputDF$DIVERSION_TYPE == toupper(recordData[3]), ]$AMOUNT = as.numeric(recordData[4])
          
        } # End of loop k through 'recordMonth'
        
      } # End of loop j through 'actionVec'
      
      
    # Another action may require replacing all values with corresponding values from another year
    } else if (grepl("^Replace with [0-9]{4} monthly values$", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Extract the selected year from the action
      actionYear <- unitsQAQC$QAQC_Action_Taken[i] %>%
        str_extract("[0-9]{4}") %>% as.numeric()
      
      
      # If 'actionYear' is outside the data range of 'inputData', "water_use_report_extended.csv" will need to be read in
      if (actionYear < min(inputDF$YEAR)) {
        
        tempDF <- fread(file = "RawData/water_use_report_extended.csv",
                        select = c("APPLICATION_NUMBER","YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE")) %>%
          filter(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] & YEAR == actionYear) %>%
          arrange(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE)
        
      } else {
        
        tempDF <- inputDF
        
      }
      
      
      # Extract the desired rows from 'tempDF' and change the year to the one with problematic data
      newRows <- tempDF %>%
        filter(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                 YEAR == actionYear &
                 DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
        mutate(YEAR = unitsQAQC$YEAR[i], FREQUENCY = NA_integer_)
      
      
      # Remove the problematic year's data from 'inputDF' and append 'newRows' to 'inputDF'
      inputDF <- inputDF %>%
        filter(!(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                   YEAR == unitsQAQC$YEAR[i] &
                   DIVERSION_TYPE %in% c("DIRECT", "STORAGE")))
      
      
      # Append the new data to 'inputDF'
      # Then, sort it again
      inputDF <- inputDF %>%
        bind_rows(newRows) %>%
        arrange(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE)
      
      
      
      # Some actions require using a measurement spreadsheet for data
    } else if (unitsQAQC$QAQC_Action_Taken[i] == "Replace with measurement spreadsheet values") {
      
      
      # Use a separate function for this step
      inputDF <- inputDF %>% useMeasurementData(unitsQAQC[i, ])
      
    
      # Throw an error for any other action  
    } else {
      
      stop("No procedure has been specified for this action")
      
    }
    
  } # End of loop through 'unitsQAQC'
  
  
  
  # Return 'inputDF' after these changes
  return(inputDF)
  
}



chooseUseType <- function (action) {
  
  # Depending on the value specified in the parentheses at the end of the action,
  # either the DIRECT or STORAGE values (or both) will be converted
  
  
  if (grepl("\\(Direct\\)$", action)) {
    
    return("DIRECT")
    
  } else if (grepl("\\(Storage\\)$", action)) {
    
    return("STORAGE")
    
  } else if (grepl("\\(All\\)$", action)) {
    
    return(c("DIRECT", "STORAGE"))
    
  } else {
    
    stop(paste0("Unknown marker at the end of the action: ", action))
    
  }
  
}



useMeasurementData <- function (inputDF, qaqcInfo) {
  
  # Using the information specified in 'qaqcInfo' (a single row DF), 
  # access a measurement spreadsheet ("Expected_Demand_Units_QAQC_Measurement_Values.xlsx") 
  # and update 'inputDF' accordingly
  
  
  
  # Read in the spreadsheet containing volumes compiled from measurement spreadsheets
  # Filter the data to this iteration's "APPLICATION_NUMBER"
  measuredData <- read_xlsx("InputData/Expected_Demand_Units_QAQC_Measurement_Values.xlsx", sheet = "Data") %>%
    filter(APPLICATION_NUMBER == qaqcInfo$APPLICATION_NUMBER[1] & YEAR == qaqcInfo$YEAR[1])
  
  
  
  # Throw an error if no entries were found for this application number in the spreadsheet
  if (nrow(measuredData) == 0) {
    stop(paste0("There is no entry in the measurement spreadsheet for this application and year (", qaqcInfo$APPLICATION_NUMBER[1], " and ", qaqcInfo$YEAR[1], ")"))
  }
  
  
  
  # There can be more than one row in 'measuredData' if different diversion types' values are specified
  # Iterate through all rows in 'measuredData' and apply changes as needed
  for (j in 1:nrow(measuredData)) {
    
    # Apply all monthly values in this row of 'measuredData' to the corresponding entries in 'inputDF'
    # ('inputDF' is sorted by month, so the values will be properly applied)
    inputDF[inputDF$APPLICATION_NUMBER == qaqcInfo$APPLICATION_NUMBER[1] &
              inputDF$YEAR == qaqcInfo$YEAR[1] &
              inputDF$DIVERSION_TYPE == measuredData$DIVERSION_TYPE[j], ]$AMOUNT <- measuredData[j, ] %>%
      select(toupper(month.abb)) %>% unlist() %>% as.vector()
    
  }
  
  
  
  # Return 'inputData' after these changes
  return(inputDF)
  
}
