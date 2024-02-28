# Prepare functions that will be used in "Scripts/Priority_Date_Postprocessing.R"
# These function will update source files with manual corrections 


# "Statistics_Final.csv" will be adjusted for unit conversion errors and duplicate reporting


# Dependencies
require(tidyverse)
require(readxl)
require(openxlsx)
require(data.table)


# Functions
unitFixer <- function (inputDF, ws) {
  
  # Given the water use report dataset ('inputDF'), perform corrections on specified values
  
  # Two spreadsheets will be used to update 'inputDF':
  #  (1) "InputData/Expected_Demand_Units_QAQC_[DATE].xlsx"
  #  (2) "InputData/Expected_Demand_Units_QAQC_Median_Based_[DATE].xlsx"
  # ("[DATE]" in these filenames should be the most recent copies available)
  
  
  
  # To assist with the analysis, ensure that 'inputDF' is sorted
  # (What mainly matters is that the months are properly ordered)
  inputDF <- inputDF %>%
    arrange(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE)
  
  
  
  # Read in both QA/QC spreadsheets (if file paths were specified)
  # If they were NOT specified, return 'inputDF' without changes and notify the user
  if (is.na(ws$QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH) || is.na(ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH)) {
    
    cat("Both filepaths for the Unit Conversion Errors manual review were not specified for this watershed.\nThe QA/QC implementation procedure will be skipped.\n")
    
    return(inputDF)
    
  }
  
  
  
  # Otherwise, read in those two spreadsheets
  # The procedure will be slightly different depending on whether the paths are SharePoint paths
  if (ws$IS_SHAREPOINT_PATH_QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET == TRUE) {
    
    unitsQAQC <- ws$QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH %>%
      makeSharePointPath() %>%
      read_xlsx(sheet = ws$QAQC_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME)
    
  # Do not use makeSharePointPath() if "IS_SHAREPOINT_PATH_QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET" is FALSE
  } else if (ws$IS_SHAREPOINT_PATH_QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET == FALSE) {
    
    unitsQAQC <- ws$QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH %>%
      read_xlsx(sheet = ws$QAQC_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME)
    
  # Error Check
  } else {
    
    stop("Invalid value for 'IS_SHAREPOINT_PATH_QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET'. Expected 'TRUE' or 'FALSE'.")
    
  }
  
  
  
  if (ws$IS_SHAREPOINT_PATH_QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET == TRUE) {
    
    unitsQAQC_Med <- ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH %>%
      makeSharePointPath() %>%
      read_xlsx(sheet = ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME)
    
    # Do not use makeSharePointPath() if "IS_SHAREPOINT_PATH_QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET" is FALSE
  } else if (ws$IS_SHAREPOINT_PATH_QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET == FALSE) {
    
    unitsQAQC_Med <- ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH %>%
      read_xlsx(sheet = ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME)
    
    # Error Check
  } else {
    
    stop("Invalid value for 'IS_SHAREPOINT_PATH_QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET'. Expected 'TRUE' or 'FALSE'.")
    
  }
  
  
  
  # Filter out entries where no action is required
  # Also, only keep records that relevant to the years in 'inputDF'
  unitsQAQC <- unitsQAQC %>%
    filter(!grepl("^[Nn]one", QAQC_Action_Taken)) %>%
    filter(YEAR >= min(inputDF$YEAR) & YEAR <= max(inputDF$YEAR))
  
  unitsQAQC_Med <- unitsQAQC_Med %>%
    filter(!grepl("^[Nn]one", QAQC_Action_Taken)) %>%
    filter(YEAR >= min(inputDF$YEAR) & YEAR <= max(inputDF$YEAR))
  
  
  # In a separate function, iterate through 'unitsQAQC' and 'unitsQAQC_Med'
  # Then make changes to 'inputDF'
  inputDF <- iterateQAQC(inputDF, unitsQAQC, ws$ID)
  
  
  inputDF <- iterateQAQC(inputDF, unitsQAQC_Med, ws$ID)
  
  
  
  # Return 'inputDF' after these changes
  return(inputDF)
  
}



dupReportingFixer <- function (inputDF, ws) {
  
  # Given the water use report dataset ('inputDF'), perform corrections on specified values
  
  # A single spreadsheet will be used for these corrections
  # Check to make sure that the path to this spreadsheet was specified
  if (is.na(ws$QAQC_DUPLICATE_REPORTING_SPREADSHEET_PATH)) {
    
    cat("The filepath for the Duplicate Reporting manual review spreadsheet was not specified for this watershed.\nThe QA/QC implementation procedure will be skipped.\n")
    
    return(inputDF)
    
  }
  
  
  
  # Otherwise, read in the spreadsheet
  # (with a slightly different procedure depending on whether a SharePoint is used)
  if (ws$IS_SHAREPOINT_PATH_QAQC_DUPLICATE_REPORTING_SPREADSHEET == TRUE) {
    
    qaqcDF <- makeSharePointPath(ws$QAQC_DUPLICATE_REPORTING_SPREADSHEET_PATH) %>%
      read_xlsx(sheet = ws$QAQC_DUPLICATE_REPORTING_WORKSHEET_NAME)
    
  } else if (ws$IS_SHAREPOINT_PATH_QAQC_DUPLICATE_REPORTING_SPREADSHEET == FALSE) {
    
    qaqcDF <- read_xlsx(ws$QAQC_DUPLICATE_REPORTING_SPREADSHEET_PATH, 
                        sheet = ws$QAQC_DUPLICATE_REPORTING_WORKSHEET_NAME)
    
  } else {
    
    stop("Invalid value for 'IS_SHAREPOINT_PATH_QAQC_DUPLICATE_REPORTING_SPREADSHEET'. Expected 'TRUE' or 'FALSE'.")
    
  }
  
  
  
  # Keep only entries in 'qaqcDF' that are relevant to the years in 'inputDF'
  qaqcDF <- qaqcDF %>%
    filter(YEAR >= min(inputDF$YEAR) & YEAR <= max(inputDF$YEAR))
  
  
  
  # Remove entries in 'qaqcDF' where no actions are required
  # Also, rename "APPL_ID" to "APPLICATION_NUMBER"
  qaqcDF <- qaqcDF %>%
    filter(!grepl("^None", QAQC_Action_Taken)) %>%
    rename(APPLICATION_NUMBER = APPL_ID)
  
  
  
  # Rely on iterateQAQC() to apply changes to 'inputDF'
  inputDF <- inputDF %>% 
    iterateQAQC(qaqcDF, ws$ID)
  
  
  
  # Return 'inputDF' after these changes
  return(inputDF)
  
}



iterateQAQC <- function (inputDF, unitsQAQC, wsID) {
  
  # Given a source dataset and data frame of corrections, apply changes based on the "QAQC_Action_Taken" column
  
  
  
  # Iterate through the different actions specified in 'unitsQAQC'
  for (i in 1:nrow(unitsQAQC)) {
    
    
    # If this row's "APPLICATION_NUMBER" value does not appear in 'inputDF', skip this row
    if (!(unitsQAQC$APPLICATION_NUMBER[i] %in% inputDF$APPLICATION_NUMBER)) {
      # S022856 for Russian River
      next
    }
    
    
    
    # For this first issue, values for this right and year will be set to 0
    if (grepl("^Change monthly (Direct )?(Storage )?values to 0$", unitsQAQC$QAQC_Action_Taken[i])) {
    
      
      # If "Direct" or "Storage" are in the action string, only those values will be set to 0
      if (grepl("Direct", unitsQAQC$QAQC_Action_Taken[i])) {
        
        useChoice <- "DIRECT"
        
      } else if (grepl("Storage", unitsQAQC$QAQC_Action_Taken[i])) {
        
        useChoice <- "STORAGE"
        
      } else {
        
        useChoice <- c("DIRECT", "STORAGE")
        
      }
      
      
      # Starting in 2022, reporting is done for the water year, not calendar year
      # These changes should affect either the calendar year or the water year based on this difference
      if (unitsQAQC$YEAR[i] < 2022) {
        
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  inputDF$YEAR == unitsQAQC$YEAR[i] &
                  inputDF$DIVERSION_TYPE %in% useChoice &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- 0
        
      } else {
        
        # The water year is the first 9 months of the current year and the last 3 months of the previous year
        # (e.g., WY2022 is from October 2021 to September 2022)
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:9) | 
                     (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 10:12)) &
                  inputDF$DIVERSION_TYPE %in% useChoice &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- 0
        
      }
      
      
      
      # For these notes, the actual units are "gallons"
      # They need to be converted into AF
    } else if (grepl("^Convert from gallons ", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Use another function to choose the actions that will be modified
      toConvert <- chooseUseType(unitsQAQC$QAQC_Action_Taken[i])
      
      
      
      if (unitsQAQC$YEAR[i] < 2022) {
        
        # There are 325,851 gallons in 1 AF
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  inputDF$YEAR == unitsQAQC$YEAR[i] &
                  inputDF$DIVERSION_TYPE %in% toConvert &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                            inputDF$YEAR == unitsQAQC$YEAR[i] &
                                                            inputDF$DIVERSION_TYPE %in% toConvert &
                                                            inputDF$AMOUNT > 0, ]$AMOUNT / 325851
        
      } else {
        
        # Apply the conversion to the water year
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:8) | 
                     (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 9:12)) &
                  inputDF$DIVERSION_TYPE %in% toConvert &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                            ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:9) | 
                                                               (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 10:12)) &
                                                            inputDF$DIVERSION_TYPE %in% toConvert &
                                                            inputDF$AMOUNT > 0, ]$AMOUNT / 325851
        
      }
      
      # For these notes, the actual units are "gpd"
      # They need to be converted into AF
    } else if (grepl("^Convert from gpd ", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Use another function to choose the actions that will be modified
      toConvert <- chooseUseType(unitsQAQC$QAQC_Action_Taken[i])
      
      
      
      if (unitsQAQC$YEAR[i] < 2022) {
        
        # There are 325,851 gallons in 1 AF and 365 days in a year
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  inputDF$YEAR == unitsQAQC$YEAR[i] &
                  inputDF$DIVERSION_TYPE %in% toConvert &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                            inputDF$YEAR == unitsQAQC$YEAR[i] &
                                                            inputDF$DIVERSION_TYPE %in% toConvert &
                                                            inputDF$AMOUNT > 0, ]$AMOUNT / 325851 * 365
        
      } else {
        
        # Apply this conversion over the water year
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:8) | 
                     (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 9:12)) &
                  inputDF$DIVERSION_TYPE %in% toConvert &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                            ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:8) | 
                                                               (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 9:12)) &
                                                            inputDF$DIVERSION_TYPE %in% toConvert &
                                                            inputDF$AMOUNT > 0, ]$AMOUNT / 325851 * 365
        
      }
      
      
      
      # For these notes, the actual units are "gpm"
      # They need to be converted into AF
    } else if (grepl("^Convert from gpm ", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Use another function to choose the actions that will be modified
      toConvert <- chooseUseType(unitsQAQC$QAQC_Action_Taken[i])
      
      
      if (unitsQAQC$YEAR[i] < 2022) {
        
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
        
      } else {
        
        # Apply these changes to the water year
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:8) | 
                     (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 9:12)) &
                  inputDF$DIVERSION_TYPE %in% toConvert &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                            ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:8) | 
                                                               (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 9:12)) &
                                                            inputDF$DIVERSION_TYPE %in% toConvert &
                                                            inputDF$AMOUNT > 0, ]$AMOUNT / 325851 * 60 * 24 * 365
        
      }
      
      
      
      # The next action involves dividing all values for a right and year by a number
    } else if (grepl("^Divide monthly reported values by [0-9]+$", unitsQAQC$QAQC_Action_Taken[i])) {
      
      # Extract the number to use in the division
      divNum <- unitsQAQC$QAQC_Action_Taken[i] %>%
        str_extract("[0-9]+$") %>% as.numeric()
      
      
      # Error Check
      # 'divNum' should not be NA, and it should not be 0
      stopifnot(!is.na(divNum) & divNum != 0)
      
      
      if (unitsQAQC$YEAR[i] < 2022) {
        
        # Divide all non-zero "AMOUNT" values by 'divNum'
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  inputDF$YEAR == unitsQAQC$YEAR[i] &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                            inputDF$YEAR == unitsQAQC$YEAR[i] &
                                                            inputDF$AMOUNT > 0, ]$AMOUNT / divNum
        
      } else {
        
        # Perform this operation over the water year
        inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                  ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:8) | 
                     (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 9:12)) &
                  inputDF$AMOUNT > 0, ]$AMOUNT <- inputDF[inputDF$APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                                                            ((inputDF$YEAR == unitsQAQC$YEAR[i] & inputDF$MONTH %in% 1:8) | 
                                                               (inputDF$YEAR == unitsQAQC$YEAR[i] - 1 & inputDF$MONTH %in% 9:12)) &
                                                            inputDF$AMOUNT > 0, ]$AMOUNT / divNum
        
      }
      
      
      
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
        
        # If 'actionYear' is a report that uses a calendar year, then only one year's data is needed
        if (actionYear < 2022) {
          
          tempDF <- fread(file = "RawData/water_use_report_extended.csv",
                          select = c("APPLICATION_NUMBER","YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE")) %>%
            filter(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] & YEAR == actionYear) %>%
            arrange(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE)
          
        # Otherwise, data from both 'actionYear' and the prior year are required
        } else {
          
          tempDF <- fread(file = "RawData/water_use_report_extended.csv",
                          select = c("APPLICATION_NUMBER","YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE")) %>%
            filter(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] & YEAR %in% c(actionYear, actionYear - 1)) %>%
            arrange(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE)
          
        }
        
        
        
        # Error Check
        stopifnot(nrow(tempDF) > 0)
        
        # RSQLite code
        # conn <- dbConnect(dbDriver("SQLite"), "RawData/water_use_report_extended_subset.sqlite")
        # water_use_report <- dbGetQuery(conn, 
        #                                paste0('SELECT DISTINCT ',
        #                                       '"APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE" ',
        #                                       'FROM "Table" ',
        #                                       'WHERE "APPLICATION_NUMBER" = "', unitsQAQC$APPLICATION_NUMBER[i], '" ',
        #                                       'AND "YEAR" = ', actionYear, ' ',
        #                                       'ORDER BY "APPLICATION_NUMBER", "YEAR", "MONTH", "DIVERSION_TYPE"')) 
        # dbDisconnect(conn)
        
      } else {
        
        tempDF <- inputDF
        
      }
      
      
      
      # Extract the desired rows from 'tempDF' and change the year to the one with problematic data
      # The specific year labels for this replacement data are dependent on 
      # whether the reporting year 'actionYear' had calendar years or water years
      if (actionYear < 2022) {
        
        # "TEMP_YEAR" is used to ensure that the if_else statement's 
        # TRUE/FALSE condition is evaluated for each row
        newRows <- tempDF %>%
          filter(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                   YEAR == actionYear &
                   DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
          mutate(TEMP_YEAR = unitsQAQC$YEAR[i]) %>%
          mutate(YEAR = if_else(TEMP_YEAR < 2022, unitsQAQC$YEAR[i],
                                if_else(MONTH < 10, unitsQAQC$YEAR[i], unitsQAQC$YEAR[i] - 1))) %>%
          select(-TEMP_YEAR)
        
      } else {
        
        newRows <- tempDF %>%
          filter(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                   ((YEAR == actionYear & MONTH %in% 1:9) | 
                      (YEAR == actionYear - 1 & MONTH %in% 10:12)) &
                   DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
          mutate(TEMP_YEAR = unitsQAQC$YEAR[i]) %>%
          mutate(YEAR = if_else(TEMP_YEAR < 2022, unitsQAQC$YEAR[i],
                                if_else(MONTH < 10, unitsQAQC$YEAR[i], unitsQAQC$YEAR[i] - 1))) %>%
          select(-TEMP_YEAR)
        
      }
      
      
      
      # Remove the problematic year's data from 'inputDF' and append 'newRows' to 'inputDF'
      if (unitsQAQC$YEAR[i] < 2022) {
        
        inputDF <- inputDF %>%
          filter(!(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                     YEAR == unitsQAQC$YEAR[i] &
                     DIVERSION_TYPE %in% c("DIRECT", "STORAGE")))
        
      } else {
        
        inputDF <- inputDF %>%
          filter(!(APPLICATION_NUMBER == unitsQAQC$APPLICATION_NUMBER[i] &
                     ((YEAR == actionYear & MONTH %in% 1:9) | 
                        (YEAR == actionYear - 1 & MONTH %in% 10:12)) &
                     DIVERSION_TYPE %in% c("DIRECT", "STORAGE")))
        
      }
      
      
      
      # Append the new data to 'inputDF'
      # Then, sort it again
      inputDF <- inputDF %>%
        bind_rows(newRows) %>%
        arrange(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE)
      
      
      
      # Some actions require using a measurement spreadsheet for data
    } else if (unitsQAQC$QAQC_Action_Taken[i] == "Replace with measurement spreadsheet values") {
      
      
      # Use a separate function for this step
      inputDF <- inputDF %>% useMeasurementData(unitsQAQC[i, ], wsID)
      
      
    # Another type of issue is needing to select one right's entries among two or more options
    # (The other rights' values are changed to zero)
    # In this case, make a selection based on priority (computed by the "Priority Date" module)
    } else if (unitsQAQC$QAQC_Action_Taken[i] == "Keep One") {
      
      
      # Apply these changes in a separate function
      inputDF <- inputDF %>%
        removeDups(unitsQAQC, i, wsID)
      
      
      # As a final step, set "QAQC_Action_Taken" to "None" for all entries with this "Primary_Key"
      # (The expectation is that only one of the rights with this Primary Key will be kept)
      # (Therefore, no further actions should be needed for any of these rights)
      unitsQAQC[unitsQAQC$Primary_Key == unitsQAQC$Primary_Key[i], ]$QAQC_Action_Taken <- "None"
      
      
      
      # If an action has multiple actions specified
    } else if (grepl("^Multiple Actions\\|", unitsQAQC$QAQC_Action_Taken[i])) {
      
      
      # Get a vector with all of the required actions listed
      # (Also removed the first part "Multiple Actions")
      actionVec <- unitsQAQC$QAQC_Action_Taken[i] %>%
        str_split("\\|") %>% unlist() %>% tail(-1) %>% trimws()
      
      
      # Create a dummy QAQC variable that repeats row 'i' for each action in 'actionVec'
      for (j in 1:length(actionVec)) {
        
        # If this is the first iteration, initialize 'dummyDF'
        # Otherwise, append row 'i' of 'unitsQAQC' to 'dummyDF'
        if (j == 1) {
          dummyDF <- unitsQAQC[i, ]
        } else {
          dummyDF <- bind_rows(dummyDF,
                               unitsQAQC[i, ])
        }
        
        
        # Change the value of "QAQC_Action_Taken" to the corresponding entry in 'actionVec' 
        dummyDF$QAQC_Action_Taken[j] <- actionVec[j]
        
      }
      
      
      # After that, call iterateQAQC() again with 'dummyDF'
      inputDF <- iterateQAQC(inputDF, dummyDF, wsID)
      
    
      # If an action is "None", skip it
    } else if (unitsQAQC$QAQC_Action_Taken[i] == "None") {
      
      next
      
      # Throw an error for any other action  
    } else {
      
      stop(paste0("No procedure has been specified for this action: ", unitsQAQC$QAQC_Action_Taken[i]))
      
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



useMeasurementData <- function (inputDF, qaqcInfo, wsID) {
  
  # Using the information specified in 'qaqcInfo' (a single row DF), 
  # access a measurement spreadsheet ("Expected_Demand_Units_QAQC_Measurement_Values.xlsx") 
  # and update 'inputDF' accordingly
  
  
  
  # Read in the spreadsheet containing volumes compiled from measurement spreadsheets
  # Filter the data to this iteration's "APPLICATION_NUMBER"
  if (qaqcInfo$YEAR[1] < 2022) {
    
    measuredData <- read_xlsx(paste0("InputData/", wsID, "_Expected_Demand_Units_QAQC_Measurement_Values.xlsx"), sheet = "Data") %>%
      filter(APPLICATION_NUMBER == qaqcInfo$APPLICATION_NUMBER[1] & YEAR == qaqcInfo$YEAR[1])
    
  } else {
    
    # Get data for the water year instead of the calendar year
    measuredData <- read_xlsx(paste0("InputData/", wsID, "_Expected_Demand_Units_QAQC_Measurement_Values.xlsx"), sheet = "Data") %>%
      filter(APPLICATION_NUMBER == qaqcInfo$APPLICATION_NUMBER[1] & YEAR %in% c(qaqcInfo$YEAR[1], qaqcInfo$YEAR[1] - 1))
    
  }
  
  
  
  # Throw an error if no entries were found for this application number in the spreadsheet
  if (nrow(measuredData) == 0) {
    stop(paste0("There is no entry in the measurement spreadsheet for this application and year (", qaqcInfo$APPLICATION_NUMBER[1], " and ", qaqcInfo$YEAR[1], ")"))
  }
  
  
  
  # There can be more than one row in 'measuredData' if different diversion types' values are specified
  # Iterate through all rows in 'measuredData' and apply changes as needed
  for (j in 1:nrow(measuredData)) {
    
    # Apply all monthly values in this row of 'measuredData' to the corresponding entries in 'inputDF'
    # ('inputDF' is sorted by month, so the values will be properly applied)
    if (qaqcInfo$YEAR[1] < 2022) {
      
      inputDF[inputDF$APPLICATION_NUMBER == qaqcInfo$APPLICATION_NUMBER[1] &
                inputDF$YEAR == measuredData$YEAR[j] &
                inputDF$DIVERSION_TYPE == measuredData$DIVERSION_TYPE[j], ]$AMOUNT <- measuredData[j, ] %>%
        select(toupper(month.abb)) %>% unlist() %>% as.vector()
      
    } else {
      
      # The operation is more complicated for newer reports, which use a water year
      # This iteration of 'measuredData' may either apply to the first 9 months of 
      # the year in 'qaqcInfo' or the last 3 months of the year before qaqcInfo$YEAR
      if (measuredData$YEAR[j] == qaqcInfo$YEAR[1]) {
        
        inputDF[inputDF$APPLICATION_NUMBER == qaqcInfo$APPLICATION_NUMBER[1] &
                  inputDF$YEAR == measuredData$YEAR[j] &
                  inputDF$MONTH %in% 1:9 &
                  inputDF$DIVERSION_TYPE == measuredData$DIVERSION_TYPE[j], ]$AMOUNT <- measuredData[j, ] %>%
          select(toupper(month.abb[1:9])) %>% unlist() %>% as.vector()
        
      } else {
        
        inputDF[inputDF$APPLICATION_NUMBER == qaqcInfo$APPLICATION_NUMBER[1] &
                  inputDF$YEAR == measuredData$YEAR[j] &
                  inputDF$MONTH %in% 10:12 &
                  inputDF$DIVERSION_TYPE == measuredData$DIVERSION_TYPE[j], ]$AMOUNT <- measuredData[j, ] %>%
          select(toupper(month.abb[10:12])) %>% unlist() %>% as.vector()
        
      }
      
    }
    
  }
  
  
  
  # Return 'inputData' after these changes
  return(inputDF)
  
}



removeDups <- function (inputDF, unitsQAQC, i, wsID) {
  
  # Excluding the right with the earliest priority date (lowest value),
  # set the values for a given year and diversion type to zero for all other rights
  
  
  # Extract a subset of 'unitsQAQC'; all records that share this iteration's PARTY_ID
  qaqcSubset <- unitsQAQC %>%
    filter(PARTY_ID == unitsQAQC$PARTY_ID[i])
  
  
  # Create a vector of unique years for the data in 'qaqcSubset'
  yearVec <- qaqcSubset$YEAR %>% unique() %>% sort()
  
  
  # Get the unique water rights for this party as well
  appVec <- qaqcSubset$APPLICATION_NUMBER %>% unique() %>% sort()
  
  
  # More than one use type may appear as well
  # Get all relevant use types as well
  useVec <- qaqcSubset$DIVERSION_TYPE %>% unique() %>% sort()
  
  
  # Get the priority dates for these application numbers next
  priorityDF <- read_xlsx(paste0("OutputData/", wsID, "_Priority_Date_Scripted.xlsx"), col_types = "text") %>%
    select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE) %>%
    filter(APPLICATION_NUMBER %in% appVec)
  
  
  # Error Check
  # Every value in 'appVec' should have a corresponding priority date in 'priorityDF'
  stopifnot(sum(appVec %in% priorityDF$APPLICATION_NUMBER) == length(appVec))
  
  
  
  # Join the data in 'priorityDF' to 'qaqcSubset'
  qaqcSubset <- qaqcSubset %>%
    left_join(priorityDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  
  # Iterate through 'yearVec' next
  for (j in 1:length(yearVec)) {
    
    
    # Nested in that loop is an iteration through the diversion types in 'useVec'
    for (k in 1:length(useVec)) {
      
      
      # Create a subset of 'qaqcSubset' that only has data for this iteration's year and diversion type
      # It should be sorted so that the lowest priority date appears first in the tibble
      qaqcSubSub <- qaqcSubset %>%
        filter(YEAR == yearVec[j] & DIVERSION_TYPE == useVec[k]) %>%
        arrange(ASSIGNED_PRIORITY_DATE, APPLICATION_NUMBER)
      
      
      # If 'qaqcSubSub' has 0 records, skip to the next iteration
      # (This may happen if "DIRECT" and "STORAGE" are not always relevant to duplicate reporting errors in all years)
      if (nrow(qaqcSubSub) == 0) {
        next
      }
      
      
      # Error Check
      # 'qaqcSubSub' should have more than one "APPLICATION_NUMBER" in it (and no repeats)
      stopifnot(nrow(qaqcSubSub) > 1)
      stopifnot(length(unique(qaqcSubSub$APPLICATION_NUMBER)) == nrow(qaqcSubSub))
      
      
      # Ignore the first row of 'qaqcSubSub' (it has the earliest priority date)
      qaqcSubSub <- qaqcSubSub[-1, ]
      
      
      # The other rights will have their values set to 0 for this year
      # The procedure will be a little different depending on the year
      if (yearVec[j] < 2021) {
        
        inputDF[inputDF$APPLICATION_NUMBER %in% qaqcSubSub$APPLICATION_NUMBER &
                  inputDF$YEAR == yearVec[j] &
                  inputDF$DIVERSION_TYPE == useVec[k], ]$AMOUNT <- 0
        
      # Starting in 2022, reports use a water year
      # Therefore, for 2021, if the owner has reported data in 2022 or later,
      # only change months 1-9 to zero (the last three months are part of water year 2022)
      } else if (yearVec[j] == 2021) {
        
        
        # If the owner has reports for 2022 and later, do not zero out the last three months in 2022
        # (Because they are part of the WY2022 dataset)
        if (sum(yearVec > 2021) > 0) {
          
          inputDF[inputDF$APPLICATION_NUMBER %in% qaqcSubSub$APPLICATION_NUMBER &
                    inputDF$YEAR == yearVec[j] &
                    inputDF$MONTH %in% 1:9 &
                    inputDF$DIVERSION_TYPE == useVec[k], ]$AMOUNT <- 0
          
        # If there are no reports after 2021, it is okay to zero out the last three months as well
        # (Because this data is from the CY2021 report)
        } else {
          
          inputDF[inputDF$APPLICATION_NUMBER %in% qaqcSubSub$APPLICATION_NUMBER &
                    inputDF$YEAR == yearVec[j] &
                    inputDF$DIVERSION_TYPE == useVec[k], ]$AMOUNT <- 0
          
        }
        
        
      # For the final case (reports in 2022 or later), water years are used
      } else if (yearVec[j] > 2021) {
        
        inputDF[inputDF$APPLICATION_NUMBER %in% qaqcSubSub$APPLICATION_NUMBER &
                  ((inputDF$YEAR == yearVec[j] & inputDF$MONTH %in% 1:9) | 
                     (inputDF$YEAR == yearVec[j] - 1 & inputDF$MONTH %in% 10:12)) &
                  inputDF$DIVERSION_TYPE == useVec[k], ]$AMOUNT <- 0
        
      }
      
      
    } # End of 'useVec' loop (k)
    
  } # End of 'yearVec' loop (j)
  
  
  
  # Return 'inputDF' after these changes
  return(inputDF)
  
}

print("The QAQC_Functions.R script is done running!")