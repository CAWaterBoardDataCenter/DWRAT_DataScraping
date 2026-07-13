#----PURPOSE:----
# Last Updated by: Aakash Prashar on 6/26/2025

# Use this script before the April DWRAT run every year
# Input the path to the March PRMS run's "rr_budget.out2" file
# This script will identify the most similar water year using our linear regression model
# It will update "Dat_PRMS.R" and "Dat_SRP.R" automatically based on this calculation


#### Setup ####

remove(list = ls())

require(tidyverse)
require(rstudioapi)
require(writexl)


#### Procedure ####

mainProcedure <- function (linModel = list(m = 1.114766768, b = 7.531105952)) {
  
  
  
  cat("\n\nStarting 'Dat_Most_Similar_Water_Year_Updater.R'...\n\n")
  
  
  
  # Get the year for which the "Most Similar Water Year" will be found
  analysisYear <- requestAnalysisYear()
  
  
  
  # Get the location of the "rr_budget.out2" file from the user
  filePath <- requestFilePath(analysisYear)
  
  
  
  # Read in the .out2 file
  outDF <- read_out2(filePath)
  
  
  
  # Focus on the precipitation column and adjust the formatting
  precipDF <- preparePrecipDF(outDF)
  
  
  
  # Summarize the data in 'outDF' to get total and Oct-Feb precipitation values
  summaryDF <- summarizePrecip(precipDF)
  
  
  
  # Using 'summaryDF', apply the linear model to estimate 
  # "WY_TOTAL_PRECIP_INCHES" for 'analysisYear'
  predictedTotal <- predictPrecip(summaryDF, analysisYear, linModel)
  
  
  
  # Find the most similar water year and output this information to the console
  # (Also output a spreadsheet with the data)
  similarYear <- findSimilarWY(summaryDF, analysisYear, predictedTotal, 
                               outDF, linModel, filePath)
  
  
  
  # TO DO
  # Update "Dat_PRMS.R" and "Dat_SRP.R" to use the most similar water year
  updateScripts(analysisYear, similarYear)
  
  
  
  # Output a completion message
  cat("The script has finished running!\n\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



requestAnalysisYear <- function () {
  
  # Get from the user the water year that will be input into the model
  
  
  analysisYear <- showPrompt(title = "Input a Water Year",
                             message = "Specify the year whose most similar water year will be found",
                             default = year(Sys.Date()))
  
  
  
  # The input should be a number
  analysisYear <- analysisYear %>%
    trimws() %>% as.numeric()
  
  
  
  # Check for errors
  if (length(analysisYear) == 0 || is.na(analysisYear) || 
      str_count(analysisYear, "[0-9]") != 4) {
    
    stop("A four-digit year must specified for this script to work")
    
  }
  
  
  
  return(analysisYear)
  
}



requestFilePath <- function (analysisYear) {
  
  # Create a dialog box that requests the "rr_budget.out2" file location from the user
  
  
  
  # To try and save time, make the default directory that opens up be 
  # the "Hydrology" folder on SharePoint (more specifically the February run folder)
  preferredDirectory <- paste0("C:/Users/", 
                               system("whoami", intern = TRUE) %>% str_remove("^epa\\\\"),
                               "/Water Boards/Supply and Demand Assessment - Documents/DWRAT/",
                               "SDU_Runs/Hydrology/")
  
  
  
  # If the SDA SharePoint folder does not exist on this computer, 
  # default to the working directory instead
  if (!dir.exists(preferredDirectory)) {
    
    preferredDirectory <- getwd()
    
  # Otherwise, try and see if a more precise folder location can be found
  } else {
    
    
    # Try and find a March run folder for the selected 'analysisYear'
    marFolder <- list.files(preferredDirectory) %>%
      str_subset("\\.", negate = TRUE) %>%
      str_subset(paste0("^", analysisYear, "-03")) %>% 
      sort() %>% tail(1)
    
    
    
    # If one is found, update 'preferredDirectory'
    # Then, try and go further
    if (length(marFolder) == 1) {
      
      preferredDirectory <- paste0(preferredDirectory, marFolder, "/")
      
      
      
      # Search the February folder for "rr_budget.out2"
      # Extract a folder path for it, if possible
      out2Folder <- list.files(preferredDirectory, pattern = "^rr_budget.out2$", 
                               full.names = TRUE, recursive = TRUE) %>%
        tail(1) %>% str_remove("rr_budget\\.out2$")
      
      
      
      # If 'out2Folder' was successfully determined, update 'preferredDirectory'
      if (length(out2Folder) == 1) {
        
        preferredDirectory <- out2Folder
        
      }
      
    }
    
  }
  
  
  
  # After updates were potentially made to 'preferredDirectory', prompt the user
  filePath <- selectFile("Select the March PRMS run's \"rr_budget.out2\" file",
                         path = preferredDirectory,
                         filter = "Out2 Files (*.out2)")
  
  
  
  # If 'filePath' is NULL, stop the script
  if (is.null(filePath)) {
    
    stop("This script will not work without the \"rr_budget.out2\" file")
    
  }
  
  
  
  # Otherwise, return 'filePath'
  return(filePath)
  
}



read_out2 <- function (outPath) {
  
  # Given the path to a .out2 file, 
  # read it in and format it as a proper tibble
  
  
  
  # First, use read_lines() to read in the file
  outDF <- read_lines(outPath)
  
  
  
  # Remove empty strings from the vector
  outDF <- outDF %>%
    str_subset("^$", negate = TRUE)
  
  
  
  # Find the row containing the headers
  headerRow <- grep("Year\\s+mo", outDF, ignore.case = TRUE)
  
  
  
  stopifnot(length(headerRow) == 1)
  
  
  
  # Remove the rows before 'headerRow' in 'outDF'
  outDF <- outDF[headerRow:length(outDF)]
  
  
  
  # Get a vector of the header names
  columnNames <- outDF[1] %>%
    spaceSplit()
  
  
  
  # If the second row contains units for the columns,
  # append them to 'columnNames'
  if (grepl("\\s+\\(in\\)\\s+", outDF[2], ignore.case = TRUE)) {
    
    unitVec <- outDF[2] %>%
      spaceSplit() %>%
      str_split("[\\(\\)]") %>% unlist() %>%
      str_subset("^$", negate = TRUE)
    
    
    
    # The "Year", "mo", and "day" variables at the beginning do not have any units
    # Add three empty strings to the start of 'unitVec'
    unitVec <- c("", "", "",
                 unitVec)
    
    
    
    stopifnot(length(unitVec) == length(columnNames))
    
    
    
    # Paste these units at the end of 'columnNames'
    columnNames <- map2_chr(columnNames, unitVec,
                            ~ if_else(.y == "", .x, paste0(.x, " (", .y, ")")))
    
  }
  
  
  
  # The last non-data row contains the value "initial"
  # Find its index
  removalIndex <- grep("^\\s*initial", outDF)
  
  
  
  stopifnot(length(removalIndex) == 1)
  
  
  
  # Remove all rows up to 'removalIndex'
  outDF <- outDF[-c(1:removalIndex)]
  
  
  
  # Remove the "Execution elapsed time" row at the end as well
  outDF <- outDF %>%
    str_subset("^\\s*Execution elapsed time", negate = TRUE)
  
  
  
  # The only rows left in 'outDF' should be the data now
  # Within each row, split the data at the spaces
  outDF <- outDF %>%
    str_split("\\s") %>%
    map(~str_subset(., "^$", negate = TRUE))
  
  
  
  # Get a vector of lengths for each row
  # Check for rows that do not have the expected length 
  # (there should be one element per column heading)
  rowLens <- lengths(outDF)
  
  
  
  # Check for entries that have the incorrect number of elements
  problemRows <- which(rowLens != length(columnNames))
  
  
  
  # Iterate through the problematic rows
  # Try to fix them
  if (length(problemRows) > 0) {
    
    for (i in 1:length(problemRows)) {
      
      # One potential error comes from having a number followed by 
      # a negative number with no space in-between
      if (sum(grepl("^\\-?[0-9\\.]+\\-[0-9\\.]+$", outDF[[problemRows[i]]])) > 0) {
        
        
        # Iterate through the entries in this row of 'outDF'
        for (j in 1:length(outDF[[problemRows[i]]])) {
          
          # If this is a row with both a number and a negative number, separate them
          if (grepl("^\\-?[0-9\\.]+\\-[0-9\\.]+$", outDF[[problemRows[i]]][j])) {
            
            # Use a positive look-ahead regex to split the numbers 
            # (while preserving the negative sign)
            outDF[[problemRows[i]]][j] <- outDF[[problemRows[i]]][j] %>%
              str_split("(?=\\-)")
            
          }
          
        }
        
        
        
        # Splitting operations within the list element may create a sub-list there
        # Remove any sub-lists
        outDF[[problemRows[i]]] <- outDF[[problemRows[i]]] %>%
          unlist() %>%
          str_subset("^$", negate = TRUE)
        
      }
      
    } # End of loop through 'problemRows'
    
  }
  
  
  
  # Make sure that every row has the proper length now
  stopifnot(outDF %>% lengths() %>% unique() %>% length() == 1)
  stopifnot(outDF %>% lengths() %>% unique() == length(columnNames))
  stopifnot(length(unlist(outDF)) %% length(columnNames) == 0)
  
  
  
  # Convert 'outDF' into a proper data frame
  outDF <- outDF %>%
    unlist() %>% as.numeric() %>%
    matrix(ncol = length(columnNames), byrow = TRUE) %>%
    data.frame() %>% tibble() %>%
    set_names(columnNames)
  
  
  
  # After these changes, return 'outDF'
  return(outDF)
  
}



spaceSplit <- function (str) {
  
  # Split a string at spaces
  # Remove empty strings and return the string
  return(str %>%
           str_split("\\s") %>% unlist() %>%
           str_subset("^$", negate = TRUE))
  
}



preparePrecipDF <- function (outDF) {
  
  # Adjust 'outDF' to contain monthly precipitation values
  # Also check for potential issues and try to address them
  
  
  
  # Keep only the date columns and the precipitation column
  outDF <- outDF %>%
    select(Year, mo, day, `ppt (in)`)
  
  
  
  # Sum the precipitation values for each year-month pair
  outDF <- outDF %>%
    group_by(Year, mo) %>%
    summarize(TOTAL_PRECIP_INCHES = sum(`ppt (in)`), .groups = "drop")
  
  
  
  # Create a water year column
  # If the month is less than 10 (October), its water year value is the same as its calendar year
  # Otherwise, it's part of the water year equal to its calendar year plus one (e.g., October 2019 is part of WY2020)
  outDF <- outDF %>%
    mutate(WATER_YEAR = if_else(mo < 10, Year, Year + 1))
  
  
  
  # The first water year 'outDF' (1990) likely lacks a complete set of data
  # Remove it if that is the case
  if (outDF %>% filter(WATER_YEAR == min(WATER_YEAR)) %>% nrow() != 12) {
    
    outDF <- outDF %>%
      filter(WATER_YEAR != min(WATER_YEAR))
    
  }
  
  
  
  # Check if any other years in 'outDF' are missing data
  # Output a warning message about that
  if (outDF %>% group_by(WATER_YEAR) %>%
      summarize(MONTHS = n(), .groups = "drop") %>%
      filter(MONTHS != 12) %>% nrow() > 0) {
    
    warning("There are water years that lack data in some months")
    
  }
  
  
  
  return(outDF)
  
}



summarizePrecip <- function (outDF) {
  
  # Create a summarized version of 'outDF'
  # It will contain total precipitation for each water year as well as
  # total precipitation for October through February
  
  
  
  # First, create a summary table for each water year
  summaryDF <- outDF %>%
    group_by(WATER_YEAR) %>%
    summarize(WY_TOTAL_PRECIP_INCHES = sum(TOTAL_PRECIP_INCHES), .groups = "drop")
  
  
  
  # There will be a subset column as well:
  # OCT to FEB
  
  
  
  # Create new data frames with columns for these subset sums
  # Join them to 'summaryDF'
  summaryDF <- summaryDF %>%
    left_join(createParitalDF(outDF, "OCT", "FEB", "OCT_TO_FEB_PARTIAL_PRECIP_INCHES"),
              by = "WATER_YEAR", relationship = "one-to-one")
  
  
  
  # Return 'summaryDF'
  return(summaryDF)
  
}



createParitalDF <- function (outDF, rangeStart, rangeEnd, colName) {
  
  # For the subset range, use 'rangeStart' and 'rangeEnd' to determine which months are included
  # To start, convert those two variables into integers
  rangeStart <- which(toupper(month.abb) == rangeStart)[1]
  rangeEnd <- which(toupper(month.abb) == rangeEnd)[1]
  
  
  
  # Define a vector that contains all months within this range
  # If 'rangeStart' is a later month than 'rangeEnd', the procedure will differ slightly
  if (rangeStart > rangeEnd) {
    
    rangeVec <- c(rangeStart:12, 1:rangeEnd)
    
  } else {
    
    rangeVec <- rangeStart:rangeEnd
    
  }
  
  
  
  # Define another variable like 'summaryDF' that sums precipitation data for
  # only the months in 'rangeVec'
  partialDF <- outDF %>%
    filter(mo %in% rangeVec) %>%
    group_by(WATER_YEAR) %>%
    summarize(!! colName := sum(TOTAL_PRECIP_INCHES), .groups = "drop")
  
  
  
  # Return 'partialDF'
  return(partialDF)
  
}



predictPrecip <- function (summaryDF, analysisYear, linModel) {
  
  # Apply a linear regression model to estimate "WY_TOTAL_PRECIP_INCHES"
  # when given "OCT_TO_FEB_PARTIAL_PRECIP_INCHES"
  
  
  # This estimation will be applied to the water year contained in 'analysisYear'
  
  
  
  # Error Check
  if (summaryDF %>% filter(WATER_YEAR == analysisYear) %>% nrow() == 0) {
    
    stop(paste0("The requested analysis year is not present in the dataset.\n\n",
                "Please select a water year that appears in \"rr_budget.out2\""))
    
  }
  
  
  
  # Apply the linear regression formula to predict the total precipitation 
  # for the requested water year
  # (The model was dervied using "PRMS_Output_Precip_Multi_Modeler.R" with
  #  the .out2 file generated from the 2024-03-14 PRMS run)
  return (linModel$m * summaryDF$OCT_TO_FEB_PARTIAL_PRECIP_INCHES[summaryDF$WATER_YEAR == analysisYear] + linModel$b)
  
}



findSimilarWY <- function (summaryDF, analysisYear, predictedTotal, outDF, linModel, filePath) {
  
  # Identify the water year in 'summaryDF' with a total precipitation that
  # is similar to 'predictedTotal'
  
  # (Ignore 'analysisYear' in this assessment)
  
  
  
  # Calculate the absolute difference between "WY_TOTAL_PRECIP_INCHES" and 'predictedTotal'
  summaryDF <- summaryDF %>%
    mutate(PREDICTED_WY_TOTAL_PRECIP_INCHES = NA_real_, 
           ABSOLUTE_DIFFERENCE_FROM_PREDICTION = if_else(WATER_YEAR == analysisYear,
                                                         NA_real_,
                                                         abs(WY_TOTAL_PRECIP_INCHES - predictedTotal)),
           MOST_SIMILAR_WATER_YEAR = NA)
  
  
  
  # Identify the year with the smallest absolute difference
  # (If there's a tie, take the more recent water year)
  similarWY <- summaryDF %>%
    filter(WATER_YEAR != analysisYear) %>%
    filter(ABSOLUTE_DIFFERENCE_FROM_PREDICTION == min(ABSOLUTE_DIFFERENCE_FROM_PREDICTION)) %>%
    select(WATER_YEAR) %>% unlist(use.names = FALSE) %>%
    sort() %>% tail(1)
  
  
  
  # Fill in "PREDICTED_WY_TOTAL_PRECIP_INCHES" and "MOST_SIMILAR_WATER_YEAR"
  # using 'predictedTotal' and 'similarWY'
  summaryDF$PREDICTED_WY_TOTAL_PRECIP_INCHES[summaryDF$WATER_YEAR == analysisYear] <- predictedTotal
  summaryDF$MOST_SIMILAR_WATER_YEAR[summaryDF$WATER_YEAR == similarWY] <- TRUE
  
  
  
  # To avoid confusion, set "WY_TOTAL_PRECIP_INCHES" to 'NA' for 'analysisYear'
  summaryDF$WY_TOTAL_PRECIP_INCHES[summaryDF$WATER_YEAR == analysisYear] <- NA_real_
  
  
  
  # Create a spreadsheet with several relevant worksheets
  write_xlsx(list(SIMILAR_WY = summaryDF,
                  LIN_MODEL = data.frame(SLOPE = linModel$m, INTERCEPT = linModel$b),
                  PROCESSED_OUT2_FILE = outDF,
                  USER_INPUT = data.frame(ANALYSIS_YEAR = analysisYear, OUT2_FILEPATH = filePath)),
             paste0("ProcessedData/WY_", analysisYear, "_Similar_Water_Year_Analysis.xlsx"))
  
  
  
  # Output messages to the user
  cat(paste0("The most similar water year to ", analysisYear, " is ", similarWY, "!\n\n"))
  cat(paste0("Saved results to 'ProcessedData/WY_", analysisYear, "_Similar_Water_Year_Analysis.xlsx'\n\n"))
  
  
  
  # Return 'similarWY' afterwards
  return(similarWY)
  
}



updateScripts <- function (analysisYear, similarYear) {
  
  # If 'analysisYear' is the current year, 
  # And the current day is between March 1st and September 30th,
  # update "Dat_PRMS.R" and "Dat_SRP.R" with the similar water year
  
  
  
  # Do nothing if the current year does not match 'analysisYear'
  if (analysisYear != year(Sys.Date())) {
    
    return()
    
  }
  
  
  
  # Do nothing if the current date is not between March 1st and September 30th
  if (month(Sys.Date()) < 3 || month(Sys.Date()) > 9) {
    
    return()
    
  }
  
  
  
  # In another function, update "Dat_PRMS.R" and "Dat_SRP.R"
  changeScriptValue("Scripts/Dat_PRMS.R", similarYear)
  changeScriptValue("Scripts/Dat_SRP.R", similarYear)
  
  
  
  # Output a message to the user
  cat("Successfully updated 'Dat_PRMS.R' and 'Dat_SRP.R' with new similar water year!\n\n")
  
  
  # Return nothing
  return()
  
}



changeScriptValue <- function (filePath, similarYear) {
  
  # Update the variable "waterYearSub" in the DAT script
  # Its new value will be 'similarYear'
  
  
  
  # First confirm that the script exists
  if (!file.exists(filePath)) {
    
    stop(paste0("The script '", filePath, "' does not exist in your repository!\n\n",
                "Automatic script updates failed. Please manually revise the variable ",
                "'waterYearSub' in 'Dat_PRMS.R' and 'Dat_SRP.R' and set its value to ", 
                similarYear))
    
  }
  
  
  
  # Read in the script's code
  scriptCode <- read_lines(filePath)
  
  
  
  # Identify the line of code where "waterYearSub" is defined
  lineToUpdate <- grep("waterYearSub <\\-", scriptCode)
  
  
  
  # Throw an error if the line cannot be found
  if (length(lineToUpdate) == 0) {
    
    stop(paste0("The location where the variable 'waterYearSub' is defined ",
                "could not be located in '", filePath, "'\n\n",
                "Automatic script updates failed. Please manually revise the variable ",
                "'waterYearSub' in 'Dat_PRMS.R' and 'Dat_SRP.R' and set its value to ", 
                similarYear))
    
  # There should only be one line in the code where "waterYearSub" is defined
  # If multiple lines are found, throw an error and ask the user to investigate
  } else if (length(lineToUpdate) > 1) {
    
    stop(paste0("It appears that 'waterYearSub' is defined ",
                "in multiple lines across '", filePath, "'. The line that should be ",
                "updated could not be determined.\n\n",
                "Automatic script updates failed. Please manually revise the variable ",
                "'waterYearSub' in 'Dat_PRMS.R' and 'Dat_SRP.R' and set its value to ", 
                similarYear))
    
  }
  
  
  
  # Update the identified line in 'scriptCode' 
  # Change the year in the script to 'similarYear'
  scriptCode[lineToUpdate] <- scriptCode[lineToUpdate] %>%
    str_replace("<\\- .+$", paste0("<- ", similarYear))
  
  
  
  # Write 'scriptCode' to 'filePath"
  write_lines(scriptCode, filePath)
  
}


#### Script Execution ####


mainProcedure()
