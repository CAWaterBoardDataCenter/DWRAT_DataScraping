# These are generic functions used in multiple processes


#### Dependencies ####

require(data.table)
require(tidyverse)
require(readxl)
require(cli)


#### Functions ####

makeSharePointPath <- function (filePathFragment) {
  
  # Given 'filePathFragment' (most of the filepath), write a complete filepath to the file
  
  # 'filePathFragment' should continue from the SharePoint drive onwards 
  # Everything up to the SharePoint directory name (inclusive) will already be specified by this function
  # The rest of the path is needed as input
  
  # (This function assumes that the SharePoint filepath is "C:/Users/[username]/[Initial SharePoint Path String]/...")
  
  return(paste0("C:/Users/", Sys.info()[["user"]], "/", 
                getFromMasterControl("INITIAL_SHAREPOINT_FILE_PORTION"), 
                filePathFragment) |>
           normalizePath(mustWork = FALSE))
  
}



getFile <- function (filePath, parameterVec = NULL, fileType = NULL, largeFile = FALSE) {
  
  # Given the path to a file, read it into a tibble
  
  # There are several optional arguments as well:
  #   (*) 'parameterVec' is a vector of additional information for reading in the file
  #       (such as the worksheet of a spreadsheet)
  
  #   (*) 'fileType' is the type of the file
  #       It can have these values: "XLSX", "CSV", "DELIM", or "OTHER"
  #       If 'fileType' is NULL, this function will guess the type
  
  #   (*) 'largeFile' is a boolean that applies to "CSV" and "DELIM" files only
  #       If this value is TRUE, fread() from the 'data.table' package will 
  #       be used instead of read_delim() from the 'readr' package
  
  
  # Check if 'fileType' is NULL
  # If so, guess the type
  if (is.null(fileType)) {
    
    fileType <- guessFileType(filePath, parameterVec)
    
  }
  
  
  
  # Make sure 'fileType' is one of the accepted values
  if (!(fileType %in% c("XLSX", "CSV", "DELIM", "OTHER"))) {
    
    stop(paste0("Unknown File Type\n\n",
                "The file type specified in `getFile()` should only be one of ",
                "these strings: ",
                "\"XLSX\", \"CSV\", \"DELIM\", or \"OTHER\"") |>
           strwrap(width = 0.99 * getOption("width")) |>
           paste0(collapse = "\n") |>
           str_replace_all("\"(.+)\"", paste0("\"", col_green("\\1"), "\"")))
    
  }
  
  
  # Next, check if 'filePath' contains a SharePoint path
  # If it is, modify 'filePath' to be a complete SharePoint path
  if (file.exists(makeSharePointPath(filePath))) {
    
    filePath <- makeSharePointPath(filePath)
    
  }
  
  
  # If it is not a SharePoint path, and the file still does not exist,
  # output an error message
  if (!file.exists(filePath)) {
    
    stop(paste0("File Does Not Exist\n\n",
                "The specified file could not be found\n\n",
                "Please confirm that this path is correct: '", 
                normalizePath(filePath, mustWork = FALSE), "'") |>
           strwrap(width = 0.99 * getOption("width")) |>
           paste0(collapse = "\n") |>
           str_replace("(could not be found)", col_red("\\1")))
    
  }
  
  
  # Finally, call different functions to read in the file
  if (fileType == "XLSX") {
    
    return(getXLSX(filePath, parameterVec))
    
  } else if (fileType == "CSV") {
    
    return(getDelim(filePath, ",", largeFile))
    
  } else if (fileType == "DELIM") {
    
    return(getDelim(filePath, parameterVec[1], largeFile))
    
  } else {
    
    # For files labeled as "OTHER", just use read_lines()
    return(read_lines(filePath))
    
  }
  
}



guessFileType <- function (filePath, parameterVec = NULL) {
  
  # Guess the type of file input by the user
  
  # The returned string will be one of these values:
  # "XLSX", "CSV", "DELIM", or "OTHER"
  
  
  # If the filepath ends in something akin to ".xlsx", assume it is a spreadsheet
  if (grepl("\\.xls[xm]?$", filePath, ignore.case = TRUE)) {
    
    return("XLSX")
    
  # If the file extension is ".csv", return "CSV"
  } else if (grepl("\\.csv$", filePath, ignore.case = TRUE)) {
    
    return("CSV")
    
  # If the filepath has a parameter specified in 'parameterVec',
  # and 'parameterVec' contains a single character, assume it is a delimited file
  } else if (!is.null(parameterVec) && length(parameterVec) == 1 &&
             is.character(parameterVec[1]) && nchar(parameterVec[1]) == 1) {
    
    return("DELIM")
    
  # For all other cases, return "OTHER"
  } else {
    
    return("OTHER")
    
  }
  
}



getXLSX <- function (filePath, worksheet = NULL, 
                     range = NULL, col_names = TRUE, col_types = NULL, skip = 0,
                     n_max = Inf, guess_max = min(1000, n_max)) {
  
  # This function is a wrapper for readxl's read_xlsx() function
  
  # It has additional error handling processes
  
  
  sheetDF <- try(read_xlsx(filePath, sheet = worksheet, range = range,
                           col_names = col_names, col_types = col_types,
                           skip = skip, n_max = n_max, guess_max = guess_max), silent = TRUE)
  
  
  if (is.character(sheetDF)) {
    
    # In every case, output the actual error message first
    message(sheetDF)
    
    
    # Next, address different errors with custom messages
    if (grepl("zip file .+ cannot be opened", sheetDF, ignore.case = TRUE)) {
      
      stop(paste0("Inaccessible File Issue\n\n",
                  "The above error message usually occurs if the target ",
                  "spreadsheet is open in Excel.\n\nPlease close '", filePath, 
                  "' and try again.") |>
             strwrap(width = 0.99 * getOption("width")) |>
             paste0(collapse = "\n") |>
             str_replace("(open)", col_red("\\1")) |>
             str_replace("(close)", col_green("\\1")))
      
    } else if (grepl("path. does not exist", sheetDF, ignore.case = TRUE)) {
      
      stop(paste0("File Does Not Exist\n\n",
                  "The above error message usually occurs if the filepath ",
                  "is incorrect.\n\nPlease double-check that '", 
                  normalizePath(filePath, mustWork = FALSE), 
                  "' is a valid path.") |>
             strwrap(width = 0.99 * getOption("width")) |>
             paste0(collapse = "\n") |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else if (grepl("Error in UseMethod..as.cell_limits", sheetDF, ignore.case = TRUE)) {
      
      stop(paste0("Worksheet Name Issue\n\n",
                  "The above error message usually occurs if the specified ",
                  "worksheet name is incorrect.\n\nPlease double-check '", 
                  filePath, "' and verify that the worksheet name '",
                  worksheet, "' is correct.") |>
             strwrap(width = 0.99 * getOption("width")) |>
             paste0(collapse = "\n") |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             strwrap(width = 0.99 * getOption("width")) |>
             paste0(collapse = "\n"))
      
    }
    
  }
  
  
  # If there are no errors, return 'sheetDF'
  return(sheetDF)
  
}



getDelim <- function (filePath, delim, largeFile = FALSE, 
                      select = NULL, col_types = NULL) {
  
  # Use read_delim() or fread() to import a file as a data frame
  
  
  # If 'largeFile' is TRUE, use fread() and import the file as a data frame
  # Otherwise, use read_delim() and read in the file as a tibble
  if (largeFile) {
    
    fileDF <- try(fread(filePath, sep = delim, select = select), silent = TRUE)
    
  } else {
    
    fileDF <- try(read_delim(filePath, delim = delim, 
                             col_types = col_types, show_col_types = FALSE))
    
  }
  
  
  # Check for errors in 'fileDF'
  if (is.character(fileDF)) {
    
    # In every case, output the actual error message first
    message(fileDF)
    
    
    # Next, address different errors with custom messages
    if (grepl("does not exist", fileDF, ignore.case = TRUE)) {
      
      stop(paste0("File Does Not Exist\n\n",
                  "The above error message usually occurs if the filepath ",
                  "is incorrect.\n\nPlease double-check that '", filePath, 
                  "' is a valid path.") |>
             strwrap(width = 0.99 * getOption("width")) |>
             paste0(collapse = "\n") |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             strwrap(width = 0.99 * getOption("width")) |>
             paste0(collapse = "\n"))
      
    }
    
  }
  
  
  # If there are no issues, return 'fileDF'
  return(fileDF)
  
}



getFromMasterControl <- function (fieldName) {
  
  # Extract a value from the main control file for the repository
  # ("Master_Control_File.xlsx")
  
  
  # 'fieldName' should appear in a row under the table's "FIELD" column
  # The corresponding "VALUE" string will be returned
  
  
  # First, read in the primary spreadsheet
  controlDF <- getXLSX("../Master_Control_File.xlsx")
  
  
  # Find a match for 'fieldName' in the "FIELD" column
  if (!(fieldName %in% controlDF[["FIELD"]])) {
    
    stop(paste0("Field Does Not Exist\n\n",
                "'", fieldName, "' does not appear in the 'FIELD' column of the Master ",
                "Control File\n\n",
                "Please ensure that the scripts are up-to-date\n\n",
                "Also, please confirm that the correct version of ",
                "'../Master_Control_File.xlsx' is in use") |>
           strwrap(width = 0.99 * getOption("width")) |>
           paste0(collapse = "\n"))
    
  }
  
  
  # If the control file has a blank entry for this field, notify the user
  # For most fields, this will be an error message
  # SharePoint-related fields will be an exception
  if (is.na(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])) {
    
    # For "INITIAL_SHAREPOINT_FILE_PORTION", it will just be a message
    if (fieldName == "INITIAL_SHAREPOINT_FILE_PORTION") {
      
      # This message will only display once per day
      # It does that using a custom option called "sdaDisplayedSharePointWarning"
      
      # This option's value will either be NULL or a date
      optionRes <- getOption("sdaDisplayedSharePointWarning")
      
      
      # Check if 'optionRes' exists (if not, this is first message of the session)
      # If 'optionRes' does exist, check if the date is earlier than today
      if (is.null(optionRes) || Sys.Date() > optionRes) {
        
        message(paste0("Empty SharePoint Field in Control File\n\n",
                       "SharePoint connectivity is disabled because the corresponding ",
                       "'VALUE' entry for the field '", fieldName, "' is empty\n\n",
                       "Please consider updating '../Master_Control_File.xlsx'\n\n",
                       "\n\n_______\n\n",
                       "(This message will only display once per session/day)") |>
                  strwrap(width = 0.99 * getOption("width")) |>
                  paste0(collapse = "\n"))
        
        
        # After the message has been displayed, update the custom option 
        # with today's date
        options(sdaDisplayedSharePointWarning = Sys.Date())
        
        
        # After that, do not stop the code and allow the function to return 
        # "NA" for "INITIAL_SHAREPOINT_FILE_PORTION"
        
      }
      
      
    # For other SharePoint-related fields, do nothing
    } else if (fieldName %in% c("SHAREPOINT_DEMAND_CONTROL_FILE",
                                "SHAREPOINT_RR_SUPPLY_CONTROL_FILE")) {
      
      # No messages or errors
      # Since these are optional fields, let the regular procedure return NA
      
    } else {
      
      stop(paste0("Empty Field in Control File\n\n",
                  "The corresponding 'VALUE' entry for the field '", fieldName, "' ",
                  "is empty\n\n",
                  "Please update '../Master_Control_File.xlsx'") |>
             strwrap(width = 0.99 * getOption("width")) |>
             paste0(collapse = "\n"))
      
    }
    
  }
  
  
  # Extract a string from the "VALUE" column based on the row where
  # 'fieldName' matches the string in "FIELD"
  return(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])
  
}



getFromSupplyControl_RR <- function (fieldName) {
  
  # Return a value from the RR Supply control file
  
  # The name of the parameter is given in 'fieldName'
  # The "FIELD" column of the spreadsheet should have a matching value
  # Return the corresponding string in the "VALUE" column
  
  
  # The first step is to read in the spreadsheet
  # It can either be a SharePoint version or a local copy
  
  # For SharePoint paths to be usable, both "INITIAL_SHAREPOINT_FILE_PORTION"
  # and "SHAREPOINT_RR_SUPPLY_CONTROL_FILE" must be specified in 
  # "Master_Control_File.xlsx"
  if (!is.na(getFromMasterControl("INITIAL_SHAREPOINT_FILE_PORTION"))) {
    
    # Try and read the SharePoint fragment for the RR Supply control file
    controlPath <- getFromMasterControl("SHAREPOINT_RR_SUPPLY_CONTROL_FILE")
    
    
    # If that value is indeed specified, read it in as 'controlDF'
    if (!is.na(controlPath)) {
      
      controlDF <- controlPath |>
        makeSharePointPath() |>
        getXLSX()
      
    }
    
  }
  
  
  # In all other cases, use the local version of the control file
  if (!exists("controlDF")) {
    
    controlPath <- "InputData/RR_Supply_Control_File.xlsx"
    
    controlDF <- getXLSX(controlPath)
    
  }
  
  
  # Find a match for 'fieldName' in the "FIELD" column
  if (!(fieldName %in% controlDF[["FIELD"]])) {
    
    stop(paste0("Field Does Not Exist\n\n",
                "'", fieldName, "' does not appear in the 'FIELD' column of the ",
                "RR Supply Control File\n\n",
                "Please ensure that the scripts are up-to-date\n\n",
                "Also, please confirm that the correct version of '",
                controlPath, "' is in use") |>
           strwrap(width = 0.99 * getOption("width")) |>
           paste0(collapse = "\n"))
    
  }
  
  
  # If the control file has a blank entry for this field, notify the user
  if (is.na(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])) {
    
    stop(paste0("Empty Field in Control File\n\n",
                "The corresponding 'VALUE' entry for the field '", fieldName, "' ",
                "is empty\n\n",
                "Please update '", controlPath, "'") |>
           strwrap(width = 0.99 * getOption("width")) |>
           paste0(collapse = "\n"))
    
  }
  
  
  # Extract a string from the "VALUE" column based on the row where
  # 'fieldName' matches the string in "FIELD"
  return(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])
  
}



anyFalse <- function (logVec) {
  
  # Given a logical vector, return TRUE if any of these elements are FALSE
  # (This works with single element logical variables too)
  
  return(FALSE %in% logVec)
  
}



twoDigitText <- function (num) {
  
  # This function is called when a number is being written to a string
  # If it has only one digit, a zero will be added to the beginning
  
  return(sprintf("%.2d", num))
  
}
