# These are generic functions used in multiple processes


#### Dependencies ####

# Import packages
source("Scripts/HLP_000_Load_Packages.R")


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
  
  
  # First make sure that 'filePath' is not NA
  # This is a sign of a missing input
  if (is.na(filePath)) {
    
    stop(paste0("No Filepath Specified\n\n",
                "The function `getFile` was called without a proper filepath as ",
                " input\n\n", 
                "Please investigate this issue and revise the script, ",
                "if needed") |>
           errWrap() |>
           str_replace("(without)", col_red("\\1")))
    
  }
  
  
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
           errWrap() |>
           str_replace_all("\"(.+)\"", paste0("\"", col_green("\\1"), "\"")))
    
  }
  
  
  # Next, check if 'filePath' contains a SharePoint path
  # If it is, modify 'filePath' to be a complete SharePoint path
  filePath <- sharepointPathCheck(filePath, isFolder = FALSE)
  
  
  # If it is not a SharePoint path, and the file still does not exist,
  # output an error message
  if (!file.exists(filePath)) {
    
    stop(paste0("File Does Not Exist\n\n",
                "The specified file could not be found\n\n",
                "Please confirm that this path is correct: '", 
                normalizePath(filePath, mustWork = FALSE), "'") |>
           errWrap() |>
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



sharepointPathCheck <- function (path, isFolder = FALSE) {
  
  # Check if a file/folder is a SharePoint file/folder
  
  # Use its path ('path') to make this assessment
  
  # If yes, return 'path' as a full SharePoint path
  # Otherwise, just return the path as-is
  
  
  # 'isFolder' is TRUE if the input path is for a folder
  # Otherwise, it should be FALSE for files
  
  
  # Make a SharePoint version of 'path'
  sharepointPath <- makeSharePointPath(path)
  
  
  # Based on 'isFolder', use either `file.exists` or `dir.exists`
  if (isFolder) {
    
    # Check if the folder exists on SharePoint
    if (dir.exists(sharepointPath)) {
      
      # If yes, return the SharePoint path
      return(sharepointPath)
      
    }
    
  } else {
    
    # Check if the file exists on SharePoint
    if (file.exists(sharepointPath)) {
      
      # If yes, return the SharePoint path
      return(sharepointPath)
      
    }
    
  }
  
  
  # If the procedure reaches this point, the file/folder is NOT on SharePoint
  # In that case, return 'path' without any changes
  return(path)
  
}



getXLSX <- function (filePath, worksheet = NULL, 
                     range = NULL, col_names = TRUE, col_types = NULL, skip = 0,
                     n_max = Inf, guess_max = min(1000, n_max)) {
  
  # This function is a wrapper for readxl's read_xlsx() function
  
  # It has additional error handling processes
  
  
  # First, make sure 'filePath' is a character variable
  if (!is.character(filePath)) {
    
    stop(paste0("Unusable File Path\n\n",
                "The provided file path is not a character variable.\n\n",
                "Please double-check that '", filePath, "' is a valid path. ",
                "Script revisions may be necessary.") |>
           errWrap() |>
           str_replace("(incorrect)", col_red("\\1")))
    
  }
  
  
  sheetDF <- try(read_xlsx(filePath, sheet = worksheet, range = range,
                           col_names = col_names, col_types = col_types,
                           skip = skip, n_max = n_max, guess_max = guess_max), silent = TRUE)
  
  
  if ("try-error" %in% class(sheetDF)) {
    
    # In every case, output the actual error message first
    message(sheetDF)
    
    
    # Next, address different errors with custom messages
    if (grepl("zip file .+ cannot be opened", sheetDF, ignore.case = TRUE)) {
      
      # There are at least two different situations where this error can occur
      
      # This error can occur when trying to read in a file that is NOT 
      # an Excel spreadsheet
      if (!grepl("\\.xls.?$", filePath)) {
        
        stop(paste0("Incorrect File Issue\n\n",
                    "The input file '", filePath, "' does not appear to be ",
                    "an Excel spreadsheet. Please investigate.") |>
               errWrap())
        
      # Alternatively, it can happen when trying to read a SharePoint 
      # spreadsheet that is already open locally
      } else {
        
        stop(paste0("Inaccessible File Issue\n\n",
                    "The above error message usually occurs if the target ",
                    "spreadsheet is open in Excel.\n\nPlease close '", filePath, 
                    "' and try again.") |>
               errWrap() |>
               str_replace("(open)", col_red("\\1")) |>
               str_replace("(close)", col_green("\\1")))
        
      }
      
    } else if (grepl("path. does not exist", sheetDF, ignore.case = TRUE)) {
      
      stop(paste0("File Does Not Exist\n\n",
                  "The above error message usually occurs if the filepath ",
                  "is incorrect.\n\nPlease double-check that '", 
                  normalizePath(filePath, mustWork = FALSE), 
                  "' is a valid path.") |>
             errWrap() |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else if (grepl("Error in UseMethod..as.cell_limits", sheetDF, ignore.case = TRUE)) {
      
      stop(paste0("Worksheet Name Issue\n\n",
                  "The above error message usually occurs if the specified ",
                  "worksheet name is incorrect.\n\nPlease double-check '", 
                  filePath, "' and verify that the worksheet name '",
                  worksheet, "' is correct.") |>
             errWrap() |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             errWrap())
      
    }
    
  }
  
  
  # If there are no errors, return 'sheetDF'
  return(sheetDF)
  
}



getDelim <- function (filePath, delim, largeFile = FALSE, 
                      select = NULL, col_types = NULL, skip = 0,
                      trim_ws = FALSE) {
  
  # Use read_delim() or fread() to import a file as a data frame
  
  
  # First, make sure 'filePath' is a character variable
  if (!is.character(filePath)) {
    
    stop(paste0("Unusable File Path\n\n",
                "The provided file path is not a character variable.\n\n",
                "Please double-check that '", filePath, "' is a valid path. ",
                "Script revisions may be necessary.") |>
           errWrap() |>
           str_replace("(incorrect)", col_red("\\1")))
    
  }
  
  
  # If 'largeFile' is TRUE, use fread() and import the file as a data frame
  # Otherwise, use read_delim() and read in the file as a tibble
  if (largeFile) {
    
    fileDF <- try(fread(filePath, sep = delim, select = select,
                        strip.white = trim_ws), silent = TRUE)
    
  } else {
    
    fileDF <- try(read_delim(filePath, delim = delim, 
                             col_types = col_types, show_col_types = FALSE,
                             skip = skip, trim_ws = trim_ws))
    
  }
  
  
  # Check for errors in 'fileDF'
  if ("try-error" %in% class(fileDF)) {
    
    # In every case, output the actual error message first
    cat("\n\n")
    print(fileDF)
    cat("\n\n")
    
    
    # Next, address different errors with custom messages
    if (grepl("does not exist", fileDF, ignore.case = TRUE)) {
      
      stop(paste0("File Does Not Exist\n\n",
                  "The above error message usually occurs if the filepath ",
                  "is incorrect.\n\nPlease double-check that '", filePath, 
                  "' is a valid path.") |>
             errWrap() |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             errWrap())
      
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
           errWrap())
    
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
                  errWrap())
        
        
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
             errWrap())
      
    }
    
  }
  
  
  # Extract a string from the "VALUE" column based on the row where
  # 'fieldName' matches the string in "FIELD"
  return(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])
  
}



getFromControl_RR <- function (fieldName) {
  
  # Return a value from the RR Workflow control file
  
  # The name of the parameter is given in 'fieldName'
  # The "FIELD" column of the spreadsheet should have a matching value
  # Return the corresponding string in the "VALUE" column
  
  
  # The first step is to read in the spreadsheet
  # It can either be a SharePoint version or a local copy
  
  # For SharePoint paths to be usable, both "INITIAL_SHAREPOINT_FILE_PORTION"
  # and "SHAREPOINT_RR_WORKFLOW_CONTROL_FILE" must be specified in 
  # "Master_Control_File.xlsx"
  if (!is.na(getFromMasterControl("INITIAL_SHAREPOINT_FILE_PORTION"))) {
    
    # Try and read the SharePoint fragment for the RR Worfklow control file
    controlPath <- getFromMasterControl("SHAREPOINT_RR_WORKFLOW_CONTROL_FILE")
    
    
    # If that value is indeed specified, read it in as 'controlDF'
    if (!is.na(controlPath)) {
      
      controlDF <- controlPath |>
        makeSharePointPath() |>
        getXLSX()
      
    }
    
  }
  
  
  # In all other cases, use the local version of the control file
  if (!exists("controlDF")) {
    
    controlPath <- "InputData/RR_Workflow_Control_File.xlsx"
    
    controlDF <- getXLSX(controlPath)
    
  }
  
  
  # Find a match for 'fieldName' in the "FIELD" column
  if (!(fieldName %in% controlDF[["FIELD"]])) {
    
    stop(paste0("Field Does Not Exist\n\n",
                "'", fieldName, "' does not appear in the 'FIELD' column of the ",
                "RR Workflow Control File\n\n",
                "Please ensure that the scripts are up-to-date\n\n",
                "Also, please confirm that the correct version of '",
                controlPath, "' is in use") |>
           errWrap())
    
  }
  
  
  # If the control file has a blank entry for this field, notify the user
  if (is.na(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])) {
    
    stop(paste0("Empty Field in Control File\n\n",
                "The corresponding 'VALUE' entry for the field '", fieldName, "' ",
                "is empty\n\n",
                "Please update '", controlPath, "'") |>
           errWrap())
    
  }
  
  
  # Extract a string from the "VALUE" column based on the row where
  # 'fieldName' matches the string in "FIELD"
  return(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])
  
}



writeOutput <- function (x, outPath, writeFunction = "write_csv", quietly = FALSE,
                         col_names = TRUE) {
  
  # Write a variable 'x' to 'outPath'
  
  # Use "write_csv", "write_tsv", or "write_lines" depending on the specification in 'writeFunction'
  
  # 'quietly' is a boolean for whether an output message will be given
  
  # If 'col_names' is TRUE, column names will be written in the output for 
  # "write_csv" and "write_tsv"
  
  
  # If 'writeFunction' is "write_csv" or a similar function, 'x' has to be a data frame
  if (!is.data.frame(x) && writeFunction %in% c("write_csv", "write_tsv", "write_xlsx")) {
    
    stop(paste0("Improper Input For `writeOutput()`\n\n",
                "If `write_csv`, `write_tsv`, or `write_xlsx` will be ",
                "called to write this output, ",
                "the input variable has to be a data frame. Please revise ",
                "the procedure.\n\n",
                "(Note: Nothing was written to \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Try to apply the file writing functions next
  if (writeFunction == "write_csv") {
    
    writeRes <- try(write_csv(x, outPath, col_names = col_names))
    
  } else if (writeFunction == "write_lines") {
    
    writeRes <- try(write_lines(x, outPath))
    
  } else if (writeFunction == "write_xlsx") {
    
    writeRes <- try(write_xlsx(x, outPath, col_names = col_names))
    
  } else if (writeFunction == "write_tsv") {
    
    writeRes <- try(write_tsv(x, outPath, col_names = col_names))
    
  } else {
    
    stop(paste0("Improper Input For `writeOutput()`\n\n",
                "\"", writeFunction, "\" is not a recognized value for ",
                "the function argument 'writeFunction'. Please revise it.\n\n",
                "\"write_csv\", \"write_tsv\", \"write_xlsx\", and ",
                "\"write_lines\" are the only acceptable values.\n\n",
                "(Note: Nothing was written to \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Check for any errors in the process
  if ("try-error" %in% class(writeRes)) {
    
    # Output the actual error message first
    message(writeRes)
    
    
    if (grepl("Cannot open file for writing", writeRes, ignore.case = TRUE)) {
      
      stop(paste0("Inaccessible File Issue\n\n",
                  "The above error message usually occurs if the file already ",
                  "exists and is open in a program like Excel. This prevents ",
                  "the script from overwriting the file.\n\nPlease close '", 
                  filePath, "' and try again.") |>
             errWrap() |>
             str_replace("(open)", col_red("\\1")) |>
             str_replace("(close)", col_green("\\1")))
      
    # In all other cases, output a custom acknowledgement 
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             errWrap())
      
    }

  }
  
  
  # As a penultimate step, confirm that the output file was generated
  if (!file.exists(outPath)) {
    
    stop(paste0("File Output Failed\n\n",
                "The output file was not detected in the expected location. ",
                "`", writeFunction, "` may have failed, please investigate ",
                "this issue.\n\n",
                "(Note: Nothing was written to \"", outPath, "\")") |>
           errWrap() |>
           str_replace("(not)", col_red("\\1")) |>
           str_replace("(investigate)", col_green("\\1")))
    
  }
  
  
  # Output a message to the user about the result (if 'quietly' is FALSE)
  if (!quietly) {
    
    cat(paste0("\nWrote data to \"", normalizePath(outPath), "\"!\n\n") |>
          col_cyan())
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



copyFile <- function (from, to, overwrite = TRUE, quietly = FALSE) {
  
  # Copy a file to a new location
  
  # This is a wrapper for `file.copy` with a custom error message
  
  
  # First, confirm that the file 'from' actually exists
  if (!file.exists(from)) {
    
    paste0("Attempting to Copy Non-Existent File\n\n",
           "The script attempted to copy a file (\"", from, "\") to a new ",
           "location. However, the original file does not appear to exist! ",
           "Please investigate.\n\n",
           "The intended new file was: \"", to, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there is no issue with 'from', attempt to copy it
  copyRes <- file.copy(from = from, to = to, overwrite = overwrite)
  
  
  # Confirm that the file was copied successfully
  if (!copyRes || !file.exists(to)) {
    
    paste0("Could Not Copy File\n\n",
           "The script attempted to copy a file (\"", from, "\") to a new ",
           "location. However, the processed failed for an unknown reason ",
           "(possibly a permission issue). Please investigate.\n\n",
           "The intended new file was: \"", to, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Output a message too if 'quietly' is FALSE
  if (!quietly) {
    
    cat(paste0("Copied \"", from, "\" to \"", to, "\"!\n\n") |>
          col_cyan())
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



errWrap <- function (message, widthRatio = 0.99) {
  
  # Modify the wrapping of an error message to reduce the need for horizontal scrolling
  return(message |>
           strwrap(width = widthRatio * getOption("width")) |>
           paste0(collapse = "\n"))
  
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



getModeledWY <- function (endDate) {
  
  # Based on the value of 'endDate', identify the water year being modeled
  # Then, return a vector of dates containing the bounds of that water year
  
  
  # (1) Determining the starting bound:
  
  #     If 'endDate' is between January and September (inclusive), the water year
  #     matches the calendar year in 'endDate'
  
  #     If that is true, then the water year starts on October 1st of the 
  #     preceding calendar year
  
  #     (e.g., if it's 2026-03-09, we are modeling WY2026, which began on 
  #      2025-10-01)
  
  #     If 'endDate' is in the October - December range, the water year is the
  #     calendar year plus one
  
  #     In addition, the start of the water year is October 1st of the same year
  #     as 'endDate'
  
  #     (e.g., if it's 2024-12-12, we are modeling WY2025, which began on 
  #      2024-10-01)
  
  
  # (2) Determining the ending bound:
  
  #     If 'endDate' is between January and September (inclusive), the water year
  #     matches the calendar year in 'endDate'
  
  #     If that is true, then the water year ends on September 30th of the 
  #     current calendar year
  
  #     (e.g., if it's 2026-03-09, we are modeling WY2026, which ends on 
  #      2026-09-30)
  
  #     If 'endDate' is in the October - December range, the water year is the
  #     calendar year plus one
  
  #     In addition, the end of the water year is September 30th of the next year
  #     after the year in 'endDate'
  
  #     (e.g., if it's 2024-12-12, we are modeling WY2025, which ends on 
  #      2025-09-30)
  
  
  # Get the start and end dates of the water year
  # Then, return them as a vector of dates
  return(c(start = if_else(month(endDate) < 10,
                           paste0(year(endDate) - 1, "-10-01"),
                           paste0(year(endDate), "-10-01")),
           end = if_else(month(endDate) < 10,
                         paste0(year(endDate), "-09-30"),
                         paste0(year(endDate) + 1, "-09-30"))) |>
           as.Date(format = "%Y-%m-%d"))
  
}



vec2QuotedStr <- function (strVec) {
  
  # Given a vector of strings, wrap each element in quotation marks
  # Then, return them in a single string as a list
  
  # Example Strings:
  # '"Element 1", "Element 2", and "Element 3"'
  # '"Element 1" and "Element 2"'
  # '"Element 1"'
  
  
  # First, add quotation marks to each element
  strVec <- paste0("\"", strVec, "\"")
  
  
  # If 'strVec' has only one element, return the string 
  # without any further changes
  if (length(strVec) == 1) {
    return(strVec)
  }
  
  
  # If 'strVec' has only two elements, return a string 
  # with the elements separated by " and "
  if (length(strVec) == 2) {
    
    return(paste0(strVec, collapse = " and "))
    
  }
  
  
  # If 'strVec' has 3 or more elements, separate the elements with commas
  # However, the final element should also have "and" after the comma
  if (length(strVec) > 2) {
    
    strVec[length(strVec)] <- paste0("and ", strVec[length(strVec)])
    
    
    return(paste0(strVec, collapse = ", "))
    
  }
  
}



read_out2 <- function (outPath) {
  
  # Given the path to a .out2 file, 
  # read it in and format it as a proper tibble
  
  # The data is space-delimited
  
  # However, the column headers are trapped between rows of metadata
  
  # In addition, sometimes, data entries don't have any spaces between them
  # (generally it can happen when there's a negative number)
  
  
  # First, use read_lines() to read in the file
  outDF <- read_lines(outPath)
  
  
  # Remove empty strings from the vector
  outDF <- outDF |> 
    str_subset("^$", negate = TRUE)
  
  
  # Find the row containing the headers
  headerRow <- grep("Year\\s+mo", outDF, ignore.case = TRUE)
  
  
  if (length(headerRow) != 1) {
    
    stop(paste0("Could Not Find Header Row of Out2 File\n\n",
                "The header was expected to be a line that uniquely starts with ",
                "\"Year\", followed by spaces, and then \"mo\". However ",
                length(headerRow), " matches were found with this pattern. ",
                "Please investigate the file and update the script if ",
                "needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Remove the rows before 'headerRow' in 'outDF'
  outDF <- outDF[headerRow:length(outDF)]
  
  
  # Get a vector of the header names
  columnNames <- outDF[1] |>
    spaceSplit()
  
  
  # If the second row contains units for the columns,
  # append them to 'columnNames'
  if (grepl("\\s+\\(in\\)\\s+", outDF[2], ignore.case = TRUE)) {
    
    unitVec <- outDF[2] |>
      spaceSplit() |>
      str_split("[\\(\\)]") |> unlist() |>
      str_subset("^$", negate = TRUE)
    
    
    # The "Year", "mo", and "day" variables at the beginning do not have any units
    # Add three empty strings to the start of 'unitVec'
    unitVec <- c("", "", "",
                 unitVec)
    
    
    if (length(unitVec) != length(columnNames)) {
      
      stop(paste0("Could Not Assign Units in Out2 File\n\n",
                  "There are ", length(columnNames), " columns in this file, ",
                  "and units were detected in the second row. However, they ",
                  "could not be properly matched to their corresponding ",
                  "headings. Please investigate the file and update the ",
                  "script if needed.\n\n",
                  "(This issue occurred for \"", outPath, "\")") |>
             errWrap())
      
    }
    
    
    # Paste these units at the end of 'columnNames'
    columnNames <- map2_chr(columnNames, unitVec,
                            ~ if_else(.y == "", .x, paste0(.x, " (", .y, ")")))
    
  }
  
  
  # The last non-data row contains the value "initial"
  # Find its index
  removalIndex <- grep("^\\s*initial", outDF)
  
  
  if (length(removalIndex) != 1) {
    
    stop(paste0("Could Not Find Data Cutoff Row of Out2 File\n\n",
                "The non-data rows were expected to end with a line that ",
                "uniquely starts with \"initial\" (and maybe some spaces at ",
                "the beginning). However, ", length(removalIndex), " matches ",
                "were found with this pattern. Please investigate the file ",
                "and update the script if needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Remove all rows up to 'removalIndex'
  outDF <- outDF[-c(1:removalIndex)]
  
  
  # Remove the "Execution elapsed time" row at the end as well
  outDF <- outDF |>
    str_subset("^\\s*Execution elapsed time", negate = TRUE)
  
  
  # The only rows left in 'outDF' should be the data now
  # Within each row, split the data at the spaces
  outDF <- outDF |>
    map(spaceSplit)
  
  
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
      
      # Issue Type #1
      # One potential error comes from having a number followed by 
      # a negative number with no space in-between
      if (sum(grepl("^\\-?[0-9\\.]+\\-[0-9\\.]+$", outDF[[problemRows[i]]])) > 0) {
        
        # Iterate through the entries in this row of 'outDF'
        for (j in 1:length(outDF[[problemRows[i]]])) {
          
          # If this is a row with both a number and a negative number, separate them
          if (grepl("^\\-?[0-9\\.]+\\-[0-9\\.]+$", outDF[[problemRows[i]]][j])) {
            
            # Use a positive look-ahead regex to split the numbers 
            # (while preserving the negative sign)
            outDF[[problemRows[i]]][j] <- outDF[[problemRows[i]]][j] |>
              str_split("(?=\\-)")
            
          }
          
        }
        
        
        # Splitting operations within the list element may create a sub-list there
        # Remove any sub-lists
        outDF[[problemRows[i]]] <- outDF[[problemRows[i]]] |>
          unlist() |> 
          str_subset("^$", negate = TRUE)
        
      } # End of Issue Type #1 resolution
      
    } # End of loop through 'problemRows'
    
  }
  
  
  # Make sure that every row has the proper length now
  rowLens <- lengths(outDF)
  
  
  # Output an error if there are still issues
  if (unique(rowLens) |> length() != 1) {
    
    problemRows <- which(rowLens != length(columnNames))
    
    stop(paste0("Problematic Data in the Out2 File\n\n",
                "Each row is expected to have ", length(columnNames),
                " values. However, ", length(problemRows), " row",
                if_else(length(problemRows) > 1, "s have ", " has "),
                "an issue: \n\n", 
                outDF[problemRows] |>
                  map_chr(~ paste0(., collapse = " ")) |>
                  vec2QuotedStr(), 
                "\n\n",
                "Please investigate the file and update the script if ",
                "needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  } else if (unique(rowLens) != length(columnNames)) {
    
    stop(paste0("Problematic Data in the Out2 File\n\n",
                "Each row is expected to have ", length(columnNames),
                " values. However, every row has ", unique(rowLens),
                if_else(unique(rowLens) > 1, " entries", " entry"), ". ",
                "Please investigate the file and update the script if ",
                "needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Convert 'outDF' into a proper tibble
  outDF <- outDF |>
    unlist() |> as.numeric() |>
    matrix(ncol = length(columnNames), byrow = TRUE) |>
    data.frame() |> tibble() |>
    set_names(columnNames)
  
  
  # After these changes, return 'outDF'
  return(outDF)
  
}



spaceSplit <- function (str) {
  
  # Split a string at spaces
  # Remove empty strings and return the string
  return(str|>
           str_split("\\s") |> unlist(use.names = FALSE) |>
           str_subset("^$", negate = TRUE))
  
}



getPRISM <- function (prismPath) {
  
  # Read in the PRISM web data CSV
  
  # Because multiple lines serve as a header, first find the number of header lines
  # Then, use the proper CSV processing function to 
  
  
  prismVec <- getFile(prismPath, fileType = "OTHER")
  
  
  # Get the line where the headers start
  
  # The line will start with "Name,Longitude,Latitude"
  headerRegex <- "^Name,Longitude,Latitude"
  
  
  headerLine <- grep(headerRegex, prismVec)
  
  
  # Return an error if 'headerLine' was not found (or if multiple matches were found)
  if (length(headerLine) == 0) {
    
    stop(paste0("PRISM Data File - Missing Column Header Issue\n\n", 
                "This script attempted to find the header row in the PRISM ",
                "CSV file containing climate data. However, the header row ",
                "could not be found.\n\n",
                "There could be data corruption issues, or the formatting of ",
                "PRISM's output files may have changed. This script may need ",
                "updates, depending on the cause of this problem.\n\n",
                "Please investigate '", prismPath, "'") |>
           errWrap())
    
  } else if (length(headerLine) > 1) {
    
    stop(paste0("PRISM Data File - Could Not Identify Column Header\n\n", 
                "This script attempted to find the header row in the PRISM ",
                "CSV file containing climate data. However, an unusual issue ",
                "was encountered.\n\n",
                "The header row is usually identified via this regular expression:\n\n",
                "(*) \"", headerRegex, "\"\n\n",
                "There should be exactly one row in the input file that has this ",
                "pattern. However, more than one match was found.\n\n", 
                "Please investigate '", prismPath, "'") |>
           errWrap())
    
  }
  
  
  # If there are no issues, read in the PRISM data again
  # This time, skip lines so that the first row is the header row (identified by 'headerLine')
  prismDF <- getDelim(prismPath, delim = ",", skip = headerLine - 1)
  
  
  # Make sure no rows that only contain "NA" are present
  # Use the "Name" column for that check
  prismDF <- prismDF |>
    filter(!is.na(Name))
  
  
  # Also, confirm that "Date" was parsed as a date column correctly
  # If not, apply that type manually
  if (is.character(prismDF$Date[1])) {
    
    prismDF <- prismDF |>
      mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
    
  }
  
  
  # Return 'prismDF'
  return(prismDF)
  
}



read_gag <- function (gagPath) {
  
  # Read in a ".gag" file as a tibble
  # (These files are outputs from SRP)
  
  
  # Read in the lines of the file
  gagVec <- getFile(gagPath, fileType = "OTHER")
  
  
  # GAG files start with metadata
  
  # Find the actual headers using the "DATA" text string 
  # that starts the header row
  headerRegex <- "\"DATA:"
  
  
  headerLine <- grep(headerRegex, gagVec)
  
  
  # Return an error if 'headerLine' was not found (or if multiple matches were found)
  if (length(headerLine) == 0) {
    
    paste0("GAG Data File - Missing Column Header Issue\n\n", 
           "This script attempted to find the header row in an SRP ",
           "GAG output file. However, the header row could not be ",
           "found.\n\n",
           "There could be data corruption issues, or the formatting of ",
           "the GAG files may have changed. This script may need ",
           "updates, depending on the cause of this problem.\n\n",
           "Please investigate \"", gagPath, "\"") |>
      errWrap() |>
      stop()
    
  } else if (length(headerLine) > 1) {
    
    paste0("GAG Data File - Could Not Identify Column Header\n\n", 
           "This script attempted to find the header row in an SRP ",
           "GAG output file. However, an unusual issue was ",
           "encountered.\n\n",
           "The header row is usually identified via this regular ",
           "expression:\n\n",
           "(*) \"", headerRegex, "\"\n\n",
           "There should be exactly one row in the input file that has ",
           "this pattern. However, more than one match was found.\n\n", 
           "Please investigate \"", gagPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, remove the metadata from 'gagVec'
  gagVec <- gagVec[headerLine:length(gagVec)]
  
  
  # The actual data of the GAG file is stored in a fixed-width format
  # Remove 'headerRegex' and any quotation marks in the dataset 
  # Then, break apart rows at the spaces
  gagDF <- gagVec |>
    str_remove(headerRegex) |>
    str_remove_all("\"") |>
    trimws() |>
    str_split("\\s+") |> unlist()
    
  
  # Make sure that the length of 'gagDF' is divisible by the length of 'gagVec'
  # If there is no remainder, that means that an equal number of columns 
  # were detected in each row of 'gagVec'
  if (length(gagDF) %% length(gagVec) != 0) {
    
    paste0("GAG Data File - Data Parsing Issue\n\n", 
           "This script attempted to split each row of data in an SRP ",
           "GAG output file. However, a consistent number of columns ",
           "per row could not be identified\n\n",
           "Please investigate \"", gagPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Convert 'gagDF' into a matrix and then a data frame
  gagDF <- gagDF |>
    matrix(nrow = length(gagVec), byrow = TRUE) |>
    data.frame()
  
  
  # Use the first row of 'gagDF' as headers
  # Then, reformat 'gagDF' into a tibble
  gagDF <- gagDF[-1, ] |>
    set_names(gagDF[1, ] |> unlist(use.names = FALSE)) |>
    tibble()
  
  
  # Finally, convert columns in 'gagDF' into numeric if they contain numbers
  for (j in 1:ncol(gagDF)) {
    
    # Check if at least 90% of a column match this regular expression
    # If yes, convert the column into numeric
    if (sum(grepl("^-?[0-9]+(\\.[0-9]+)?([Ee][+-][0-9]+)?$", gagDF[[j]])) > 
        0.90 * nrow(gagDF)) {
      
      gagDF[[j]] <- gagDF[[j]] |> as.numeric()
      
    }
    
    # Explanation of the regex: 
    # "^-?[0-9]+(\\.[0-9]+)?([Ee][+-][0-9]+)?$"
    
    #  (*) The string may start with a minus sign ("-")
    #  (*) The string contains some number of digits (1 or more)
    #  (*) The string may contain a decimal point, followed by more digits
    #  (*) The string may end with scientific notation 
    #      ("e" followed by a plus or minus, and then one or more digits)
    
  }
  
  
  # Return 'gagDF'
  return(gagDF)
  
}



updateMetadataCSV <- function (dirPath, newCols, filename = "metadata.csv") {
  
  # Update a CSV file containing metadata 
  # Add new columns to it using 'newCols'
  
  # 'newCols' should be a list containing named elements
  
  
  # Start by validating 'newCols'
  # Every element should have a name
  if (is.null(names(newCols)) || "" %in% names(newCols)) {
    
    cat("\n\nNames in 'newCols':\n")
    print(names(newCols))
    
    
    paste0("Improper Value for 'newCols'\n\n",
           "The function `updateMetadataCSV` requires a named list of new ",
           "columns to add to the metadata table. However, the input 'newCols' ",
           "does not have names for each element in the list.\n\n",
           "Please correct this issue and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # In addition, every entry in 'newCols' should only have one element each
  # (One value for each new column)
  if (anyFalse(lengths(newCols) == 1)) {
    
    cat("\n\nNumber of Elements per Entry in 'newCols':\n")
    print(lengths(newCols))
    
    
    paste0("Improper Value for 'newCols'\n\n",
           "The function `updateMetadataCSV` requires a named list of new ",
           "columns to add to the metadata table. Each element of this list ",
           "should have only one value. However, the input 'newCols' does not ",
           "abide by this requirement.\n\n",
           "Please correct this issue and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, read in the metadata CSV
  metaPath <- paste0(dirPath, "/", filename) |>
    normalizePath(mustWork = FALSE)
  
  
  # The metadata file should be a CSV file
  metaDF <- getDelim(metaPath, ",")
  
  
  # Add 'newCols to 'metaDF'
  metaDF[names(newCols)] <- newCols
  
  
  # Save 'metaDF'
  writeOutput(metaDF, metaPath, "write_csv", quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getGitHash <- function () {
  
  # Generate a temporary batch file that calls "git rev-parse"
  # to get the short hash of the current commit in the repository
  
  # Obtain the hash string from that command and return it
  
  
  # First, start by generating a temporary batch file
  # Save it into the current repository location
  tempName <- "temp_git_check.bat"
  
  
  c("git rev-parse --short HEAD",
    "exit") |>
    writeOutput(tempName, "write_lines", quietly = TRUE)
  
  
  # Execute the batch file
  hashRes <- system(tempName, intern = TRUE)
  
  
  # The hash will be located on a line by itself after the "rev-parse" call
  hashLoc <- grep("rev-parse", hashRes)[1] + 1
  
  
  # Check for errors at this point
  if (length(hashLoc) == 0 || is.na(hashLoc) || 
      any(grepl("Error", hashRes, ignore.case = TRUE))) {
    
    print(hashRes)
    
    paste0("Git Call Failed\n\n",
           "An error occurred when running a batch script that contanied ",
           "a git command (\"", normalizePath(tempName, mustWork = TRUE), 
           "\"). The results were printed above. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If no issues occurred, extract the hash string from 'hashRes'
  hashRes <- hashRes[hashLoc]
  
  
  # Finally, delete the temporary batch file
  unlink(tempName)
  
  
  # Return the short commit hash
  return(hashRes)
  
}



detectAnacondaBat <- function () {
  
  # Locate an installation of "Anaconda" on the user's device
  # Get the path to "activate.bat", which is located under "Scripts"
  
  # Return that path as a string to the user
  
  
  # First, check for for "Anaconda" in the "ProgramData" folder
  anacondaInstallation <- list.files("C:/ProgramData", pattern = "[Aa]naconda",
                                     full.names = TRUE) |>
    sort() |> tail(1)
  
  
  # If no match was found, throw an error
  if (length(anacondaInstallation) == 0) {
    
    paste0("Anaconda Not Found\n\n",
           "This procedure requires an installation of Anaconda. However, ",
           "the program was not found. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Otherwise, look for "activate.bat"
  batPath <- paste0(anacondaInstallation, "/Scripts/activate.bat") |>
    normalizePath(mustWork = FALSE)
  
  
  # Confirm that 'batPath' exists
  if (!file.exists(batPath)) {
    
    paste0("Anaconda Prompt Batch File Not Found\n\n",
           "This procedure executes Python scripts using Anaconda Prompt. ",
           "However, the required batch file was not found in \"", 
           anacondaInstallation, "\". Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If no issues were encountered, return 'batPath'
  return(batPath)
  
}



detectRScriptExe <- function () {
  
  # Look for "Rscript.exe" in the active installation of R
  # (It should be in the "bin" folder)
  
  # Return a path to that exe file
  
  
  # Check the expected location of "Rscript.exe"
  exePath <- paste0(R.home(), "/bin/Rscript.exe") |>
    normalizePath(mustWork = FALSE)
  
  
  # Confirm that 'exePath' exists
  if (!file.exists(exePath)) {
    
    paste0("RScript.Exe Not Found\n\n",
           "This procedure requires the executable version of R. However, \"",
           exePath, "\" did not point to a valid file. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'exePath' if there are no issues
  return(exePath)
  
}



installAnacondaEnv <- function (batPath, envPath) {
  
  # Given the path to an Anaconda installation's "activate.bat" file,
  # install a new environment using the file referenced in 'envPath'
  
  
  # Double-check that 'envPath' exists
  if (!file.exists(envPath)) {
    
    paste0("Environment File Not Found\n\n",
           "The input variable 'envPath' (", envPath, ") is invalid. It ",
           "does not point to a real file. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Create a new Anaconda environment using the requirements in 'envPath'
  envRes <- system(paste0(shQuote(batPath), " && ",
                          "conda env create -f ", shQuote(envPath)), 
                   intern = TRUE)
  
  
  # The output of the environment creation command is stored in 'envRes'
  # Check for process errors using this variable
  
  
  # If the final "To activate this environment, use..." message does not
  # appear in 'envRes', that means that the environment was NOT created
  # successfully
  if (!any(grepl("To activate this environment, use", envRes))) {
    
    cat("\n\n")
    print(envRes)
    cat("\n\n")
    
    
    # If an authentication error occurs and a token is required, 
    # perform that update automatically here
    if (any(grepl("AnacondaAuthError", envRes)) &&
        any(grepl("anaconda token install", envRes))) {
      
      cat("\n\n")
      paste0("It appears that a token is required! Attempting install...") |>
        errWrap() |>
        str_replace("Attempting install...", col_green("Attempting install...")) |>
        cat()
      cat("\n\n")
      
      
      # Create a temporary batch file to initiate this process
      tokenBat <- "temp-token.bat"
      
      
      paste0("echo y | (", shQuote(batPath),  " && anaconda token install)") |>
        writeOutput(tokenBat, "write_lines", quietly = TRUE)
      
      # "echo y" provides the "yes" input needed to confirm installation
      # of the token
      
      
      # Try to install the token
      tokenRes <- system(tokenBat, intern = TRUE)
      
      
      # Output the results of the attempt
      cat("\n\n")
      print(tokenRes)
      cat("\n\n")
      
      
      # Delete the batch file next
      unlink(tokenBat)
      
      
      # If the process is successful, retry this function
      if (any(grepl("Success!", tokenRes))) {
        
        paste0("The token was installed successfully! Retrying the ",
               "environment creation process...") |>
          errWrap() |>
          cat()
        cat("\n\n")
        
        
        # Recursively call this function
        return(installAnacondaEnv(batPath, envPath))
        
      # If the token installation process failed, simply return an error 
      } else {
        
        paste0("Could Not Install Token\n\n",
               "The procedure failed for an unknown reason. Please ",
               "investigate the messages from Anaconda shown above.") |>
          errWrap() |>
          stop()
        
      }
      
      
    # In all other cases, output a generic error message
    } else {
      
      paste0("Could Not Create Environment\n\n",
             "The procedure failed for an unknown reason. Please ",
             "investigate the messages from Anaconda shown above.") |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}
