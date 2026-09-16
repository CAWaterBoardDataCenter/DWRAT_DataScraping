# These functions help catch problems with a dataset (e.g., missing values)


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "!Shared_Functions_Importer.R"


#### Functions ####

checkMissingCol <- function (df, colNames, sourcePath = NA_character_, 
                             infoStr = "file", msg = NULL) {
  
  # Check a named object 'df' and confirm that it contains all columns that appear
  # in the vector 'colNames'
  
  # If any are missing, output those missing values to the console 
  
  # Then output an error message
  
  # There are two options for this error message:
  #   (1) A generic message can be used 
  #       (with limited details provided using 'infoStr')
  #
  #  (2) A full custom message can be specified using 'msg'
  #      (if 'msg' is not NULL, it will take priority over using 'infoStr')
  
  
  # Check if any values in 'colNames' are missing in the names of 'df'
  if (!all(colNames %in% names(df))) {
    
    # Get the names of missing values
    missingVals <- colNames[!(colNames %in% names(df))]
    
    
    # Print those names to the console
    cat("\n\n")
    cat(paste0("Missing Name", if_else(length(missingVals) > 1, "s", ""), ":\n"))
    print(missingVals)
    cat("\n\n")
    
    
    # Check if 'msg' is NULL
    # If yes, output a generic error message
    if (is.null(msg)) {
      
      paste0("Missing Name", 
             if_else(length(missingVals) > 1, "s", ""), " Issue\n\n",
             "The ", infoStr, " was expected to have ", length(colNames), " ",
             "key columns. However, ", length(missingVals), " column", 
             if_else(length(missingVals) > 1, "s are", " is"), " missing. Please ",
             "investigate the dataset for issues.\n\n",
             "(This error occurred for ", vec2QuotedStr(sourcePath), ")") |>
        errWrap() |>
        stop()
      
      # Otherwise, if 'msg' is NOT empty, use that as the error message
    } else {
      
      msg |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



check_if_missing_file <- function (fileVec, message = NA_character_) {
  
  # 'fileVec' should be a character vector containing one or more file paths
  
  # This function will check if any of these files are missing
  
  # If any file is missing, an error message will be displayed
  # There is a generic message that is used by default (when 'message' is NA)
  # Alternatively, a custom message can be input into the function
  
  
  # First, check whether the path(s) in 'fileVec' are SharePoint paths
  fileVec <- fileVec |>
    sharepointPathCheck(isFolder = FALSE)
  
  
  # Then, use `file.exists` to check every file in 'fileVec'
  checkRes <- file.exists(fileVec)
  
  
  # If not all files exist, output an error message
  if (!all(checkRes)) {
    
    # Identify the missing file(s)
    missingNames <- fileVec[!checkRes]
    
    
    # Paste this list to the console
    cat("\n\n")
    cat(paste0("Missing File", if_else(length(missingNames) > 1, "s", ""), ":\n"))
    print(missingNames)
    cat("\n\n")
    
    
    if (is.na(message)) {
      
      paste0("Missing File", if_else(length(missingNames) > 1, "s", ""), " Error\n\n",
             "Please review the list above. ", length(missingNames), " required ",
             "file", if_else(length(missingNames) > 1, "s", ""), " could not be ",
             "found. Please investigate.") |>
        stop_script()
      
    } else {
      
      message |>
        stop_script()
      
    }
    
  }
  
  
  # If there are no errors, return nothing
  return(invisible(NULL))
  
}
