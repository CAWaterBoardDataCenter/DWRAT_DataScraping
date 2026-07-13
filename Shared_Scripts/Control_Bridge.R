# These functions connect to the various control file spreadsheets stored within
# the repository:

#   (*) Master_Control_File.xlsx
#   (*) Watershed_Demand_Dataset_Paths.xlsx
#   (*) RR_Workflow_Control_File.xlsx
#   (*) LSPC_Weather_Control.xlsx


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "!Shared_Functions_Importer.R"


#### Functions ####

getFromMasterControl <- function (fieldName) {
  
  # Extract a value from the main control file for the repository
  # ("Master_Control_File.xlsx")
  
  
  # 'fieldName' should appear in a row under the table's "FIELD" column
  # The corresponding "VALUE" string will be returned
  
  
  # First, read in the primary spreadsheet
  controlDF <- getXLSX("Master_Control_File.xlsx")
  
  
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
        
        cat("\n\n")
        paste0("Empty SharePoint Field in Control File\n\n",
               "SharePoint connectivity is disabled because the corresponding ",
               "'VALUE' entry for the field '", fieldName, "' is empty\n\n",
               "Please consider updating 'Master_Control_File.xlsx'\n\n",
               "\n\n_______\n\n",
               "(This message will only display once per session/day)") |>
          errWrap() |>
          message()
        cat("\n\n")
        
        
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
                  "The corresponding 'VALUE' entry for the field '", fieldName, 
                  "' is empty\n\n",
                  "Please update 'Master_Control_File.xlsx'") |>
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
    
    controlPath <- "W2_Russian_River/Input/RR_Workflow_Control_File.xlsx"
    
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
    
    # Exceptions:
    if (fieldName %in% c("ADDITIONAL_ARCHIVE_LOCATION", 
                         "CIMIS_LOGIN_CREDENTIALS",
                         "LONG-RUNNING_METADATA_FILE_LOCATION",
                         "USGS_API_KEY")) {
      
      # These fields are optional, so it is okay if they are "NA"
      
    } else {
      
      stop(paste0("Empty Field in Control File\n\n",
                  "The corresponding 'VALUE' entry for the field '", fieldName, 
                  "' is empty\n\n",
                  "Please update '", controlPath, "'") |>
             errWrap())
      
    }
    
  }
  
  
  # Extract a string from the "VALUE" column based on the row where
  # 'fieldName' matches the string in "FIELD"
  return(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])
  
}
