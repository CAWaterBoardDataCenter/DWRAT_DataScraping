# These functions connect to the various control file spreadsheets stored within
# the repository:

#   (*) Master_Control_File.xlsx
#   (*) Watershed_Demand_Dataset_Paths.xlsx
#   (*) RR_Workflow_Control_File.xlsx
#   (*) LSPC_Weather_Control.xlsx


#### Master Control File ####

#' @title Read in the Master Control File as a Tibble
#' 
#' @description
#' To Be Written
#' 
#' @details
#' To Be Written
#' 
#' Wrapper for `getXLSX`, which in turn calls [readxl::read_xlsx()]
#' 
#' @usage read_master_control()
#' 
#' @returns A [tibble::tibble()] containing the "Main" worksheet of the master control file
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' masterDF <- read_master_control()
#' }
read_master_control <- function () {
  
  # Read in the repository's primary control file
  return(master_control_path() |>
           getXLSX(worksheet = master_control_worksheet()))
  
}


#' @title Get the Value for a Field in the Master Control File
#' 
#' @description
#' To Be Written
#' 
#' @details
#' To Be Written
#' 
#' Type of returned value is most likely a string
#' read_xlsx will assign the type for "VALUE" based on all of its values
#' If there's at least one text string in a row of that column, the entire 
#' column will be read as [character]
#' 
#' @usage get_from_master_control(fieldName)
#' 
#' @param fieldName A [character] string exactly as it appears in the "FIELD" 
#' column of the control file
#' 
#' @returns A single [character] string or similar value
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Extract the "VALUE" entry in the row that corresponds to the 
#' # "INITIAL_SHAREPOINT_FILE_PORTION" field
#' get_from_master_control("INITIAL_SHAREPOINT_FILE_PORTION")
#' }
get_from_master_control <- function (fieldName) {
  
  # Extract a value from the main control file for the repository
  # ("Master_Control_File.xlsx")
  
  
  # 'fieldName' should appear in a row under the table's "FIELD" column
  # The corresponding "VALUE" string will be returned
  
  
  # First, read in the primary spreadsheet
  controlDF <- read_master_control()
  
  
  # Find a match for 'fieldName' in the "FIELD" column
  if (!(fieldName %in% controlDF[["FIELD"]])) {
    
    stop(paste0("Field Does Not Exist\n\n",
                "'", fieldName, "' does not appear in the 'FIELD' column of the Master ",
                "Control File\n\n",
                "Please ensure that the scripts are up-to-date\n\n",
                "Also, please confirm that the correct version of \"",
                master_control_path(), "\" is in use") |>
           errWrap())
    
  }
  
  
  # If the control file has a blank entry for this field, notify the user
  # For most fields, this will be an error message
  # SharePoint-related fields will be an exception
  if (is.na(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])) {
    
    # For "INITIAL_SHAREPOINT_FILE_PORTION", it will just be a message
    if (fieldName == "INITIAL_SHAREPOINT_FILE_PORTION") {
      
      # This message will only display once per day
      # It does that using a custom option called "sda_displayed_sharepoint_warning"
      
      # This option's value will either be NULL or a date
      optionRes <- getOption("sda_displayed_sharepoint_warning")
      
      
      # Check if 'optionRes' exists (if not, this is first message of the session)
      # If 'optionRes' does exist, check if the date is earlier than today
      if (is.null(optionRes) || Sys.Date() > optionRes) {
        
        cat("\n\n")
        paste0("Empty SharePoint Field in Control File\n\n",
               "SharePoint connectivity is disabled because the corresponding ",
               "'VALUE' entry for the field '", fieldName, "' is empty\n\n",
               "Please consider updating \"", master_control_path(), "\"\n\n",
               "\n\n_______\n\n",
               "(This message will only display once per session/day)") |>
          errWrap() |>
          message()
        cat("\n\n")
        
        
        # After the message has been displayed, update the custom option 
        # with today's date
        options(sda_displayed_sharepoint_warning = Sys.Date())
        
        
        # After that, do not stop the code and allow the function to return 
        # "NA" for "INITIAL_SHAREPOINT_FILE_PORTION"
        
      }
      
      
      # For other SharePoint-related fields, do nothing
    } else if (fieldName %in% c("SHAREPOINT_DEMAND_CONTROL_FILE",
                                "SHAREPOINT_RR_SUPPLY_CONTROL_FILE",
                                "SHAREPOINT_LSPC_WORKFLOW_CONTROL_FILE")) {
      
      # No messages or errors
      # Since these are optional fields, let the regular procedure return NA
      
    } else {
      
      stop(paste0("Empty Field in Control File\n\n",
                  "The corresponding 'VALUE' entry for the field '", fieldName, 
                  "' is empty\n\n",
                  "Please update \"", master_control_path(), "\"") |>
             errWrap())
      
    }
    
  }
  
  
  # Extract a string from the "VALUE" column based on the row where
  # 'fieldName' matches the string in "FIELD"
  return(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])
  
}



#### General Control File Functions ####

read_control_file <- function () {
  
  # This function is not exported publicly, but it supports 
  # reading in control files for every workflow
  
  
  
}


#### LSPC Weather Control File ####

#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
read_lspc_weather_control <- function () {
  
  # Read in the repository's weather control file for LSPC
  return(lspc_weather_control_path() |>
           getXLSX(worksheet = lspc_weather_control_worksheet()))
  
}



read_lspc_master_control <- function () {
  
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

