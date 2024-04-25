# Functions that are used across multiple scripts

makeSharePointPath <- function (filePathFragment) {
  
  # Given 'filePathFragment' (most of the filepath), write a complete filepath to the file
  
  # 'filePathFragment' should continue from the SharePoint drive onwards 
  # Everything up to "Supply and Demand Assessment - Documents" (inclusive) will be already specified by this function
  # The rest of the path is needed as input
  
  # (This function assumes that the SharePoint filepath is "C:/Users/[username]/Water Boards/Supply and Demand Assessment - Documents/...")
  
  system("whoami", intern = TRUE) %>%
    str_split("\\\\") %>% unlist() %>% tail(1) %>%
    paste0("C:/Users/", ., "/Water Boards/Supply and Demand Assessment - Documents/", filePathFragment)
  
}



getXLSX <- function (ws, SHAREPOINT_BOOL, FILEPATH, WORKSHEET_NAME) {
  
  # For a given spreadsheet, 'ws' contains three relevant columns:
  #  The filepath ('FILEPATH')
  #  The spreadsheet's worksheet name ('WORKSHEET_NAME')
  #  A TRUE/FALSE variable for whether the file path is a SharePoint path ('SHAREPOINT_BOOL')
  # Based on these variables, attempt to read in the spreadsheet
  
  
  if (ws[[SHAREPOINT_BOOL]] == TRUE) {
    
    sheetDF <- ws[[FILEPATH]] %>%
      makeSharePointPath() %>%
      read_xlsx(sheet = ws[[WORKSHEET_NAME]])
    
  } else if (ws[[SHAREPOINT_BOOL]] == FALSE) {
    
    sheetDF <- ws[[FILEPATH]] %>%
      read_xlsx(sheet = ws[[WORKSHEET_NAME]])
    
  } else {
    
    stop(paste0("Invalid value for '", SHAREPOINT_BOOL, "'. Expected 'TRUE' or 'FALSE'."))
    
  }
  
  
  
  return(sheetDF)
  
}



getGIS <- function (ws, GIS_SHAREPOINT_BOOL, GIS_FILE_PATH, GIS_FILE_LAYER_NAME) {
  
  # 'ws' contains filepaths that link to GIS layers
  # This function can help extract that data
  
  
  
  # First ensure that a path to a layer was specified for this watershed
  if (is.na(ws[[GIS_FILE_PATH]])) {
    
    stop(paste0(ws$NAME, " not recognized. ", GIS_FILE_PATH, " has not been specified for this watershed in the spreadsheet."))
    
  }
  
  
  
  # Then, define 'gisPath' to be equal to the value in "GIS_FILE_PATH"
  # If "GIS_SHAREPOINT_BOOL" is TRUE, then makeSharePointPath() will be applied too
  if (ws[[GIS_SHAREPOINT_BOOL]] == TRUE) {
    
    gisPath <- ws[[GIS_FILE_PATH]] %>%
      makeSharePointPath()
    
    # If "GIS_SHAREPOINT_BOOL" is FALSE, no function call is needed
  } else if (ws[[GIS_SHAREPOINT_BOOL]] == FALSE) {
    
    gisPath <- ws[[GIS_FILE_PATH]]
    
    # Error Check
  } else {
    
    stop(paste0("Invalid value for '", GIS_SHAREPOINT_BOOL, "'. Expected 'TRUE' or 'FALSE'."))
    
  }
  
  
  
  # Next, if "GIS_FILE_LAYER_NAME" has a value, 
  # that means that "GIS_FILE_PATH" is a geodatabase/geopackage/GIS container
  # If that is NOT the case, then st_read() should be called directly on "GIS_FILE_PATH"
  
  
  
  # This statement is for cases where "GIS_FILE_PATH" is NOT a GIS container
  # (So "GIS_FILE_LAYER_NAME" is empty)
  if (is.na(ws[[GIS_FILE_LAYER_NAME]])) {
    
    wsBound <- st_read(gisPath)
    
    # If "GIS_FILE_LAYER_NAME" DOES contain a layer name, 
    # then both columns are needed to define 'wsBound'
  } else {
    
    # Perform a similar step as above, but with both columns involved
    
    wsBound <- st_read(gisPath,
                       layer = ws[[GIS_FILE_LAYER_NAME]])
    
  }
  
  
  
  # After these steps, return 'wsBound'
  return(wsBound)
  
}

#Create add_date_column function to generate a custom date to serve as the 7th column in the
# various iterations of the Dat_PRMS files and dataframes

add_date_column <- function(dat, col_position) {
  # dat is your input dat dataframe and col_position refers to the desired position of the Date column
  # you are creating
  
  # Check that year, month, and day columns exist
  required_cols <- c("Year", "month", "day")
  
  if (!all(required_cols %in% names(dat))) {
    stop("One or more of the date columns (Year, month, day) is missing.")
  }
  # Check that year, month, and day are all numeric
  if (!all(sapply(dat[required_cols], is.numeric))) {
    stop("One or more of the date columns (Year, month, day) is not numeric.")
  }
  
  # Create the date_column
  date_column <- paste0(dat$Year, "/", dat$month, "/", dat$day) %>% as.Date(format = "%Y/%m/%d")
  
  # Bind the date_column to the original dataframe dat
  dat <- cbind(dat[, 1:col_position-1], Date = date_column, dat[, col_position:ncol(dat)])
  
  return(dat)
}