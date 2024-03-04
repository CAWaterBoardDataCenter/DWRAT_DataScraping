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



getWatershedBoundaries <- function (ws) {
  
  # 'ws' contains filepaths that link to the watershed boundary layer
  # (a single polygon that represents the entire watershed)
  
  # This function can help extract that data
  # (Used in "GIS_Preprocessing.R" and "POD_StreamStats_Analysis.R")
  
  
  
  # First ensure that a path to a boundary layer was specified for this watershed
  if (is.na(ws$WATERSHED_BOUNDARY_DATABASE_PATH)) {
    
    stop(paste0(ws$NAME, " not recognized. A corresponding boundary layer has not been specified for this watershed in the spreadsheet."))
    
  }
  
  
  
  # Then, define 'gisPath' to be equal to the value in "WATERSHED_BOUNDARY_DATABASE_PATH"
  # If "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY" is TRUE, then makeSharePointPath() will be applied too
  if (ws$IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY == TRUE) {
    
    gisPath <- ws$WATERSHED_BOUNDARY_DATABASE_PATH %>%
      makeSharePointPath()
    
    # If "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY" is FALSE, no function call is needed
  } else if (ws$IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY == FALSE) {
    
    gisPath <- ws$WATERSHED_BOUNDARY_DATABASE_PATH
    
    # Error Check
  } else {
    
    stop("Invalid value for 'IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY'. Expected 'TRUE' or 'FALSE'.")
    
  }
  
  
  
  # Next, if "WATERSHED_BOUNDARY_LAYER_NAME" has a value, 
  # that means that "WATERSHED_BOUNDARY_DATABASE_PATH" is a geodatabase/geopackage/GIS container
  # If that is NOT the case, then st_read() should be called directly on "WATERSHED_BOUNDARY_DATABASE_PATH"
  
  
  
  # This statement is for cases where "WATERSHED_BOUNDARY_DATABASE_PATH" is NOT a GIS container
  # (So "WATERSHED_BOUNDARY_LAYER_NAME" is empty)
  if (is.na(ws$WATERSHED_BOUNDARY_LAYER_NAME)) {
    
    wsBound <- st_read(gisPath)
    
    # If "WATERSHED_BOUNDARY_LAYER_NAME" DOES contain a layer name, 
    # then both columns are needed to define 'wsBound'
  } else {
    
    # Perform a similar step as above, but with both columns involved
    
    wsBound <- st_read(gisPath,
                       layer = ws$WATERSHED_BOUNDARY_LAYER_NAME)
    
  }
  
  
  
  # After these steps, return 'wsBound'
  return(wsBound)
  
}

