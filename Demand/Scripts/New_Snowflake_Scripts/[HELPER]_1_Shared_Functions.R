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



fileRead <- function (filePath, commandType, col_types = NULL, select = NULL) {
  
  # Try to read a file
  
  
  
  # Verify that a valid 'commandType' is specified
  if (!(commandType %in% c("read.csv", "read_csv", "fread"))) {
    
    stop("The function fileReadTry() can only be used with read.csv(), read_csv(), and fread()" %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n"))
    
  }
  
  
  
  # Based on the desired command, try to read the file
  # (read_csv() and fread() have optional additional arguments)
  if (commandType == "read.csv") {
    
    
    fileDF <- try(read.csv(filePath), silent = TRUE)
    
    
  } else if (commandType == "read_csv") {
    
    
    fileDF <- try(read_csv(filePath, show_col_types = FALSE, col_types = col_types), silent = TRUE)
    
    
  } else if (commandType == "fread") {
    
    
    fileDF <- try(fread(filePath, select = select), silent = TRUE)
    
    
  }
  
  
  
  # Check for errors in the read attempt
  if (is.null(ncol(fileDF)) && length(fileDF) == 1 && grepl("Error", fileDF)) {
    
    cat("\n\n")
    message(fileDF)
    cat("\n")
    
    
    if (grepl("invalid 'description' argument", fileDF) ||
        grepl("cannot open the connection", fileDF) ||
        grepl("does not exist", fileDF) || grepl("No such file", fileDF)) {
      
      stop(paste0("The input filepath is likely incorrect.",
                  " It does not lead to a readable file.",
                  "\n\nPath being used by the code: ", 
                  filePath, "\n\n") %>%
             strwrap(width = getOption("width")) %>%
             paste0(collapse = "\n") %>%
             str_replace("likely incorrect", red("likely incorrect")))
      
    } else {
      
      stop("An unexpected error occurred. See the error message above for details." %>%
             strwrap(width = getOption("width")) %>%
             paste0(collapse = "\n"))
      
    } 
    
  }
  
  
  
  # If no error occurred, return 'fileDF'
  return(fileDF)
  
}



readFlagTable <- function () {
  
  # Wrapper for importing the latest flag table
  
  
  
  # Use the extended CSV filename to get the expected ID of the flag table 
  # (This ensures that the flag table was properly generated and belongs to the latest flat file)
  flagDF <- makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                            makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                              list.files() %>% sort() %>% tail(1) %>% 
                              str_replace("water_use_report_extended", "Flag_Table"))) %>%
    fileRead("read_csv")
  
  
  
  # Return 'flagDF'
  return(flagDF)
  
}



writeFlagTable <- function (flagDF) {
  
  # Wrapper for writing the latest flag table
  
  
  
  # Use the extended CSV filename to get the expected ID of the flag table 
  # (This ensures that the flag table belongs to the latest flat file)
  makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                            makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                              list.files() %>% sort() %>% tail(1) %>% 
                              str_replace("water_use_report_extended", "Flag_Table"))) %>%
    write_csv(x = flagDF)
  
  
  
  # Return 'flagDF'
  return(flagDF)
  
}
