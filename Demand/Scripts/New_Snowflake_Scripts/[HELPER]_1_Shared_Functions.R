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



getPath <- function (ws, FILEPATH_COLUMN) {
  
  # From 'ws', extract the watershed's path value for a specified column
  
  # The first column of 'ws' contains each of the path names
  # The second column contains the actual paths for the watershed (that correspond to the names)
  
  # The first column is used to identify the desired filepath
  # Then, it is extracted from the second column and returned
  
  
  return(ws %>%
           filter(WATERSHED == FILEPATH_COLUMN) %>%
           select(2) %>%
           unlist(use.names = FALSE))
  
}



getXLSX <- function (ws, FILEPATH_COLUMN, WORKSHEET_NAME) {
  
  # For a given spreadsheet, 'ws' contains two relevant columns:
  #  The filepath ('FILEPATH')
  #  The spreadsheet's worksheet name ('WORKSHEET_NAME')
  # Based on these variables, attempt to read in the spreadsheet
  
  return(ws %>%
           getPath(FILEPATH_COLUMN) %>%
           makeSharePointPath() %>%
           fileRead(commandType = "read_xlsx", 
                    sheet = getPath(ws, WORKSHEET_NAME)))
  
}



getGIS <- function (ws, GIS_FILE_PATH_COLUMN, GIS_FILE_LAYER_NAME) {
  
  # 'ws' contains filepaths that link to GIS layers
  # This function can help extract that data
  
  
  
  # First ensure that a path to a layer was specified for this watershed
  if (is.na(ws %>% getPath(GIS_FILE_PATH_COLUMN))) {
    
    stop(paste0("'", GIS_FILE_PATH_COLUMN, "' has not been specified for this watershed in ",
                "the spreadsheet 'Snowflake_Watershed_Demand_Dataset_Paths.xlsx'") %>%
           strwrap(width = 0.98 * getOption("width")) %>%
           paste0(collapse = "\n"))
    
  }
  
  
  
  # Next, if "GIS_FILE_LAYER_NAME" has a value, 
  # that means that "GIS_FILE_PATH_COLUMN" is a geodatabase/geopackage/GIS container
  # If that is NOT the case, then st_read() should be called directly on "GIS_FILE_PATH_COLUMN"
  
  
  
  # This statement is for cases where "GIS_FILE_PATH_COLUMN" is NOT a GIS container
  # (So "GIS_FILE_LAYER_NAME" is empty)
  if (is.na(ws %>% getPath(GIS_FILE_LAYER_NAME))) {
    
    wsBound <- ws %>%
      getPath(GIS_FILE_PATH_COLUMN) %>%
      makeSharePointPath() %>%
      st_read()
    
    # If "GIS_FILE_LAYER_NAME" DOES contain a layer name, 
    # then both columns are needed to define 'wsBound'
  } else {
    
    # Perform a similar step as above, but with both columns involved
    
    wsBound <- ws %>%
      getPath(GIS_FILE_PATH_COLUMN) %>%
      makeSharePointPath() %>%
      st_read(layer = getPath(ws, GIS_FILE_LAYER_NAME))
    
  }
  
  
  
  # After these steps, return 'wsBound'
  return(wsBound)
  
}



fileRead <- function (filePath, commandType, col_types = NULL, select = NULL, sheet = NULL) {
  
  # Try to read a file
  
  
  
  # Verify that a valid 'commandType' is specified
  if (!(commandType %in% c("read.csv", "read_csv", "fread", "read_xlsx"))) {
    
    stop("The function fileReadTry() can only be used with read.csv(), read_csv(), fread(), and read_xlsx()" %>%
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
    
    
  } else if (commandType == "read_xlsx") {
    
    fileDF <- try(read_xlsx(filePath, sheet = sheet, col_types = col_types), silent = TRUE)
    
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
             str_replace("likely incorrect", col_red("likely incorrect")))
      
    } else if (grepl("Error in utils::unzip.+cannot be opened", fileDF)) {
      
      stop(paste0("The target spreadsheet is open locally on your computer. ",
                  "Please close the file before attempting to run the script again.") %>%
             strwrap(width = 0.98 * getOption("width")) %>%
             paste0(collapse = "\n") %>%
             str_replace("open", col_red("open")) %>%
             str_replace("locally", col_red("locally")) %>%
             str_replace("close", col_green("close")) %>%
             str_replace("the", col_green("the")) %>%
             str_replace("file", col_green("file")))
      
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
