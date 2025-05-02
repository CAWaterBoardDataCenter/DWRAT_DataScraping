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
           wrapStr())
    
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
      fileRead("st_read")
    
    # If "GIS_FILE_LAYER_NAME" DOES contain a layer name, 
    # then both columns are needed to define 'wsBound'
  } else {
    
    # Perform a similar step as above, but with both columns involved
    
    wsBound <- ws %>%
      getPath(GIS_FILE_PATH_COLUMN) %>%
      makeSharePointPath() %>%
      fileRead("st_read", layer = getPath(ws, GIS_FILE_LAYER_NAME))
    
  }
  
  
  
  # After these steps, return 'wsBound'
  return(wsBound)
  
}



fileRead <- function (filePath, commandType, col_types = NULL, select = NULL, sheet = NULL, layer = NULL) {
  
  # Try to read a file
  
  
  
  # Verify that the file exists
  if (!file.exists(filePath)) {
    
    stop(paste0("No file exists at the specified path (",
                filePath, ")") %>%
           wrapStr() %>%
           str_replace("exists", col_red("exists")) %>%
           str_replace("path", col_magenta("path")))
    
  }
  
  
  
  # Verify that a valid 'commandType' is specified
  if (!(commandType %in% c("read.csv", "read_csv", "fread", 
                           "read_xlsx", "st_read"))) {
    
    stop(paste0("The function fileReadTry() can only be used with read.csv(),", 
                "read_csv(), fread(), read_xlsx(), and st_read()") %>%
           wrapStr())
    
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
    
  } else if (commandType == "st_read") {
    
    
    if (is.null(layer)) {
      fileDF <- try(st_read(filePath), silent = TRUE)
    } else {
      fileDF <- try(st_read(filePath, layer = layer), silent = TRUE)
    }
    
    
  }
  
  
  
  # Check for errors in the read attempt
  if (is.null(ncol(fileDF)) && length(fileDF) == 1 && grepl("Error", fileDF)) {
    
    cat("\n\n")
    message(fileDF)
    cat("\n")
    
    
    if (grepl("invalid 'description' argument", fileDF) ||
        grepl("cannot open the connection", fileDF) ||
        grepl("does not exist", fileDF) || grepl("No such file", fileDF) || 
        grepl("doesn.t seem to exist", fileDF)) {
      
      stop(paste0("The input filepath is likely incorrect.",
                  " It does not lead to a readable file.",
                  "\n\nPath being used by the code: ", 
                  filePath, "\n\n") %>%
             wrapStr() %>%
             str_replace("likely incorrect", col_red("likely incorrect")))
      
    } else if (grepl("Error in utils::unzip.+cannot be opened", fileDF)) {
      
      stop(paste0("The target spreadsheet is open locally on your computer. ",
                  "Please close the file before attempting to run the script again.") %>%
             wrapStr() %>%
             str_replace("open", col_red("open")) %>%
             str_replace("locally", col_red("locally")) %>%
             str_replace("close", col_green("close")) %>%
             str_replace("the", col_green("the")) %>%
             str_replace("file", col_green("file")))
      
    } else if (grepl("Opening layer failed", fileDF)) {
      
      stop(paste0("There is a problem with the layer argument '", layer, "'. ", 
                  "It seems that the GIS object (", filePath, ") does not contain ",
                  "a layer with that name.\n\nThese are the available options:\n\n",
                  st_layers(filePath)[["name"]] %>% paste0(collapse = "\n\n")) %>%
             wrapStr() %>%
             str_replace("layer", col_red("layer")) %>%
             str_replace("does", col_red("does")) %>%
             str_replace("not", col_red("not")) %>%
             str_replace("available", col_green("available")) %>%
             str_replace("options", col_green("options")))
      
    } else if (grepl("argument is not a character vector", fileDF)) {
      
      stop(paste0("There is a problem with the layer argument '", layer, "'. ", 
                  "It should either be unused or specified as a character string.") %>%
             wrapStr() %>%
             str_replace("layer", col_red("layer")) %>%
             str_replace("does", col_red("does")) %>%
             str_replace("not", col_red("not")) %>%
             str_replace("available", col_green("available")) %>%
             str_replace("options", col_green("options")))
      
    } else {
      
      stop("An unexpected error occurred. See the error message above for details." %>%
             wrapStr())
      
    } 
    
  } else if (commandType == "fread" && nrow(fileDF) == 0 && length(fileDF) == 0) {
    
    stop("An unknown error occurred with 'fread'. Please consult the above output." %>%
           wrapStr())
    
  }
  
  
  
  # If no error occurred, return 'fileDF'
  return(fileDF)
  
}




fileWrite <- function (fileObj, commandType, filePath, layer = NULL, delete_dsn = FALSE) {
  
  # Try to write a file
  
  
  
  # Verify that a valid 'commandType' is specified
  if (!(commandType %in% c("write_xlsx", "write_csv", "st_write"))) {
    
    stop(paste0("The function fileReadTry() can only be used with ",
                "write_xlsx(), write_csv(), and st_write()") %>%
           wrapStr())
    
  }
  
  
  
  # Based on the desired command, try to write the file
  if (commandType == "write_xlsx") {
    
    
    writeRes <- try(write_xlsx(fileObj, filePath), silent = TRUE)
    
    
  } else if (commandType == "write_csv") {
    
    
    writeRes <- try(write_csv(fileObj, filePath), silent = TRUE)
    
    
    
  } else if (commandType == "st_write") {
    
    
    writeRes <- try(st_write(fileObj, filePath, layer = layer, 
                             delete_dsn = delete_dsn), silent = TRUE)
    
    
  }
  
  
  
  # Check for errors in the read attempt
  if (length(writeRes) == 1 && grepl("Error", writeRes)) {
    
    cat("\n\n")
    message(writeRes)
    cat("\n")
    
    
    if ((grepl("permissions", writeRes) & commandType == "write_xlsx") ||
        grepl("Cannot open file for writing", writeRes)) {
      
      stop(paste0("The most common cause of this error is that '", filePath, 
                  "' already exists AND is currently open. Please close the file ", 
                  "and rerun the script.", "\n\n") %>%
             wrapStr() %>%
             str_replace("AND", col_red("AND")) %>%
             str_replace("exists", col_blue("exists")) %>%
             str_replace("open", col_blue("open")) %>%
             str_replace("close", col_green("close")) %>%
             str_replace("rerun", col_green("rerun")))
      
    } else {
      
      stop("An unexpected error occurred. See the error message above for details." %>%
             wrapStr())
      
    } 
    
  }
  
  
  
  # No object is returned
  return(invisible(NULL))
  
}




readFlagTable <- function () {
  
  # Wrapper for importing the latest flag table
  
  
  
  # Use the extended CSV filename to get the expected ID of the flag table 
  # (This ensures that the flag table was properly generated and belongs to the latest flat file)
  flagDF <- makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                            makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                              list.files() %>% sort() %>% tail(1) %>% 
                              str_replace("water_use_report_extended", "Flag_Table"))) %>%
    fileRead("fread")
  
  
  
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
    fileWrite(fileObj = flagDF, commandType = "write_csv")
  
  
  
  # Return 'flagDF'
  return(flagDF)
  
}



wrapStr <- function (str, width = 0.98 * getOption("width"), collapse = "\n") {
  
  # Split a string based on the console width and paste it back together
  # using a newline character
  
  return(str %>%
           strwrap(width = width) %>%
           paste0(collapse = collapse))
  
}

