# CATEGORY 1 Script: Flagging Script

# Functions that are used across multiple scripts

makeSharePointPath <- function (filePathFragment) {
  
  # Given 'filePathFragment' (most of the filepath), write a complete filepath to the file
  
  # 'filePathFragment' should continue from the SharePoint drive onwards 
  # Everything up to "Supply and Demand Assessment - Documents" (inclusive) will be already specified by this function
  # The rest of the path is needed as input
  
  # (This function assumes that the SharePoint filepath is "C:/Users/[username]/[Initial SharePoint Path String]/...")
  
  paste0("C:/Users/", Sys.info()[["user"]], "/", getFromMasterControl("INITIAL_SHAREPOINT_FILE_PORTION"), filePathFragment)
  
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
    
    stop(paste0(ws[["NAME"]], " not recognized. ", GIS_FILE_PATH, " has not been specified for this watershed in the spreadsheet."))
    
  }
  
  
  
  # Check if the input path for the geospatial layer is a SharePoint path
  gisPath <- ws[[GIS_FILE_PATH]] |>
    sharepointPathCheck()
  
  
  
  # Next, check if 'gisPath' contains a URL
  # If that's the case, assume it's a Portal layer in a FeatureServer or FeatureLayer
  # Use ArcGIS functions to read it in
  if (grepl("^http", gisPath, ignore.case = TRUE) || 
      grepl("\\.ca\\.gov", gisPath, ignore.case = TRUE)) {
    
    
    # If "GIS_FILE_LAYER_NAME" has a value, assume that it is a FeatureLayer selection
    # In that case, 'gisPath' should be a URL to a FeatureServer
    if (!is.na(ws[[GIS_FILE_LAYER_NAME]])) {
      
      # There are two ways to specify a layer name:
      # Either using the numeric ID or the layer name
      
      # If "GIS_FILE_LAYER_NAME" contains something integer-like, 
      # assume it is meant to be an ID
      if (grepl("^[0-9]+$", ws[[GIS_FILE_LAYER_NAME]])) {
        
        return(arc_open(gisPath) |>
                 get_layer(id = ws[[GIS_FILE_LAYER_NAME]]) |>
                 arc_select())
        
      # Otherwise, assume that the layer is being selected by name
      } else {
        
        return(arc_open(gisPath) |>
                 get_layer(name = ws[[GIS_FILE_LAYER_NAME]]) |>
                 arc_select())
        
      }
      
    # Otherwise, if "GIS_FILE_LAYER_NAME" is blank, 
    # assume the URL in 'gisPath' directly points to a specific FeatureLayer
    } else {
      
      return(arc_open(gisPath) |>
               arc_select())
      
    }
    
  }
  
  
  
  # For regular files (not URLs), use `st_read`
  
  # Similar to before, if "GIS_FILE_LAYER_NAME" has a value, 
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



getFromMasterControl <- function (fieldName) {
  
  # Extract a value from the main control file for the repository
  # ("Repo_Control_File.xlsx")
  
  
  # First, read in the primary spreadsheet
  controlDF <- read_xlsx("Master_Control_File.xlsx")
  
  
  
  # Find a match for 'fieldName' in the "FIELD" column
  if (!(fieldName %in% controlDF[["FIELD"]])) {
    
    stop(paste0("The field '", fieldName, "' does not exist in the repo control spreadsheet!"))
    
  }
  
  
  # If the control file has a blank entry for this field, notify the user
  if (is.na(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])) {
    
    stop(paste0("'Master_Control_File.xlsx' has 'NA' for required field '", 
                fieldName, "'",
                "\n\n",
                "Please update this file."))
    
  }
  
  
  
  # Extract a string from the "VALUE" column based on the location where
  # 'fieldName' matches the string in "FIELD"
  return(controlDF[["VALUE"]][fieldName == controlDF[["FIELD"]]][1])
  
  
}



st_poi <- function (x) {
  
  # Given an 'sf' object, calculate the approximate 
  # Pole of Inaccessibility (POI) for each polygon
  
  # Return an 'sf' object with this information
  
  
  # First, use the `poi` function to calculate the POI for each element
  poiRes <- poi(x)
  
  
  # 'poiRes' contains a list with a separate entry for each element of 'x'
  # Inside each list element, there is a vector, and the first two entries 
  # of these vectors are "x" and "y" coordinates
  
  
  # Convert 'poiRes' to a data frame with "x" and "y" columns
  poiRes <- data.frame(x = poiRes |> map_dbl(~ pluck(., 1)),
                       y = poiRes |> map_dbl(~ pluck(., 2)))
  
  
  # Convert 'poiRes' into a "sf" object next
  # (Get the "CRS" from the original object 'x')
  poiRes <- poiRes |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(x))
  
  
  # Return 'poiRes'
  return(poiRes)
  
}
