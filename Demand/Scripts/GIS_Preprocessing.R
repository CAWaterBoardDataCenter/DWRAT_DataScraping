# Perform the steps to identify PODs within a watershed
# (A manual review will be needed on the output)

# This script automates the procedure described in "GIS Pre-Processing SDU Manual.docx"


# The watershed used in this script is defined here
wsName <- "Russian"


#### Dependencies ####

require(tidyverse)
require(sf)
require(openxlsx)

#### Functions ####

mainProcedure <- function (wsName) {
  
  # Given the watershed name 'wsName', perform the GIS pre-processing steps

  
  
  # Based on the value of 'wsName', read in a different boundary layer
  # (The assigned variable name should always be 'wsBound')
  if (grepl("^Navarro", wsName, ignore.case = TRUE)) {
    wsBound <- st_read("../../../../Water Boards/Supply and Demand Assessment - Documents/Watershed Folders/Navarro/Data/GIS Datasets/Navarro_River_Watershed_GIS/Navarro_River_Watershed.gpkg", "WBD_HU10_Navarro")
  } else if (grepl("^Russian", wsName, ignore.case = TRUE)) {
    wsBound <- st_read("../../../../Water Boards/Supply and Demand Assessment - Documents/GIS/Russian River.gdb/", "RR_NHD_1801_HUC8")
  } else {
    stop(paste0(wsName, " not recognized. A corresponding boundary layer has not been specified for this watershed in the script."))
  }
  
  
  
  # After that, import a full list of PODs from eWRIMS
  # (1) To do that, get the most recent "Flat_File_eWRIMS_[DATE].csv" file in the "IntermediateData" folder
  # (2) Every column is read in as a character column by default
  #     Make new columns for "LATITUDE" and "LONGITUDE" that are numeric 
  # (3) Then, make 'pod_points_statewide' into a GIS layer
  #     Use the numeric "LATITUDE" and "LONGITUDE" layers as coordinates
  #     (The data in these columns will not be easily accessible afterwards, so that's why copies were used)
  pod_points_statewide <- list.files("IntermediateData/", full.names = TRUE, pattern = "^Flat_File_eWRIMS") %>%
    sort() %>% tail(1) %>%
    read_csv(show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    mutate(LONGITUDE2 = as.numeric(LONGITUDE), LATITUDE2 = as.numeric(LATITUDE)) %>%
    filter(!is.na(LONGITUDE2)) %>% 
    unique() %>%
    st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"))
  
  
  
  # The coordinate system of this layer is "NAD83"
  st_crs(pod_points_statewide) <- "NAD83"
  
  
  
  # From the Russian River geodatabase, import the layer "PLSS_Sections_Fill"
  # (PLSS Sections for the entire state)
  PLSS_Sections_Fill <- st_read("../../../../Water Boards/Supply and Demand Assessment - Documents/GIS/Russian River.gdb/", 
                                layer = "PLSS_Sections_Fill")
  
  
  
  # Ensure that all of these layers' have the same projected coordinate system
  # Use "NAD83(NSRS2007) / California Albers" for that
  wsBound <- confirmCS(wsBound)
  pod_points_statewide <- confirmCS(pod_points_statewide)
  PLSS_Sections_Fill <- confirmCS(PLSS_Sections_Fill)
  
  
  
  # There are four different tasks that will be completed:
  
  # (1) Get PODs with a MTRS or FFMTRS that lies within the watershed boundaries
  # (Based on PLSS overlap with watershed polygon)
  
  # (2) Get all PODs within one mile of the boundary
  
  # (3) Get all PODs that intersect with the watershed polygon
  
  # (4) Get all PODs that mention the watershed in their source/tributary information
  
  
  
  # Start with Task 1 
  # (one of the resultant layers, 'pod_points_statewide_spatial' will be useful in other tasks)
  
  
  
  #### Task 1 (MTRS and FFMTRS) ####
  
  # Gather PODs based on their stated "MTRS" and/or "FFMTRS" values
  
  
  
  # Join 'pod_points_statewide' and 'PLSS_Sections_Fill' (their 'FFMTRS' and 'MTRS' will be compared)
  pod_points_statewide_spatial <- st_join(pod_points_statewide, PLSS_Sections_Fill)
  
  
  
  # Add a new field to 'pod_points_statewide_spatial' that checks if FFMTRS and MTRS are equal
  pod_points_statewide_spatial <- pod_points_statewide_spatial %>%
    mutate(MTRS_Match = if_else(MTRS == FFMTRS, "Y", "N"))
  
  
  
  # Add another field that links to the rights' respective documents
  pod_points_statewide_spatial <- pod_points_statewide_spatial %>%
    mutate(URL = paste0("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/DocumentRetriever.jsp?", 
                        "appNum=", APPLICATION_NUMBER,
                        "&wrType=", WATER_RIGHT_TYPE, 
                        "&docType=DOCS"))
  
  
  
  # Get a subset of 'PLSS_Sections_Fill' that intersects with 'wsBound'
  WS_Section_Intersect <- st_intersection(PLSS_Sections_Fill, wsBound)
  
  
  
  # Get the points in 'pod_points_statewide_spatial' that have a "MTRS" value in 'WS_Section_Intersect'
  # Check both the "MTRS" and "FFMTRS" columns for matches
  WS_pod_points_Merge <- pod_points_statewide_spatial %>%
    filter(MTRS %in% WS_Section_Intersect$MTRS | MTRS %in% WS_Section_Intersect$FFMTRS)
  
  
  
  # Check the result for duplicate entries
  # (There should only be one row per unique "POD_ID" value)
  WS_pod_points_Merge <- WS_pod_points_Merge %>%
    deleteIdentical("POD_ID")
  
  
  
  #### Task 2 (Watershed Boundary Line Buffer) ####
  
  # The next task will be to get PODs that are near the boundary of the watershed
  # These will require a manual review
  
  
  
  # The next operations require a polyline of the watershed boundaries
  # Create that layer now
  wsLine <- st_cast(wsBound, "MULTILINESTRING")
  
  
  
  # Create a boundary line layer with a one-mile buffer on both sides of the line
  # "epsg:3488" has units of meters
  wsLine_Buffer <- st_buffer(wsLine, 1 * 5280 / 3.28084) # 1 mile > feet > meters
  # https://gis.stackexchange.com/questions/303126/how-to-buffer-a-pair-of-longitude-and-latitude-coordinates-in-r
  
  
  
  # Get all PODs that intersect with this buffer
  wsLine_Buffer_Intersect <- st_intersection(pod_points_statewide_spatial, wsLine_Buffer)
  
  
  
  #### Task 3 (Watershed Intersect) ####
  
  # Get all PODs that intersect with the watershed polygon
  # (Exclude the ones within the one-mile buffer)
  
  
  # Get a smaller version of 'wsBound' (removing the overlap with 'wsLine_Buffer')
  wsBound_Inner <- st_difference(wsBound, wsLine_Buffer)
  
  
  
  # Get all PODs that intersect with 'wsBound_Inner'
  wsBound_Inner_Intersect <- st_intersection(pod_points_statewide_spatial, wsBound_Inner)
  
  
  
  #### Task 4 (Watershed Tributary/Source) ####
  
  # Get all PODs that mention the watershed in their source/tributary information
  if (grepl("^Navarro", wsName, ignore.case = TRUE)) {
    
    wsMention <- pod_points_statewide_spatial %>%
      filter(grepl("Navarro", WATERSHED, ignore.case = TRUE) |
               grepl("Navarro", SOURCE_NAME, ignore.case = TRUE) |
               grepl("Navarro", TRIB_DESC, ignore.case = TRUE)) #|
               #grepl("^Anderson Creek$", HUC_12_NAME, ignore.case = TRUE) |
               #grepl("^Indian Creek$", HUC_12_NAME, ignore.case = TRUE) |
               #grepl("^North Fork Navarro River$", HUC_12_NAME, ignore.case = TRUE) |
               #grepl("^(Upper|Lower) Rancheria Creek$", HUC_12_NAME, ignore.case = TRUE) |
               #grepl("^(Upper|Lower) Navarro River$", HUC_12_NAME, ignore.case = TRUE) |
               #grepl("^(North|South) Branch North Fork Navarro River$", HUC_12_NAME, ignore.case = TRUE))
    
  } else if (grepl("^Russian", wsName, ignore.case = TRUE)) {
    
    wsMention <- pod_points_statewide_spatial %>%
      filter(grepl("Russian", WATERSHED, ignore.case = TRUE) |
               grepl("Russian", SOURCE_NAME, ignore.case = TRUE) |
               grepl("Russian", TRIB_DESC, ignore.case = TRUE))
    
  } else {
    
    stop(paste0(wsName, " not recognized. A corresponding filter has not been specified for this watershed in the script."))
    
  }
  
  
  
  # Output the four variables to a spreadsheet for further analysis
  # ('WS_pod_points_Merge', 'wsLine_Buffer_Intersect', 'wsBound_Inner_Intersect', and 'wsMention')
  outputResults(wsName, WS_pod_points_Merge, wsLine_Buffer_Intersect, wsBound_Inner_Intersect, wsMention)
  
  
  
  # Output a completion message
  print("Done!")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



confirmCS <- function (layerDF) {
  
  # Confirm that the coordinate reference system of a GIS layer is "California Albers"
  # If that is not the case, convert it
  # (A projected coordinate system is needed for much of these operations)
  
  
  # Return 'layerDF' after converting its coordinate system
  layerDF <- st_transform(layerDF, "epsg:3488")
  
  
  
  return(layerDF)
  
}



deleteIdentical <- function (gisDF, colName) {
  
  # In the spatial dataset 'gisDF', 
  # keep only one row of data per unique value in the column identified by 'colName'
  
  # To complete this objective, a new column will be added to 'gisDF'
  # All 'colName' values with a frequency greater than 1 will be flagged for deletion initially
  # Then, the first entry for each unique 'colName' value will have that flag removed
  
  
  
  # Start by getting a vector of 'colName' values with more than one instance in 'gisDF'
  multInstance <- gisDF[[colName]] %>%
    table() # table() gives the frequency of each value in 'colName'
  
  
  
  # This gets the values with frequencies greater than 1
  multInstance <- names(multInstance[multInstance > 1])
  
  
  
  # Convert 'multInstance' into a data frame with an attached column that flags for deletion
  multInstance <- multInstance %>%
    matrix(ncol = 1) %>% data.frame() %>%
    set_names(colName) %>%
    mutate(DELETE_ROW = TRUE)
  
  
  
  # Join 'multInstance' to 'gisDF'
  # All 'colName' values that have multiple instances have a value of "TRUE" for "DELETE_ROW"
  # All 'colName' values with only one instance have a value of "NA"
  gisDF <- gisDF %>%
    left_join(multInstance, by = colName, relationship = "many-to-one")
  
  
  
  # Group by 'colName' and update "DELETE_ROW" for each 'colName' value
  # The first entry will have "DELETE_ROW" changed to NA
  # The remaining entries will retain their value of "TRUE"
  # (Flags for 'colName' values with only one entry will be unchanged by this code)
  gisDF <- gisDF %>%
    ungroup() %>%
    group_by(!! as.name(colName)) %>%
    mutate(DELETE_ROW = c(NA, DELETE_ROW %>% tail(-1))) 
  # For groups of rows with the same 'colName' value, all of their values for "DELETE_ROW" are initially "TRUE"
  # The first instance is changed to "NA", and the others retain their initial values ("TRUE")
  # For a row that is the only instance of a 'colName' value, it is assigned "NA" (which it already had), so nothing is changed effectively
  
  
  
  # Filter 'gisDF' to keep only entries with "NA" entries for "DELETE_ROW"
  # Also ungroup the data frame and remove "DELETE_ROW"
  gisDF <- gisDF %>%
    filter(is.na(DELETE_ROW)) %>%
    ungroup() %>% select(-DELETE_ROW)
  
  
  
  # Return 'gisDF' after these changes
  return(gisDF)
  
}



outputResults <- function (wsName, WS_pod_points_Merge, wsLine_Buffer_Intersect, wsBound_Inner_Intersect, wsMention) {
  
  # Write the four output variables to a spreadsheet
  # (Create a shapefile as well)
  
  
  
  # Initialize the workbook
  wb <- createWorkbook()
  
  
  
  # Create a combined version of all four variables
  allDF <- wsBound_Inner_Intersect %>%
    bind_rows(wsLine_Buffer_Intersect, WS_pod_points_Merge, wsMention) %>%
    unique() %>%
    arrange(APPLICATION_NUMBER, POD_ID) %>%
    deleteIdentical("POD_ID")
  
  
  
  # Write 'allDF' to a GeoJSON file
  st_write(allDF, paste0("IntermediateData/PODs_of_Interest_", wsName, ".GeoJSON"))
  
  
  
  # Drop the coordinate data from 'allDF' (making it just a tibble)
  # (This is necessary for writing the data in a tabular format)
  # (This is also why duplicated latitude and longitude columns were used)
  allDF <- allDF %>%
    st_drop_geometry()
  
  
  
  # Add a worksheet for the manual review that contains a portion of the columns in this variable
  addWorksheet(wb, "Review")
  
  writeData(wb, "Review",
            allDF %>%
              select(APPLICATION_NUMBER, POD_ID, WATER_RIGHT_TYPE, URL, COUNTY,
                     FFMTRS, MTRS, MTRS_Match, PARCEL_NUMBER, 
                     LATITUDE, LONGITUDE, NORTH_COORD, EAST_COORD,
                     SOURCE_NAME, TRIB_DESC) %>%
              unique() %>%
              mutate(ERROR_CASE = NA_character_,
                     ERROR_RESOLVED = NA,
                     NEW_LATITUDE = NA_real_,
                     NEW_LONGITUDE = NA_real_,
                     NEW_MTRS = NA_character_,
                     NOTES = NA_character_,
                     REVIEWED_BY = NA_character_))
  
  
  
  # Add a separate worksheet to hold 'allDF' with all of its columns
  addWorksheet(wb, "Combined")
  
  
  writeData(wb, "Combined", allDF)
  
  
  
  # Have separate worksheets for each variable too
  addWorksheet(wb, "MTRS_and_FFMTRS")
   
  writeData(wb, "MTRS_and_FFMTRS", WS_pod_points_Merge %>% st_drop_geometry())
  
   
   
  addWorksheet(wb, "One_Mile_Buffer")
   
  writeData(wb, "One_Mile_Buffer", wsLine_Buffer_Intersect %>% st_drop_geometry())
  
   
   
  addWorksheet(wb, "One_Mile_or_More_Inside_WS")
  
  writeData(wb, "One_Mile_or_More_Inside_WS", wsBound_Inner_Intersect %>% st_drop_geometry())
   
   
   
  addWorksheet(wb, "Mentions_WS")
   
  writeData(wb, "Mentions_WS", wsMention %>% st_drop_geometry())
  
  
  
  # Finally, create a summary table that identifies which task each POD was gathered from
  task1 <- WS_pod_points_Merge %>%
    st_drop_geometry() %>%
    select(APPLICATION_NUMBER, POD_ID) %>%
    mutate(MATCHING_MTRS_OR_FFMTRS = TRUE)
  
  
  
  task2 <- wsLine_Buffer_Intersect %>%
    st_drop_geometry() %>%
    select(APPLICATION_NUMBER, POD_ID) %>%
    mutate(WITHIN_ONE_MILE_OF_WATERSHED_BOUNDARY_LINE = TRUE)
  
  
  
  task3 <- wsBound_Inner_Intersect %>%
    st_drop_geometry() %>%
    select(APPLICATION_NUMBER, POD_ID) %>%
    mutate(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = TRUE)
  
  
  
  task4 <- wsMention %>%
    st_drop_geometry() %>%
    select(APPLICATION_NUMBER, POD_ID) %>%
    mutate(MENTIONS_WATERSHED_IN_SOURCE_INFORMATION = TRUE)
  
  
  
  # Join all four variables together
  combinedDF <- task1 %>%
    full_join(task2, by = c("APPLICATION_NUMBER", "POD_ID")) %>%
    full_join(task3, by = c("APPLICATION_NUMBER", "POD_ID")) %>%
    full_join(task4, by = c("APPLICATION_NUMBER", "POD_ID")) %>%
    arrange(APPLICATION_NUMBER, POD_ID) %>%
    mutate(MATCHING_MTRS_OR_FFMTRS = replace_na(MATCHING_MTRS_OR_FFMTRS, FALSE),
           WITHIN_ONE_MILE_OF_WATERSHED_BOUNDARY_LINE = replace_na(WITHIN_ONE_MILE_OF_WATERSHED_BOUNDARY_LINE, FALSE),
           ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = replace_na(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY, FALSE),
           MENTIONS_WATERSHED_IN_SOURCE_INFORMATION = replace_na(MENTIONS_WATERSHED_IN_SOURCE_INFORMATION, FALSE))
  
  
  
  # Add that variable to the spreadsheet
  addWorksheet(wb, "POD_Selection_Info")
  
  writeData(wb, "POD_Selection_Info", combinedDF)
  
  
  
  # Also, make it the second sheet in the workbook
  worksheetOrder(wb) <- c(worksheetOrder(wb)[1],
                          tail(worksheetOrder(wb), 1),
                          worksheetOrder(wb)[-c(1, tail(worksheetOrder(wb), 1))])
  
  
  
  # Save 'wb' to a file
  saveWorkbook(wb, 
               paste0("IntermediateData/GIS_Preprocessing_", wsName, ".xlsx"), overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####


print(paste0("Starting 'GIS_Preprocessing.R' with watershed name ", wsName, "..."))


mainProcedure(wsName)


remove(mainProcedure, confirmCS, deleteIdentical, outputResults, wsName)