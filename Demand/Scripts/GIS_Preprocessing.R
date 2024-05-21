# Perform the steps to identify PODs within a watershed
# (A manual review will be needed on the output)

# This script automates the procedure described in "GIS Pre-Processing SDU Manual.docx"


#### Dependencies ####

require(tidyverse)
require(sf)
require(openxlsx)

#### Functions ####

mainProcedure <- function () {
  
  # Given the watershed in 'ws', perform the GIS pre-processing steps
  source("Scripts/Watershed_Selection.R")
  
  
  
  # Based on the value of "NAME" in 'ws', read in a different boundary layer
  # (The assigned variable name should always be 'wsBound')
  wsBound <- getGIS(ws = ws, 
                    GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                    GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                    GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME")
  
  
  
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
  
  
  
  # Import PLSS Sections for the entire state
  PLSS_Sections_Fill <- st_read(makeSharePointPath(filePathFragment = "Watershed Folders/Navarro/Data/GIS Datasets/Public_Land_Survey_System_(PLSS)%3A_Sections.geojson"))
  
  
  
  # Ensure that all of these layers' have the same projected coordinate system
  # Use "NAD83(NSRS2007) / California Albers" for that
  wsBound <- confirmCS(wsBound)
  pod_points_statewide <- confirmCS(pod_points_statewide)
  PLSS_Sections_Fill <- confirmCS(PLSS_Sections_Fill)
  
  
  
  # There are four different tasks that will be completed:
  
  # (1) Get PODs with a MTRS or FFMTRS that lies within the watershed boundaries
  # (Based on PLSS overlap with watershed polygon)
  
  # (2) Get all PODs within one mile of the boundary
  # (This action is currently not being performed; the code is there, but it is commented out)
  # (This can add a lot of extra work for very little gain)
  
  # NEW (2) Get all PODs within one mile of the boundary (on the inside only)
  
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
  
  
  
  #### (Inactive) Task 2 (Watershed Boundary Line Buffer) ####
  
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
  #wsLine_Buffer_Intersect <- st_intersection(pod_points_statewide_spatial, wsLine_Buffer)
  
  
  
  #### Task 3 (Watershed Intersect) ####
  
  # Get all PODs that intersect with the watershed polygon
  # (Exclude the ones within the one-mile buffer)
  
  
  # Get a smaller version of 'wsBound' (removing the overlap with 'wsLine_Buffer')
  wsBound_Inner <- st_difference(wsBound, wsLine_Buffer)
  
  
  
  # Get all PODs that intersect with 'wsBound_Inner'
  wsBound_Inner_Intersect <- st_intersection(pod_points_statewide_spatial, wsBound_Inner)
  
  
  
  #### Task NEW 2 (Less than one mile from the boundary--inside only) ####
  
  # (Set after Task 3 because its variables will be used)
  
  # Get PODs that are within one mile of the watershed boundary (inside only)
  
  
  # Create a polygon using the difference between 'wsBound' and 'wsBound_Inner'
  # (It will be the one-mile strip on the inside of the boundaries)
  oneMilePoly <- st_difference(wsBound, wsBound_Inner)
  
  
  # Get all PODs that intersect with 'oneMilePoly'
  wsBound_OneMile_Intersect <- st_intersection(pod_points_statewide_spatial, oneMilePoly)
  
  
  
  #### Task 4 (Watershed Tributary/Source) ####
  
  
  # Get all PODs that mention the watershed in their source/tributary information
  # (Case-Insensitive Regex searching)
  wsMention <- pod_points_statewide_spatial %>%
    filter(grepl(ws$WATERSHED_COLUMN_SEARCH_STRING, WATERSHED, ignore.case = TRUE) |
             grepl(ws$SOURCE_NAME_COLUMN_SEARCH_STRING, SOURCE_NAME, ignore.case = TRUE) |
             grepl(ws$TRIB_DESC_COLUMN_SEARCH_STRING, TRIB_DESC, ignore.case = TRUE))

  
  
  # Additional watershed-specific searches can be added here as needed
  # For example:
  # if (ws$ID == "NV") {
  #   wsMention <- bind_rows(wsMention,
  #                          pod_points_statewide_spatial %>%
  #                            filter(grepl(???, WATERSHED, ignore.case = TRUE))) %>%
  #    unique()
  # }
  
  
  
  # Output the four variables to a spreadsheet for further analysis
  # ('WS_pod_points_Merge', 'wsLine_Buffer_Intersect', 'wsBound_Inner_Intersect', and 'wsMention')
  #outputResults(ws, WS_pod_points_Merge, wsLine_Buffer_Intersect, wsBound_Inner_Intersect, wsMention)
  #outputResults_NoTask2(ws, WS_pod_points_Merge, wsBound_Inner_Intersect, wsMention)
  outputResults(ws, WS_pod_points_Merge, wsBound_OneMile_Intersect, wsBound_Inner_Intersect, wsMention)
  
  
  
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



outputResults <- function (ws, WS_pod_points_Merge, wsBound_OneMile_Intersect, wsBound_Inner_Intersect, wsMention) {
  
  # Write the four output variables to a spreadsheet
  # (Create a shapefile as well)
  
  
  
  # Initialize the workbook
  wb <- createWorkbook()
  
  
  
  # Create a combined version of all three variables
  allDF <- wsBound_Inner_Intersect %>%
    bind_rows(wsBound_OneMile_Intersect, WS_pod_points_Merge, wsMention) %>%
    unique() %>%
    arrange(APPLICATION_NUMBER, POD_ID) %>%
    deleteIdentical("POD_ID")
  
  
  
  # Write 'allDF' to a GeoJSON file
  # (But first remove the older version, if it exists in the directory)
  if (paste0(ws$ID, "_PODs_of_Interest.GeoJSON") %in% list.files("OutputData")) {
    
    #system("rm OutputData/NV_PODS_of_Interest.GeoJSON", intern = TRUE, wait = TRUE, invisible = FALSE, minimized = FALSE)
    invisible(file.remove(paste0("OutputData/", ws$ID, "_PODs_of_Interest.GeoJSON")))
    
  }
  
  
  
  # Check for fields with identical names (they create errors when writing the dataset)
  if (length(names(allDF)) != length(unique(toupper(names(allDF))))) {
    
    # Get a list of names that appear more than once
    multiNames <- names(table(toupper(names(allDF)))[table(toupper(names(allDF))) > 1])
    
    
    
    # Iterate through the names in 'multiNames'
    for (i in 1:length(multiNames)) {
      
      # Get the indices where these duplicate names occur
      multiIndex <- which(toupper(names(allDF)) == multiNames[i])
      
      
      # Append a number to these names (from the second instance onwards)
      for (j in 2:length(multiIndex)) {
        
        names(allDF)[multiIndex[j]] <- paste0(names(allDF)[multiIndex[j]], "_", j)
        
      }
      
    }
    
  }
  
  
  
  st_write(allDF, paste0("OutputData/", ws$ID, "_PODs_of_Interest.GeoJSON"), delete_dsn = TRUE)
  
  
  
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
  
  
  addWorksheet(wb, "Up_to_One_Mile_Inside_WS")
  
  writeData(wb, "Up_to_One_Mile_Inside_WS", wsBound_OneMile_Intersect %>% st_drop_geometry())
  
  
  addWorksheet(wb, "One_Mile_or_More_Inside_WS")
  
  writeData(wb, "One_Mile_or_More_Inside_WS", wsBound_Inner_Intersect %>% st_drop_geometry())
  
  
  
  addWorksheet(wb, "Mentions_WS")
  
  writeData(wb, "Mentions_WS", wsMention %>% st_drop_geometry())
  
  
  
  # After that, create a summary table that identifies which task each POD was gathered from
  task1 <- WS_pod_points_Merge %>%
    st_drop_geometry() %>%
    select(APPLICATION_NUMBER, POD_ID) %>%
    mutate(MATCHING_MTRS_OR_FFMTRS = TRUE)
  
  
  
  task2 <- wsBound_OneMile_Intersect %>%
    st_drop_geometry() %>%
    select(APPLICATION_NUMBER, POD_ID) %>%
    mutate(LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY = TRUE)
  
  
  
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
           LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY = replace_na(LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY, FALSE),
           ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = replace_na(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY, FALSE),
           MENTIONS_WATERSHED_IN_SOURCE_INFORMATION = replace_na(MENTIONS_WATERSHED_IN_SOURCE_INFORMATION, FALSE))
  
  
  
  # Add that variable to the spreadsheet
  addWorksheet(wb, "POD_Selection_Info")
  
  writeData(wb, "POD_Selection_Info", combinedDF)
  
  
  
  # Also, make it the second sheet in the workbook
  worksheetOrder(wb) <- c(worksheetOrder(wb)[1],
                          tail(worksheetOrder(wb), 1),
                          worksheetOrder(wb)[-c(1, tail(worksheetOrder(wb), 1))])
  
  
  
  # As a final step, add a second review sheet focused on plotting points via Stream Stats ('POD_StreamStats_Analysis.R')
  addWorksheet(wb, "R_Review")
  
  writeData(wb, "R_Review",
            allDF %>%
              left_join(task3, by = c("APPLICATION_NUMBER", "POD_ID")) %>%
              select(APPLICATION_NUMBER, POD_ID, URL, LATITUDE, LONGITUDE, NORTH_COORD, EAST_COORD, ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY) %>%
              mutate(REPORT_LATITUDE = NA, REPORT_LONGITUDE = NA, LAT_LON_CRS = NA,
                     REPORT_NORTHING = NA, REPORT_EASTING = NA, NOR_EAS_CRS = NA,
                     REPORT_SECTION_CORNER = NA, REPORT_NS_MOVE_FT = NA, REPORT_NS_DIRECTION = NA, REPORT_EW_MOVE_FT = NA, REPORT_EW_DIRECTION = NA,
                     REPORT_SECTION = NA, REPORT_TOWNSHIP = NA, REPORT_RANGE = NA, REPORT_DATUM = NA, MULTI_OPTIONS_CHOICE = NA_integer_, NOTES2 = "--") %>%
              mutate(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = replace_na(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY, FALSE)) %>%
              unique())
  
  
  
  # Save 'wb' to a file
  saveWorkbook(wb, 
               paste0("OutputData/", ws$ID, "_GIS_Preprocessing.xlsx"), overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
  
}



outputResults_NoTask2 <- function (ws, WS_pod_points_Merge, wsBound_Inner_Intersect, wsMention) {
  
  # Write the four output variables to a spreadsheet
  # (Create a shapefile as well)
  
  
  
  # Initialize the workbook
  wb <- createWorkbook()
  
  
  
  # Create a combined version of all three variables
  allDF <- wsBound_Inner_Intersect %>%
    bind_rows(WS_pod_points_Merge, wsMention) %>%
    unique() %>%
    arrange(APPLICATION_NUMBER, POD_ID) %>%
    deleteIdentical("POD_ID")
  
  
  
  # Write 'allDF' to a GeoJSON file
  # (But first remove the older version, if it exists in the directory)
  if (paste0(ws$ID, "_PODs_of_Interest.GeoJSON") %in% list.files("OutputData")) {
    
    #system("rm OutputData/NV_PODS_of_Interest.GeoJSON", intern = TRUE, wait = TRUE, invisible = FALSE, minimized = FALSE)
    invisible(file.remove(paste0("OutputData/", ws$ID, "_PODs_of_Interest.GeoJSON")))
    
  }
  
  
  
  # Check for fields with identical names (they create errors when writing the dataset)
  if (length(names(allDF)) != length(unique(toupper(names(allDF))))) {
    
    # Get a list of names that appear more than once
    multiNames <- names(table(toupper(names(allDF)))[table(toupper(names(allDF))) > 1])
    
    
    
    # Iterate through the names in 'multiNames'
    for (i in 1:length(multiNames)) {
      
      # Get the indices where these duplicate names occur
      multiIndex <- which(toupper(names(allDF)) == multiNames[i])
      
      
      # Append a number to these names (from the second instance onwards)
      for (j in 2:length(multiIndex)) {
        
        names(allDF)[multiIndex[j]] <- paste0(names(allDF)[multiIndex[j]], "_", j)
        
      }
      
    }
    
  }
  
  
  
  st_write(allDF, paste0("OutputData/", ws$ID, "_PODs_of_Interest.GeoJSON"), delete_dsn = TRUE)
  
  
  
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
  
  
  
  addWorksheet(wb, "One_Mile_or_More_Inside_WS")
  
  writeData(wb, "One_Mile_or_More_Inside_WS", wsBound_Inner_Intersect %>% st_drop_geometry())
  
  
  
  addWorksheet(wb, "Mentions_WS")
  
  writeData(wb, "Mentions_WS", wsMention %>% st_drop_geometry())
  
  
  
  # After that, create a summary table that identifies which task each POD was gathered from
  task1 <- WS_pod_points_Merge %>%
    st_drop_geometry() %>%
    select(APPLICATION_NUMBER, POD_ID) %>%
    mutate(MATCHING_MTRS_OR_FFMTRS = TRUE)
  
  
  
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
    full_join(task3, by = c("APPLICATION_NUMBER", "POD_ID")) %>%
    full_join(task4, by = c("APPLICATION_NUMBER", "POD_ID")) %>%
    arrange(APPLICATION_NUMBER, POD_ID) %>%
    mutate(MATCHING_MTRS_OR_FFMTRS = replace_na(MATCHING_MTRS_OR_FFMTRS, FALSE),
           ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = replace_na(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY, FALSE),
           MENTIONS_WATERSHED_IN_SOURCE_INFORMATION = replace_na(MENTIONS_WATERSHED_IN_SOURCE_INFORMATION, FALSE))
  
  
  
  # Add that variable to the spreadsheet
  addWorksheet(wb, "POD_Selection_Info")
  
  writeData(wb, "POD_Selection_Info", combinedDF)
  
  
  
  # Also, make it the second sheet in the workbook
  worksheetOrder(wb) <- c(worksheetOrder(wb)[1],
                          tail(worksheetOrder(wb), 1),
                          worksheetOrder(wb)[-c(1, tail(worksheetOrder(wb), 1))])
  
  
  
  # As a final step, add a second review sheet focused on plotting points via Stream Stats ('POD_StreamStats_Analysis.R')
  addWorksheet(wb, "R_Review")
  
  writeData(wb, "R_Review",
            allDF %>%
              left_join(task3, by = c("APPLICATION_NUMBER", "POD_ID")) %>%
              select(APPLICATION_NUMBER, POD_ID, URL, LATITUDE, LONGITUDE, NORTH_COORD, EAST_COORD, ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY) %>%
              mutate(REPORT_LATITUDE = NA, REPORT_LONGITUDE = NA, LAT_LON_CRS = NA,
                     REPORT_NORTHING = NA, REPORT_EASTING = NA, NOR_EAS_CRS = NA,
                     REPORT_SECTION_CORNER = NA, REPORT_NS_MOVE_FT = NA, REPORT_NS_DIRECTION = NA, REPORT_EW_MOVE_FT = NA, REPORT_EW_DIRECTION = NA,
                     REPORT_SECTION = NA, REPORT_TOWNSHIP = NA, REPORT_RANGE = NA, REPORT_DATUM = NA, MULTI_OPTIONS_CHOICE = NA_integer_, `ANY_VAL?` = FALSE, NOTES2 = "--") %>%
              mutate(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = replace_na(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY, FALSE),
                     `MANUAL_OVERRIDE: KEEP POD` = NA,
                     `MANUAL_OVERRIDE: REMOVE POD` = NA) %>%
              unique())
  
  
  
  # Save 'wb' to a file
  saveWorkbook(wb, 
               paste0("OutputData/", ws$ID, "_GIS_Preprocessing.xlsx"), overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####


print("Starting 'GIS_Preprocessing.R'...")


mainProcedure()


remove(mainProcedure, confirmCS, deleteIdentical, outputResults, outputResults_NoTask2)
