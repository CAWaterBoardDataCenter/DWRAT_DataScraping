# This script reads in a shapefile of POD points and polygon data of Russian River subbasins
# Then, it uses the sf package's st_intersection() function to assign 

library(tidyverse)
library(readxl)
library(writexl)
library(sf)


cat("Starting 'Assign_Subbasin_to_POD.R'...\n")


#### Functions ####


mainProcedure <- function (ws) {
  
  # Read in a spreadsheet with coordinate data and convert it into a spatial feature
  #   (Also, keep copies of the latitude and longitude coordinates in new columns)
  #   (Otherwise, when the geometry is dropped, the coordinate data is removed)
  POD <- getXLSX(ws, "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
                 "POD_COORDINATES_SPREADSHEET_PATH",
                 "POD_COORDINATES_WORKSHEET_NAME") %>%
    select(APPLICATION_NUMBER, POD_ID, LONGITUDE, LATITUDE) %>% unique() %>%
    mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
    st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = ws$POD_COORDINATES_REFERENCE_SYSTEM[1])
  
  
  
  # Also import a layer with the watershed's subbasins
  # (There should be one polygon per subbasin)
  subWS <- getGIS(ws, "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                  "SUBBASIN_POLYGONS_DATABASE_PATH",
                  "SUBBASIN_POLYGONS_LAYER_NAME")
  
  
  
  # Change the CRS of 'subWS' and 'POD' to the same projection
  subWS <- st_transform(subWS, "epsg:3488")
  POD <- st_transform(POD, "epsg:3488")
  
  
  
  # From 'ws', extract the field name(s) that will be carried over from 'subWS' to 'POD'
  # (They will help uniquely identify different subbasins)
  fieldNames <- ws$SUBBASIN_FIELD_ID_NAMES %>%
    str_split(";") %>% unlist() %>% trimws()
  
  
  
  # Check that each 'POD' intersects with a subbasin
  checkOverlap(POD, subWS)
  
  
  
  # Perform the intersections for 'POD' with both 'subWS'
  subbasinPOD <- st_intersection(POD,
                                 subWS %>% select(all_of(fieldNames)))
  
  
  
  # Extract a data frame of the attribute table from 'subbasinPOD'
  podTable <- st_drop_geometry(subbasinPOD)
  
  
  
  # Check for water rights with multiple subbasins assigned
  # (This can happen for rights with multiple PODs)
  # If that is the case, a manual review will be needed
  podTable <- checkForMultiBasinRights(podTable, fieldNames, ws)
  
  
  
  # Make sure 'podTable' is sorted by "APPLICATION_NUMBER" and "POD_ID"
  podTable <- podTable %>%
    arrange(APPLICATION_NUMBER, POD_ID)
  
  
  
  # The next step is to export 'podTable' to a file
  # If a manual review is needed for this watershed, add an extra worksheet to the file
  if ("MULTIPLE_SUBBASINS_ASSIGNED" %in% names(podTable)) {
    
    # Develop the manual review table in another variable
    reviewTable <- podTable %>%
      filter(MULTIPLE_SUBBASINS_ASSIGNED == TRUE) %>%
      select(APPLICATION_NUMBER) %>%
      unique()
    
    
    
    # Add columns for the field names that need to be specified
    reviewTable[fieldNames] <- NA_character_
    
    
    
    # Write both 'reviewTable' and 'podTable' to a file
    write_xlsx(list("Review" = reviewTable, 
                    "POD_Table" = podTable),
               paste0("OutputData/", ws$ID, "_POD_Subbasin_Assignment.xlsx"))
    
  } else {
   
    # Otherwise, just export 'podTable' to a file
    podTable %>%
      write_xlsx(paste0("OutputData/", ws$ID, "_POD_Subbasin_Assignment.xlsx")) 
    
  }
  
}



checkOverlap <- function (POD, subWS) {
  
  # Verify that each POD overlaps with one subbasin in 'subWS'
  
  
  
  # Check for intersections
  overlapCheck <- st_intersects(POD, subWS) %>% lengths()
  
  
  
  # Stop if there are any lengths greater than or less than 1 
  # (points present in more than one subbasin or points not present in any subbasin)
  # (No code has been written to handle either case)
  stopifnot(sum(overlapCheck > 1) == 0)
  stopifnot(sum(overlapCheck < 1) == 0)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkForMultiBasinRights <- function (podTable, fieldNames, ws) {
  
  # Rights with more than one POD might have multiple subbasins assigned to them
  # These cases must be manually reviewed because DWRAT only accepts
  # one subbasin per water right/"APPLICATION_NUMBER" value
  
  
  
  # Create a table with unique combinations of "APPLICATION_NUMBER" and the subbasin ID field(s)
  appRecords <- podTable %>%
    select(APPLICATION_NUMBER, all_of(fieldNames)) %>%
    unique()
  
  
  
  # If each "APPLICATION_NUMBER" in 'appRecords' appears once, then no rights have 
  # multiple sub-basins assigned; in that case, return 'podTable' with no changes
  if (length(unique(appRecords$APPLICATION_NUMBER)) == nrow(appRecords)) {
    
    return(podTable)
    
  }
  
  
  
  # If a manual review was already performed for this issue,
  # update 'podTable' and 'appRecords' accordingly
  if (!is.na(ws$SUBBASIN_MANUAL_ASSIGNMENT_SPREADSHEET_PATH[1])) {
    
    # Import the review spreadsheet
    reviewDF <- getXLSX(ws, "IS_SHAREPOINT_PATH_SUBBASIN_MANUAL_ASSIGNMENT",
                        "SUBBASIN_MANUAL_ASSIGNMENT_SPREADSHEET_PATH",
                        "SUBBASIN_MANUAL_ASSIGNMENT_WORKSHEET_NAME") %>%
      select(APPLICATION_NUMBER, all_of(fieldNames))
    
    
    
    # Error Check
    stopifnot(length(unique(reviewDF$APPLICATION_NUMBER)) == nrow(reviewDF))
    
    
    
    # Implement the subbasin assignments iteratively
    for (i in 1:nrow(reviewDF)) {
      
      # Similarly, update each column in 'fieldNames' one at a time
      for (j in 1:length(fieldNames)) {
        
        
        # Update the column name stored in 'fieldNames' (row 'j') 
        # where the "APPLICATION_NUMBER" matches that of 'reviewDF' (row 'i')
        # It will equal the value specified for that column in 'reviewDF'
        podTable[[fieldNames[j]]][podTable$APPLICATION_NUMBER == reviewDF$APPLICATION_NUMBER[i]] <- reviewDF[[fieldNames[j]]][i]
        
      } # End of 'j' loop
      
    } # End of 'i' loop
    
    
    
    # After the iterations, recalculate 'appRecords'
    appRecords <- podTable %>%
      select(APPLICATION_NUMBER, all_of(fieldNames)) %>%
      unique()
    
    
    
    # If all cases with multiple subbasin assignments has been addressed,
    # this statement will successfully return the updated 'podTable'
    # Otherwise, the rest of the function's procedure will be applied
    if (length(unique(appRecords$APPLICATION_NUMBER)) == nrow(appRecords)) {
      
      return(podTable)
      
    }
    
  } # End of 'if' statement (!is.na(ws$SUBBASIN_MANUAL_ASSIGNMENT_SPREADSHEET_PATH[1]))
  
  
  
  # If the code reaches this point, there are cases in 'podTable' where
  # an "APPLICATION_NUMBER" is associated with multiple subbasins
  # A manual review will be necessary then
  
  
  
  # Output a message to the console about the need for a manual review
  message("A manual review is needed for this watershed. At least one water right has more than one assigned sub-basin")
  
  
  
  # Get a list of rights with multiple entries
  appRecords <- appRecords %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(FREQ = n(), .groups = "drop") %>%
    filter(FREQ > 1) %>%
    select(APPLICATION_NUMBER)
  
  
  
  # Add a column to 'podTable' that specifies whether the row will need to be reviewed
  podTable <- podTable %>%
    mutate(MULTIPLE_SUBBASINS_ASSIGNED = APPLICATION_NUMBER %in% appRecords$APPLICATION_NUMBER)
  
  
  
  # Return 'podTable'
  return(podTable)
  
}


#### Script Execution ####


# Run the script
mainProcedure(ws)


cat("Done!\n")


# Remove the functions from the workspace
remove(mainProcedure, checkOverlap, checkForMultiBasinRights)



#### Old Code for a Buffer Subbasin Analysis ####

# Create an alternate version of 'subWS'
# This will have a 50 meter buffer added to the subbasin boundaries
#bufferPoly <- st_buffer(subWS, 50)
#bufferOverlap <- st_intersects(POD, bufferPoly) %>% lengths()





#subbasinPOD_Buffer <- st_intersection(POD,
#                                      bufferPoly %>% select(Basin_ID, Basin_Num, Grouping))



#podTable_Buffer <- st_drop_geometry(subbasinPOD_Buffer)


#uniqueCounts_Buffer <- podTable_Buffer %>%
#  group_by(APPLICATION_NUMBER, POD_ID) %>%
#  summarize(COUNTS = n(), .groups = "drop")


#podTable_Buffer <- podTable_Buffer %>%
#  left_join(uniqueCounts_Buffer, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "many-to-one") %>%
#  mutate(HAS_DUPLICATES = if_else(COUNTS > 1, TRUE, FALSE)) %>%
#  select(-COUNTS)


#podTable_Buffer %>%
#write.xlsx("OutputData/POD_Subbasin_Assignment_50m_Buffer.xlsx", overwrite = TRUE)
