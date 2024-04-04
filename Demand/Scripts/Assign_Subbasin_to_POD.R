# This script reads in a shapefile of POD points and polygon data of Russian River subbasins
# Then, it uses the sf package's st_intersection() function to assign 

library(tidyverse)
library(readxl)
library(openxlsx)
library(sf)


cat("Starting 'Assign_Subbasin_to_POD.R'...\n")


# Read in a spreadsheet with coordinate data and convert it into a spatial feature
#   (Also, keep copies of the latitude and longitude coordinates in new columns)
#   (Otherwise, when the geometry is dropped, the coordinate data is removed)
POD <- getXLSX(ws, "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
               "POD_COORDINATES_SPREADSHEET_PATH",
               "POD_COORDINATES_WORKSHEET_NAME") %>%
  select(APPLICATION_NUMBER, POD_ID, LONGITUDE, LATITUDE) %>% unique() %>%
  mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = ws$POD_COORDINATES_REFERENCE_SYSTEM[1])



subWS <- getGIS(ws, "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                "SUBBASIN_POLYGONS_DATABASE_PATH",
                "SUBBASIN_POLYGONS_LAYER_NAME")



# Change the CRS of 'subWS' and 'POD' to the same projection
subWS <- st_transform(subWS, "epsg:3488")
POD <- st_transform(POD, "epsg:3488")



# Check that each 'POD' intersects with a subbasin
overlapCheck <- st_intersects(POD, subWS) %>% lengths()



# Stop if there are any lengths greater than or less than 1 (points present in more than one subbasin)
# (No code has been written to handle that case)
stopifnot(sum(overlapCheck > 1) == 0)
stopifnot(sum(overlapCheck < 1) == 0)



# Create an alternate version of 'subWS'
# This will have a 50 meter buffer added to the subbasin boundaries
#bufferPoly <- st_buffer(subWS, 50)
#bufferOverlap <- st_intersects(POD, bufferPoly) %>% lengths()



# Perform the intersections for 'POD' with both 'subWS' and 'bufferPoly'
subbasinPOD <- st_intersection(POD,
                               subWS %>% select(all_of(ws$SUBBASIN_FIELD_ID_NAMES %>%
                                                         str_split(";") %>% unlist() %>% trimws())))


#subbasinPOD_Buffer <- st_intersection(POD,
#                                      bufferPoly %>% select(Basin_ID, Basin_Num, Grouping))



# Extract data frames of the attribute tables from 'subbasinPOD' and 'subbasinPOD_Buffer'
podTable <- st_drop_geometry(subbasinPOD)
#podTable_Buffer <- st_drop_geometry(subbasinPOD_Buffer)



# Add a column to indicate whether each row's "POD_ID" and "APPLICATION_NUMBER" pair appears more than once
uniqueCounts <- podTable %>%
  group_by(APPLICATION_NUMBER, POD_ID) %>%
  summarize(COUNTS = n(), .groups = "drop")


podTable <- podTable %>%
  left_join(uniqueCounts, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "many-to-one") %>%
  mutate(HAS_DUPLICATES = if_else(COUNTS > 1, TRUE, FALSE)) %>%
  select(-COUNTS)



#uniqueCounts_Buffer <- podTable_Buffer %>%
#  group_by(APPLICATION_NUMBER, POD_ID) %>%
#  summarize(COUNTS = n(), .groups = "drop")


#podTable_Buffer <- podTable_Buffer %>%
#  left_join(uniqueCounts_Buffer, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "many-to-one") %>%
#  mutate(HAS_DUPLICATES = if_else(COUNTS > 1, TRUE, FALSE)) %>%
#  select(-COUNTS)



# Check if each right has more than one subbasin assigned
# Select unique combinations of "APPLICATION_NUMBER" and the subbasin ID field(s)
appRecords <- podTable %>%
  select(APPLICATION_NUMBER, 
         all_of(ws$SUBBASIN_FIELD_ID_NAMES %>% str_split(";") %>% unlist() %>% trimws())) %>%
  unique()



# If there are fewer instances of "APPLICATION_NUMBER" than the number of rows in 'appRecords',
# a water right's number appears more than once with different sets of data
if (length(unique(appRecords$APPLICATION_NUMBER)) != nrow(appRecords)) {
  
  message("A manual review is needed for this watershed. At least one water right has more than one assigned sub-basin")
  
  
  
  # Get a list of rights with multiple entries
  appRecords <- appRecords %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(FREQ = n(), .groups = "drop") %>%
    filter(FREQ > 1) %>%
    select(APPLICATION_NUMBER)
  
  
  
  # Add fields to 'podTable' about this issue
  podTable <- podTable %>%
    mutate(MULTIPLE_SUBBASINS_ASSIGNED = APPLICATION_NUMBER %in% appRecords$APPLICATION_NUMBER,
           MANUALLY_ASSIGNED_SUBBASIN = NA) %>%
    arrange(APPLICATION_NUMBER)
  
}



# Output the results to spreadsheets
podTable %>%
  write_xlsx(paste0("OutputData/", ws$ID, "_POD_Subbasin_Assignment.xlsx"))



#podTable_Buffer %>%
  #write.xlsx("OutputData/POD_Subbasin_Assignment_50m_Buffer.xlsx", overwrite = TRUE)


cat("Done!\n")



# Remove the variables from the workspace
remove(podTable, #podTable_Buffer, 
       subbasinPOD, #subbasinPOD_Buffer, 
       #bufferPoly, 
       POD, subWS, 
       uniqueCounts, #uniqueCounts_Buffer, bufferOverlap, 
       overlapCheck,
       appRecords)

print("The 'Assign_Subbasin_to_POD.R' script is done running!")
