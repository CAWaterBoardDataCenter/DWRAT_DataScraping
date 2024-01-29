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
# After that, assign a coordinate system to the PODs (assumed to be "NAD83")
# Then, also read in a layer containing the subbasins of the watershed
if (grepl("^Russian", ws$NAME)) {
  
  POD <- read_xlsx("InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx") %>%
    mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "NAD83")

  
  # Read in the Russian River sub-basins
  # Note: st_layers() can be used to see all available layers in the geodatabase
  subWS <- st_read("InputData/DWRAT_ForImportIntoPortal.gdb", layer = "RR_Basins_UpprAndLwr")
  
} else {
  
  stop("No POD and subbasin data was specified for this watershed")
  
}



# Change the CRS of 'subWS' to match 'POD'
subWS <- st_transform(subWS, st_crs(POD))



# Check that each 'POD' intersects with a subbasin
overlapCheck <- st_intersects(POD, subWS) %>% lengths()



# Stop if there are any lengths greater than or less than 1 (points present in more than one subbasin)
# (No code has been written to handle that case)
stopifnot(sum(overlapCheck > 1) == 0)
stopifnot(sum(overlapCheck < 1) == 0)



# Create an alternate version of 'subWS'
# This will have a 50 meter buffer added to the subbasin boundaries
bufferPoly <- st_buffer(subWS, 50)
bufferOverlap <- st_intersects(POD, bufferPoly) %>% lengths()



# Perform the intersections for 'POD' with both 'subWS' and 'bufferPoly'
subbasinPOD <- st_intersection(POD,
                               subWS %>% select(Basin_ID, Basin_Num, Grouping))


subbasinPOD_Buffer <- st_intersection(POD,
                                      bufferPoly %>% select(Basin_ID, Basin_Num, Grouping))



# Extract data frames of the attribute tables from 'subbasinPOD' and 'subbasinPOD_Buffer'
podTable <- st_drop_geometry(subbasinPOD)
podTable_Buffer <- st_drop_geometry(subbasinPOD_Buffer)



# Add a column to indicate whether each row's "POD_ID" and "APPL_NUM" pair appears more than once
uniqueCounts <- podTable %>%
  group_by(APPLICATION_NUMBER, POD_ID) %>%
  summarize(COUNTS = n(), .groups = "drop")


podTable <- podTable %>%
  left_join(uniqueCounts, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "one-to-one") %>%
  mutate(HAS_DUPLICATES = if_else(COUNTS > 1, TRUE, FALSE)) %>%
  select(-COUNTS)



uniqueCounts_Buffer <- podTable_Buffer %>%
  group_by(APPLICATION_NUMBER, POD_ID) %>%
  summarize(COUNTS = n(), .groups = "drop")


podTable_Buffer <- podTable_Buffer %>%
  left_join(uniqueCounts_Buffer, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "many-to-one") %>%
  mutate(HAS_DUPLICATES = if_else(COUNTS > 1, TRUE, FALSE)) %>%
  select(-COUNTS)



# Output the results to spreadsheets
podTable %>%
  write.xlsx(paste0("OutputData/", ws$ID, "_POD_Subbasin_Assignment.xlsx"), overwrite = TRUE)



#podTable_Buffer %>%
  #write.xlsx("OutputData/POD_Subbasin_Assignment_50m_Buffer.xlsx", overwrite = TRUE)


cat("Done!\n")



# Remove the variables from the workspace
remove(podTable, podTable_Buffer, subbasinPOD, subbasinPOD_Buffer, bufferPoly, 
       POD, subWS, uniqueCounts, uniqueCounts_Buffer, bufferOverlap, overlapCheck)

print("The 'Assign_Subbasin_to_POD.R' script is done running!")
 
# test <- st_intersects(POD, subWS)
# 
# 
# test <- st_intersection(POD, 
#                         subWS %>% select(Basin_ID, Basin_Num, Grouping))
