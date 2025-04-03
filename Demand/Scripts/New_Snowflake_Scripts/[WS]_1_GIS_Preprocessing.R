# Generate a manual review spreadsheet for determining 
# the water rights that divert from a watershed

# PODs will be collected for review using one of several flags:
#     (1) POD PLSS information matches watershed PLSS information
#     (2) PODs spatially located one mile or more within the watershed boundary
#     (3) PODs spatially located up to one mile within the watershed boundary
#     (4) PODs whose "WATERSHED", "SOURCE_NAME", and/or "TRIB_DESC" fields 
#         reference the watershed



#### Setup ####


remove(list = ls())



require(cli)
require(tidyverse)
require(sf)
require(writexl)



print("Starting '[WS]_1_GIS_Preprocessing.R'...")



source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")
source("Scripts/New_Snowflake_Scripts/[HELPER]_3_Watershed_Selection.R")


#### Procedure ####



cat("\n\n")
cat("Gathering watershed and water right files...\n\n\n\n")



# Based on the selection in "[HELPER]_3_Watershed_Selection.R", 
# 'ws' will correspond to a specific watershed
# Given a filepath in 'ws', a different boundary layer will be read in
# (The assigned variable name will always be 'wsBound')
wsBound <- getGIS(ws, 
                  "WATERSHED_BOUNDARY_DATABASE_SHAREPOINT_PATH",
                  "WATERSHED_BOUNDARY_LAYER_NAME")



cat("\n\n")



# After that, import a full list of PODs from eWRIMS
# (1) Use the most recent "POD Subset" CSV file
# (2) Read in every column and make new columns for "LATITUDE" and "LONGITUDE" that are numeric 
#     (The data in these columns will not be easily accessible afterwards, so that's why copies were used)
# (3) Then, make 'pod_points_statewide' into a GIS layer
#     Use the numeric "LATITUDE2" and "LONGITUDE2" layers as coordinates
#     Set the coordinate reference system as "NAD83"
pod_points_statewide <- "Program Watersheds/7. Snowflake Demand Data Downloads/eWRIMS Flat File POD Subset/" %>% 
  makeSharePointPath() %>% 
  list.files(full.names = TRUE) %>% sort() %>% tail(1) %>%
  fileRead("read_csv") %>%
  mutate(LONGITUDE2 = as.numeric(LONGITUDE), LATITUDE2 = as.numeric(LATITUDE)) %>%
  filter(!is.na(LONGITUDE2)) %>% 
  unique() %>%
  st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = "NAD83")



# Import PLSS Sections for the entire state
PLSS_Sections_Fill <- "Program Watersheds/1. Watershed Folders/Navarro River/Data/GIS Datasets/Public_Land_Survey_System_(PLSS)%3A_Sections.geojson" %>%
  makeSharePointPath() %>%
  st_read()



#### IMPORTANT NOTE ####
message("\nThe PLSS Sections file should really be updated eventually\n\n")



# Ensure that all of these layers' have the same projected coordinate system
# Use "epsg:3488" / "NAD83(NSRS2007) / California Albers" for that
wsBound <- wsBound %>%
  st_transform("epsg:3488")

pod_points_statewide <- pod_points_statewide %>%
  st_transform("epsg:3488")

PLSS_Sections_Fill <- PLSS_Sections_Fill %>%
  st_transform("epsg:3488")



# Flag #1: PLSS Method

# Get the PLSS sections that intersect with the watershed
# Get the PODs that intersect with these PLSS sections AND/OR have matching MTRS fields



# First verify that 'pod_points_statewide" and 'PLSS_Sections_Fill' have similar column formats
if (sum(pod_points_statewide$FFMTRS %in% PLSS_Sections_Fill$MTRS) == 0) {
  
  stop(paste0("The 'MTRS' field in the PLSS dataset and the 'FFMTRS' field in the ", 
              "POD dataset have no matches. This should not happen and is somewhat concerning.\n\n",
              "Check to see if the formatting of the PLSS strings are the same in both ",
              "datasets. It should be '[MERIDIAN]-[TOWNSHIP]-[RANGE]-[SECTION]' with a ",
              "three-character meridian; two-digit township (between 'T' and either 'N' or 'S'); ",
              "two-digit range (between 'R' and either 'E' or 'W'); and a one- or two-digit section.",
              "\n\nIf the columns are dissimilar and no errors are present, modifications to ",
              "either the PLSS dataset or '[CA]_2_POD_Flat_File_Prep.R' may be needed.") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("no", col_red("no")) %>%
         str_replace("matches", col_red("matches")) %>%
         str_replace("formatting", col_blue("formatting")) %>%
         str_replace("same", col_blue("same")) %>%
         str_replace(".MERIDIAN...TOWNSHIP...RANGE...SECTION.", 
                     col_green("[MERIDIAN]-[TOWNSHIP]-[RANGE]-[SECTION]")) %>%
         str_replace("modifications", col_magenta("modifications")))
  
}




