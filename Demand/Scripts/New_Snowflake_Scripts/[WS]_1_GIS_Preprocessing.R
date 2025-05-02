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
# Save the filepath to a variable (because it will be used again later)
podPath <- "Program Watersheds/7. Snowflake Demand Data Downloads/eWRIMS Flat File POD Subset/" %>% 
  makeSharePointPath() %>% 
  list.files(full.names = TRUE) %>% sort() %>% tail(1)



# Using 'podPath', 
# (1) Use the most recent "POD Subset" CSV file
# (2) Read in every column and make new columns for "LATITUDE" and "LONGITUDE" that are numeric 
#     (The data in these columns will not be easily accessible afterwards, so that's why copies were used)
# (3) Then, make 'pod_points_statewide' into a GIS layer
#     Use the numeric "LATITUDE2" and "LONGITUDE2" layers as coordinates
#     Set the coordinate reference system as "NAD83"
pod_points_statewide <- podPath %>%
  fileRead("read_csv") %>%
  mutate(LONGITUDE2 = as.numeric(LONGITUDE), LATITUDE2 = as.numeric(LATITUDE)) %>%
  filter(!is.na(LONGITUDE2)) %>% 
  unique() %>%
  st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = "NAD83")



# Import PLSS Sections for the entire state
PLSS_Sections_Fill <- "Program Watersheds/1. Watershed Folders/Navarro River/Data/GIS Datasets/Public_Land_Survey_System_(PLSS)%3A_Sections.geojson" %>%
  makeSharePointPath()  %>%
  fileRead("st_read")



#### IMPORTANT NOTE ####
message("\n\nNote: The PLSS Sections file should really be updated eventually\n\n")



# Ensure that all of these layers' have the same projected coordinate system
# Use "epsg:3488" / "NAD83(NSRS2007) / California Albers" for that
wsBound <- wsBound %>%
  st_transform("epsg:3488")

pod_points_statewide <- pod_points_statewide %>%
  st_transform("epsg:3488")

PLSS_Sections_Fill <- PLSS_Sections_Fill %>%
  st_transform("epsg:3488")



# Flag #1: PLSS Method


cat("\nApplying Flag #1: PLSS Method\n\n")



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
         wrapStr() %>%
         str_replace("no", col_red("no")) %>%
         str_replace("matches", col_red("matches")) %>%
         str_replace("formatting", col_blue("formatting")) %>%
         str_replace("same", col_blue("same")) %>%
         str_replace(".MERIDIAN...TOWNSHIP...RANGE...SECTION.", 
                     col_green("[MERIDIAN]-[TOWNSHIP]-[RANGE]-[SECTION]")) %>%
         str_replace("modifications", col_magenta("modifications")))
  
}



# Spatially join 'pod_points_statewide' and 'PLSS_Sections_Fill' 
# (The result will have both 'FFMTRS' and 'MTRS' fields)
# ('FFMTRS' is the eWRIMS PLSS specification, and 'MTRS' is the PLSS information
#  of overlapping PLSS sections from 'PLSS_Sections_Fill')
pod_points_statewide_spatial <- st_join(pod_points_statewide, PLSS_Sections_Fill)



# Get a subset of 'PLSS_Sections_Fill' that intersects with 'wsBound'
WS_Section_Intersect <- st_intersection(PLSS_Sections_Fill, wsBound)



# Get the points in 'pod_points_statewide_spatial' that have a "MTRS" value in 'WS_Section_Intersect'
# Check both the "MTRS" and "FFMTRS" columns for matches
WS_pod_points_Merge <- pod_points_statewide_spatial %>%
  filter(MTRS %in% WS_Section_Intersect$MTRS | MTRS %in% WS_Section_Intersect$FFMTRS |
           FFMTRS %in% WS_Section_Intersect$MTRS | FFMTRS %in% WS_Section_Intersect$FFMTRS)



WS_pod_points_Merge %>%
  select(APPLICATION_NUMBER, POD_ID, WATER_RIGHT_TYPE, 
         LATITUDE, LONGITUDE, MTRS, FFMTRS,
         WATERSHED, SOURCE_NAME, TRIB_DESC)



# Flag #2: Overlapping PODs (One Mile or More Inside Boundary) 



cat("\n\nApplying Flag #2: Overlapping PODs (One Mile or More)\n\n")



# Identify PODs that overlap with the watershed polygon 
# AND are at least one mile inside the boundary



# The operations require a polyline of the watershed boundaries
wsLine <- st_cast(wsBound, "MULTILINESTRING")



# Create a boundary line layer with a one-mile buffer on both sides of the line
# "epsg:3488" has units of meters
wsLine_Buffer <- st_buffer(wsLine, 1 * 5280 / 3.28084) # 1 mile > feet > meters
# https://gis.stackexchange.com/questions/303126/how-to-buffer-a-pair-of-longitude-and-latitude-coordinates-in-r



# Get a smaller version of 'wsBound' (removing the overlap with 'wsLine_Buffer')
wsBound_Inner <- st_difference(wsBound, wsLine_Buffer)



# Get all PODs that intersect with 'wsBound_Inner'
wsBound_Inner_Intersect <- st_intersection(pod_points_statewide_spatial, wsBound_Inner)



# Flag #3: Overlapping PODs (Up to One Mile Inside Boundary)



cat("\n\nApplying Flag #3: Overlapping PODs (Up to One Mile)\n\n")



# Similar to the previous flag, identify PODs that overlap 
# with the watershed polygon AND are up to one mile inside the boundary



# Create a polygon using the difference between 'wsBound' and 'wsBound_Inner'
# (It will be the one-mile strip on the inside of the boundaries)
oneMilePoly <- st_difference(wsBound, wsBound_Inner)



# Get all PODs that intersect with 'oneMilePoly'
wsBound_OneMile_Intersect <- st_intersection(pod_points_statewide_spatial, oneMilePoly)



# Flag #4: Search Strings



cat("\n\nApplying Flag #4: Search Strings\n\n")



# 'ws' contains "search strings" for the
# "WATERSHED", "SOURCE_NAME", and "TRIB_DESC" eWRIMS fields
# They will be used in a regex search to identify potentially relevant PODs 



# Get all PODs that mention the watershed in their source/tributary information
# (Case-Insensitive Regex searching)
wsMention <- pod_points_statewide_spatial %>%
  filter(grepl(getPath(ws, "WATERSHED_COLUMN_SEARCH_STRING"), WATERSHED, ignore.case = TRUE) |
           grepl(getPath(ws, "SOURCE_NAME_COLUMN_SEARCH_STRING"), SOURCE_NAME, ignore.case = TRUE) |
           grepl(getPath(ws, "TRIB_DESC_COLUMN_SEARCH_STRING"), TRIB_DESC, ignore.case = TRUE))



# Additional watershed-specific searches can be added here as needed
# For example:
# if (getPath(ws, "ID") == "NV") {
#   wsMention <- bind_rows(wsMention,
#                          pod_points_statewide_spatial %>%
#                            filter(grepl(???, WATERSHED, ignore.case = TRUE))) %>%
#    unique()
# }



# The next task is to prepare a compiled data frame
# Every flagged POD will be included
# Additional columns will be appended
cat("\n\nPreparing final tables...\n\n")



# Initialize a dataframe with all unique pairs of "APPLICATION_NUMBER" and
# "POD_ID" that appear in the four flag variables
resDF <- rbind(WS_pod_points_Merge %>% select(APPLICATION_NUMBER, POD_ID),
               wsBound_Inner_Intersect %>% select(APPLICATION_NUMBER, POD_ID),
               wsBound_OneMile_Intersect %>% select(APPLICATION_NUMBER, POD_ID),
               wsMention %>% select(APPLICATION_NUMBER, POD_ID)) %>%
  unique() %>%
  st_drop_geometry()



# Mark the PODs in 'resDF' that were identified by Flag #1
resDF <- WS_pod_points_Merge %>%
  st_drop_geometry() %>%
  mutate(FLAG_PLSS_MATCH_MTRS_OR_FFMTRS = TRUE) %>%
  select(APPLICATION_NUMBER, POD_ID, FLAG_PLSS_MATCH_MTRS_OR_FFMTRS) %>%
  right_join(resDF, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "one-to-one") %>%
  mutate(FLAG_PLSS_MATCH_MTRS_OR_FFMTRS = 
           FLAG_PLSS_MATCH_MTRS_OR_FFMTRS %>% replace_na(FALSE))



# Mark the PODs in 'resDF' that were identified by Flag #2
resDF <- wsBound_Inner_Intersect %>%
  st_drop_geometry() %>%
  mutate(FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = TRUE) %>%
  select(APPLICATION_NUMBER, POD_ID, FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY) %>%
  right_join(resDF, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "one-to-one") %>%
  mutate(FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY = 
           FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY %>% replace_na(FALSE))



# Mark the PODs in 'resDF' that were identified by Flag #3
resDF <- wsBound_OneMile_Intersect %>%
  st_drop_geometry() %>%
  mutate(FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY = TRUE) %>%
  select(APPLICATION_NUMBER, POD_ID, FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY) %>%
  right_join(resDF, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "one-to-one") %>%
  mutate(FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY = 
           FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY %>% replace_na(FALSE))



# Mark the PODs in 'resDF' that were identified by Flag #4
resDF <- wsMention %>%
  st_drop_geometry() %>%
  mutate(FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION = TRUE) %>%
  select(APPLICATION_NUMBER, POD_ID, FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION) %>%
  right_join(resDF, by = c("APPLICATION_NUMBER", "POD_ID"), relationship = "one-to-one") %>%
  mutate(FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION = 
           FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION %>% replace_na(FALSE))



# Generate a manual review spreadsheet
cat("\n\nGenerating a manual review spreadsheet and Flagged POD geopackage...\n\n")



# There will be several worksheets:
# (1) A list of all flagged PODs and important columns
# (2) 'resDF', which highlights the PODs and flags that identified them
# (3) The main manual review table



# Before generating a manual review spreadsheet, 
# check if one already exists for this watershed

# If it does, attempt to populate previously reviewed rows?
# Remove those rows from the output?

# For now, just notify the user if there is overlap between the review sheets
# Note that they may be able to reuse some of their previous review effort
if (!is.na(getPath(ws, "GIS_PREPROCESSING_SPREADSHEET_SHAREPOINT_PATH"))) {
  
  cat(paste0("NOTE: A previous GIS manual review spreadsheet exists for this ",
             "watershed. There may be some overlap between that prior review and ",
             "this current spreadsheet that could be reused.") %>%
        wrapStr() %>%
        str_replace("NOTE", col_red("NOTE")) %>%
        str_replace("previous", col_blue("previous")) %>%
        str_replace("overlap", col_magenta("overlap")) %>%
        str_replace("reused", col_green("reused")))
  
}



# (1) A list of all flagged PODs and important columns
# (2) 'resDF', which highlights the PODs and flags that identified them
# (3) The main manual review table
fileWrite(list("POD_TABLE" = pod_points_statewide_spatial %>%
                 st_drop_geometry() %>%
                 right_join(resDF, by = c("APPLICATION_NUMBER", "POD_ID"), 
                            relationship = "one-to-one") %>%
                 select(APPLICATION_NUMBER, POD_ID,
                        WR_WATER_RIGHT_ID, 
                        WATER_RIGHT_TYPE,
                        WATER_RIGHT_STATUS, 
                        LATITUDE, LONGITUDE, 
                        MTRS, FFMTRS, QUARTER, QUARTER_QUARTER,
                        HUC_8_NAME, HUC_12_NAME,
                        PARCEL_NUMBER, PERMIT_ID, POD_COUNT,
                        POD_TYPE, QUAD_MAP_NAME, QUAD_MAP_NUMBER,
                        WATERSHED, SOURCE_NAME, TRIB_DESC,
                        FLAG_PLSS_MATCH_MTRS_OR_FFMTRS,
                        FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY,
                        FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY,
                        FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION) %>%
                 arrange(APPLICATION_NUMBER, POD_ID),
               "FLAG_REF" = resDF,
               "REVIEW_SHEET" = pod_points_statewide_spatial %>%
                 st_drop_geometry() %>%
                 mutate(URL = paste0("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/DocumentRetriever.jsp?", 
                                     "appNum=", APPLICATION_NUMBER,
                                     "&wrType=", WATER_RIGHT_TYPE, 
                                     "&docType=DOCS")) %>%
                 right_join(resDF, by = c("APPLICATION_NUMBER", "POD_ID"), 
                            relationship = "one-to-one") %>%
                 select(APPLICATION_NUMBER, POD_ID, URL, 
                        LATITUDE, LONGITUDE, 
                        MTRS, FFMTRS,
                        HUC_8_NAME, HUC_12_NAME,
                        WATERSHED, SOURCE_NAME, TRIB_DESC,
                        FLAG_PLSS_MATCH_MTRS_OR_FFMTRS,
                        FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY,
                        FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY,
                        FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION) %>%
                 mutate(REPORT_LAT_LON_COORDINATES = NA_character_,
                        REPORT_NOR_EAS_COORDINATES = NA_character_,
                        REPORT_PLSS_DISPLACEMENT = NA_character_,
                        NOTES = NA_character_, 
                        KEEP_OR_REMOVE_POD = NA_character_,
                        REPORT_NOR_EAS_AS_LAT_LON_COORDINATES = NA_character_,
                        REPORT_PLSS_DISPLACEMENT_AS_LAT_LON_COORDINATES = NA_character_,
                        CORRECTED_POD_COORDINATES = NA_character_) %>%
                 arrange(APPLICATION_NUMBER, POD_ID)),
          "write_xlsx",
          paste0("OutputData/", getPath(ws, "ID"), "_GIS_Manual_Review_", 
                 podPath %>% str_extract("/[0-9]+_Flat_File") %>% str_extract("[0-9]+"),
                 ".xlsx"))




# Write a gpkg to a file with all flagged PODs
pod_points_statewide_spatial %>%
  right_join(resDF, by = c("APPLICATION_NUMBER", "POD_ID"), 
             relationship = "one-to-one") %>%
  select(APPLICATION_NUMBER, POD_ID,
         WR_WATER_RIGHT_ID, 
         WATER_RIGHT_TYPE,
         WATER_RIGHT_STATUS, 
         LATITUDE, LONGITUDE, 
         MTRS, FFMTRS, QUARTER, QUARTER_QUARTER,
         HUC_8_NAME, HUC_12_NAME,
         PARCEL_NUMBER, PERMIT_ID, POD_COUNT,
         POD_TYPE, QUAD_MAP_NAME, QUAD_MAP_NUMBER,
         WATERSHED, SOURCE_NAME, TRIB_DESC,
         FLAG_PLSS_MATCH_MTRS_OR_FFMTRS,
         FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY,
         FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY,
         FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION) %>%
  fileWrite("st_write",
            paste0("OutputData/", getPath(ws, "ID"), "_Flagged_PODs_of_Interest_",
                   podPath %>%
                     str_extract("/[0-9]+_Flat_File") %>% str_extract("[0-9]+"),
                   ".gpkg"), 
            delete_dsn = TRUE)



# Output a completion message
cat(paste0("\n\n\nCreated '", 
           getPath(ws, "ID"), "_GIS_Manual_Review_",
           podPath %>%
             str_extract("/[0-9]+_Flat_File") %>% str_extract("[0-9]+"),
           ".xlsx' and '",
           getPath(ws, "ID"), "_Flagged_PODs_of_Interest_",
           podPath %>%
             str_extract("/[0-9]+_Flat_File") %>% str_extract("[0-9]+"),
           ".gpkg' ",
           "in the 'OutputData' folder!") %>%
      wrapStr() %>%
      str_replace("OutputData", col_green("OutputData")))
cat("\n\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())
