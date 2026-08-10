# Check if all PODs in a watershed's dataset intersect with a catchment layer

# Required fields in "Watershed_Demand_Dataset_Paths.xlsx":

# Watershed Boundary Layer:
#    (*) WATERSHED_BOUNDARY_DATABASE_PATH
#    (*) WATERSHED_BOUNDARY_LAYER_NAME
#    (*) IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY

# Watershed Catchments Layer:
#    (*) SUBBASIN_POLYGONS_DATABASE_PATH
#    (*) SUBBASIN_POLYGONS_LAYER_NAME
#    (*) IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS

# POD Coordinates Spreadsheet:
#    (*) POD_COORDINATES_SPREADSHEET_PATH
#    (*) POD_COORDINATES_WORKSHEET_NAME
#    (*) IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET
#    (*) POD_COORDINATES_REFERENCE_SYSTEM


#### SETUP ####

base::remove(list = ls())

require(dplyr)
require(sf)
require(mapview)
require(polylabelr)

source("Shared_Scripts/!Shared_Functions_Importer.R")

# Import relevant functions from the subbasin assignment script
functionStealer("W1_Watershed_Demand/Scripts/Assign_Subbasin_via_Connectivity_Matrix.R", "checkOverlap")


# Choose a watershed using "Watershed_Selection.R"
source("W1_Watershed_Demand/Scripts/Watershed_Selection.R")


# Read in a spreadsheet with coordinate data and convert it into a spatial feature
#   (Also, keep copies of the latitude and longitude coordinates in new columns)
#   (Otherwise, when the geometry is dropped, the coordinate data is removed)
POD <- getXLSX(ws = ws, 
               SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
               FILEPATH = "POD_COORDINATES_SPREADSHEET_PATH",
               WORKSHEET_NAME = "POD_COORDINATES_WORKSHEET_NAME") %>%
  select(APPLICATION_NUMBER, POD_ID, LONGITUDE, LATITUDE) %>% unique() %>%
  mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
  st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = ws$POD_COORDINATES_REFERENCE_SYSTEM[1])


# Also import a layer with the watershed's subbasins
# (There should be one polygon per subbasin)
subWS <- getGIS(ws = ws, 
                GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME")


# Change the CRS of 'subWS' and 'POD' to the same projection
subWS <- st_transform(subWS, "epsg:3488")
POD <- st_transform(POD, "epsg:3488")


# Confirm that all PODs overlap with a catchment polygon
POD_rev <- checkOverlap(POD, subWS) |>
  suppressMessages()


# Check for any PODs that were revised by `checkOverlap`
# (That's an indication that those PODs did not overlap with a catchment)
if (any(st_geometry(POD_rev) != st_geometry(POD))) {
  
  # PODs modified by `checkOverlap` will have different point geometries from 'POD'
  issuePODs <- POD[which(st_geometry(POD_rev) != st_geometry(POD)), ]
  
  
  # Output messages
  cat("\n\n")
  print(paste0(nrow(issuePODs), " POD(s) did not intersect with a catchment!"))
  cat("\n\n")
  print(issuePODs)
  
  
  # Create a map as well
  print(mapview(subWS, col.regions = "gray") + mapview(issuePODs))
  
  
  # Alternatively, if no PODs were modified, no issues were detected
} else {
  
  cat("\n\n")
  print("All PODs intersect with a catchment!")
  
}


# Clear the environment afterwards
base::remove(list = ls())
