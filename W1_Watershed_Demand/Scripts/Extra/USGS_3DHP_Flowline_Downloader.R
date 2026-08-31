# Download the flowlines for a single watershed boundary from USGS

# This script relies on a link to the "Flowlines" feature layer managed by USGS

# The full Feature Server can be viewed here:
# https://3dhp.nationalmap.gov/arcgis/rest/services/usgs_3dhp_all/FeatureServer


#### Setup ####


# Clear the environment
base::remove(list = ls())


# Required packages
require(arcgis)
require(tidyverse)
require(sf)


# Watershed selection and shared scripts
source("W1_Watershed_Demand/Scripts/Watershed_Selection.R")
source("W1_Watershed_Demand/Scripts/Shared_Functions_Demand.R")


#### Procedure ####

# Read in the selected watershed's boundaries
# Use WGS84 as its coordinate reference system
wsBound <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                  GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") |>
  st_transform("WGS84")


# Get the bounding box of the watershed
targetBox <- st_bbox(wsBound)


# Make a connection to the "Flowline" layer
usgs <- arc_open("https://3dhp.nationalmap.gov/arcgis/rest/services/usgs_3dhp_all/FeatureServer/50")


# Import flowlines present within the bounding box geometry
wsFlowlines <- arc_select(usgs, filter_geom = targetBox)


# Transform the watershed boundaries to use the same CRS as the flowlines
wsBound <- wsBound |>
  st_transform(st_crs(wsFlowlines))


# Keep only flowlines that intersect with the watershed boundaries
wsFlowlines <- wsFlowlines[st_intersects(wsFlowlines, wsBound) |> lengths() > 0, ]


# Save 'wsFlowlines' to a geoJSON file
st_write(wsFlowlines, paste0("W1_Watershed_Demand/Output/", ws$ID[1], 
                             "_USGS_3DHP_Flowlines.geojson"), append = FALSE)
