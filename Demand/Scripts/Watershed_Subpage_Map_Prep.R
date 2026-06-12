

remove(list = ls())


require(tidyverse)
require(sf)
require(mapview)


options(viewer = NULL)


source("Scripts/Watershed_Selection.R")


projLoc <- paste0("C:/Users/", Sys.info()[["user"]], 
                  "/Documents/ArcGIS/Projects/Watershed_Subpage_Map/")


huc <- c("HUC-08", "HUC-10", "HUC-12")[2]


wsBound <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                  GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") |>
  st_transform("epsg:3488") |>
  st_buffer(0)


nhdFlowlines <- getGIS(ws,
                       "IS_SHAREPOINT_PATH_NHD_FLOWLINES",
                       "NHD_FLOWLINES_DATABASE_PATH",
                       "NHD_FLOWLINES_LAYER_NAME") |>
  st_zm() |>
  st_transform(st_crs(wsBound))


# nhdFlowlines <- st_read("NHD_H_California_State_GPKG/NHD_H_California_State_GPKG.gpkg",
#                         layer = "NHDFlowline") |>
#   st_zm() |>
#   st_transform(st_crs(wsBound))
# 
# 
# nhdFlowlines <- nhdFlowlines[lengths(st_intersects(nhdFlowlines, st_buffer(wsBound, -5))) > 0, ]



load("InputData/GIS_General/NHD_H_California_State_WBDHU12.RData")


huc12 <- huc12 |>
  st_transform(st_crs(wsBound))


huc12 <- huc12[lengths(st_intersects(huc12, st_buffer(wsBound, -5))) > 0, ]


mapview(huc12) + mapview(wsBound, col.regions = "gray")




pacific <- st_read("InputData/GIS_General/3853-s3_2002_s3_reg_pacific_ocean-geojson.json") |>
  st_transform(st_crs(wsBound))


wsBound <- wsBound |>
  st_difference(pacific)


huc12 <- huc12 |>
  st_difference(wsBound)




wsBound <- wsBound |>
  st_transform("WGS84")


huc12 <- huc12 |>
  st_transform(st_crs(wsBound))

nhdFlowlines <- nhdFlowlines |>
  st_transform(st_crs(wsBound))


# paste0(projLoc, "/Layers/Watershed_Boundary/") |>
#   list.files(full.names = TRUE) |>
#   unlink()
# 
# paste0(projLoc, "/Layers/Subwatershed_Boundary/") |>
#   list.files(full.names = TRUE) |>
#   unlink()
# 
# paste0(projLoc, "/Layers/Watershed_Boundary/") |>
#   list.files(full.names = TRUE) |>
#   unlink()


st_write(wsBound |> select(),
         paste0(projLoc, "/Layers/Watershed_Boundary/Watershed_Boundaries.shp"),
         append = FALSE)


st_write(huc12 |> select(),
         paste0(projLoc, "/Layers/Subwatershed_Boundary/Subwatershed_Boundaries.shp"),
         append = FALSE)


st_write(nhdFlowlines |> select(),
         paste0(projLoc, "/Layers/NHD_Flowlines/Modified_NHDFlowlines.shp"),
         append = FALSE)


write_lines("HUC-12",
            paste0(projLoc, "/Layers/Subwatershed_Boundary/HUC.txt"))


write_lines(huc,
            paste0(projLoc, "/Layers/Watershed_Boundary/HUC.txt"))

write_lines(ws$NAME[1],
            paste0(projLoc, "/Layers/Watershed_Boundary/NAME.txt"))

