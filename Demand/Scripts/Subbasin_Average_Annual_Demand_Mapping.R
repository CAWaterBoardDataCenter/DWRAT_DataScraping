# Output a mapview plot of average annual demand (AF) in each sub-basin



#### Setup ####

require(tidyverse)
require(sf)
require(mapview)

options(viewer = NULL)


source("Scripts/Watershed_Selection.R")
source("Scripts/Dataset_Year_Range.R")



#### Procedure ####


# Sub-basins
subWS <- getGIS(ws = ws, 
                GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME")



# Master Demand Table
mdt <- list.files("OutputData", 
                  pattern = paste0(ws$ID, "_", yearRange[1], "_", yearRange[2], "_MDT_"),
                  full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  read_csv(show_col_types = FALSE)



# Calculate total annual demand by sub-basin
subbasinTotals <- mdt %>%
  select(BASIN, TOTAL_EXPECTED_ANNUAL_DIVERSION) %>%
  group_by(BASIN) %>%
  summarize(TOTAL_DIVERSION_AF = sum(TOTAL_EXPECTED_ANNUAL_DIVERSION, na.rm = TRUE)) %>%
  rename(!! head(ws$SUBBASIN_FIELD_ID_NAMES %>%
           str_split(";") %>% unlist() %>% trimws(), 1) := BASIN)



# Append this data to 'subWS'
subWS <- subWS %>%
  left_join(subbasinTotals, 
            by = head(ws$SUBBASIN_FIELD_ID_NAMES %>%
                        str_split(";") %>% unlist() %>% trimws(), 1))



# Output a plot of 'subWS'
print(mapview(subWS, zcol = "TOTAL_DIVERSION_AF"))



# Clear the environment
remove(list = ls())
