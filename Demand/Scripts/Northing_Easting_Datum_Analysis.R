# Given Northing and Easting values (projected coordinates), 
# try different datums (i.e., NAD83 and NAD27)
# and State Plane Coordinate System (SPCS) zones
# Then, output corresponding NAD83 geographic coordinates (latitude and longitude)


require(tidyverse)
require(readxl)
require(sf)
require(cli)


# STEP 1
# !!! Input Northing and Easting coordinates here !!!
# !!! Only one number per variable !!!
Northing <- 348200
Easting <- 1649800


# STEP 2
# !!! Hit the "Source" button to run this script !!!
# !!! It's located in the UPPER-RIGHT corner of the script pane !!!!



#### DO NOT CHANGE ANYTHING ELSE IN THIS SCRIPT ####






#### Script Procedure ####

# Create a tibble with different CCS Zone Northing/Easting reference systems
resTibble <- data.frame(ZONE = sort(c(1:7, 1:7)),
                        DATUM = c("NAD27", "NAD83"),
                        EPSG = c("epsg:26741", "epsg:2225",     # Zone 1
                                 "epsg:26742", "epsg:2226",     # Zone 2
                                 "epsg:26743", "epsg:2227",     # Zone 3
                                 "epsg:26744", "epsg:2228",     # Zone 4
                                 "epsg:26745", "epsg:2229",     # Zone 5
                                 "epsg:26746", "epsg:2230",     # Zone 6
                                 "epsg:26799", NA_character_),  # Zone 7 
                        #(NOTE: LA County is the sole county in Zone 7, 
                        # but with the NAD83 revision, it is included as part of Zone 5)
                        UNITS = c("FT", "FT"),
                        NAD83_LATITUDE = NA_real_,
                        NAD83_LONGITUDE = NA_real_,
                        DIST_TO_WS = NA_real_) %>%
  filter(!is.na(EPSG))



# Read in the watershed boundaries as well
source("Scripts/Watershed_Selection.R")



# Based on the value of "NAME" in 'ws', read in a different boundary layer
# (The assigned variable name should always be 'wsBound')
wsBound <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                  GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") %>%
  st_transform("NAD83")



# In each row, assume 'Northing' and 'Easting' are given in that reference system
# Then, convert the values to NAD83 geographic coordinates and 
# save the latitude/longitude values
for (i in 1:nrow(resTibble)) {
  
  pointVals <- data.frame(x = Easting, y = Northing) %>%
    st_as_sf(coords = 1:2, crs = resTibble$EPSG[i]) %>%
    st_transform("NAD83")
  
  
  resTibble$NAD83_LATITUDE[i] <- st_coordinates(pointVals)[2]
  
  resTibble$NAD83_LONGITUDE[i] <- st_coordinates(pointVals)[1]
  
  
  resTibble$DIST_TO_WS[i] <- st_distance(pointVals, wsBound)
  
}



# Output the results



# Identify rows that fall within the approximate bounding box of California
boundedRows <- resTibble %>%
  filter(NAD83_LATITUDE >= 33 &
           NAD83_LATITUDE <= 43 &
           NAD83_LONGITUDE >= -125 &
           NAD83_LONGITUDE <= -113) %>%
  select(ZONE, DATUM) %>%
  mutate(KEY = paste0(ZONE, "_", DATUM))



# Identify the row that is closest to the currently selected watershed as well
closestRow <- resTibble %>%
  mutate(KEY = paste0(ZONE, "_", DATUM)) %>%
  filter(KEY %in% boundedRows$KEY) %>%
  filter(DIST_TO_WS == min(DIST_TO_WS)) %>%
  select(ZONE, DATUM, KEY)



cat("\nNAD83 latitude and longitude coordinates assuming different initial datums and zones:\n\n")



outputSep <- c("\t", "\t", "\t", "\t", "\t")



cat(resTibble %>%
      select(ZONE, DATUM, UNITS, NAD83_LATITUDE, NAD83_LONGITUDE) %>%
      names() %>%
      map2_chr(outputSep, ~ paste0(.x, .y)) %>%
      paste0(collapse = ""))
cat("\tNOTES")
cat("\n")



for (i in 1:nrow(resTibble)) {
  
  outputStr <- sprintf("%s\t%s\t%s\t%.5f\t%.5f",
                       resTibble$ZONE[i],
                       resTibble$DATUM[i],
                       resTibble$UNITS[i],
                       resTibble$NAD83_LATITUDE[i],
                       resTibble$NAD83_LONGITUDE[i])
  
  
  # Gray out rows with coordinates that exceed California's bounding box
  if (!(paste0(resTibble$ZONE[i], "_", resTibble$DATUM[i]) %in% boundedRows$KEY)) {
    
    outputStr %>%
      col_grey() %>%
      cat()
    
    cat(col_grey("\t\tOut of CA bounds"))
    
  } else if (paste0(resTibble$ZONE[i], "_", resTibble$DATUM[i]) %in% closestRow$KEY) {
    
    outputStr %>%
      col_blue() %>%
      style_bold() %>%
      cat()
    
    cat(style_bold(col_blue(paste0("\t\tClosest to ", ws$ID[1]))))
    
  } else {
    
    cat(outputStr)
    
  }
  
  
  
  cat("\n")
  
}



cat("\n\nIf you need help identifying the correct zone, please see:\nhttps://www.conservation.ca.gov/cgs/rgm/state-plane-coordinate-system\n")
cat("(Different counties correspond to each zone)\n")


# Remove all variables
remove(list = ls())
