# Compare the latitude/longitude and northing/easting coordinates in the reports to the eWRIMS coordinates
# Get the distance between these coordinates and the eWRIMS coordinates

require(tidyverse)
require(sf)
require(readxl)
require(writexl)



mainProcedure <- function (ws) {
  
  # Read in the coordinate records from the GIS Pre-processing spreadsheet
  if (grepl("^Navarro", ws$NAME)) {
    
    podDF <- read_xlsx("../../../../Water Boards/Supply and Demand Assessment - Documents/Watershed Folders/Navarro/Data/GIS Preprocessing/NV_GIS_Preprocessing.xlsx", sheet = "R_Review")
    podDF <- podDF %>% select(APPLICATION_NUMBER, POD_ID, LATITUDE, LONGITUDE, REPORT_LATITUDE, REPORT_LONGITUDE, LAT_LON_CRS, REPORT_NORTHING, REPORT_EASTING, NOR_EAS_CRS, NOTES2)
    
  } else {
    
    stop(paste0("No POD review spreadsheet was specified for watershed ", ws$NAME))
    
  }
  
  
  
  # Filter 'podDF' to only columns with a report latitude/longitude or northing/easting value
  podDF <- podDF %>%
    filter(!is.na(REPORT_LATITUDE) | !is.na(REPORT_NORTHING))
  
  
  
  # Add comparison columns to 'podDF'
  podDF <- podDF %>%
    mutate(LAT_LON_DISTANCE_METERS = NA_real_,
           NOR_EAS_DISTANCE_METERS = NA_real_) %>%
    relocate(LAT_LON_DISTANCE_METERS, .after = LAT_LON_CRS) %>%
    relocate(NOR_EAS_DISTANCE_METERS, .after = NOR_EAS_CRS)
  
  
  
  # Iterate through the rows of 'podDF'
  for (i in 1:nrow(podDF)) {
    
    # Verify that the eWRIMS coordinates are not -999
    # (If that is the case, check if the correct coordinates are specified 
    # elsewhere for this APPLICATION_NUMBER/POD_ID pair)
    if (podDF$LATITUDE[i] == -999) {
      
      if (podDF %>% filter(LATITUDE != -999 & 
                           APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i] &
                           POD_ID == podDF$POD_ID[i]) %>%
          nrow() != 1) {
        
        podDF$LATITUDE[i] <- podDF$LATITUDE[podDF$LATITUDE != -999 &
                                              podDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i] &
                                              podDF$POD_ID == podDF$POD_ID[i]]
        
        
        podDF$LONGITUDE[i] <- podDF$LONGITUDE[podDF$LATITUDE != -999 &
                                                podDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i] &
                                                podDF$POD_ID == podDF$POD_ID[i]]
        
      } else {
        
        next
        
      }
      
    }
    
    
    
    # After that check, save the eWRIMS coordinates as a spatial object
    ewrimsPOD <- podDF[i, ] %>%
      select(LONGITUDE, LATITUDE) %>%
      st_as_sf(coords = 1:2, crs = "NAD83")
    
    
    
    # If the right's report contains coordinates too compare those coordinates to the eWRIMS coordinates
    if (!is.na(podDF$REPORT_LATITUDE[i]) && !is.na(podDF$REPORT_LONGITUDE[i])) {
      
      podDF$LAT_LON_DISTANCE_METERS[i] <- podDF[i, ] %>%
        select(REPORT_LONGITUDE, REPORT_LATITUDE) %>%
        st_as_sf(coords = 1:2, crs = podDF$LAT_LON_CRS[i]) %>%
        compareCoords(ewrimsPOD)
      
    }
    
    
    
    # If the right's report contains northing/easting data, compare that to the eWRIMS coordinates
    if (!is.na(podDF$REPORT_NORTHING[i]) && !is.na(podDF$REPORT_EASTING[i])) {
      
      
      # Find the CRS for this right's northing/easting data
      if (grepl("NAD83", podDF$NOR_EAS_CRS[i]) || grepl("1983", podDF$NOR_EAS_CRS[i])) {
        
        # epsg:2226 (NAD83 / California zone 2 (ftUS))
        iterCRS <- "epsg:2226"
        
      } else if (grepl("NAD27", podDF$NOR_EAS_CRS[i]) || grepl("1927", podDF$NOR_EAS_CRS[i])) {
        
        # epsg:26742 (NAD27 Zone II)
        iterCRS <- "epsg:26742"
        
      } else {
        stop(paste0("Unrecognized northing/easting CRS used: ", podDF$NOR_EAS_CRS[i]))
      }
      
      
      
      podDF$NOR_EAS_DISTANCE_METERS[i] <- podDF[i, ] %>%
        select(REPORT_EASTING, REPORT_NORTHING) %>%
        st_as_sf(coords = 1:2, crs = iterCRS) %>%
        compareCoords(ewrimsPOD)
      
    }
    
  } # End of 'i' loop through 'podDF'
  
  
  
  # Output the results to a spreadsheet
  podDF %>%
    write_xlsx(paste0("OutputData/", ws$ID, "_PODs_eWRIMS_and_Report_Coordinates_Comparison.xlsx"))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



compareCoords <- function (pod, ewrimsPOD) {
  
  # Get the distance between 'pod' and 'ewrimsPOD'
  
  
  
  # Make sure the two PODs have the same coordinate system
  # Use California Albers (m)
  pod <- st_transform(pod, "epsg:3488")
  ewrimsPOD <- st_transform(ewrimsPOD, "epsg:3488")
  
  
  
  # Calculate the distance between the points
  dist <- st_distance(pod, ewrimsPOD)
  
  
  
  # Check that 'dist' was successfully calculated
  stopifnot(length(dist) == 1)
  stopifnot(!is.na(dist))
  
  
  
  # The units should be meters
  stopifnot(attr(dist, "units")$numerator == "m")
  stopifnot(length(attr(dist, "units")$denominator) == 0)
  
  
  
  
  # Convert 'dist' from a units variable to a numeric variable
  # Then return it
  return(as.numeric(dist))
  
}


