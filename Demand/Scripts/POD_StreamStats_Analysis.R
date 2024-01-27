# Analyze the POD coordinates specified in a GIS Preprocessing spreadsheet
# Use USGS StreamStats to see the water drainage path from each set of coordinates
# Make notes in the dataset about whether the coordinates drain out of the watershed's main river

# http://www.merebrookllc.com/reading-descriptions.html


#### Dependencies ####

require(tidyverse)
require(sf)
require(mapview)
require(lwgeom)
require(readxl)
require(writexl)
require(httr)

#### Functions ####

mainProcedure <- function (ws) {
  
  # Start with gathering datasets related to the watershed
  
  
  
  # Get the watershed boundaries first
  if (grepl("^Navarro", ws$NAME)) {
    
    wsBound <- st_read("../../../../Water Boards/Supply and Demand Assessment - Documents/Watershed Folders/Navarro/Data/GIS Datasets/Navarro_River_Watershed_GIS/Navarro_River_Watershed.gpkg", "WBD_HU10_Navarro")
    
  } else {
    
    stop(paste0("No boundary filepath was specified for watershed ", ws$NAME))
    
  }
  
  
  
  # Convert that polygon into points
  # (Also, set the CRS to "California Albers", epsg:3488, which is a projected CRS with units of meters)
  wsPoints <- wsBound %>%
    st_cast("POINT") %>%
    st_transform("epsg:3488")
  
  
  
  # After that, choose a point around the exit of the watershed
  # Look for the point ID closest to the middle of the watershed exit using this code:
  # wsPoints %>% mapview()
  if (grepl("^Navarro", ws$NAME)) {
    
    wsExit <- wsPoints[525, ]
    
  } else {
    
    stop(paste0("No exit point was chosen for watershed ", ws$NAME, ". Please use the code 'mapview(wsPoints)' to view the points in 'wsPoints'. Then, choose the point (using its ID/row number) that best represents the watershed's exit area."))
    
  }
  
  
  
  # wsExit %>%
  #   st_buffer(1 * 5280 / 3.28084) %>%
  #   mapview()
  
  
  
  # Then, read in the coordinate records from the GIS Pre-processing spreadsheet
  if (grepl("^Navarro", ws$NAME)) {
    
    podDF <- read_xlsx("../../../../Water Boards/Supply and Demand Assessment - Documents/Watershed Folders/Navarro/Data/GIS Preprocessing/NV_GIS_Preprocessing.xlsx", sheet = "R_Review")
    podDF <- podDF %>% select(APPLICATION_NUMBER, POD_ID, URL, LATITUDE, LONGITUDE, NORTH_COORD, EAST_COORD, REPORT_LATITUDE, REPORT_LONGITUDE, LAT_LON_CRS, REPORT_NORTHING, REPORT_EASTING, NOR_EAS_CRS, 
                              REPORT_SECTION_CORNER, REPORT_NS_MOVE_FT, REPORT_NS_DIRECTION, REPORT_EW_MOVE_FT, REPORT_EW_DIRECTION, REPORT_SECTION, REPORT_TOWNSHIP, REPORT_RANGE, REPORT_DATUM, MULTI_OPTIONS_CHOICE, 
                              NOTES2, ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY)
    
  } else {
    
    stop(paste0("No POD review spreadsheet was specified for watershed ", ws$NAME))
    
  }
  
  
  
  # After that, load in the PLSS sections
  plssDF <- st_read("../../../../Water Boards/Supply and Demand Assessment - Documents/Watershed Folders/Navarro/Data/GIS Datasets/Public_Land_Survey_System_(PLSS)%3A_Sections.geojson")
  
  
  
  # For now, focus on PODs that are not one mile or more within the watershed boundary
  podDF <- podDF %>%
    filter(is.na(ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY) | ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY == FALSE)
  
  
  
  # Verify that all sections listed in 'podDF' correspond to exactly one row in 'plssDF'
  # (If that is not the case, stop the script)
  checkSectionMatches(podDF, plssDF)  
  
  
  
  # The next step will be to use StreamStats and check that water drains from the listed coordinates into the watershed
  # Add logical columns for whether each recorded coordinate type has a positive StreamStats result 
  podDF <- podDF %>%
    mutate(EWRIMS_LATLON_EXITS_WS = NA, EWRIMS_LATLON_OVERLAPS_WS = NA,
           REPORT_LATLON_EXITS_WS = NA, REPORT_LATLON_OVERLAPS_WS = NA,
           REPORT_NOREAS_EXITS_WS = NA, REPORT_NOREAS_OVERLAPS_WS = NA,
           REPORT_SECTION_MOVE_EXITS_WS = NA,
           REPORT_SECTION_MOVE_OVERLAPS_WS = NA) %>%
    relocate(EWRIMS_LATLON_OVERLAPS_WS, EWRIMS_LATLON_EXITS_WS, .after = LONGITUDE) %>%
    relocate(REPORT_LATLON_OVERLAPS_WS, REPORT_LATLON_EXITS_WS, .after = LAT_LON_CRS) %>%
    relocate(REPORT_NOREAS_OVERLAPS_WS, REPORT_NOREAS_EXITS_WS, .after = NOR_EAS_CRS) %>%
    relocate(REPORT_SECTION_MOVE_OVERLAPS_WS, REPORT_SECTION_MOVE_EXITS_WS, .after = MULTI_OPTIONS_CHOICE)

  
  
  # Iterate through each row of 'podDF' 
  # Evaluate every non-empty pair of coordinates using StreamStats
  for (i in 1:nrow(podDF)) {
    
    cat(paste0("Reviewing Row ", i, " of 'podDF' (", nrow(podDF), " total rows)\n"))
    
    
    
    # First check the eWRIMS latitude and longitude coordinates
    if (!is.na(podDF$LATITUDE[i]) && !is.na(podDF$LONGITUDE[i]) &&
        podDF$LATITUDE[i] != -999 && podDF$LONGITUDE[i] != -999) {
      
      
      # Get the column indices where the latitude and longitude columns are located
      colIndices <- colIndex(podDF,
                             c("EWRIMS_LATLON_OVERLAPS_WS", "EWRIMS_LATLON_EXITS_WS"))
      
      
      # Use another function to check StreamStats for the flow path from this point
      podDF[i, colIndices] <- data.frame(x = podDF$LONGITUDE[i], y = podDF$LATITUDE[i]) %>%
        st_as_sf(coords = 1:2, crs = "NAD83") %>%
        verifyWatershedOverlap(wsExit, wsBound)
      
    }
    
    
    
    # Next, check the report's latitude and longitude coordinates
    if (!is.na(podDF$REPORT_LATITUDE[i]) && !is.na(podDF$REPORT_LONGITUDE[i])) {
      
      
      # Get the column indices where the latitude and longitude columns are located
      colIndices <- colIndex(podDF,
                             c("REPORT_LATLON_OVERLAPS_WS", "REPORT_LATLON_EXITS_WS"))
      
      
      # Use another function to check StreamStats for the flow path from this point
      podDF[i, colIndices] <- data.frame(x = podDF$REPORT_LONGITUDE[i], y = podDF$REPORT_LATITUDE[i]) %>%
        st_as_sf(coords = 1:2, crs = podDF$LAT_LON_CRS[i]) %>%
        verifyWatershedOverlap(wsExit, wsBound)
      
    }
    
    
    
    # Then consider the report's northing and easting values, if provided
    if (!is.na(podDF$REPORT_EASTING[i]) && !is.na(podDF$REPORT_NORTHING[i])) {
      
      
      # Get the column indices where the northing and eastung columns are located
      colIndices <- colIndex(podDF,
                             c("REPORT_NOREAS_OVERLAPS_WS", "REPORT_NOREAS_EXITS_WS"))
      
      
      
      # Find the CRS used next
      if (podDF$NOR_EAS_CRS[i] == "NAD83 Zone 2") {
        
        # epsg:2226 (NAD83 / California zone 2 (ftUS))
        iterCRS <- "epsg:2226"
        
      } else if (podDF$NOR_EAS_CRS[i] == "Zone 2, 1927") {
        
        # epsg:26742 (NAD27 Zone II)
        iterCRS <- "epsg:26742"
        
      } else {
        stop(paste0("Unrecognized northing/easting CRS used: ", podDF$NOR_EAS_CRS[i]))
      }
      
      
      
      # Use another function to check StreamStats for the flow path from this point
      podDF[i, colIndices] <- data.frame(x = podDF$REPORT_EASTING[i], y = podDF$REPORT_NORTHING[i]) %>%
        st_as_sf(coords = 1:2, crs = iterCRS) %>%
        verifyWatershedOverlap(wsExit, wsBound)
      
    }
    
    
    
    # Finally, check the entries that are movements from a section corner
    if (!is.na(podDF$REPORT_SECTION_CORNER[i])) {
      
      
      # Create a POD using the section information and movement data in 'podDF'
      movePOD <- sectionMovePOD(podDF[i, ], plssDF)
      
      
      # Get the column indices where the section movement columns are located
      colIndices <- colIndex(podDF,
                             c("REPORT_SECTION_MOVE_OVERLAPS_WS", "REPORT_SECTION_MOVE_EXITS_WS"))
      
      
      
      # Use another function to check StreamStats for the flow path from this point
      podDF[i, colIndices] <- movePOD %>%
        verifyWatershedOverlap(wsExit, wsBound)
      
    }
    
  } # End of loop i through 'podDF'
  
  
  
  # As a final step, add columns to indicate whether at least one of the columns contains "TRUE" 
  podDF <- podDF %>%
    mutate(AT_LEAST_ONE_OVERLAP = NA,
           AT_LEAST_ONE_EXIT = NA)
  
  
  
  # Iterate through the rows of 'podDF' and check if at least one of the overlap/exit checks is "TRUE"
  # The new columns will contain "FALSE" otherwise
  for (i in 1:nrow(podDF)) {
    
    podDF$AT_LEAST_ONE_OVERLAP[i] <- TRUE %in% c(podDF$EWRIMS_LATLON_OVERLAPS_WS[i],
                                                 podDF$REPORT_LATLON_OVERLAPS_WS[i],
                                                 podDF$REPORT_NOREAS_OVERLAPS_WS[i],
                                                 podDF$REPORT_SECTION_MOVE_OVERLAPS_WS[i])
    
    
    
    podDF$AT_LEAST_ONE_EXIT[i] <- TRUE %in% c(podDF$EWRIMS_LATLON_EXITS_WS[i],
                                              podDF$REPORT_LATLON_EXITS_WS[i],
                                              podDF$REPORT_NOREAS_EXITS_WS[i],
                                              podDF$REPORT_SECTION_MOVE_EXITS_WS[i])
    
  }
  
  
  
  # Save the updated 'podDF' to a file
  write_xlsx(podDF, paste0("IntermediateData/", ws$ID, "_POD_StreamStats_Review.xlsx"))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkSectionMatches <- function (podDF, plssDF) {
  
  # Wherever PLSS sections are used, there can be issues if more than one section matches the information given for a POD
  # Only one section from 'plssDF' should correspond to each match in 'podDF'
  # This function will verify that this is the case
  
  
  
  # Define a vector to hold the indices of problematic rows
  issueVec <- c()
  
  
  
  # Iterate through 'podDF' and check the rows that have PLSS information
  for (i in 1:nrow(podDF)) {
    
    # If no section information was specified for this row, skip it
    if (is.na(podDF$REPORT_SECTION[i])) {
      next
    }
    
    
    
    # Make a new variable that contains the matches for this row's PLSS information 
    matchDF <- plssDF %>%
      filter(Section == podDF$REPORT_SECTION[i] &
               Township == paste0("T", podDF$REPORT_TOWNSHIP[i] %>% str_replace("^([0-9][A-Z])$", "0\\1")) &
               Range == paste0("R", podDF$REPORT_RANGE[i] %>% str_replace("^([0-9][A-Z])$", "0\\1")) &
               Meridian == "MDM")
    
    
    
    # If there is more than one matching row, but a value was stated in "MULTI_OPTIONS_CHOICE", use that to narrow down 'matchDF'
    if (nrow(matchDF) > 1 && !is.na(podDF$MULTI_OPTIONS_CHOICE[i])) {
      matchDF <- matchDF[podDF$MULTI_OPTIONS_CHOICE[i], ]
    }
    
    
    
    # If there is more than one row in 'matchDF', or if 'matchDF' has no rows, 
    # there is an issue, and the index of this iteration should be noted 
    if (nrow(matchDF) != 1) {
      issueVec <- c(issueVec, i)
    }
    
    
    
  }
  
  
  
  if (length(issueVec) > 0) {
    
    stop(paste0("There is a problem with the PLSS information provided for one or more PODs.\n",
    "Each POD with section information should correspond to exactly one PLSS section.\n\n", 
    "If there are no matches for a POD's stated section, check that the values were input correctly.\n", 
    "If there is more than one match, use the column 'MULTI_OPTIONS_CHOICE' to narrow down the selection to one row.\n\n", 
    "The following row(s) had an issue:\n", issueVec %>% paste0(collapse = ", ")))
    
  }
  
  
  
  # If there are no issues, conclude the function and return nothing
  return(invisible(NULL))
  
}



colIndex <- function (df, nameVec) {
  
  # Get the indices where the column names in 'nameVec' appear in 'df'
  return(which(names(df) %in% nameVec))
  
}



verifyWatershedOverlap <- function (pod, wsExit, wsBound) {
  
  # Get the StreamStats flow path for 'pod'
  # Then, check if the water flows via 'wsExit'
  # (Look for overlap between the flow path and 'wsExit' with a 200m buffer distance included)
  # Also look for overlap between the flow path and the watershed boundaries ('wsBound')
  
  
  # Use StreamStats to get a flow path from 'pod'
  flowPath <- requestFlowPath(pod)
  
  
  
  # Check if any point in 'flowPath' overlaps with 'wsBound'
  overlapRes <- checkForIntersection(wsBound, flowPath)

  
  
  # Get the minimum distance between 'wsExit' and the points in 'flowPath'
  exitDist <- calcMinDistance(wsExit, flowPath)
  
  
  
  # Return a list that contains two logical values
  # The first element should indicate whether there is overlap with 'wsBound'
  # The second element should indicate whether a point is within at least 200 meters of 'wsExit'
  return(list(overlapRes, exitDist < 200))
  
}



requestFlowPath <- function (pod) {
  
  # Send a POST request to USGS StreamStats using coordinates (given in 'pod')
  # The response should contain line strings that can build a flow path from 'pod'
  
  
  
  # Ensure that 'pod' has the correct CRS (WGS84 is needed)
  pod <- pod %>%
    st_transform("epsg:4326")
  
  
  
  # Submit the POST request
  flowReq <- POST("https://streamstats.usgs.gov/navigationservices/navigation/flowpath/route", 
                  add_headers(.headers = c(#:authority:
                    #  streamstats.usgs.gov
                    ":method:" = "POST",
                    ":path:" = "/navigationservices/navigation/flowpath/route",
                    ":scheme:" = "https",
                    "Accept" = "application/json, text/plain, */*",
                    "Accept-Encoding" = "gzip, deflate, br",
                    "Accept-Language" = "en-US,en;q=0.9",
                    #Content-Length:
                    #  294
                    "Content-Type" = "application/json;charset=UTF-8", 
                    # Cookie:
                    #   AWSALB=47A+MRlQ4OVQMuc5ytXvkQekgQsquFNd1ZLy8T2C4vXMJXMgmX5KzilKA8imFfX7emnbioHjsY5QMua5CQAs65u9UtfLZiyuiarVOFgBDH8SgPmpiQtX6vhkpyzP; AWSALBCORS=47A+MRlQ4OVQMuc5ytXvkQekgQsquFNd1ZLy8T2C4vXMJXMgmX5KzilKA8imFfX7emnbioHjsY5QMua5CQAs65u9UtfLZiyuiarVOFgBDH8SgPmpiQtX6vhkpyzP
                    "Dnt" = 1,
                    #Origin:
                    #  https://streamstats.usgs.gov
                    "Referer" = "https://streamstats.usgs.gov/ss/", 
                    "Sec-Ch-Ua" = '"Not_A Brand";v="8", "Chromium";v="120", "Microsoft Edge";v="120"', 
                    "Sec-Ch-Ua-Mobile" = "?0",
                    "Sec-Ch-Ua-Platform" = "Windows",
                    #Sec-Fetch-Dest:
                    #  empty
                    #Sec-Fetch-Mode:
                    #  cors
                    #Sec-Fetch-Site:
                    #  same-origin
                    "User-Agent" = "R version 4.2.3",
                    "User-Contact" = "aakash.prashar@waterboards.ca.gov")),
                  body = paste0('[{"id":1,"name":"Start point location","required":true,"description":"Specified lat/long/crs  navigation start location","valueType":"geojson point geometry",', 
                                '"value":{"type":"Point","coordinates":[', st_coordinates(pod) %>% paste0(collapse = ","), '],"crs":{"properties":{"name":"EPSG:4326"},"type":"name"}}}]'))
  
  
  
  # Wait a bit after sending the request
  Sys.sleep(runif(1, min = 1.1, max = 1.5))
  
  
  
  # Verify that the request was successful
  stopifnot(flowReq$status_code == 200)
  
  
  
  # Extract the response from USGS
  flowRes <- content(flowReq)
  
  
  
  # Prepare to extract the coordinate data from 'flowRes'
  # Most of these features are linestrings, but all data will be kept as points
  # They will all be stored in a single data frame
  pointDF <- data.frame()
  
  
  
  # Iterate through 'flowRes'
  for (i in 1:length(flowRes$features)) {
    
      
    # All features should be "LineString" (with one "Point")
    # Throw an error if that is not the case
    if (!(flowRes$features[[i]]$geometry$type %in% c("LineString", "Point"))) {
      stop("Unknown feature type")
    }
    
    
    
    # Each feature in 'flowRes' is a linestring/point
    # They are each given as a list of different point coordinates
    # Extract those coordinates and append them to 'pointDF'
    pointDF <- bind_rows(pointDF,
                         flowRes$features[[i]]$geometry$coordinates %>%
                           unlist() %>%
                           matrix(ncol = 2, byrow = TRUE) %>%
                           data.frame() %>%
                           set_names(c("X", "Y")))
    
  }
  
  
  
  # Convert 'pointDF' into a spatial features dataset and return it
  return(pointDF %>%
           st_as_sf(coords = 1:2, crs = "WGS84"))
  
}



checkForIntersection <- function (boundary, points) {
  
  # Check if at least one of the points in 'points' overlaps with 'boundary'
  
  
  
  # First, ensure that the coordinate system is the same for both variables
  # Use California Albers
  boundary <- st_transform(boundary, "epsg:3488")
  points <- st_transform(points, "epsg:3488")
  
  
  
  # Use st_intersects() to get whether each point intersects with 'boundary'
  overlapCheck <- st_intersects(points, boundary)
  
  
  
  # Convert the list into a vector
  # If the length is greater than 0, then at least one point overlaps with the boundary
  return(length(unlist(overlapCheck)) > 0)
  
}



calcMinDistance <- function (refPoint, distPoints) {
  
  # Calculate the minimum distance between 'refPoint' and the points in 'distPoints'
  
  
  # Make sure both variables have the same coordinate system
  # Use California Albers (m)
  refPoint <- st_transform(refPoint, "epsg:3488")
  distPoints <- st_transform(distPoints, "epsg:3488")
  
  
  
  # Get the distance between 'refPoint' and each of the points in 'distPoints'
  # Extract the minimum value
  minDist <- st_distance(refPoint, distPoints) %>%
    min(na.rm = TRUE)
  
  
  
  # Check that 'minDist' was successfully calculated
  stopifnot(length(minDist) == 1)
  stopifnot(!is.na(minDist))
  
  
  
  # The units should be meters
  stopifnot(attr(minDist, "units")$numerator == "m")
  stopifnot(length(attr(minDist, "units")$denominator) == 0)
  
  
  
  # Change 'minDist' from a "units" object to a numeric object
  minDist <- minDist %>%
    as.numeric()
  
  
  
  # Return 'minDist'
  return(minDist)
  
}



sectionMovePOD <- function (podData, plssDF) {
  
  # Get POD coordinates based on approximate translations from a corner of a PLSS section
  # The column "REPORT_SECTION_CORNER" identifies the corner (sometimes it is a corner of a subsection)
  # "REPORT_NS_MOVE_FT" and "REPORT_EW_MOVE_FT" contain the distance to translate (in ft)
  # "REPORT_NS_DIRECTION" and "REPORT_EW_DIRECTION" mention the direction of movement for the distance values
  # "REPORT_SECTION", "REPORT_TOWNSHIP", "REPORT_RANGE", "REPORT_DATUM", and "MULTI_OPTIONS_CHOICE" are used to select a section in 'plssDF'
  
  
  
  # Use the information in 'podData' to extract a PLSS section from 'plssDF'
  section <- chooseSection(plssDF, podData$REPORT_SECTION, podData$REPORT_TOWNSHIP,
                           podData$REPORT_RANGE, podData$REPORT_DATUM, podData$MULTI_OPTIONS_CHOICE)
  
  
  
  # Convert 'section' from a polygon into points
  # Then, extract a single point from it based on the required corner in "REPORT_SECTION_CORNER"
  chosenPOD <- section2point(section, podData$REPORT_SECTION_CORNER)
  
  
  
  # After that, translate 'chosenPOD' by the values specified in "REPORT_NS_MOVE_FT" and "REPORT_EW_MOVE_FT"
  chosenPOD <- chosenPOD %>%
    translatePoint(podData$REPORT_NS_MOVE_FT, podData$REPORT_NS_DIRECTION,
                   podData$REPORT_EW_MOVE_FT, podData$REPORT_EW_DIRECTION)
  
  
  
  # Return 'chosenPOD' 
  return(chosenPOD)
  
}



chooseSection <- function (plssDF, section, township, range, datum, multiOptionsTiebreaker) {
  
  # Filter 'plssDF' to a section whose information matches the data in the other input variables
  # ('multiOptionsTiebreaker' is used if more than one section is present after applying the other filters)
  
  
  # If 'datum' is 'MDB&M', that corresponds to "MDM" in the "Meridian" column of 'plssDF'
  if (datum == "MDB&M") {
    meridian <- "MDM"
  } else {
    stop(paste0("Unknown datum ", datum))
  }
  
  
  
  # Filter 'plssDF'
  filteredSelection <- plssDF %>%
    filter(Section == section & 
             Township == paste0("T", township) &
             Range == paste0("R", range) &
             Meridian == meridian)
  
  
  
  # There should be only one section in 'filteredSelection'
  # If that is not the case, use 'multiOptionsTiebreaker'
  if (nrow(filteredSelection) > 1) {
    
    # If no index was specified in 'multiOptionsTiebreaker', throw an error
    if (is.na(multiOptionsTiebreaker)) {
      
      stop(paste0("The provided arguments are insufficient to identify a single section in 'plssDF'. Please use 'multiOptionsTiebreaker' to choose one option. ",
                  "This error applies to Section ", section, " Township ", township, " Range ", range, " Datum ", datum))
      
    # Otherwise, select one row of 'filteredSelection' based on the provided index
    } else {
      
      filteredSelection <- filteredSelection[multiOptionsTiebreaker, ]
      
    }
    
  }
  
  
  
  # One last error check
  stopifnot(nrow(filteredSelection) == 1)
  
  
  
  # Return 'filteredSelection'
  return(filteredSelection)
  
}



section2point <- function (section, corner) {
  
  # 'section' is a polygon that corresponds to a PLSS section
  # Convert that polygon into points and then choose a point that corresponds to 'corner'
  
  
  
  # Though, if 'corner' isn't a simple corner, a more complicated procedure is necessary
  
  
  
  # First, handle the simple cases (a corner or midpoint of the section)
  if (corner %in% c("NE", "NW", "SE", "SW")) {
    
    return(section %>% st_cast("POINT") %>%
             extractCorner(corner))
    
  # Use st_segmentize() for the midpoints to get more points
  } else if (corner %in% c("N1/4", "E1/4", "S1/4", "W1/4")) {
    
    return(section %>% st_segmentize(dfMaxLength = 0.05) %>%
             st_coordinates() %>% as.data.frame() %>% unique() %>%
             st_as_sf(coords = 1:2, crs = st_crs(section)) %>%
             extractCorner(corner))
    
  # For a center point, return the average of the minimum and maximum lat/long values in 'section'
  # (Treat "???" as "CENTER" too)
  } else if (corner == "CENTER" || corner == "???") {
    
    coordDF <- section %>% st_coordinates() %>% as.data.frame()
    
    
    return(data.frame(X = mean(c(min(coordDF$X), max(coordDF$X))), 
                      Y = mean(c(min(coordDF$Y), max(coordDF$Y)))) %>%
             st_as_sf(coords = 1:2, crs = st_crs(section)))
    
  }
  
  
  
  # The remaining cases should be complicated ones (they contain " of " in their text)
  stopifnot(grepl(" of ", corner))
  
  
  # These strings are mainly corners of subsections of 'section'
  # They will be written like "E1/4 of NW1/4 of NE1/4" (the E1/4 point of the NW quarter of the NE quarter of 'section')
  
  
  # A second PLSS data frame will be consulted for quarter-quarter sections
  # If it lacks the necessary polygon, 'section' will be iteratively divided into quarters until the desired subsection is found
  
  
  
  # First split 'corner' into the component steps
  cornerVec <- corner %>%
    str_split(" of ") %>%
    unlist() %>%
    trimws()
  
  
  
  # If 'cornerVec' has three elements, a quarter-quarter section is needed
  # The alternative PLSS dataset may already have that subsection
  # Try to find it using another function
  if (length(cornerVec) == 3) {
    
    subsection <- getSubPLSS(section$Section, section$Township, section$Range, section$Meridian)
    
    
    
    # Try to filter 'subsection' down to the required quarter-quarter section
    # The second division level ("SECDIVNO") should contain the two quarter codes as a single string
    # (For example, the SW quarter of the NE quarter of the PLSS section would be "SWNE")
    
    
    
    # Make the quarter-quarter string first
    qqStr <- cornerVec[2:3] %>%
      str_remove("1/4") %>%
      paste0(collapse = "")
    
    
    
    # Filter 'subsection'
    subsection <- subsection %>%
      filter(SECDIVNO == qqStr)
    
    
    
    # Ideally, 'subsection' should only contain one polygon now
    # In rare instances, it may have two rows containing identical polygons
    # (This is the case for Section 25, T05S R04E MDM)
    # If the polygons are identical, either one is sufficient for this procedure
    if (nrow(subsection) == 2 && NA %in% st_bbox(st_difference(subsection[1, ], subsection[2, ]))) {
      subsection <- subsection[1, ]
    }
    
    
    
    # If 'subsection' contains only one feature, the operation was successful
    # Then, this function can be rerun with 'subsection' and the first element of 'cornerVec' as inputs
    if (nrow(subsection) == 1) {
      return(section2point(subsection, cornerVec[1]))
    }
    
    
    
    # If 'subsection' contains multiple features, this is an unexpected occurrence that should be studied
    if (nrow(subsection) > 1) {
      stop(paste0("Interesting case to study: ", corner, " and ", section$MTRS))
    }
    
    
    
    # The final case is when 'subsection' contains zero rows
    # In that situation, the alternative PLSS dataset did not have a proper quarter-quarter section
    # Make a note of that and use the manual subdivision methodology to find the quarter-quarter section
    message(paste0("The BLM PLSS dataset did not have a quarter-quarter section for corner ", corner, " and section ", section$MTRS))
    
  }
  
  
  
  # If 'cornerVec' contains two elements, with the second value being "Lot ##",
  # the alternative PLSS dataset will again be useful
  # Instead of quarter sections, lot subdivisions are needed
  # The alternative PLSS dataset from the BLM contains that information too
  if (grepl(" of Lot ", corner, ignore.case = TRUE)) {
    
    
    # Right now, this script only handles cases where a corner/midpoint of a lot is needed
    # Subdivision of lots is not currently supported
    stopifnot(length(cornerVec) == 2)
    stopifnot(grepl("^[NSEW][NSEW]?(1/4)? of Lot [0-9]+$", corner, ignore.case = TRUE))
    
    
    
    # Use another function to extract the lot from the alternative PLSS dataset
    lot <- findLot(section,
                   cornerVec[2] %>% str_extract("[Ll]ot [0-9]+") %>% str_extract("[0-9]+") %>% as.numeric())
    
    
    
    # Then, rerun this function from the beginning with the new lot polygon
    return(section2point(lot, cornerVec[1]))
    
  }
  
  
  
  # For all other cases, manually split 'section' into four quarters iteratively
  # (and select the quarter polygon that matches the directional element in 'cornerVec')
  # While 'cornerVec' requires further subdivisions of 'section', its length will be greater than 1
  while (length(cornerVec) > 1) {
    
    # First, split 'section' into four quadrants using the midpoint
    section <- splitSection(section)
    
    
    
    # Choose the subsection that matches the direction stated within the last element of 'cornerVec'
    # The last element corresponds to the first subdivision action to implement
    
    
    
    # Before that, identify the proper "SUBDIVISION_CODE_" column to check
    # (It will be the one with the largest integer at the end)
    # (That one will correspond to the most recent subdivision)
    varName <- names(section) %>%
      str_subset("SUBDIVISION_CODE_") %>%
      str_extract("[0-9]+$") %>%
      as.numeric() %>% max() %>%
      paste0("SUBDIVISION_CODE_", .)
    
    
    
    # Filter the subdivision code column (identified in 'varName') using the last value in 'cornerVec'
    # 'cornerVec' has the subsection with "1/4" attached at the end, but that portion is unnecessary
    # (For example, from "SW1/4", only "SW" is needed)
    section <- section[section[[varName]] == cornerVec %>% tail(1) %>% str_remove("1/4"), ]
    
    
    
    # Double-check that only one polygon remains in 'section'
    stopifnot(nrow(section) == 1)
    
    
    
    # Then, remove the last element from 'cornerVec'
    cornerVec <- cornerVec[-length(cornerVec)]
    
  }
  
  
  
  # After the manual subdivision process, 'section' contains the required polygon
  # Rerun this function using 'section' and the remaining element in 'cornerVec'
  return(section2point(section, cornerVec[1]))
  
}



extractCorner <- function (sectionPoints, corner) {
  
  # Extract from 'sectionPoints' the point that matches 'corner' 
  
  
  # Create a data frame containing the coordinate data
  latLon <- sectionPoints %>%
    st_coordinates() %>%
    as.data.frame() %>%
    unique()
  
  
  
  # To find the correct corner point, set target values for the latitude and longitude
  # (e.g., for the NW or SW corner, the longitude should be around the minimum x-value in the dataset)
  # (The corner is not guaranteed to have the proper minimum/maximum values in the dataset, so we'll have to rely on this method)
  
  
  # First set a target longitude value
  if (corner %in% c("NW", "W1/4", "SW")) {
    
    targetLon <- min(latLon$X)
    
  } else if (corner %in% c("N1/4", "S1/4")) {
    
    targetLon <- mean(c(min(latLon$X), max(latlon$X)))
    
  } else if (corner %in% c("NE", "E1/4", "SE")) {
    
    targetLon <- max(latLon$X)
    
  } else {
    
    stop(paste0("Unknown 'corner' value: ", corner))
    
  }
  
  
  
  # Then set a target latitude value
  if (corner %in% c("NW", "N1/4", "NE")) {
    
    targetLat <- max(latLon$Y)
    
  } else if (corner %in% c("W1/4", "E1/4")) {
    
    targetLat <- mean(c(min(latLon$Y), max(latLon$Y)))
    
  } else if (corner %in% c("SW", "S1/4", "SE")) {
    
    targetLat <- min(latLon$Y)
    
  } else {
    
    stop(paste0("Unknown 'corner' value: ", corner))
    
  }
  
  
  
  # Choose a point from 'latLon' that has the minimum square error across its latitude and longitude 
  chosenPoint <- latLon %>%
    mutate(ERROR = (X - targetLon)^2 + (Y - targetLat)^2) %>%
    filter(ERROR == min(ERROR))
  
  
  
  # Ensure that only one point was chosen
  stopifnot(nrow(chosenPoint) == 1)
  
  
  
  # Return 'chosenPoint' as a spatial feature
  return(chosenPoint %>%
           select(X, Y) %>%
           st_as_sf(coords = 1:2, crs = st_crs(sectionPoints)))
  
  
  
  #mapview(section) + mapview(sectionPoints, col.regions = "red") + 
    #mapview(sectionPoints[which(TRUE == map_lgl(sectionPoints$geometry, ~ chosenPoint$X %in% . && chosenPoint$Y %in% .)), ], col.regions = "green")
    #mapview(sectionPoints[which(TRUE == map_lgl(sectionPoints$geometry, ~ max(latLon$Y) %in% .)), ], col.regions = "green")
  
  
  # sectionPoints <- section %>% st_segmentize(0.1) %>%
  #   st_coordinates() %>%
  #   as.data.frame() %>%
  #   unique() %>%
  #   st_as_sf(coords = 1:2, crs = st_crs(section))
  
  # mapview(test, col.regions = "red") + mapview(sectionPoints, col.regions = "blue")
  # 
  # 
  # chosenPoint2 <- chosenPoint %>% select(X, Y) %>% st_as_sf(coords = 1:2, crs = st_crs(section))
  # 
  # sectionPoints <- section %>% st_cast("POINT")
  # 
  # mapview(sectionPoints, col.regions = "blue") + mapview(chosenPoint2, col.regions = "red") +
  #   mapview(chosenPoint %>% select(X, Y) %>% st_as_sf(coords = 1:2, crs = st_crs(section)), col.regions = "green")
  
  
  # mapview(centroid) + mapview(section) + 
  # mapview(chosenPoint %>% st_as_sf(coords = 1:2, crs = st_crs(section)), col.regions = "green") + 
  # mapview(chosenPoint2 %>% st_as_sf(coords = 1:2, crs = st_crs(section)), col.regions = "red") + 
  # mapview(chosenPoint3 %>% st_as_sf(coords = 1:2, crs = st_crs(section)), col.regions = "blue") + 
  # mapview(chosenPoint4 %>% st_as_sf(coords = 1:2, crs = st_crs(section)), col.regions = "orange")
  
}



findLot <- function (section, lotNumber) {
  
  # Read in the BLM PLSS dataset and try to find the government lot with an ID matching 'lotNumber'
  
  
  
  # First get the subdivisions of this PLSS section ('section')
  subsections <- getSubPLSS(section$Section, section$Township, section$Range, section$Meridian)
  
  
  
  # Filter 'subsections' to just lots ("SECDIVTYP" should be "L")
  # Then, filter "SECDIVNO" to equal 'lotNumber'
  subsections <- subsections %>%
    filter(SECDIVTYP == "L") %>%
    filter(SECDIVNO == lotNumber)
  
  
  
  # Check that the operations were successful
  stopifnot(nrow(subsections) == 1)
  
  
  
  # Return the filtered lot
  return(subsections)
  
}



getSubPLSS <- function (section, township, range, meridian) {
  
  # Given:
  # 'section': The PLSS section number (as an integer)
  # 'township': The PLSS township (as 'T##N' or 'T##S')
  # 'range': The PLSS range (as 'R##E' or 'R##W')
  # 'meridian': Expecting 'MDM'
  
  
  # Find the corresponding polygons from the BLM's PLSS subdivision dataset
  
  
  
  # First read in that dataset
  # (It will appear as a variable called 'plssSub')
  load("../../../../Water Boards/Supply and Demand Assessment - Documents/Watershed Folders/Navarro/Data/GIS Datasets/PLSS_Subdivisions_BLM_20240123.RData")
  
  
  
  # Filter 'plssSub' based on the given conditions
  # Start with the meridian
  if (meridian == "MDM") {
    plssSub <- plssSub %>%
      filter(PRINMER == "Mount Diablo Meridian")
  } else {
    stop(paste0("Unknown meridian ", meridian))
  }
  
  
  
  # Then proceed with the township
  # In 'plssSub', the direction is stored separately from the township value
  # 'TWNSHPNO' contains the township number (as a three-digit value)
  # 'TWNSHPDIR' has the direction (either 'N' or 'S')
  
  
  # Split 'township' into these two values
  townshipDir <- township %>%
    str_extract("[NS]$")
  
  
  townshipNum <- township %>%
    str_extract("[0-9]+") %>% as.numeric()
  
  
  
  # For 'townshipNum', add zeroes to the value to make it a three-digit string
  townshipNum <- rep(0, 3 - str_count(townshipNum, "[0-9]")) %>%
    paste0(collapse = "") %>%
    paste0(townshipNum)
  
  
  
  # Apply these filters to 'plssSub'
  plssSub <- plssSub %>%
    filter(TWNSHPNO == townshipNum & TWNSHPDIR == townshipDir)
  
  
  
  # Handle the range after that
  # Its number and direction are separated just like the township
  # "RANGENO" contains a three-digit string and "RANGEDIR" contains either "E" or "W"
  
  
  # Split 'range' into these two values
  rangeDir <- range %>%
    str_extract("[EW]$")
  
  
  rangeNum <- range %>%
    str_extract("[0-9]+") %>% as.numeric()
  
  
  
  # For 'rangeNum', add zeroes to the value to make it a three-digit string
  rangeNum <- rep(0, 3 - str_count(rangeNum, "[0-9]")) %>%
    paste0(collapse = "") %>%
    paste0(rangeNum)
  
  
  
  # Apply these filters to 'plssSub'
  plssSub <- plssSub %>%
    filter(RANGENO == rangeNum & RANGEDIR == rangeDir)
  
  
  
  # Filter the section next
  # The section is stored under both "FRSTDIVNO" and "FRSTDIVLAB" as a two-digit string, but only if 'FRSTDIVTXT' is "SECTION"
  # This function assumes that the first division is indeed a PLSS section
  stopifnot(length(unique(plssSub$FRSTDIVTXT)) == 1 && unique(plssSub$FRSTDIVTXT) == "Section")
  
  
  
  # Convert 'section' into a two-digit string
  sectionStr <- paste0(rep(0, 2 - str_count(section, "[0-9]")),
                       section)
  
  
  
  # Apply that filter
  plssSub <- plssSub %>%
    filter(FRSTDIVNO == sectionStr)
  
  
  
  # Finally, return the filtered 'plssSub' variable
  return(plssSub)
  
}



splitSection <- function (section) {
  
  # Divide a section into four subsections
  # The split is based on the *midpoint* between the bounding lat/long values 
  
  
  
  # Extract the coordinates from 'section'
  sectionCoord <- section %>%
    st_coordinates() %>%
    as.data.frame()
  
  
  
  # Use the minimum and maximum values in 'sectionCoord' to get the midpoint
  midPoint <- data.frame(X = mean(c(min(sectionCoord$X), max(sectionCoord$X))),
                         Y = mean(c(min(sectionCoord$Y), max(sectionCoord$Y))))
  
  
  
  # Define a linestring that will split the section vertically at the midpoint
  # (This will create eastern and western halves of the section)
  vertSplit <- c(midPoint$X, min(sectionCoord$Y),
                 midPoint$X, max(sectionCoord$Y)) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    st_linestring() %>%
    st_sfc(crs = st_crs(section))
  
  
  
  # Split 'section' and save the intermediate result to 'subSections'
  # (st_split() will save it as a GEOMETRY_COLLECTION, but convert the result into POLYGONs instead)
  subSections <- st_split(section, vertSplit) %>%
    st_collection_extract("POLYGON", warn = TRUE)

  
  
  # Next, define the linestring to split the two polygons horizontally
  # This will create northern and southern halves of each polygon
  horiSplit <- c(min(sectionCoord$X), midPoint$Y,
                 max(sectionCoord$X), midPoint$Y) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    st_linestring() %>%
    st_sfc(crs = st_crs(section))
  
  
  
  # Perform the split (and again convert the result into separate polygons)
  subSections <- st_split(subSections, horiSplit) %>%
    st_collection_extract("POLYGON", warn = TRUE)
  
  
  
  # The next step will be to label each new polygon
  # "NE", "NW", "SE", and "SW" will be the labels
  # (Northeast, Northwest, Southeast, and Southwest)
  
  
  # If this is the first time splitting the section, a new variable called
  # "SUBDIVISION_CODE_1" will be added to 'subSections'
  
  # If this is NOT the first time a polygon was split, that variable already exists
  # Therefore, the column will be named "SUBDIVISION_CODE_#" instead (with the number being the next highest integer)
  
  
  
  # Decide which column name to use
  if (sum(grepl("SUBDIVISION_CODE", names(subSections))) > 0) {
    
    varName <- paste0("SUBDIVISION_CODE_",
                      names(subSections) %>% str_subset("SUBDIVISION_CODE") %>%
                        str_extract("[0-9]+$") %>% as.numeric() %>% `+`(1))
    
  } else {
    
    varName <- "SUBDIVISION_CODE_1"
    
  }
  
  
  
  # Add this column to 'subSections'
  # For now, all values in this column will be NA
  subSections <- subSections %>%
    mutate(!! varName := NA_character_)
  
  
  
  
  # To figure out which polygon corresponds to which quadrant, use the boundary boxes
  
  
  
  # Get the bboxes of each subsection
  # "POLY_ID" will help relate these values to their original polygons
  # "NS_ZONE" and "EW_ZONE" will be updated later to contain labels for north/south and east/west
  bboxes <- c(st_bbox(subSections[1, ]),
              st_bbox(subSections[2, ]),
              st_bbox(subSections[3, ]),
              st_bbox(subSections[4, ])) %>%
    matrix(ncol = 4, byrow = TRUE) %>%
    data.frame() %>%
    set_names(c("xmin", "ymin", "xmax", "ymax")) %>%
    relocate(xmax, .before = ymin) %>%
    mutate(POLY_ID = 1:nrow(subSections),
           NS_ZONE = NA_character_,
           EW_ZONE = NA_character_)
  
  
  
  # Sort the data frame by 'xmin'
  bboxes <- bboxes %>%
    arrange(xmin)
  
  
  
  # The first two rows will have the smallest longitude values
  # The next two rows will have larger values
  # Therefore, "EW_ZONE" should be "W" for the first two rows and "E" for the next two
  bboxes$EW_ZONE <- c("W", "W", "E", "E")
  
  
  
  # Next, sort the data frame by 'ymin'
  bboxes <- bboxes %>%
    arrange(ymin)
  
  
  
  # The first two rows will have the smallest latitude values
  # The next two rows will have larger values
  # Therefore, "NS_ZONE" should be "S" for the first two rows and "N" for the next two
  bboxes$NS_ZONE <- c("S", "S", "N", "N")
  
  
  
  # Based on "POLY_ID", assign a quadrant tag to the corresponding rows in 'subSections'
  for (j in 1:nrow(bboxes)) {
    
    subSections[[varName]][bboxes$POLY_ID[j]] <- paste0(bboxes$NS_ZONE[j], bboxes$EW_ZONE[j])
    
  }
  
  
  
  # Return 'subSections' after these changes
  return(subSections)
  
  
  
    
  # mapview(subSections, col.regions = "green") + mapview(section)
  # 
  # 
  # load("combinedSF.RData")
  # 
  # 
  # subTest <- combinedSF %>% filter(PRINMER == "Mount Diablo Meridian" &
  #                              grepl(section$Township %>% str_extract("[0-9]+"), TWNSHPNO) &
  #                              TWNSHPDIR == section$Township %>% str_extract(".$") &
  #                              FRSTDIVNO == if_else(section$Section < 10, paste0("0", section$Section), as.character(section$Section)) &
  #                              grepl(section$Range %>% str_extract("[0-9]+"), RANGENO) &
  #                              RANGEDIR == section$Range %>% str_extract(".$"))
  # 
  # 
  # mapview(subTest) + mapview(section, col.regions = "red")
  # 
  # 
  # 
  # mapview(test %>% filter(PRINMER == "Mount Diablo Meridian" &
  #                           TWNSHPDIR == section$Township %>% str_extract(".$") &
  #                           FRSTDIVNO == section$Section &
  #                           grepl(section$Range %>% str_extract("[0-9]+"), RANGENO) &
  #                           RANGEDIR == section$Range %>% str_extract(".$"))) + mapview(section, col.regions = "red")
  
}



translatePoint <- function (pod, nsMove, nsDirection, ewMove, ewDirection) {
  
  # Move the coordinates in 'pod' by a certain distance north/south and another distance east/west
  # 'nsMove' and 'ewMove' have units of feet (assumed to be US Survey Feet)
  # Whether these values are north/south or east/west is given by 'nsDirection' and 'ewDirection'
  
  
  
  # First, check the movement magnitude and direction variables to ensure that they are not NA
  stopifnot(!anyNA(c(nsMove, nsDirection, ewMove, ewDirection)))
  
  
  
  # If 'nsMove' and 'ewMove' are both 0, return 'pod' without any changes
  if (nsMove == 0 && ewMove == 0) {
    return(pod)
  }
  
  
  
  # Otherwise, define movement values based on the input variables
  # The vertical movement magnitude is based on 'nsMove'
  # Horizontal movement is specified by 'ewMove'
  # North and East are treated as positive values
  # South and West are treated as negative values
  if (nsDirection == "North") {
    
    vertMove <- abs(nsMove)
    
  } else if (nsDirection == "South") {
    
    vertMove <- -abs(nsMove)
    
  } else {
    
    stop("Unknown input value for 'nsDirection'")
    
  }
  
  
  
  if (ewDirection == "East") {
    
    horiMove <- abs(ewMove)
    
  } else if (ewDirection == "West") {
    
    horiMove <- -abs(ewMove)
    
  } else {
    
    stop("Unknown input value for 'ewDirection'")
    
  }
  
  
  
  # Change the coordinate reference system of 'pod' to a projection that uses U.S. survey feet
  # epsg:2226 (NAD83 / California zone 2 (ftUS))
  newPOD <- st_transform(pod, "epsg:2226")
  
  
  
  # Apply 'vertMove' and 'horiMove' to the coordinates of 'newPOD'
  newPOD <- newPOD %>%
    st_coordinates() %>% as.data.frame() %>%
    mutate(X = X + horiMove, Y = Y + vertMove) %>%
    st_as_sf(coords = 1:2, crs = st_crs(newPOD))
  
  
  
  # Convert 'newPOD' back to the original CRS
  newPOD <- newPOD %>%
    st_transform(st_crs(pod))
  
  
  
  # Actual Translation Distance (m)
  # st_distance(newPOD, pod)
  
  # Expected Translation Distance (m)
  # sqrt(vertMove^2 + horiMove^2) * 1200/3937
  print(paste0("Difference between expected and actual translation distance (m): ",
               (sqrt(vertMove^2 + horiMove^2) * 1200/3937) - as.numeric(st_distance(newPOD, pod)), "\n",
               "Error is ", 100 * ((sqrt(vertMove^2 + horiMove^2) * 1200/3937) - as.numeric(st_distance(newPOD, pod))) / as.numeric(st_distance(newPOD, pod)), "%"))
  
  
  
  # Finally, return 'newPOD'
  return(newPOD)
  
}



#### Script Execution ####


print("Starting 'POD_StreamStats_Analysis.R'")
# mainProcedure(ws)
print("The script has finished running!")


# remove(mainProcedure, checkSectionMatches, colIndex, verifyWatershedOverlap,
#        requestFlowPath, checkForIntersection, calcMinDistance, sectionMovePOD,
#        chooseSection, section2point, extractCorner, findLot, getSubPLSS,
#        splitSection, translatePoint)