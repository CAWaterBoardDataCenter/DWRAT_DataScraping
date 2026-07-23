# For a user-specified water right, create a mapview map of their flowpaths
# Use USGS StreamStats to construct the flow paths for the PODs


require(tidyverse)
require(sf)
require(mapview)
require(httr)


mainProcedure <- function (flowlines = NULL) {
  
  
  source("Scripts/Watershed_Selection.R")
  
  
  
  # Get the eWRIMS POD coordinates
  POD <- getXLSX(ws, "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
                 "POD_COORDINATES_SPREADSHEET_PATH",
                 "POD_COORDINATES_WORKSHEET_NAME") %>%
    select(APPLICATION_NUMBER, POD_ID, LONGITUDE, LATITUDE) %>% unique() %>%
    mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
    st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = ws$POD_COORDINATES_REFERENCE_SYSTEM[1])
  
  
  
  # Also import a layer with the watershed's subbasins
  # (There should be one polygon per subbasin)
  subWS <- getGIS(ws, "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                  "SUBBASIN_POLYGONS_DATABASE_PATH",
                  "SUBBASIN_POLYGONS_LAYER_NAME")
  
  
  
  # Define a vector of unique water rights in 'POD'
  wrVec <- POD$APPLICATION_NUMBER %>% unique() %>% sort()
  
  
  
  # Have the user choose a water right
  rightSelection <- menu(wrVec, graphics = TRUE, title = "Select a Water Right")
  
  
  
  # Create a filtered version of 'POD' that only has PODs for this right
  filteredPOD <- POD %>%
    filter(APPLICATION_NUMBER == wrVec[rightSelection])
  
  
  
  # Define a list variable to hold StreamStats flowpaths for every POD in 'filteredPOD'
  flowpaths <- vector(mode = "list", length = nrow(filteredPOD))
  
  
  
  # Iterate through 'filteredPOD'
  # Make queries to USGS StreamStats
  for (i in 1:nrow(filteredPOD)) {
    
    flowpaths[[i]] <- requestFlowPath(filteredPOD[i, ])
    
  }
  
  
  names(flowpaths) <- filteredPOD$POD_ID
  
  
  colorPalette <- c("#c44601", "#8babf1", "#5ba300", "#5928ed", "#e6308a", "#111111")
  
  
  
  # Print out the map
  # If 'flowlinesPath' is not NULL, it will be included too
  if (!is.null(flowlines)) {
    
    print(mapview(flowlines) +
            mapview(POD, col.regions = "white") +
            mapview(flowpaths, col.regions = colorPalette[1:length(flowpaths)]) + 
            mapview(subWS, col.regions = "lightgray"))
    
  } else {
    
    print(mapview(POD, col.regions = "white") +
            mapview(flowpaths, col.regions = colorPalette[1:length(flowpaths)]) + 
            mapview(subWS, col.regions = "lightgray"))
    
  }
  
  
  
  # Return nothing
  return(invisible(NULL))
  
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



#### Script Execution ####

mainProcedure() 
              #flowlines = makeSharePointPath("Watershed Folders\\Navarro\\Data\\GIS Datasets\\NHDPlus_Delineations\\LSPC_Delineations") %>%
                #st_read(layer = "NHDFlowline_EditedforLSPC"))


#### Cleanup ####

remove(mainProcedure, requestFlowPath)

