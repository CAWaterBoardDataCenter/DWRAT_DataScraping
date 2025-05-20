# Functions relevant to GIS analysis



requestFlowPath <- function (pod) {
  
  # Send a POST request to USGS StreamStats using coordinates (given in 'pod')
  # The response should contain line strings that can build a flow path from 'pod'
  
  
  # Return a collection of points
  # (That can also be converted into linestrings using a "LINEID" column)
  
  
  
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
                    #   AWSALB=47A+MRlQ4OVQMuc5ytXvkQekgQsquFNd1ZLy8T2C4vXMJXMgmX5
                    #   KzilKA8imFfX7emnbioHjsY5QMua5CQAs65u9UtfLZiyuiarVOFgBDH8Sg
                    #   PmpiQtX6vhkpyzP; AWSALBCORS=47A+MRlQ4OVQMuc5ytXvkQekgQsquF
                    #   Nd1ZLy8T2C4vXMJXMgmX5KzilKA8imFfX7emnbioHjsY5QMua5CQAs65u9
                    #   UtfLZiyuiarVOFgBDH8SgPmpiQtX6vhkpyzP
                    "Dnt" = 1,
                    #Origin:
                    #  https://streamstats.usgs.gov
                    "Referer" = "https://streamstats.usgs.gov/ss/", 
                    #"Sec-Ch-Ua" = '"Not_A Brand";v="8", "Chromium";v="120", "Microsoft Edge";v="120"', 
                    #"Sec-Ch-Ua-Mobile" = "?0",
                    "Sec-Ch-Ua-Platform" = "Windows",
                    #Sec-Fetch-Dest:
                    #  empty
                    #Sec-Fetch-Mode:
                    #  cors
                    #Sec-Fetch-Site:
                    #  same-origin
                    #"User-Agent" = "R version 4.2.3",
                    "User-Contact" = "DWR-SDA@waterboards.ca.gov")),
                  body = paste0('[{"id":1,"name":"Start point location","required":true,',
                                '"description":"Specified lat/long/crs  navigation start location",', 
                                '"valueType":"geojson point geometry",', 
                                '"value":{"type":"Point","coordinates":[', 
                                st_coordinates(pod) %>% paste0(collapse = ","), 
                                '],"crs":{"properties":{"name":"EPSG:4326"},"type":"name"}}}]'))
  
  
  
  # Wait a bit after sending the request
  Sys.sleep(runif(1, min = 1.2, max = 2.0))
  
  
  
  # Verify that the request was successful
  # If the request failed, check if the POD is located in the ocean
  if (flowReq$status_code != 200) {
    
    
    # The search can fail if the POD is located in the Pacific Ocean
    # Check for overlap with a layer containing a polygon of the ocean
    if (oceanOverlapCheck(pod)) {
      
      # If there is overlap, then this issue was the source of the StreamStats request failure
      # In that case, simply a data frame that expresses this issue
      return(data.frame(LINEID = NA, ERROR = "Coordinates in Pacific Ocean!"))
      
    } else {
      
      # If the POD does not overlap with the ocean, then a different issue caused the request failure
      # Just return 'pod' in that case
      return(pod)
      
    }
    
  }
  
  
  
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
    extractedRes <- flowRes$features[[i]]$geometry$coordinates %>%
      unlist() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      data.frame() %>%
      set_names(c("X", "Y"))
    
    
    
    # Include a 'LINEID' column in the output
    # This can be used to cast the points back into linestrings
    extractedRes <- extractedRes %>%
      mutate(LINEID = i)
    
    
    
    # Append 'extractedRes' to 'pointDF'
    pointDF <- bind_rows(pointDF,
                         extractedRes)
    
  }
  
  
  
  # Convert 'pointDF' into a spatial features dataset and return it
  return(pointDF %>%
           st_as_sf(coords = 1:2, crs = "WGS84"))
  
}



oceanOverlapCheck <- function (pod) {
  
  # Check whether the POD is located in the ocean
  # Return "TRUE" or "FALSE" based on the presence of overlap
  
  
  
  # Read in a polygon containing the Pacific Ocean (the ocean closest to California)
  pacific <- "Program Watersheds/1. Watershed Folders/Navarro River/Data/GIS Datasets/pacific_ocean/3853-s3_2002_s3_reg_pacific_ocean-geojson.json" %>%
    makeSharePointPath() %>%
    st_read() %>%
    st_transform("epsg:3488")
  
  
  
  # Make sure 'pacific' and 'pod' have the same coordinate reference system
  pod <- st_transform(pod, st_crs(pacific))
  
  
  
  # Return "TRUE" or "FALSE" depending on whether st_intersects() returns a non-empty value
  # (A non-empty value means that there is intersection between the layers)
  return(lengths(st_intersects(pod, pacific))[1] > 0)
  
}



exitPointCheck <- function (flowPath, exitPoint) {
  
  # Check if any part of a flow path is within 100 meters of an exit point
  
  
  
  # Calculate the distance between each of the flowlines in 'flowPath'
  # and 'exitPoint' (the distances will have units of meters)
  vertexDist <- st_distance(flowPath, exitPoint) %>%
    as.vector()
  
  
  
  # Verify that the distance calculation occurred smoothly
  if (length(vertexDist) == 1 && is.na(vertexDist[1])) {
    
    stop(paste0("There is a problem with the flowpath or exit point. st_distance() ",
                "returned only one value and it was 'NA'.") %>%
           wrapStr())
    
  }
  
  
  
  # Return a boolean for whether a point in the flow path is within 100 meters
  # of the specified exit point
  return(min(vertexDist, na.rm = TRUE) < 100) 
  
}


