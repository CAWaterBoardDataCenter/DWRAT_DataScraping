


remove(list = ls())



require(tidyverse)
require(sf)
require(mapview)
require(readxl)
require(writexl)



options(viewer = NULL)


source("Scripts/Watershed_Selection.R")



# Based on the value of "NAME" in 'ws', read in a different boundary layer
# (The assigned variable name should always be 'wsBound')
wsBound <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                  GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") %>%
  st_transform("epsg:3488")



POD <- getXLSX(ws = ws, 
               SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
               FILEPATH = "POD_COORDINATES_SPREADSHEET_PATH",
               WORKSHEET_NAME = "POD_COORDINATES_WORKSHEET_NAME") %>%
  select(APPLICATION_NUMBER, POD_ID, LONGITUDE, LATITUDE) %>% unique() %>%
  mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
  st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = ws$POD_COORDINATES_REFERENCE_SYSTEM[1]) %>%
  st_transform(st_crs(wsBound))



subWS <- getGIS(ws = ws, 
                GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME") %>%
  st_transform(st_crs(wsBound))





connMat <- getXLSX(ws,
                   "IS_SHAREPOINT_PATH_CONNECTIVITY_MATRIX_SPREADSHEET",
                   "CONNECTIVITY_MATRIX_SPREADSHEET_PATH",
                   "CONNECTIVITY_MATRIX_WORKSHEET_NAME")




#wsLine <- st_cast(wsBound, "MULTILINESTRING")
fieldName <- ws$SUBBASIN_FIELD_ID_NAMES %>%
  str_split(";") %>% unlist() %>% trimws() %>%
  head(1)



#XXXX
# Get ~25 points around the watershed boundary line
# Get the catchments that intersect
#XXXX








# Sub-watersheds should have fewer than 5% of the total number of PODs

# Maximum sub-watershed size: The HUC-12

# In Paradigm's flow connectivity matrix, each catchment
# has at most one downstream catchment 
# (Verify this assumption before running)


# Consolidate two catchments if:
#   (1) They are empty
#   (2) Within the same HUC-12
#   (3) Adjacent and connected 
#   (4) In the same flow path



# If both catchments contain PODs, do NOT combine them
# Some are downstream of the other
# Water usage of the upstream catchment may inhibit the downstream users
# That nuance would be lost by combining the catchments



# If one catchment contains PODs, and the other is empty, consolidate them if:
#   (1) Within the same HUC-12
#   (2) The empty catchment is upstream of the POD catchment


# The areas around the PODs will not be consolidated much because of this algorithm
# Since the HUC-12 is integrated into this procedure, maybe that could be
# used for filtering in the dashboard?
# Trade-off between usability and model resolution



huc12 <- st_read("C:/Users/aprashar/Water Boards/Supply and Demand Assessment - Documents/Program Watersheds/1. Watershed Folders/Navarro River/Data/GIS Datasets/WBD_18_HU2_Shape/Shape/",
                 layer = "WBDHU12") %>%
  st_transform(st_crs(wsBound)) %>%
  select(huc12)


# Shrink 'wsBound' by 1 meter and find relevant boundaries from 'huc12'
huc12 <- huc12[lengths(st_intersects(huc12, st_buffer(wsBound, dist = -1))) > 0, ]



stopifnot(nrow(huc12) == length(unique(huc12$huc12)))



#stopifnot(as.numeric(abs(sum(st_area(huc12)) - st_area(wsBound))) < 10)



mapview(huc12, col.regions = "gray") + mapview(subWS)



names(connMat)[1] <- "BASIN"


subWS <- subWS %>%
  mutate(NUM_PODS = st_intersects(subWS, POD) %>% lengths())



subHUC12 <- huc12[["huc12"]][st_intersects(st_centroid(subWS), huc12) %>% unlist()]


stopifnot(length(subHUC12) == nrow(subWS))



subWS <- subWS %>%
  mutate(huc12 = subHUC12)



connMat <- connMat %>%
  mutate(ROWSUMS = rowSums(connMat[, -1])) %>%
  relocate(ROWSUMS, .after = BASIN)



#unreviewed <- connMat$BASIN
#reducedConnMat <- connMat
actionList <- data.frame()#vector(mode = "list", length = nrow(subWS))
#outletCatchmentEncountered <- FALSE



for (i in 1:nrow(subWS)) {
  
  
  #
  conRow <- connMat %>%
    filter(BASIN == subWS[[fieldName]][i])
  
  
  downstreamBasins <- names(conRow)[unlist(conRow) == 1 & 
                                      names(conRow) != subWS[[fieldName]][i] & 
                                      names(conRow) != "ROWSUMS" & 
                                      names(conRow) != "BASIN"]
  
  
  if(length(downstreamBasins) == 0) {
    
    next
    # if (outletCatchmentEncountered == FALSE) {
    #   outletCatchmentEncountered <- TRUE
    #   
    #   next
    #   
    # } else {
    #   
    #   stop("Unexpected Error")
    #   
    # }
    
    
  }
  
  
  relevantRows <- connMat %>%
    filter(BASIN %in% downstreamBasins)
  
  
  downstreamCounts <- colSums(relevantRows[, -c(1:2)])
  
  downstreamCounts <- downstreamCounts[downstreamCounts > 0]
  
  stopifnot(length(downstreamCounts) == length(downstreamBasins))
  
  
  nearestDownBasin <- names(downstreamCounts)[downstreamCounts == min(downstreamCounts)]
  
  
  stopifnot(length(nearestDownBasin) == 1)
  
  
  
  # Check the POD counts of both this catchment and its downstream catchment
  # If they are both 0, and they are in the same HUC-12 sub-basin, combine the catchments
  if (subWS$NUM_PODS[i] == 0 && subWS$NUM_PODS[subWS[[fieldName]] == nearestDownBasin] == 0 &&
      subWS$huc12[i] == subWS$huc12[subWS[[fieldName]] == nearestDownBasin]) {
    
    actionList <- rbind(actionList,
                        data.frame(ID1 = subWS[[fieldName]][i],
                                   ID2 = nearestDownBasin))
    #actionList[[i]] <- c(subWS$FEATUREID[i], nearestDownBasin)
    
    
  # Alternatively, combine the catchments if the upstream catchment is empty
  # And both are in the same HUC-12 sub-basin
  } else if (subWS$NUM_PODS[i] == 0 &&
             subWS$huc12[i] == subWS$huc12[subWS[[fieldName]] == nearestDownBasin]) {
    
    actionList <- rbind(actionList,
                        data.frame(ID1 = subWS[[fieldName]][i],
                                   ID2 = nearestDownBasin))
    #actionList[[i]] <- c(subWS$FEATUREID[i], nearestDownBasin)
    
  }
  
  
  # In all other cases, leave the catchments separate
  
}



newSubWS <- subWS %>%
  filter(!(!!sym(fieldName) %in% actionList$ID1) &
           !(!!sym(fieldName) %in% actionList$ID2)) %>%
  mutate(!!sym(fieldName) := as.character(!!sym(fieldName)))



newConnMat <- connMat



while (!is.null(nrow(actionList)) && nrow(actionList) > 0) {
  
  
  # Get all catchment IDs that will be combined with 
  # the IDs that appear in row 1 of 'actionList'
  idVec <- c(actionList$ID1[1],
             actionList$ID2[1])
  
  
  
  # However, IDs added to 'idVec' may in-turn appear in
  # additional rows of 'actionList'
  # A loop will be needed to obtain the full list
  
  
  # The previous iteration's version of 'idVec' will be used to 
  # track progress in the loop
  prevVec <- c()
  
  
  
  # While more IDs are still being added to 'idVec', keep running this loop
  while (length(prevVec) != length(idVec)) {
    
    prevVec <- idVec
    
    
    # Add IDs from 'actionList' to 'idVec' if either of its two IDs appear in 'idVec'
    # (Those IDs are added to 'idVec', and they are included in the next iteration's check)
    idVec <- c(idVec,
               actionList$ID1[actionList$ID1 %in% idVec | actionList$ID2 %in% idVec],
               actionList$ID2[actionList$ID1 %in% idVec | actionList$ID2 %in% idVec]) %>%
      unique() %>% 
      sort()
    
  }
  
  
  
  # At least two unique IDs should be present
  stopifnot(length(idVec) > 1)
  
  
  
  # Create a combined polygon
  joinedPoly <- NULL
  
  
  
  for (i in 2:length(idVec)) {
    
    if (i == 2) {
      
      joinedPoly <- st_snap(subWS %>% filter(!!sym(fieldName) == idVec[1]),
                            subWS %>% filter(!!sym(fieldName) == idVec[2]), 
                            tolerance = 0.00001) %>%
        st_union(subWS %>% filter(!!sym(fieldName) == idVec[2]),
                 by_feature = FALSE, is_coverage = TRUE) %>%
        mutate(!!sym(fieldName) := paste0(!!sym(fieldName), ";", !!sym(paste0(fieldName, ".1")))) %>%
        select(!!sym(fieldName))
      
    } else {
      
      joinedPoly <- st_union(joinedPoly,
                             st_snap(subWS %>% filter(!!sym(fieldName) == idVec[i]),
                                     joinedPoly,
                                     tolerance = 0.00001),
                             by_feature = FALSE, is_coverage = TRUE) %>%
        mutate(!!sym(fieldName) := paste0(!!sym(fieldName), ";", !!sym(paste0(fieldName, ".1")))) %>%
        select(!!sym(fieldName))
      
    }
    
  }
  
  
  
  # Update 'newConnMat' next
  # Add a new row for the combined sub-basin
  
  # Its value for each column should be 1 wherever 
  # any of its component sub-basins lists 1
  # (This means that the combined sub-basin drains into whichever 
  #  sub-basins its component sub-basins drain into)
  newConnMat <- rbind(newConnMat,
                      newConnMat %>%
                        filter(BASIN %in% idVec) %>%
                        map_dfr(max) %>%    # map_dfr will sequentially go through each column and apply max()
                        mutate(BASIN = joinedPoly[[fieldName]][1]))
  
  
  
  # The columns for each of the sub-basin components will also be replaced
  # For each sub-basin in the matrix, if it has a 1 for at least one of 
  # the component sub-basins, it should have a 1 for the combined sub-basin
  # (This means that a sub-basin that drains into a component sub-basin also
  #  drains into a combined sub-basin)
  newAddition <- newConnMat %>%
    select(BASIN, all_of(idVec)) %>%
    rowwise() %>%
    mutate(!! joinedPoly[[fieldName]][1] := max(!!!syms(idVec))) %>%
    select(BASIN, joinedPoly[[fieldName]][1])
  
  
  
  # Append the newly calculated column to 'newConnMat'
  # (And remove the columns and rows of the component sub-basins)
  newConnMat <- newConnMat %>%
    left_join(newAddition, by = "BASIN") %>%
    select(-idVec) %>%
    filter(!(BASIN %in% idVec))
  
  
  
  # Also recalculate "ROWSUMS"
  newConnMat <- newConnMat %>%
    mutate(ROWSUMS = rowSums(newConnMat[, -c(1:2)]))
  
  
  
  # Error check
  stopifnot(!anyNA(newConnMat))
  
  
  
  # Add 'joinedPoly' to 'newSubWS'
  newSubWS <- bind_rows(newSubWS, 
                        joinedPoly %>% st_cast("MULTIPOLYGON"))
  
  
  
  # Remove these indices from 'actionList'
  actionList <- actionList[-which(actionList$ID1 %in% idVec |
                                    actionList$ID2 %in% idVec), ]
  
}




#print(mapview(subWS) + mapview(newSubWS, col.regions = "red"))




print(mapview(huc12 %>% mutate(ID = row_number()), 
              col.regions = "gray", layer.name = "HUC-12") + 
  mapview(subWS, col.regions = "purple") + 
  mapview(newSubWS, col.regions = "red") + 
  mapview(POD))





subHUC12 <- huc12[["huc12"]][st_intersects(st_centroid(newSubWS), huc12) %>% unlist()]


stopifnot(length(subHUC12) == nrow(newSubWS))



newSubWS <- newSubWS %>%
  mutate(huc12 = subHUC12)




newSubWS %>%
  select(all_of(fieldName), huc12) %>%
  st_make_valid() %>%
  st_buffer(dist = 0) %>%
  st_write(paste0("OutputData/", 
                  ws$ID, "_DWRAT_Subwatersheds_", Sys.Date(), ".geojson"),
           delete_dsn = TRUE)



newConnMat %>%
  select(-ROWSUMS) %>%
  write_xlsx(paste0("OutputData/", ws$ID, 
                    "_Connectivity_Matrix_DWRAT_Subwatersheds_",
                    Sys.Date(),
                    ".csv"))