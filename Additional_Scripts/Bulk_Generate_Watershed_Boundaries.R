# Regenerate boundaries for every program watershed
# Create a consistent set of watershed boundaries based on their model sub-basin layers

# Use "The National Map" as a source of boundaries and construct the layers

# Clip the resultant layer if it extends past the coastline 
# (Use California State boundaries and a layer of the Pacific Ocean from 
#  Santa Barbara County to accomplish this)


# Each watershed's layer will be exported as a geoJSON file


#### Setup ####


# Clear the environment
base::remove(list = ls())


# Shared functions and required packages
# (Some packages are loaded by "!Shared_Functions_Importer.R")
source("W1_Watershed_Demand/Scripts/Shared_Functions_Demand.R")

source("Shared_Scripts/!Shared_Functions_Importer.R")

require(sf)
require(units)
require(mapview)
require(webshot)


#### Functions ####

mainProcedure <- function () {
  
  # Produce watershed boundaries based on the sub-basin layers in use
  
  # Start by reading in the demand workflow's control file
  ctrlDF <- getDemandControlFile()
  
  
  # Keep only rows with sub-basin layers specified
  ctrlDF <- ctrlDF |>
    filter(!is.na(SUBBASIN_POLYGONS_DATABASE_PATH)) |>
    filter(!is.na(IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS))
  
  
  stopifnot(nrow(ctrlDF) > 0)
  stopifnot(nrow(ctrlDF) == length(unique(ctrlDF$ID)))
  
  
  # Load in the required supporting layers
  
  # Note: "!SOURCE_INFO.csv" in the "GIS_General" folder has source information
  #       for these layers
  #       (It will be used by this script too)
  
  
  # First specify the files' paths explicitly here
  # (These strings will be useful in a later step too)
  hucPath <- "W1_Watershed_Demand/Input/GIS_General/NHD_H_California_State_WBDHU12.RData"
  
  statePath <- "W1_Watershed_Demand/Input/GIS_General/State_of_California_Boundary_with_Bay_Cuts.geojson"
  
  pacificPath <- "W1_Watershed_Demand/Input/GIS_General/3853-s3_2002_s3_reg_pacific_ocean-geojson.json"
  
  metaPath <- "W1_Watershed_Demand/Input/GIS_General/!SOURCE_INFO.csv"
  
  
  # Read in the HUC-12 layer from the USGS National Map project for California
  # It will appear in the environment as the variable 'huc12'
  load(hucPath)
  
  
  # Transform the coordinate system of 'huc12' into a projected system
  huc12 <- huc12 |>
    st_transform("epsg:3488")
  
  
  # Read in state boundary layers for California
  caState <- statePath |>
    st_read() |>
    st_transform(st_crs(huc12)) |>
    select()
  
  
  # Read in a layer for the Pacific Ocean too
  pacific <- pacificPath |> 
    st_read() |>
    st_transform(st_crs(huc12)) |>
    select()
  
  # For both of these layers, their coordinate systems have been updated
  # to match 'huc12'
  
  # In addition, any non-geometry columns in their layers have been removed
  # by the empty `select` call
  
  
  # The final required input is the source metadata for these layers
  metaDF <- metaPath |>
    getFile()
  
  
  # In another function, generate watershed boundaries for each watershed
  # contained within 'ctrlDF'
  generateBoundaries(ctrlDF, huc12, caState, pacific, metaDF,
                     hucPath, statePath, pacificPath)
  
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getDemandControlFile <- function () {
  
  # Load in the main control file for the demand workflow
  # It can either be a SharePoint version or a local copy
  
  # For SharePoint paths to be usable, both "INITIAL_SHAREPOINT_FILE_PORTION"
  # and "SHAREPOINT_DEMAND_CONTROL_FILE" must be specified in 
  # "Master_Control_File.xlsx"
  if (!is.na(getFromMasterControl("INITIAL_SHAREPOINT_FILE_PORTION"))) {
    
    # Try and read the SharePoint fragment for the Demand control file
    controlPath <- getFromMasterControl("SHAREPOINT_DEMAND_CONTROL_FILE")
    
    
    # If that value is indeed specified, read it in as 'ctrlDF'
    if (!is.na(controlPath)) {
      
      ctrlDF <- controlPath |>
        makeSharePointPath() |>
        getXLSX(worksheet = "Main_Sheet", skip = 1)
      
    }
    
  }
  
  
  # In all other cases, use the local version of the control file
  if (!exists("ctrlDF")) {
    
    controlPath <- "W1_Watershed_Demand/Input/Watershed_Demand_Dataset_Paths.xlsx"
    
    ctrlDF <- getXLSX(controlPath, worksheet = "Main_Sheet", skip = 1)
    
  }
  
  
  # Either way, return 'ctrlDF'
  return(ctrlDF)
  
}



generateBoundaries <- function (ctrlDF, huc12, caState, pacific, metaDF,
                                hucPath, statePath, pacificPath) {
  
  # Develop watershed boundaries for each entry in 'ctrlDF'
  # Use their sub-basin layers to extract relevant HUC-12 sub-basins
  
  # Additional adjustments may be needed using the state boundaries
  # and Pacific Ocean layer
  
  # The resultant layer will receive attribute fields from 'ctrlDF' and 'metaDF'
  
  
  # Iterate through each watershed
  for (i in 1:nrow(ctrlDF)) {
    
    # Read in the watershed's sub-basin layer as a single MULTIPOLYGON entity
    combinedSub <- ctrlDF[i, ] |>
      getConsolidatedSubbasinLayer(st_crs(huc12))
    
    
    # Next, find all HUC-12 sub-basins that overlap with 'combinedSub'
    
    # First, get the row index values for every HUC-12 sub-basin in 'huc12' that 
    # intersects with 'combinedSub'
    hucIndices <- which(st_intersects(huc12, combinedSub) |> lengths() > 0)
    
    
    # Get a subset of 'huc12' with only intersecting sub-basins
    hucSubset <- huc12[hucIndices, ]
    
    
    # The filtering and adjustments to 'hucSubset' will be done in several steps:
    #
    #   (1) Preliminary overlapping area-based filtering
    #
    #   (2) Boundary adjustment (needed for coastal sub-basins only)
    #
    #   (3) Final overlapping area-based filtering
    #
    #   (4) Sub-basin clipping (for partially intersecting sub-basins only)
    #
    #   (5) Sub-basin merge
    #
    #   (6) Removal of stray slivers and extraneous polygons
    
    
    # Start by filtering down 'hucSubset' based on 
    # the amount of overlap with 'combinedSub'
    
    # Remove sub-basins that barely overlap with 'combinedSub' (< 2%)
    
    # Calculate the current overlap percentage in 'hucSubset'
    hucSubset <- hucSubset |>
      calcPercentOverlap(combinedSub)
    
    
    # Filter out any HUC-12 sub-basins with less than 2% of overlap
    hucSubset <- hucSubset |>
      filter_out(PERCENT_OVERLAP < 2)
    
    
    # The next step is to adjust any coastal HUC-12 sub-basins (if present)
    # In the National Map boundaries, these sub-basins have portions that 
    # extend out into the Pacific Ocean
    
    # There are some small islands off the coast of California
    # There are no water rights that, but just in case, to future-proof this
    # procedure and avoid any potential flagging of water rights in those isolated areas,
    # we will remove the portion of the boundaries that extend into the ocean
    
    # There is no huge risk of this happening (and it's not a big deal if it does), 
    # but perhaps having cleaner boundaries will be useful in some other applications)
    hucSubset <- hucSubset |>
      removeOceanOverlap(caState, pacific, ctrlDF$NAME[i])
    
    
    # Check if any sub-basin layers do not completely overlap with 'combinedSub'
    # The sub-basin may need to be trimmed in this case
    hucSubset <- hucSubset |>
      trimSubbasins(combinedSub)
    
    
    # The next step is to merge 'hucSubset' together into a single layer
    newBound <- hucSubset |>
      mergeSubbasins()
    
    
    # Before writing 'newBound' to a file, add some fields to it
    newBound <- newBound |>
      addFields(ctrlDF[i, ], metaDF, hucPath, statePath, pacificPath)
    
    
    # Write 'newBound' to a geoJSON file
    outPath <- paste0("W1_Watershed_Demand/Output/",
                      ctrlDF$ID[i], "_SDA_Boundary.geojson")
    
    
    # Delete the file if it exists already
    if (file.exists(outPath)) {
      unlink(outPath)
    }
    
    newBound |>
      st_write(outPath,
               append = FALSE)
      
    
    # Save a map containing 'newBound' and 'combinedSub' too
    
    
    # print(mapview(newBound) + mapview(combinedSub, col.regions = "gray"))
    # 
    # readline("Check boundary")
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getConsolidatedSubbasinLayer <- function (wsSelect, targetCRS) {
  
  subWS <- getGIS(ws = wsSelect, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                  GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME") |>
    st_transform(targetCRS)
  
  
  # Make sure all catchment polygons are valid
  if (!all(st_is_valid(subWS))) {
    
    subWS[!st_is_valid(subWS), ] <- subWS[!st_is_valid(subWS), ] |>
      st_make_valid() |>
      st_buffer(0)
    
  }
  
  
  # Combine all sub-basin polygons into a single layer
  combinedSub <- subWS |>
    summarize()
  
  
  # Return 'combinedSub'
  return(combinedSub)
  
}



calcPercentOverlap <- function (basins, boundary) {
  
  # Get the percent overlap between a layer of sub-basins and
  # a boundary layer
  
  # Find the amount of intersection with 'boundary' for each sub-basin
  # Divide that by the total area of each sub-basin to get the overlap ratio
  # (And multiply by 100 to make it a percent)
  
  
  # First, calculate the total area of each sub-basin
  # (and include a placeholder column for the overlapping area)
  basins <- basins |>
    mutate(AREA = st_area(basins),
           OVERLAP_AREA = NA_real_)
  
  
  # Next, iterate through each sub-basin
  for (j in 1:nrow(basins)) {
    
    # Get a layer containing only the intersection between the sub-basin and 'boundary'
    tempIntersect <- basins[j, ] |>
      select() |>
      st_intersection(boundary)
    
    
    # If 'tempIntersect' is empty, the overlapping area is zero
    if (nrow(tempIntersect) == 0) {
      
      basins$OVERLAP_AREA[j] <- 0
      
    # Otherwise, use `st_area` to get the total area of 'tempIntersect'
    } else {
      
      basins$OVERLAP_AREA[j] <- st_area(tempIntersect)
      
    }
    
  }
  
  
  stopifnot(!anyNA(basins$OVERLAP_AREA))
  
  
  # Once all sub-basins have received a value for "OVERLAP_AREA", 
  # calculate the percent overlap
  basins <- basins |>
    mutate(PERCENT_OVERLAP = 100 * OVERLAP_AREA / AREA) |>
    mutate(PERCENT_OVERLAP = drop_units(PERCENT_OVERLAP))
  
  # (Also, drop any units attached to "PERCENT_OVERLAP")
  
  
  # Finally, return 'basins'
  return(basins)
  
}



removeOceanOverlap <- function (hucSubset, caState, pacific, wsName) {
  
  # If any HUC-12 sub-basins extend past the coastline into the Pacific Ocean,
  # remove the sections that intersect with the ocean
  
  # To do this, restrict the sub-basin polygons to the extent of 'caState'
  # (a layer containing a boundary for California)
  
  # However, some portions of the Pacific Ocean still appear within 'caState'
  
  # As a result, some watersheds may required additional trimming
  
  # The 'pacific' layer, which contains a polygonal representation of the 
  # Pacific Ocean, can help with this
  
  
  # Start by checking if any boundaries extend past the coastline
  if (st_difference(hucSubset |> select(), caState) |> nrow() > 0) {
    
    # `st_difference` will remove all portions of every sub-basin that overlap
    # with 'caState'
    
    # If there's still any polygons left after that, it's a sign
    # that at least one coastal HUC-12 sub-basin is present
    
    
    # Notify the user
    print(paste0("Preliminary boundaries for ", wsName, 
                 " extend into the Pacific Ocean!"))
    
    
    # Clip all HUC-12 sub-basins to the extent of the state boundaries
    hucSubset <- hucSubset |> select() |>
      st_intersection(caState)
    
    
    # Check too if the Pacific Ocean layer overlaps with 'hucSubset'
    if (any(st_intersects(hucSubset, pacific) |> lengths() > 0)) {
      
      # However, even if the Pacific Ocean layer does overlap with 
      # one or more HUC-12 sub-basins, only make adjustments if the overlap
      # is significant enough
      
      # Arbitrarily, the percent overlap between a sub-basin and the ocean layer
      # should be at least 10% (in terms of area)
      hucSubset <- hucSubset |>
        calcPercentOverlap(pacific)
      
      
      if (any(hucSubset$PERCENT_OVERLAP > 10)) {
        
        # Output a message about the operation to the user
        print("Clipping based on Pacific Ocean overlap too!")
        
        
        # Remove overlap with 'pacific' from 'hucSubset'
        hucSubset <- hucSubset |>
          select() |>
          st_difference(pacific)
        
      }
      
    }
    
  }  
  
  
  # Return 'hucSubset' afterwards
  return(hucSubset)
  
}



trimSubbasins <- function (hucSubset, combinedSub, clipThreshold = 95) {
  
  # Some model layers may only model a portion of a HUC-12 sub-basin
  
  # As a result, not every sub-basin in 'hucSubset' may be completely required
  # in the final watershed boundaries
  
  
  # Calculate the percent overlap between each sub-basin of 'hucSubset' and 'combinedSub'
  hucSubset <- hucSubset |>
    calcPercentOverlap(combinedSub)
  
  
  # If any sub-basins fail to meet 'clipThreshold' in their overlap with 'combinedSub', 
  # clip them to match 'combinedSub' better
  while (any(hucSubset$PERCENT_OVERLAP < clipThreshold)) {
    
    print(paste0("One or more HUC-12 sub-basin(s) only partially overlap ",
                 "with the sub-basin layer!"))
    
    
    # Iterate through every sub-basin
    for (j in 1:nrow(hucSubset)) {
      
      # Skip sub-basins that have sufficient overlap with 'combinedSub'
      if (hucSubset$PERCENT_OVERLAP[j] >= clipThreshold) {
        next
      }
      
      
      # Extract the differences between this sub-basin and 'combinedSub'
      # Convert this result into distinct polygons and extract the largest one
      nonOverlap <- hucSubset[j, ] |>
        select() |>
        st_difference(combinedSub) |>
        extractLargestPolygon()
      
      
      # Remove 'nonOverlap' (the largest non-overlapping polygon) 
      # from the HUC-12 sub-basin
      tempBasin <- hucSubset[j, ] |>
        select() |>
        st_difference(nonOverlap |> select())
      
      
      # Get the largest leftover polygon from 'tempBasin' too
      tempBasin <- tempBasin |>
        extractLargestPolygon()
      
      
      # Replace the sub-basin in 'hucSubset' with the layer in 'tempBasin'
      # (For now, just add it to 'hucSubset')
      # (After the loop, the current iteration's sub-basin will be removed)
      hucSubset <- rbind(hucSubset, 
                         tempBasin |>
                           mutate(OVERLAP_AREA = 0,
                                  PERCENT_OVERLAP = clipThreshold))
      
      # To join 'tempBasin' properly to 'hucSubset', it needs the same fields, so
      # "OVERLAP_AREA" and "PERCENT_OVERLAP" must be added
      # For now, these are just semi-random placeholder values
      # Only the "PERCENT_OVERLAP" value is important
      
    }
    
    
    # After the loop, remove any sub-basins with a "PERCENT_OVERLAP" 
    # that is less than the clip threshold
    hucSubset <- hucSubset |>
      filter_out(PERCENT_OVERLAP < clipThreshold)
    
    
    # Finally, recalculate the overlap areas
    hucSubset <- hucSubset |>
      calcPercentOverlap(combinedSub)
    
  } # This loop will continue as long as sub-basins must be trimmed
  
  
  # Return 'hucSubset'
  return(hucSubset)
  
}



extractLargestPolygon <- function (basin) {
  
  # Given a layer containing a sub-basin, convert it into distinct polygons
  # Each polygon will have its own row
  # Calculate the area of each polygon
  # Then, return the polygon with the largest area
  
  
  # Start by casting 'basin' into a polygon-only layer
  basin <- basin |>
    select() |>
    st_cast("POLYGON") 
  
  
  # Calculate the area of each polygon
  basin <- basin |>
    mutate(AREA = st_area(basin))
  
  
  # Identify the row corresponding to the polygon with the greatest area
  maxIndex <- which.max(basin$AREA)
  
  
  # Return only that polygon
  return(basin[maxIndex, ])
  
}



mergeSubbasins <- function (hucSubset) {
  
  # Merge together the separate sub-basin polygons in 'hucSubset'
  # Then check the result for errors
  
  
  # Combine all sub-basins together
  newBound <- hucSubset |>
    summarize()
  
  
  # Check to confirm that there are no disconnected polygons in 'newBound'
  # It should be one continuous polygon
  newBound <- newBound |>
    st_cast("POLYGON") |>
    extractLargestPolygon() |>
    select()
  
  # If there are multiple polygons, the largest one will be kept only
  
  
  # Finally, confirm that no gaps are present within the layer
  # Use `st_coordinates` to obtain this information
  coordInfo <- st_coordinates(newBound)
  
  
  # For a "POLYGON" object, `st_coordinates` gives the X and Y coordinates, 
  # as well as two columns labeled "L1" and "L2"
  stopifnot(ncol(coordInfo) == 4)
  stopifnot(all(c("X", "Y", "L1", "L2") %in% colnames(coordInfo)))
  
  
  # As stated in the documentation of `st_coordinates`, 
  # "L1" contains integer values that correspond to the main polygon ring and
  # any holes present inside the polygon
  # "L2" corresponds to different simple feature polygons within the layer
  
  # In both cases, "L1" and "L2" should only contain "1"
    
  # "L2" should never contain anything other than 1 since the sub-basins 
  # were combined into a single polygon
  stopifnot(all(coordInfo[, colnames(coordInfo) == "L2"] == 1))
  
  
  # However, in the case of "L1", if there are numbers other than 1,
  # that's a sign that holes and gaps are present within the boundary layer
  if (!all(coordInfo[, colnames(coordInfo) == "L1"] == 1)) {
    
    print("Gaps were detected within the merged boundary layer!")
    print("Attempting to resolve now!")
    
    
    # Extract only the first set of points within "L1"
    # These correspond to the outer boundary of the polygon
    # (without the holes)
    
    
    # Find the rows of 'coordInfo' where "L1" is equal to 1
    polyRows <- which(coordInfo[, colnames(coordInfo) == "L1"] == 1)
    
    
    # Get a subset of 'coordInfo' that only contains the outer polygon
    # boundary points
    coordSubset <- coordInfo[polyRows, ]
    
    
    # Convert 'coordSubset' into a polygon and then a spatial feature collection
    # Update 'newBound' with this new polygon
    newBound <- list(coordSubset) |>
      st_polygon() |>
      st_sfc(crs = st_crs(newBound)) |>
      st_sf()
    
    
    # As a final check, update 'coordInfo' 
    # and confirm that "L1" only contains 1 now
    coordInfo <- st_coordinates(newBound)
    
    
    # Just stop the script if this procedure failed
    stopifnot(all(coordInfo[, colnames(coordInfo) == "L1"] == 1))
    
  }
  
  
  # If there are no issues, return 'newBound'
  # (while making sure that it contains no additional dimensions)
  return(newBound |>
           st_zm())
  
}



addFields <- function (newBound, ws, metaDF, hucPath, statePath, pacificPath) {
  
  # Add data to 'newBound'
  
  # Include the following:
  #   (*) Watershed name
  #   (*) Watershed ID
  #   (*) Today's date
  #   (*) HUC-12 layer obtain date
  #   (*) State boundary layer obtain date 
  #   (*) Pacific Ocean layer obtain date
  #   (*) Latest git commit for the repository
  
  
  # Add the watershed name and ID from 'ws'
  newBound <- newBound |>
    mutate(SDA_ID = ws$ID,
           WATERSHED = ws$NAME)
  
  
  # Include today's date
  newBound <- newBound |>
    mutate(GENERATED_ON = Sys.Date())
  
  
  # Extract from 'metaDF' the download date for each of the supporting layers
  newBound <- newBound |>
    mutate(DATE_OBTAINED_HUC12 = extractDate(metaDF, hucPath),
           DATE_OBTAINED_STATE_BOUNDARY = extractDate(metaDF, statePath),
           DATE_OBTAINED_PACIFIC = extractDate(metaDF, pacificPath))
  
  
  # Finally, add the current git commit hash of the repository
  newBound <- newBound |>
    mutate(LATEST_GIT_HASH = getGitHash())
  
  
  # Return 'newBound' afterwards
  return(newBound)
  
}



extractDate <- function (metaDF, filePath) {
  
  # Given a filepath (whose base filename should appear 
  # in the "FILENAME" column of 'metaDF'),
  # extract the corresponding "DATE_OBTAINED" value
  
  
  # Extract the file name from 'filePath'
  fileName <- filePath |>
    str_remove("^.+[/\\\\]")
  
  
  # Find the row where 'fileName' appears in the "FILENAME" column of 'metaDF'
  matchIndex <- which(tolower(metaDF$FILENAME) == tolower(fileName))
  
  
  if (length(matchIndex) != 1) {
    stop(paste0("Couldn't find one exact match for \"", fileName, "\" in the ",
                "source information file"))
  }
  
  
  # Return the "DATE_OBTAINED" value that corresponds to 'matchIndex'
  return(metaDF$DATE_OBTAINED[matchIndex])
  
}



#### Script Execution ####

mainProcedure()


# Clear the environment
base::remove(list = ls())
