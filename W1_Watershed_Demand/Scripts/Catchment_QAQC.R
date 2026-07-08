# QA/QC the watershed catchments layer

# This script will try to identify potential issues with the layer 
# and produce maps for the user to inspect


# This script checks for:
#  (*) Catchments with disconnected polygons
#  (*) Very small catchments



#### SETUP ####

# Clear the environment
base::remove(list = ls())


require(tidyverse)
require(sf)
require(units)
require(colorspace)
require(leaflet)
require(leafem)
require(mapview)
require(webshot)
require(polylabelr)


#### FUNCTIONS ####

mainProcedure <- function() {
  
  # Start with a message to the user
  cat("Starting 'Catchment_QAQC.R'...\n\n")
  
  
  # Get the selected watershed
  source("W1_Watershed_Demand/Scripts/Watershed_Selection.R")
  cat("\n")
  
  
  # Read in its catchment layer next
  catchDF <- getGIS(ws = ws, 
                    GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                    GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                    GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME")
  
  
  # Get the name of the column that contains the catchment IDs too
  if (is.na(ws$SUBBASIN_FIELD_ID_NAMES[1])) {
    
    print(catchDF |> head())
    
    paste0("Missing Value in Demand Paths Spreadsheet\n\n",
           "\"SUBBASIN_FIELD_ID_NAMES\" is blank. Please fill it in with ",
           "the column that contains the catchment IDs (e.g., \"COMID\").\n\n",
           "The attribute table has been partially printed out above to help ",
           "identify this column.") |>
      strwrap(width = 0.99 * getOption("width")) |>
      paste0(collapse = "\n") |>
      stop()
    
    
  } else {
    
    fieldName <- ws$SUBBASIN_FIELD_ID_NAMES[1] |> str_split(";") |>
      unlist() |> pluck(1) |> trimws()
    
  }
  
  
  cat("\n[1/2]\tChecking for issues...\n")
  
  
  # Check for catchments that contain disconnected polygons
  catchDF <- catchDF |>
    checkDisconnected()
  
  
  # The next test is for small catchments
  catchDF <- catchDF |>
    checkArea()
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/2]\tPreparing a map...\n")
  
  
  # Finally, produce maps for the user to view
  catchDF |>
    generateMap(fieldName, ws)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat("'Catchment_QAQC.R' is complete!\n\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkDisconnected <- function (catchDF) {
  
  # Look for catchments that have disconnected components
  
  # Some catchments may be comprised of several polygons that do not connect
  # to each other
  
  
  # Initiate variables related to this check
  catchDF <- catchDF |>
    mutate(NUM_POLYGONS = 0,
           DISCONNECTED_POLYGONS = FALSE)
  
  
  # Iterate through each individual catchment
  for (i in 1:nrow(catchDF)) {
    
    # Take that catchment and convert it into a polygon
    tempDF <- catchDF[i, ] |> select() |>
      st_cast("POLYGON")
    
    
    # Record the number of polygons to 'catchDF'
    catchDF$NUM_POLYGONS <- nrow(tempDF)
    
    
    # If there is only one polygon, there are no disconnected sections
    # Skip the rest of the iteration then 
    # (The default value for "DISCONNECTED_POLYGONS" is already "FALSE", 
    #  so no value assignments are needed)
    if (nrow(tempDF) == 1) {
      next
    }
    
    
    # If a catchment's iteration reaches this point, it ended up being split 
    # into more than one polygon
    # The next test then is whether these polygons are *disconnected*
    
    # Check the intersections of the polygons between themselves
    polyOverlaps <- st_intersects(tempDF) |> lengths()
    
    
    # Every polygon intersects with itself, but we want to ensure that every
    # polygon intersects with at least one other polygon
    # Therefore, every element in 'polyOverlaps' should have at least 2 values
    
    # If there are any polygons that have only one overlap (i.e., with itself),
    # it is disconnected from the other polygons in the catchment
    
    # That is a "disconnected polygon" issue 
    # and "DISCONNECTED_POLYGONS" should be set to TRUE
    catchDF$DISCONNECTED_POLYGONS[i] <- FALSE %in% (polyOverlaps > 1)
    
  }
  
  
  # Notify the user if disconnected polygons were found
  if (TRUE %in% catchDF$DISCONNECTED_POLYGONS) {
    
    paste0(sum(catchDF$DISCONNECTED_POLYGONS), " catchment",
           if_else(sum(catchDF$DISCONNECTED_POLYGONS) > 1,
                   "s have ", 
                   " has "),
           "disconnected polygons!") |>
      message()
    
  }
  
  
  # Return 'catchDF'
  return(catchDF)
  
}



checkArea <- function (catchDF) {
  
  # Some catchments may be excessively small in area
  
  # "Small" is a subjective term, and perhaps different watersheds will want
  # different thresholds
  
  # Arbitrarily, the default threshold will be 25,000 square meters
  
  # NOTE - Approximately speaking:
  #        25,000 m^2 = 6.18 acres = 269,098 ft^2 = 0.01 mi^2 = 0.025 km^2
  
  
  # Set the default value to 25,000 m^2
  smallThreshold <- set_units(25000, "m2")
  
  
  # Watershed-specific thresholds could be set like this: 
  
  # if (ws$ID[1] == "NV") {
  #   smallThreshold <- set_units(1, "m2")
  # }
  
  
  # Also, make sure 'catchDF' is using a coordinate reference system (CRS) 
  # that also uses meters (https://epsg.io/3488)
  catchDF <- catchDF |>
    st_transform("epsg:3488")
  
  
  # After that, calculate the area of each catchment in 'catchDF'
  # The units should be square meters
  catchDF <- catchDF |>
    mutate(AREA = st_area(catchDF))
  
  
  # Look for catchments that fall below 'smallThreshold'
  catchDF <- catchDF |>
    mutate(SMALL_CATCHMENT = AREA < smallThreshold)
  
  
  # Notify the user if small catchments were found
  if (TRUE %in% catchDF$SMALL_CATCHMENT) {
    
    paste0(sum(catchDF$SMALL_CATCHMENT), " catchment",
           if_else(sum(catchDF$SMALL_CATCHMENT) > 1,
                   "s are ", 
                   " is "),
           "unusually small! (Area < ",
           smallThreshold, " ", 
           deparse_unit(smallThreshold), ")") |>
      message()
    
  }
  
  
  # Return 'catchDF'
  return(catchDF)
  
}



generateMap <- function (catchDF, fieldName, ws) {
  
  # Generate a map of the catchments for visual inspection
  
  
  # If earlier checks identified issues, include distinct layers for them
  
  
  # Get a variable with all layers that will appear in the map
  layerDF <- tibble(NAME = c("Catchment_QAQC", "Disconnected_Polygons",
                             "Small_Catchments"),
                    INCLUDE = c(TRUE, FALSE, FALSE))
  
  
  if (TRUE %in% catchDF$DISCONNECTED_POLYGONS) {
    
    layerDF$INCLUDE[2] <- TRUE
    
  }
  
  
  if (TRUE %in% catchDF$SMALL_CATCHMENT) {
    
    layerDF$INCLUDE[3] <- TRUE
    
  }
  
  
  # Initialize the basemaps first (and add a scalebar)
  leafMap <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
    addTiles() |>
    addProviderTiles(provider = providers$CartoDB.Positron, 
                     group = "CartoDB.Positron") |>
    addProviderTiles(provider = providers$CartoDB.DarkMatter, 
                     group = "CartoDB.DarkMatter") |>
    addProviderTiles(provider = providers$OpenStreetMap, 
                     group = "OpenStreetMap") |>
    addProviderTiles(provider = providers$Esri.WorldImagery, 
                     group = "Esri.WorldImagery") |>
    addProviderTiles(provider = providers$OpenTopoMap, 
                     group = "OpenTopoMap") |>
    addScaleBar(position = "bottomleft")
  
  
  # If any QA/QC layers will be added to the map, include a grayscale version 
  # of the main catchment map too
  if (sum(layerDF$INCLUDE) > 1) {
    
    layerDF <- bind_rows(layerDF,
                         tibble(NAME = paste0(layerDF$NAME[1], "_Grayscale"),
                                INCLUDE = TRUE))
    
    
    leafMap <- leafMap |>
      addLayer(catchDF |> 
                 select(all_of(fieldName), 
                        NUM_POLYGONS, DISCONNECTED_POLYGONS,
                        AREA, SMALL_CATCHMENT) |>
                 mutate(AREA = round(AREA)) |>
                 rename(!! paste0("AREA (", 
                                  deparse_unit(catchDF$AREA[1]),
                                  ")") := AREA), 
               colPal = "lightgray", fillOpacity = 0.65, 
               lineOpacity = 1.0, lineWeight = 1.5, lineCol = "black", 
               group = tail(layerDF$NAME, 1), 
               labelFormula = paste0("Catchment ", catchDF[[fieldName]]))
    
  }
  
  
  # After that, proceed to setting up the main layer of the map
  
  
  # Choose a color palette with a variety of colors
  # (The palette colors are randomly shuffled to reduce the chance of similar
  #  colors appearing next to each other)
  set.seed(10)
  
  
  colorPal <- qualitative_hcl(n = nrow(catchDF)) |> sample()
  
  
  # Add 'catchDF' as a layer to 'leafMap'
  leafMap <- leafMap |>
    addLayer(catchDF |> 
               select(all_of(fieldName), 
                      NUM_POLYGONS, DISCONNECTED_POLYGONS,
                      AREA, SMALL_CATCHMENT) |>
               mutate(AREA = round(AREA)) |>
               rename(!! paste0("AREA (", 
                                deparse_unit(catchDF$AREA[1]),
                                ")") := AREA), 
             colPal = colorPal, fillOpacity = 0.75, 
             lineOpacity = 1.0, lineWeight = 1.5, lineCol = "black", 
             group = layerDF$NAME[1], 
             labelFormula = paste0("Catchment ", catchDF[[fieldName]])) |>
    addHomeButton(group = layerDF$NAME[1], position = "bottomleft",
                  ext = catchDF |>
                    st_transform("+proj=longlat +datum=WGS84") |>
                    st_bbox())
  
  
  # If there will be other QAQC layers in the map, hide the main layer by default
  if (sum(layerDF$INCLUDE) > 1) {
    leafMap <- leafMap |>
      hideGroup(layerDF$NAME[1])
  }
  
  
  # If there are issues with disconnected polygons, add a separate layer 
  # for that (along with a legend)
  if (layerDF$INCLUDE[2]) {
    
    filteredCatch <- catchDF |> 
      filter(DISCONNECTED_POLYGONS) |> 
      select(all_of(fieldName),
             NUM_POLYGONS, DISCONNECTED_POLYGONS)
    
    
    leafMap <- leafMap |>
      addLayer(filteredCatch, 
               colPal = qualitative_hcl(n = nrow(filteredCatch)) |> sample(), 
               fillOpacity = 1.0, 
               lineOpacity = 1.0, lineWeight = 3.0, lineCol = "red", 
               group = layerDF$NAME[2], 
               labelFormula = paste0("Catchment ", 
                                     filteredCatch |>
                                       select(all_of(fieldName)) |>
                                       st_drop_geometry() |>
                                       unlist(use.names = FALSE))) |>
      addLayer(filteredCatch |> 
                 st_cast("POLYGON", warn = FALSE) |>
                 st_poi() |>
                 mutate(!!fieldName := filteredCatch |>
                          select(all_of(fieldName)) |> 
                          st_cast("POLYGON", warn = FALSE) |>
                          st_drop_geometry() |>
                          unlist(use.names = FALSE)), 
               colPal = "red", fillOpacity = 1.0, 
               lineOpacity = 1.0, lineWeight = 2.0, lineCol = "black", 
               group = layerDF$NAME[2], 
               labelFormula = paste0("Catchment ", 
                                     filteredCatch |>
                                       select(all_of(fieldName)) |> 
                                       st_cast("POLYGON", warn = FALSE) |>
                                       st_drop_geometry() |>
                                       unlist(use.names = FALSE)), 
               type = "point", radius = 5) |>
      addLegend(position = "topright", colors = "red",
                title = paste0("QA/QC Issue #1"), 
                labels = layerDF$NAME[2], 
                group = layerDF$NAME[2], opacity = 0.80) |>
      addHomeButton(group = layerDF$NAME[2], position = "bottomleft",
                    ext = catchDF |>
                      filter(DISCONNECTED_POLYGONS) |>
                      st_transform("+proj=longlat +datum=WGS84") |>
                      st_bbox())
    
  }
  
  
  # Similarly, if there are small catchments detected, add a separate layer 
  # for that (along with a legend)
  if (layerDF$INCLUDE[3]) {
    
    leafMap <- leafMap |>
      addLayer(catchDF |> 
                 filter(SMALL_CATCHMENT) |> 
                 select(all_of(fieldName), AREA, SMALL_CATCHMENT) |>
                 mutate(AREA = round(AREA)) |>
                 rename(!! paste0("AREA (", 
                                  deparse_unit(catchDF$AREA[1]),
                                  ")") := AREA), 
               colPal = "orange", fillOpacity = 1.0, 
               lineOpacity = 1.0, lineWeight = 2.0, lineCol = "orange", 
               group = layerDF$NAME[3], 
               labelFormula = paste0("Catchment ", 
                                     catchDF |> 
                                       filter(SMALL_CATCHMENT) |>
                                       select(all_of(fieldName)) |>
                                       st_drop_geometry() |>
                                       unlist(use.names = FALSE))) |>
      addLayer(catchDF |> 
                 filter(SMALL_CATCHMENT) |> 
                 st_poi() |>
                 mutate(CATCHMENT = 
                          catchDF[[fieldName]][catchDF$SMALL_CATCHMENT]), 
               colPal = "orange", fillOpacity = 1.0, 
               lineOpacity = 1.0, lineWeight = 2.0, lineCol = "black", 
               group = layerDF$NAME[3], 
               labelFormula = paste0("Catchment ", catchDF |> 
                                       filter(SMALL_CATCHMENT) |>
                                       select(all_of(fieldName)) |>
                                       st_drop_geometry() |>
                                       unlist(use.names = FALSE)), 
               type = "point", radius = 5) |>
      addLegend(position = "topright", colors = "orange",
                title = paste0("QA/QC Issue #",
                               sum(layerDF$INCLUDE[1:3]) - 1), 
                labels = layerDF$NAME[3], 
                group = layerDF$NAME[3], opacity = 0.80) |>
      addHomeButton(group = layerDF$NAME[3], position = "bottomleft",
                    ext = catchDF |>
                      filter(SMALL_CATCHMENT) |>
                      st_transform("+proj=longlat +datum=WGS84") |>
                      st_bbox())
    
  }
  
  
  leafMap <- leafMap |>
    addLayersControl(baseGroups = c("CartoDB.Positron",
                                    "CartoDB.DarkMatter", "OpenStreetMap",
                                    "Esri.WorldImagery",
                                    "OpenTopoMap"),
                     overlayGroups = layerDF$NAME[layerDF$INCLUDE],
                     position = "topleft")
  
  
  # Finally, save 'leafMap'
  outName <- paste0("W1_Watershed_Demand/Output/", ws$ID, "_Catchment_QAQC_Map.html")
  
  
  mapshot(leafMap, outName)
  
  
  # Notify the user about the map
  paste0("Generated map and saved it in the \"Output\" folder!\n\n",
         "Please inspect \"", outName, "\" for issues.") |>
    strwrap(width = 0.99 * getOption("width")) |>
    paste0(collapse = "\n") |>
    message()
  
  
  # Return nothing
  return(invisible(NULL))
  
}



addLayer <- function (leafMap, df, colPal, fillOpacity = 0.7, lineOpacity = 1.0,
                      lineWeight = 1.5, lineCol = "black", 
                      group = "", labelFormula, type = "polygon",
                      radius = 2) {
  
  
  if (type == "polygon") {
    
    leafMap <- leafMap |>
      addPolygons(data = df |>
                    st_transform("+proj=longlat +datum=WGS84"),
                  fillColor = colPal, fillOpacity = fillOpacity,
                  opacity = lineOpacity, weight = lineWeight, 
                  color = lineCol,
                  group = group,
                  label = ~ labelFormula, 
                  popup = generatePopupTable(df),
                  popupOptions = popupOptions(maxWidth = 1500))
    
  } else if (type == "point") {
    
    leafMap <- leafMap |>
      addCircleMarkers(data = df |>
                         st_transform("+proj=longlat +datum=WGS84"), 
                       fillColor = colPal, fillOpacity = fillOpacity,
                       opacity = lineOpacity, weight = lineWeight,
                       color = lineCol, radius = radius, 
                       group = group,
                       label = ~ labelFormula,
                       popup = generatePopupTable(df),
                       popupOptions = popupOptions(maxWidth = 1500))
      
  }
  
  
  # Return 'leafMap'
  return(leafMap)
  
}



generatePopupTable <- function (df, banded = TRUE) {
  
  # Given a data frame, create a vector of tables for all of its rows 
  # Have each column appear as an entry in the tables
  
  
  # If 'df' is a "sf" object, drop the "geometry" column temporarily
  if ("sf" %in% class(df)) {
    
    df <- st_drop_geometry(df)
    
  }
  
  
  # Define some CSS styling for the table rows in advance
  tableStyling <- paste0("'border-collapse: collapse; font-size: 10pt;'")
  
  trBand <- paste0("'background-color: #f1f1f1;'")
  
  thStyling <- paste0("'border: solid 1pt black; text-align: left; ",
                      "padding: 3pt 10pt 3pt 4pt'")
  
  tdStyling <- paste0("'border: solid 1pt black; text-align: right; ",
                      "padding: 3pt 4pt 3pt 4pt'")
  
  
  # Define the inner portions of the table first
  # Iterate through each column of 'df'
  for (i in 1:ncol(df)) {
    
    # Define a table row for this column of 'df'
    newTR <- paste0("<tr style = ", 
                    if_else(banded && (i %% 2) == 0, trBand, "''"), 
                    ">",
                    "<th style = ", thStyling, ">",
                    names(df)[i], 
                    "</th>",
                    "<td style = ", tdStyling, ">",
                    df[[i]], 
                    "</td>",
                    "</tr>")
    
    
    # If this is the first column, initialize 'compiledTables' with it
    if (i == 1) {
      
      compiledTables <- newTR
      
    # Otherwise, combine 'newTR' with each existing elements
    } else {
      
      compiledTables <- paste0(compiledTables, newTR)
      
    }
    
  }
  
  
  # After the loop, add the surrounding "table" and "tbody" tags to 
  # each entry in 'compiledTables'
  # Then, return a vector of tables
  return(paste0("<table style = ", tableStyling, ">",
                  "<tbody>", compiledTables, "</tbody>",
                "</table>"))
  
}



#### EXECUTION ####

mainProcedure()


# Clean up
base::remove(list = ls())
