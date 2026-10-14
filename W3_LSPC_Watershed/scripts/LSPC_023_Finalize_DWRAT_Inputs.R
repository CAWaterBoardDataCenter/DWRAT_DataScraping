# This script gathers and validates the inputs for Paradigm DWRAT

# The "stream.csv" input from each LSPC model run is adjusted and reformatted

# A demand dataset is obtained for each watershed as well using the control file
# from the demand workflow ("Watershed_Demand_Dataset_Paths.xlsx")

# The watersheds' connectivity matrices and catchment geospatial layers
# are referenced from this spreadsheet as well

# These files are gathered and prepared for modeling via DWRAT
# They are then archived by this script, leaving modeling to the next script


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("W1_Watershed_Demand/Scripts/Shared_Functions_Demand.R")
source("Shared_Scripts/!Shared_Functions_Importer.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'LSPC_023_Finalize_DWRAT_Inputs.R'!\n")
  
  
  # Import a function from another script
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R",
                  "get_lspc_archive_folder")
  
  
  # Import the user-specified start and end dates
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/2]\tGathering files...\n")
  
  
  # Read in the LSPC weather control file
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Get the location of the archive folder
  dirPath <- get_lspc_archive_folder(startDate, endDate)
  
  
  # From there, obtain paths to the "stream.csv" file
  streamPaths <- get_lspc_stream_outputs(dirPath, controlDF$project_name)
  
  
  # Next, interface with the demand workflow's control file
  demandPaths <- read_demand_control()
  
  # To Do: Validate demand control file
  # Validation should be integrated into the file-reading functions 
  # with an optional argument like "validateFile = TRUE"
  
  
  # Try to identify the rows that correspond to each project watershed
  demandPaths <- demandPaths |>
    filter_to_project_rows(controlDF)
  
  
  # Using 'demandPaths', 
  # obtain the master demand tables, catchment layers, and connectivity matrices 
  # for each watershed
  demandPaths |>
    apply(1, get_master_demand_table)
  
  
  demandPaths |>
    apply(1, get_lspc_catchments)
  
  
  demandPaths |>
    apply(1, get_lspc_connectivity)
  
  # For each project watershed, the required files are read in and validated
  
  # They are not stored at this step in order to save memory
  
  # (Otherwise, every file would be here in the computer's RAM)
  
  
  # Next, read in HUC-12 information
  load("W1_Watershed_Demand/Input/GIS_General/NHD_H_California_State_WBDHU12.RData")
  
  # This ".RData" file adds to the environment a variable called 'huc12'
  # It contains the HUC-12 sub-basins for every watershed in California
  
  # To Do: Validate HUC-12 sub-basins layer
  
  huc12 <- huc12 |>
    select(huc12, name) |>
    st_transform(preferred_crs())
  
  
  # For modeling each watershed in DWRAT, the script can use either the LSPC 
  # catchments or the HUC-12 sub-basins (assuming there is only one flow path
  # per sub-basin)
  
  # Users can override the selection, if desired, but the default behavior is:
  
  # (1) Prefer HUC-12 sub-basins over catchments
  
  # (2) These sub-basins will be broken up if there are disconnected flow paths
  #     within a sub-basin, though
  
  # (3) And if there's only one HUC-12 sub-basin for the entire domain, the 
  #     catchments will be used instead
  
  
  # Read in the user's choice regarding this matter
  subbasinType <- get_from_lspc_master_control("SUBWATERSHED_TYPE") |>
    trimws() |> toupper()
  
  error_if(subbasinType %notin% c("CATCHMENT", "HUC12", "AUTO"),
           paste0("The sub-basin type must be exactly one of three values: ",
                  c("CATCHMENT", "HUC12", "AUTO") |> vec2QuotedStr(), ". ",
                  "However, \"", get_from_lspc_master_control("SUBWATERSHED_TYPE"), 
                  "\" was provided instead. Please investigate."))
  
  
  # With that, all of the required components are ready
  cat("\tDone!\n\n")
  
  
  # The next step is to iterate through each watershed
  # Generate DWRAT input files for each one
  # (These files will be saved in the archive folder)
  cat("[2/2]\tPreparing DWRAT files...\n")
  
  
  # Work on each watershed, one at a time
  for (i in 1:nrow(controlDF)) {
    
    cat("\n\n")
    paste0("\t[", i, "/", nrow(controlDF), "] ", controlDF$project_name[i]) |>
      cat()
    cat("\n\n")
    
    # Read in the demand components (MDT, catchments, connectivity matrix)
    mdt <- demandPaths[i, ] |> get_master_demand_table()
    
    catchments <- demandPaths[i, ] |> get_lspc_catchments()
    
    connMat <- demandPaths[i, ] |> get_lspc_connectivity()
    
    
    # Read in the streamflow output file from LSPC too
    stream <- streamPaths[i] |> getFile()
    
    
    # Get the path to the storage location for the DWRAT input files too
    writePath <- paste0(dirPath, "/", controlDF$project_name[i], "/DWRAT/Input") |>
      normalizePath(mustWork = TRUE)
    
    
    # Based on 'subbasinType', use either catchments or HUC-12 sub-basins 
    # to generate Paradigm DWRAT input files
    if (subbasinType == "CATCHMENT") {
      
      cat("\n\n")
      cat("\tUsing LSPC catchments for DWRAT files!")
      cat("\n\n")
      
      prep_dwrat_files_catchments(stream, mdt, catchments, connMat, writePath, huc12)
      
    } else if (subbasinType == "HUC12") {
      
      cat("\n\n")
      cat("\tUsing HUC-12 sub-basins for DWRAT files!")
      cat("\n\n")
      
      prep_dwrat_files_huc12(stream, mdt, catchments, connMat, writePath, huc12)
      
    } else {
      
      prep_dwrat_files_auto(stream, mdt, catchments, connMat, writePath, huc12)
      
    }
    
  }
  
  
  cat(col_green("\n'LSPC_023_Finalize_DWRAT_Inputs.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_lspc_stream_outputs <- function (dirPath, wsNames) {
  
  # Given the path to an archive folder as well as the names of the watersheds,
  # extract paths to the "stream.csv" files that were generated by LSPC model runs
  
  return(paste0(dirPath, "/", wsNames, "/Output/stream.csv") |>
           normalizePath(mustWork = TRUE))
  
}



filter_to_project_rows <- function (demandPaths, controlDF) {
  
  # Connect the watersheds listed in the demand control file with the ones
  # in the LSPC workflow control file
  
  # Find the row that corresponds to each project watershed in 'controlDF'
  
  # Then, return a modified version of 'demandPaths' that contains only those rows
  
  
  # Before beginning, remove "test" rows from 'demandPaths'
  demandPaths <- demandPaths |>
    filter(!grepl("Test", NAME, ignore.case = TRUE))
  
  
  # Then, use the "project_name" field to locate equivalent names in 'demandPaths'
  indexMatches <- controlDF$project_name |>
    map(~ grep(., demandPaths$NAME, ignore.case = TRUE))
  
  # 'indexMatches' should be a list that contains matching indices for each
  # project watershed in 'demandPaths'
  
  # Ideally, the names used in 'controlDF' should have one equivalent match in
  # 'demandPaths'--then, 'indexMatches' would contain the row index number for 
  # that watershed in 'demandPaths'
  
  
  # Ensure that exactly one match was found in each project watershed's case
  if (any(lengths(indexMatches) != 1)) {
    
    cat("\n\n")
    controlDF$project_name[lengths(indexMatches) != 1] |>
      paste0(" (", lengths(indexMatches)[which(lengths(indexMatches) != 1)], 
             " matches)") |>
      print()
    cat("\n\n")
    
    
    paste0("Could Not Identify Equivalent Row\n\n",
           "The script checked the demand control file and attempted to ",
           "identify rows that correspond to each watershed in the LSPC ",
           "weather control file. However, exactly one match could not be ",
           "found for every watershed. Please investigate the watershed(s) ",
           "listed above.") |>
      stop_script()
    
  }
  
  
  # Check next to confirm that there are no duplicate matches in 'indexMatches'
  indexMatches <- indexMatches |>
    unlist() |> unique()
  
  
  error_if(length(indexMatches) != nrow(controlDF),
           paste0("Multiple Matches for the Same Demand Row\n\n",
                  "Every watershed in the LSPC control file should have exactly ",
                  "one match in the demand control file. However, at least two ",
                  "watersheds match with the same demand row. Please investigate."))
  
  
  # If there are no issues, return 'demandPaths' with just the chosen rows
  return(demandPaths[indexMatches, ])
  
  # NOTE: The sorting of the rows in 'demandPaths' will now match 
  #       the ordering in 'controlDF'
  
}



get_master_demand_table <- function (ws) {
  
  # For a given watershed, using file information from the demand control file,
  # read in a Master Demand Table (i.e., the final product of the demand workflow)
  
  return(ws[["MASTER_DEMAND_TABLE_CSV_PATH"]] |>
           getFile())
  
  # To Do: Validate MDT file
  
}



get_lspc_catchments <- function (ws) {
  
  # For a given watershed, using file information from the demand control file,
  # read in a geospatial layer containing catchment information
  
  # Use a consistent coordinate reference system for these operations
  
  # Create a "BASIN" field in the catchment layer too
  # Rely on the user-specified "SUBBASIN_FIELD_ID_NAMES" to help adjust 
  # the catchment layer's unique identifier column and make the column name
  # consistent between watersheds
  
  
  # Get the name of the column that distinguishes catchments in the layer
  idCol <- ws[["SUBBASIN_FIELD_ID_NAMES"]] |> 
    str_split(";") |> unlist() |> 
    trimws() |> head(1)
  
  
  # Next, get the catchment layer and return it 
  # (with the coordinate system set and the "BASIN" column added)
  return(getGIS(ws = ws, 
                GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME") |>
           st_transform(preferred_crs()) |>
           rename(BASIN = all_of(idCol)) |>
           mutate(BASIN = as.character(BASIN)))
  
  # To Do: Validate subbasins
  
}



get_lspc_connectivity <- function (ws) {
  
  # For a given watershed, using file information from the demand control file,
  # read in a matrix containing catchment connectivity data
  
  # For consistency between watersheds, name the first column "BASIN"
  
  
  # Read in the matrix
  connMat <- ws[["CONNECTIVITY_MATRIX_SPREADSHEET_PATH"]] |> 
    getFile(worksheet = ws[["CONNECTIVITY_MATRIX_WORKSHEET_NAME"]])
    
  
  # To Do: Validate connectivity matrix
  
  
  # The first column will be referred to as "BASIN"
  names(connMat)[1] <- "BASIN"
  
  
  # Return 'connMat'
  return(connMat)
  
}



prep_dwrat_files_auto <- function (stream, mdt, catchments, connMat, writePath, huc12) {
  
  # Automatically determine whether HUC-12 sub-basins or LSPC catchments 
  # should be the basis of the DWRAT model run
  
  # If every catchment is contained within a single sub-basin, use catchments
  
  # Otherwise, use HUC-12 sub-basins
  
  
  # Get the intersection between catchments and HUC-12 sub-basins
  hucIntersection <- catchments |>
    find_intersecting_huc12(huc12)
  
  # This will be a tibble that lists each catchment and its corresponding sub-basin
    
  
  # After that, check if every catchment intersects with the same sub-basin
  intersectingSubbasins <- hucIntersection$huc12 |>
    unique()
  
  
  # If that's the case, use the catchment-based pathway
  if (length(intersectingSubbasins) == 1) {
    
    cat("\n\n")
    cat("\tUsing LSPC catchments for DWRAT run!")
    cat("\n\n")
    
    return(prep_dwrat_files_catchments(stream, mdt, catchments, connMat, writePath, huc12))
    
  # Otherwise, use HUC-12 sub-basins instead
  } else {
    
    cat("\n\n")
    cat("\tUsing HUC-12 sub-basins for DWRAT run!")
    cat("\n\n")
    
    return(prep_dwrat_files_huc12(stream, mdt, catchments, connMat, writePath, huc12))
    
  }
  
}



find_intersecting_huc12 <- function (catchments, huc12) {
  
  # Generate a tibble that expresses the intersection between LSPC catchments
  # and HUC-12 sub-basins 
  
  # For each catchment, identify the HUC-12 sub-basin that it intersects with
  
  # In a tibble, list each catchment and its corresponding HUC-12 sub-basin
  
  
  # Get the pole of inaccessibility for every catchment
  poiDF <- catchments |>
    st_poi()
  
  # (This will give a single central point for every catchment)
  
  
  # Check which HUC-12 sub-basins intersect with these points
  hucIntersection <- st_intersects(poiDF, huc12)
  
  
  # Make sure each catchment has at least one intersection with a HUC-12 sub-basin
  error_if(any(lengths(hucIntersection) == 0),
           paste0("Non-Intersecting Catchment(s) Detected\n\n",
                  "One or more LSPC catchments for the watershed do not ",
                  "intersect with any HUC-12 sub-basin in the dataset. Please ",
                  "investigate both input files."))
  
  
  # Error Check: There shouldn't be more than one intersecting HUC-12 sub-basin
  # for each catchment
  error_if(any(lengths(hucIntersection) > 1),
           paste0("Multi-Intersecting Catchment(s) Detected\n\n",
                  "One or more LSPC catchments for the watershed intersect ",
                  "with multiple HUC-12 sub-basins in the dataset. Please ",
                  "investigate both input files."))
  
  
  # Return a tibble that lists each catchment and its corresponding HUC-12 sub-basin
  return(tibble(BASIN = as.character(catchments$BASIN),
                huc12 = huc12$huc12[unlist(hucIntersection)]))
  
}



prep_dwrat_files_catchments <- function (stream, mdt, catchments, connMat, writePath, huc12) {
  
  # Prepare DWRAT input files using catchments as DWRAT sub-watersheds
  
  
  # Several precursor files are necessary to generate the DWRAT input files
  
  # Once they are all established, the actual inputs will be prepared
  
  
  # First, identify the outlet catchment in each sub-watershed
  
  # Since every catchment is its own sub-watershed in this case, 
  # each catchment is its own "outlet catchment" 
  lastCatch <- catchments |>
    st_drop_geometry() |>
    select(BASIN) |>
    mutate(BASIN = as.character(BASIN)) |> 
    rename(SUBWATERSHED = BASIN) |> 
    mutate(OUTLET_CATCH = SUBWATERSHED)
  
  # This is a two-column tibble with each unique subwatershed that will be used
  # in the DWRAT run (alongside their outlet catchments)
  
  # (Both columns are coerced into character-type columns too)
  
  
  # The next step is to prepare a connectivity matrix for the DWRAT subwatersheds
  # Since each catchment is its own subwatershed, the exact same connectivity matrix
  # can be reused (since no catchments are being grouped together or anything)
  dwratConn <- connMat |>
    rename(SUBWATERSHED = BASIN)
  
  # Though, in 'dwratConn', instead of "BASIN", use "SUBWATERSHED" 
  
  
  # Use these variables to generate the required input files for DWRAT
  generate_dwrat_inputs(stream, mdt, catchments, connMat, writePath, huc12,
                        lastCatch, dwratConn)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



prep_dwrat_files_huc12 <- function (stream, mdt, catchments, connMat, writePath, huc12) {
  
  # Prepare DWRAT input files using HUC-12 sub-basins* as DWRAT sub-watersheds
  
  # * = If multiple distinct flow paths exist within a sub-basin, it will be split
  
  
  # Several precursor files are necessary to generate the DWRAT input files
  
  # Once they are all established, the actual inputs will be prepared
  
  
  # First, identify the outlet catchment in each sub-watershed
  # Each HUC-12 sub-basin will have at least one outlet catchment
  lastCatch <- catchments |>
    get_huc12_subbasin_outlet(connMat, huc12)
  
  # This tibble will contain two columns: "SUBWATERSHED" and "OUTLET_CATCH"
  # These are the HUC-12 sub-basins (and sub-divisions, if required)
  # as well as the outlet catchments for each subwatershed
  
  
  # After that, set up a connectivity matrix for the DWRAT subwatersheds
  
  # Right now, 'connMat' applies to the LSPC catchments
  # However, they will need to wrapped up into the subwatershed level
  dwratConn <- connMat |>
    get_huc12_connectivity(lastCatch)
  
  
  # Use these variables to generate the required input files for DWRAT
  generate_dwrat_inputs(stream, mdt, catchments, connMat, writePath, huc12,
                        lastCatch, dwratConn)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_huc12_subbasin_outlet <- function (catchments, connMat, huc12) {
  
  # Identify the outlet catchment in each HUC-12 sub-basin
  
  # Generate a tibble that connects each sub-basin to a catchment
  
  
  # In most cases, each sub-basin will have exactly one outlet catchment
  
  # However, if a sub-basin has disconnected flow paths, 
  # it will need to be split in the model run
  
  # In this function, the HUC-12 splits (if needed) will be determined
  # and expressed in the output tibble
  
  
  # To start, link each catchment to a HUC-12 sub-basin
  catchments <- catchments |>
    left_join(find_intersecting_huc12(catchments, huc12), 
              by = "BASIN", relationship = "one-to-one")
  
  
  # Construct the initial version of the outlet tibble as well
  lastCatch <- tibble(SUBWATERSHED = unique(catchments$huc12), 
                      OUTLET_CATCH = NA_character_)
  
  
  # Iterate through every HUC-12 sub-basin in 'lastCatch' next
  for (i in 1:nrow(lastCatch)) {
    
    # The outlet catchment is the one that all other catchments in the sub-basin drain to
    
    
    # Get a temporary collection of catchments in this sub-basin
    subsetCatch <- catchments |>
      filter(huc12 == lastCatch$SUBWATERSHED[i])
    
    
    # Get a subset of the connectivity matrix too
    subsetConn <- connMat |>
      filter(BASIN %in% subsetCatch$BASIN) |>
      select(BASIN, all_of(as.character(subsetCatch$BASIN)))
    
    
    # In these connectivity matrices, if Catchment A eventually drains into Catchment B,
    # in row A, column B, there will be a "1"
    
    # Therefore, if Catchment B is the sole outlet, there should be a "1" in 
    # every row, in column B
    
    # Thus, a HUC-12 sub-basin has a single outlet catchment if there exists
    # a column whose sum (of 1's) equals the number of rows in 'subsetConn'
    
    maxIndex <- which(colSums(subsetConn[, -1]) == nrow(subsetConn))
    
    # Ignoring the first column of 'subsetConn' (which is the "BASIN" column),
    # 'maxIndex' will locate the catchment that is most downstream
    
    
    # However, if a sub-basin has multiple flow paths, 'maxIndex' will be empty
    # This is because no single catchment would be the outlet for all catchments
    # in the HUC-12 sub-basin
    if (length(maxIndex) == 0) {
      
      # In that case, leave "OUTLET_CATCH" as NA for this sub-basin
      # and skip to the next iteration
      
      # This sub-basin will be addressed later
      next
      
    }
    
    
    # Next, check if 'maxIndex' has multiple values
    # This would mean multiple catchments are "most downstream"
    # This should not happen, so output an error if it's detected
    error_if(length(maxIndex) > 1,
             paste0("Multiple Outlet Catchments\n\n",
                    "In sub-basin \"", lastCatch$SUBWATERSHED[i], "\", ",
                    "the script could not identify one specific catchment that ",
                    "is most downstream. Instead, more than one match was found. ",
                    "However, this should not happen. Please investigate."))
    
    
    # If 'maxIndex' contains exactly one value, use it to update 'lastCatch'
    lastCatch$OUTLET_CATCH[i] <- names(subsetConn)[maxIndex + 1]
    
    # NOTE: When calculating 'maxIndex', the first column ("BASIN") was excluded
    #       from the calculations. For that reason, when referencing the most 
    #       downstream catchment for this sub-basin, 'maxIndex' must be incremented
    #       by one. 
    
  }
  
  
  # Check if any entries in 'lastCatch' are still NA
  # That is a sign that a HUC-12 sub-basin contains multiple flow paths
  # The next step is to handle those sub-basins, if present
  while (anyNA(lastCatch$OUTLET_CATCH)) {
    
    # This loop will repeat until all sub-basins are addressed
    
    # Focus on one sub-basin at a time, starting with the first instance of NA
    # in 'lastCatch'
    naOutlet <- which(is.na(lastCatch$OUTLET_CATCH))[1]
    
    
    # Just like in the main `for` loop, get a subset of the tibbles with a 
    # focus on the catchments related to this HUC-12 sub-basin
    
    subsetCatch <- catchments |>
      filter(huc12 == lastCatch$SUBWATERSHED[naOutlet])
    
    
    # Get a subset of the connectivity matrix too
    subsetConn <- connMat |>
      filter(BASIN %in% subsetCatch$BASIN) |>
      select(BASIN, all_of(as.character(subsetCatch$BASIN)))
    
    
    # The next step is to divide these catchments into separate groups
    # (corresponding to each distinct flow path within the HUC-12 sub-basin)
    
    # No catchment should be assigned to multiple flow paths
    # (i.e., no double-counting)
    
    # To help track these catchments, construct a tibble of 
    # every catchment in this sub-basin
    basinCatch <- tibble(CATCHMENT = subsetConn$BASIN,
                         FLOW_PATH = NA_real_,
                         OUTLET_CATCH = NA_character_)
    
    
    # "FLOW_PATH" will be a number corresponding to a distinct flow path
    # in the HUC-12 sub-basin
    
    
    # While there are still NA values in "FLOW_PATH", continue to loop
    # (Yes, this is a nested `while` loop inside of another `while` loop)
    while (anyNA(basinCatch$FLOW_PATH)) {
      
      # First, start by setting a number that represents which flow path
      # is being considered right now ('pathNumber')
      
      
      # Set the flow path number based on 
      # the number of flow paths that have been completed
      if (all(is.na(basinCatch$FLOW_PATH))) {
        
        # In the first iteration, every entry in 'basinCatch' is still NA
        pathNumber <- 1
        
        # For that reason, 'pathNumber' will be set to '1'
        # (This is the first sub-division of the HUC-12 sub-basin)
        
      } else {
        
        pathNumber <- max(basinCatch$FLOW_PATH, na.rm = TRUE) + 1
        
        # In all other cases, "FLOW_PATH" will contain a number
        # Increment the largest value to obtain 'pathNumber'
        # (This is the start of the "n + 1" sub-division loop)
        
      }
      
      
      # Create a filtered version of 'subsetConn' that only contains catchments
      # that still have values of NA in "FLOW_PATH" of 'basinCatch'
      filteredConn <- subsetConn |>
        select(BASIN, all_of(as.character(basinCatch$CATCHMENT[is.na(basinCatch$FLOW_PATH)])))
      
      # Starting with the columns of 'subsetConn', keep only "BASIN" and 
      # any catchment that still has values of NA in "FLOW_PATH"
      
      # But that only narrows the column part
      # The rows must be filtered too
      
      filteredConn <- filteredConn |>
        filter(BASIN %in% names(filteredConn))
      
      # This filter keeps catchments in the rows of 'filteredConn' that appear 
      # among the columns
      
      
      # For this flow path, find the catchment in 'filteredConn' with the most connections
      maxIndex <- which.max(colSums(filteredConn[, -1]))
      
      
      targetCatch <- names(filteredConn)[maxIndex + 1]
      
      # NOTE: 'maxIndex' was incremented by 1 again to account for the "BASIN" column
      
      
      # After that, take a look at 'subsetConn' again
      
      # Even though 'filteredConn' was used to find 'targetCatch', 
      # for error handling purposes, check all catchments that connect to 'targetCatch'
      # using 'subsetConn' 
      
      # (All of the catchments that connect to 'targetCatch' should have NA 
      #  as their "FLOW_PATH" values, but this will help confirm that)
      
      
      # Get the catchment IDs that all connect to 'targetCatch'
      connectedCatch <- subsetConn$BASIN[subsetConn[[targetCatch]] == 1]
      
      # NOTE: 'targetCatch' already appears in 'connectedCatch' too
      #       This is because, in the connectivity matrix, every catchment "flows"
      #       into itself, meaning it has "1" in its own column
      
      
      # Error Check
      # All of these catchments should have NA as their values in 'basinCatch'
      error_if(!all(is.na(basinCatch$FLOW_PATH[basinCatch$CATCHMENT %in% connectedCatch])),
               paste0("Catchment Connectivity Error\n\n",
                      "Each catchment should be a part of one flow path only. However, ",
                      "a catchment that connects to \"", targetCatch, "\" has ",
                      "more than one distinct flow path connection. Please investigate."))
      
      
      # If there are no issues, update these catchments' entries in 'basinCatch'
      
      # First, "FLOW_PATH" should contain 'pathNumber'
      basinCatch$FLOW_PATH[basinCatch$CATCHMENT %in% connectedCatch] <- pathNumber
      
      # This number will correspond to a flow path consisting of just 
      # this group of catchments
      
      # After that, set "OUTLET_CATCH" to 'targetCatch'
      # Since it had the maximum number of connections, it is likely the
      # outlet catchment for this flow path
      basinCatch$OUTLET_CATCH[basinCatch$CATCHMENT %in% connectedCatch] <- targetCatch
      
    } # End of `while` loop that fills in 'basinCatch'
    
    
    # After the loop, every catchment within this HUC-12 sub-basin has been assigned
    # to a distinct flow path
    
    # 'lastCatch' will now be updated 
    
    # Right now, there is a single entry for this HUC-12 sub-basin in 'lastCatch'
    
    # However, since there are distinct flow paths, it will need separate entries
    # for each of these flow paths
    
    
    # First, modify 'basinCatch' to contain only "FLOW_PATH" and "OUTLET_CATCH"
    # (with duplicate rows removed)
    basinCatch <- basinCatch |>
      select(FLOW_PATH, OUTLET_CATCH) |>
      unique()
    
    
    # Make sure each "FLOW_PATH" value appears only once in 'basinCatch' now
    error_if(nrow(basinCatch) != length(unique(basinCatch$FLOW_PATH)),
             paste0("Flow Path Tracking Error\n\n",
                    "At this step, unique flow paths within HUC-12 Sub-Basin \"",
                    lastCatch$SUBWATERSHED[naOutlet], "\" have been identified. ",
                    "However, there is a script error somewhere because the ",
                    "same \"FLOW_PATH\" ID number appears more than once in the ",
                    "tibble. Please investigate."))
    
    
    # Create new "SUBWATERSHED" labels in 'basinCatch' by combining the original
    # HUC-12 sub-basin code with the "FLOW_PATH" number
    basinCatch <- basinCatch |>
      mutate(SUBWATERSHED = paste0(lastCatch$SUBWATERSHED[naOutlet], "_", FLOW_PATH)) |>
      select(SUBWATERSHED, OUTLET_CATCH)
    
    # At this step, 'basinCatch' and 'lastCatch' have the same formatting
    # (two columns, "SUBWATERSHED" and "OUTLET_CATCH")
    
    # Remove the entry identified by 'naOutlet' from 'lastCatch'
    # Then, append its replacement rows from 'basinCatch'
    lastCatch <- lastCatch[-naOutlet, ] |>
      bind_rows(basinCatch)
    
    # This removed an entry in 'lastCatch' that had a value of NA for "OUTLET_CATCH"
    
    # If there are more entries like this, the loop will continue
    
  }
  
  
  # At this step, there will be no more NA values in "OUTLET_CATCH" 
  # Return 'lastCatch'
  return(lastCatch)
  
}



get_huc12_connectivity <- function (connMat, lastCatch) {
  
  # Given the connectivity matrix for LSPC catchments, 
  # use 'lastCatch' to derive a new matrix for the DWRAT subwatersheds
  
  # This will be based on the outlet catchments listed in 'lastCatch'
  
  
  # First, reduce the number of columns in 'connMat' 
  # Exclude catchments that do not appear in "OUTLET_CATCH" of 'lastCatch'
  dwratConn <- connMat |>
    select(BASIN, all_of(as.character(lastCatch$OUTLET_CATCH)))
  
  
  # Filter the "BASIN" column too
  # It should only have rows of catchments that appear in 'lastCatch'
  dwratConn <- dwratConn |>
    filter(BASIN %in% lastCatch$OUTLET_CATCH)
  
  
  # Next, replace all of the catchment IDs in 'dwratConn' with their corresponding
  # HUC-12 sub-basin label in the "SUBWATERSHED" column
  dwratConn$BASIN <- dwratConn$BASIN |>
    map_chr(~ lastCatch$SUBWATERSHED[lastCatch$OUTLET_CATCH == .][1])
  
  
  names(dwratConn)[-1] <- names(dwratConn)[-1]  |>
    map_chr(~ lastCatch$SUBWATERSHED[lastCatch$OUTLET_CATCH == .][1])
  
  
  # Finally, to emphasize the difference of this new connectivity matrix, 
  # rename "BASIN" to "SUBWATERSHED" in 'dwratConn'
  dwratConn <- dwratConn |>
    rename(SUBWATERSHED = BASIN)
  
  
  # Return 'dwratConn' after that
  return(dwratConn)
  
}



get_immediate_downstream_basin <- function (dwratConn) {
  
  # Given a connectivity matrix that expresses the relationships between 
  # subwatersheds, create a tibble of each subwatershed and its immediately
  # downstream subwatershed
  
  # 'dwratConn' should be a *downstream* connectivity matrix
  # A "1" means that the "row" sub-basin eventually flows into
  # the "column" sub-basin / the "column" sub-basin is downstream of the "row" 
  # sub-basin
  
  
  # This function may be useful in other scripts with connectivity matrices
  # not directly being applied to DWRAT subwatersheds
  
  # As such, the input 'dwratConn' may have "BASIN" or "SUBWATERSHED" 
  # as its first column name
  
  # For consistency, this function will use "SUBWATERSHED" 
  if ("BASIN" %in% names(dwratConn)) {
    
    dwratConn <- dwratConn |>
      rename(SUBWATERSHED = BASIN)
    
  }
  
  
  # Prepare the tibble that will ultimately link every subwatershed to its
  # immediately downstream subwatershed
  flowsTo <- dwratConn |>
    select(SUBWATERSHED) |>
    unique() |> 
    arrange(SUBWATERSHED) |>
    mutate(SUBWATERSHED = as.character(SUBWATERSHED),
           FLOWS_TO = NA_character_)
  
  # The above code extracts the "SUBWATERSHED" field from 'dwratConn', 
  # ensures that each value only appears once, sorts the values, 
  # converts "SUBWATERSHED" into a character column (if not already), and 
  # then adds a "FLOWS_TO" column that is a placeholder for the immediately 
  # downstream subwatersheds
  
  
  # The next step is to fill in the "FLOWS_TO" column with values
  
  
  # Iterate through each subwatershed
  for (i in 1:nrow(flowsTo)) {
    
    # Use 'dwratConn' to find the next subwatershed after this one
    
    # Looking at the row that corresponds to this iteration's subwatershed,
    # downstream subwatersheds will have a "1" in their respective columns
    
    
    # First filter 'dwratConn' to this subwatershed's row
    basinRow <- dwratConn |>
      filter(SUBWATERSHED == as.character(flowsTo$SUBWATERSHED[i]))
    
    
    # Get the names of downstream subwatersheds
    downstreamBasins <- names(basinRow)[basinRow == 1] |>
      base::setdiff(flowsTo$SUBWATERSHED[i])
    
    # `setdiff` is used to exclude the current iteration's subwatershed 
    # from its own vector of downstream basins
    
    
    # If 'downstreamBasins' is empty, there is no downstream subwatershed
    # That can happen if this subwatershed is the outlet basin
    if (length(downstreamBasins) == 0) {
      
      # In that case, set "FLOWS_TO" to "000"
      flowsTo$FLOWS_TO[i] <- basin_end_marker()
      
      # A string of three zeros signifies that there is no downstream subwatershed
      # (That's how the Mad River example from Paradigm did it, so I assume
      #  it means something)
      
      
      # Then move on to the next subwatershed
      next
      
    }
    
    
    # Alternatively, if exactly one match is found, there is only one subwatershed
    # that is downstream
    if (length(downstreamBasins) == 1) {
      
      # In that case, it has to be the immediately downstream subwatershed
      flowsTo$FLOWS_TO[i] <- downstreamBasins
      
      
      # Skip to the next subwatershed
      next
      
    }
    
    
    # If a subwatershed's iteration reaches this point, there is more than one
    # downstream subwatershed
    
    # In that case, a procedure is required to determine which subwatershed 
    # is *immediately* downstream of the current iteration's basin
    
    # Subwatersheds that are more downstream will have more subwatersheds that
    # eventually flow into them
    
    # Therefore, a subwatershed whose column in 'dwratConn' has a *lower* total
    # will be the immediately downstream one
    
    
    # For each of the downstream subwatersheds, get their column sums
    downstreamSums <- dwratConn[, names(dwratConn) %in% downstreamBasins] |>
      colSums()
    
    
    # The immediately downstream subwatershed has the smaller value
    flowsTo$FLOWS_TO[i] <- names(downstreamSums)[which.min(downstreamSums)]
    
  }
  
  
  # After the loop is complete, 'flowsTo' is completely filled in
  
  
  # Return it
  return(flowsTo)
  
}



get_most_downstream_basin <- function (dwratConn) {
  
  # Given a connectivity matrix that expresses the relationships between 
  # subwatersheds, create a tibble of each subwatershed and its 
  # most downstream subwatershed
  
  # 'dwratConn' should be a *downstream* connectivity matrix
  # A "1" means that the "row" sub-basin eventually flows into
  # the "column" sub-basin / the "column" sub-basin is downstream of the "row" 
  # sub-basin
  
  
  # This function may be useful in other scripts with connectivity matrices
  # not directly being applied to DWRAT subwatersheds
  
  # As such, the input 'dwratConn' may have "BASIN" or "SUBWATERSHED" 
  # as its first column name
  
  # For consistency, this function will use "SUBWATERSHED" 
  if ("BASIN" %in% names(dwratConn)) {
    
    dwratConn <- dwratConn |>
      rename(SUBWATERSHED = BASIN)
    
  }
  
  
  # Prepare the tibble that will ultimately link every subwatershed to its
  # most downstream subwatershed
  flowsTo <- dwratConn |>
    select(SUBWATERSHED) |>
    unique() |> 
    arrange(SUBWATERSHED) |>
    mutate(SUBWATERSHED = as.character(SUBWATERSHED), 
           LAST_BASIN = NA_character_)
  
  # The above code extracts the "SUBWATERSHED" field from 'dwratConn', 
  # ensures that each value only appears once, sorts the values, 
  # converts "SUBWATERSHED" into a character column (if not already), and 
  # then adds a "LAST_BASIN" column that is a placeholder for the most 
  # downstream subwatersheds
  
  
  # The next step is to fill in the "LAST_BASIN" column with values
  
  
  # Iterate through each subwatershed
  for (i in 1:nrow(flowsTo)) {
    
    # Use 'dwratConn' to find the next subwatershed after this one
    
    # Looking at the row that corresponds to this iteration's subwatershed,
    # downstream subwatersheds will have a "1" in their respective columns
    
    
    # First filter 'dwratConn' to this subwatershed's row
    basinRow <- dwratConn |>
      filter(SUBWATERSHED == as.character(flowsTo$SUBWATERSHED[i]))
    
    
    # Get the names of downstream subwatersheds
    downstreamBasins <- names(basinRow)[basinRow == 1] |>
      base::setdiff(flowsTo$SUBWATERSHED[i])
    
    # `setdiff` is used to exclude the current iteration's subwatershed 
    # from its own vector of downstream basins
    
    
    # If 'downstreamBasins' is empty, there is no downstream subwatershed
    # That can happen if this subwatershed is the outlet basin
    if (length(downstreamBasins) == 0) {
      
      # In that case, set "LAST_BASIN" to that subwatershed
      flowsTo$LAST_BASIN[i] <- flowsTo$SUBWATERSHED[i]
      
      
      # Then move on to the next subwatershed
      next
      
    }
    
    
    # Alternatively, if exactly one match is found, there is only one subwatershed
    # that is downstream
    if (length(downstreamBasins) == 1) {
      
      # In that case, it has to be the most downstream subwatershed
      flowsTo$LAST_BASIN[i] <- downstreamBasins
      
      
      # Skip to the next subwatershed
      next
      
    }
    
    
    # If a subwatershed's iteration reaches this point, there is more than one
    # downstream subwatershed
    
    # In that case, a procedure is required to determine which subwatershed 
    # is *most* downstream of the current iteration's basin
    
    # Subwatersheds that are more downstream will have more subwatersheds that
    # eventually flow into them
    
    # Therefore, a subwatershed whose column in 'dwratConn' has a *larger* total
    # will be the most downstream one
    
    
    # For each of the downstream subwatersheds, get their column sums
    downstreamSums <- dwratConn[, names(dwratConn) %in% downstreamBasins] |>
      colSums()
    
    
    # The most downstream subwatershed has the larger value
    flowsTo$LAST_BASIN[i] <- names(downstreamSums)[which.max(downstreamSums)]
    
  }
  
  
  # After the loop is complete, 'flowsTo' is completely filled in
  
  
  # Return it
  return(flowsTo)
  
}



basin_end_marker <- function () {
  
  # For basin connectivity tables, this string marks the end of a flow path
  
  # There is no further downstream subwatershed when this string is applied
  
  return("000")
  
  # In Paradigm DWRAT's example files, a string of three consecutive zeros
  # were used to mark the end of the flow path
  
}



generate_dwrat_inputs <- function (stream, mdt, catchments, connMat, writePath, 
                                   huc12, lastCatch, dwratConn) {
  
  # Generate the three primary input files for DWRAT
  
  # There are three files:
  #  (*) formatted_demand.csv
  #  (*) formatted_supply.csv
  #  (*) generated_basins.csv
  
  
  # In reality, distinct flow paths require separate sets of files, 
  # but that task will not be performed by this script
  
  # Instead, only the three files will be created and a later script will have to
  # split the data to run separate instances of DWRAT
  
  
  # Start by calculating a "Flows To" tibble
  # For every subwatershed in 'dwratConn', get the immediately downstream subwatershed
  flowsTo <- get_immediate_downstream_basin(dwratConn)
  
  
  # Make the three DWRAT input files after that
  
  
  # First, adjust the formatting of the demand dataset ('mdt')
  formattedDemand <- mdt |> 
    format_demand(catchments, huc12, connMat, lastCatch, flowsTo)
  
  
  # Then, adjust the data in 'stream' into the proper formatting
  formattedSupply <- stream |>
    format_supply(lastCatch, flowsTo)
  
  
  # After that, modify 'flowsTo' for modeling with DWRAT
  generatedBasins <- format_basins(flowsTo)
  
  
  
  # Finally, prepare to write the three key variables to 'writePath'
  formattedDemand |>
    writeOutput(paste0(writePath, "/formatted_demand.csv"))


  formattedSupply |>
    writeOutput(paste0(writePath, "/formatted_supply.csv"))


  generatedBasins |>
    writeOutput(paste0(writePath, "/generated_basins.csv"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



format_demand <- function (mdt, catchments, huc12, connMat, lastCatch, flowsTo) {
  
  # Update the Master Demand Table to include additional columns
  
  # Right now, it has "BASIN", which contains catchment IDs
  
  # These columns include:
  #  (*) HUC-12 sub-basin
  #  (*) The last LSPC catchment in the flow path
  #  (*) The DWRAT subwatershed (must be called "BASIN")
  #  (*) The immediately downstream subwatershed in the flow path 
  #      (must be called "FLOWS_TO")
  
  
  # Use `find_intersecting_huc12` to add a "huc12" column to the table
  mdt <- mdt |>
    mutate(BASIN = as.character(BASIN)) |> 
    left_join(find_intersecting_huc12(catchments, huc12),
              by = "BASIN", relationship = "many-to-one")
  
  
  # Next, check 'connMat' and get the most downstream catchment for each LSPC catchment
  # This column will serve as a stepping stone that helps link 'mdt' to 'lastCatch'
  # Thus, this step is necessary to link each water right to a "SUBWATERSHED" assignment
  
  
  # Connect each LSPC catchment to its most downstream catchment
  catchFlows <- get_most_downstream_basin(connMat)
  
  # The "SUBWATERSHED" column will contain a catchment
  # The "LAST_BASIN" column will contain the last catchment in that flow path
  
  # Rename the columns in 'catchFlows' to match 'mdt' and 'lastCatch'
  catchFlows <- catchFlows |>
    rename(BASIN = SUBWATERSHED,
           OUTLET_CATCH = LAST_BASIN)
  
  # In 'mdt', the catchments are still stored under "BASIN"
  # And in 'lastCatch', the outlet catchments are listed under "OUTLET_CATCH"
  
  # After that, join 'catchFlows' to 'mdt'
  mdt <- mdt |>
    left_join(catchFlows,
              by = "BASIN", relationship = "many-to-one")
    
  # This adds "OUTLET_CATCH" to 'mdt'
  # With this column, now 'mdt' and 'lastCatch' can be merged
  mdt <- mdt |>
    left_join(lastCatch, 
              by = "OUTLET_CATCH", relationship = "many-to-one")
  
  
  # Now 'mdt' has gained the "SUBWATERSHED" column
  # These are the assignments required for DWRAT
  # (Note: This column will have to be renamed to "BASIN")
  
  
  # Now that each water right has its subwatershed assignment, 
  # the "FLOWS_TO" column must be added
  
  # "SUBWATERSHED" can bridge 'mdt' and 'flowsTo'
  mdt <- mdt |>
    left_join(flowsTo, by = "SUBWATERSHED", relationship = "many-to-one")
  
  
  # Now, 'mdt' contains all required columns
  
  # A few edits are still necessary though
  
  
  # Before continuing, check if any values are missing in these columns
  error_if(anyNA(mdt$SUBWATERSHED) || anyNA(mdt$FLOWS_TO),
           paste0("Missing Values in Subwatershed Assignments\n\n",
                  "While assigning a DWRAT subwatershed to each water right, ",
                  "assignment failed in at least one instance. This column ",
                  "was incorporated by linking LSPC catchment information, but ",
                  "something went wrong along the way. Please investigate."))
  
  
  # Now proceed with the final adjustments
  
  
  # First, rename the catchments column ("BASIN") to "CATCHMENT"
  # Then, rename "SUBWATERSHED" to "BASIN"
  mdt <- mdt |>
    rename(CATCHMENT = BASIN,
           BASIN = SUBWATERSHED)
  
  
  # Finally, to prevent issues with type parsing when reading the tables into DWRAT,
  # add strings to the values in "BASIN" and "FLOWS_TO" to ensure that they are
  # parsed as strings
  mdt <- mdt |>
    mutate(BASIN = ensure_correct_typing(BASIN),
           FLOWS_TO = ensure_correct_typing(FLOWS_TO))
  
  
  # Return 'mdt' after these edits
  return(mdt)
  
}



ensure_correct_typing <- function (col) {
  
  # Given a vector of values ('col'), add characters to each element
  
  return(paste0("SUBBASIN_", col))
  
  # This ensures that, when R or Python functions automatically parse the content
  # of this column, it will be interpreted as a character string column
  
}



format_supply <- function (stream, lastCatch, flowsTo) {
  
  # 'stream' is the raw output from LSPC
  
  # DWRAT requires the flow generated in each subwatershed (in acre-feet per month)
  
  # The most similar parameter in 'stream' is "RO", which gives the cumulative
  # flow at each catchment outlet (in cubic feet per month)
  
  # This function will reformat and decumulate the streamflow values
  
  
  # First, focus only on the "RO" parameter in 'stream'
  RO <- stream |>
    filter(parmname == "RO")
  
  
  # Make sure 'date' is interpreted as a Date variable
  RO <- RO |>
    mutate(date = as.Date(date, format = "%m/%d/%Y"))
  
  
  # After that, convert the units of 'value1'
  # This variable has units of cubic feet per month (ft^3/month)
  # Change that into acre-feet per month (AF/month)
  
  # For that, only the volume portion of the unit must be converted
  # There are 43,559.9 ft^3 in one AF
  RO <- RO |>
    mutate(value1 = value1 / 43559.9)
  
  
  # Next, not all catchments' values are necessary (if this is a HUC-12 based run)
  # To remove extraneous values, filter 'RO' based on 'lastCatch'
  # Only keep the final catchment's value for each subwatershed
  RO <- RO |>
    filter(rchid %in% lastCatch$OUTLET_CATCH)
  
  
  # For the next steps, rename "rchid" into "OUTLET_CATCH" and 
  # join 'lastCatch' to 'RO'
  RO <- RO |>
    mutate(rchid = as.character(rchid)) |>
    rename(OUTLET_CATCH = rchid) |>
    left_join(lastCatch, by = "OUTLET_CATCH", relationship = "many-to-one")
  
  # `mutate` is used to ensure "rchid" is a character column to match 'lastCatch'
  
  
  # Now each outlet value is associated with a DWRAT subwatershed
  
  # 'RO' will be reformatted next
  # Make it a wider tibble, where each subwatershed has its own column
  
  pivotRO <- RO |>
    arrange(date, SUBWATERSHED) |>
    pivot_wider(id_cols = date, names_from = SUBWATERSHED, values_from = value1)
  
  # Each row corresponds to a different date
  # Meanwhile, each subwatershed has its own column
  # The values in these columns are the monthly streamflow volumes (in AF)
  
  
  # Adjust the date values in 'pivotRO'
  # They are set to the end of the month, but make them the beginning instead
  pivotRO <- pivotRO |>
    mutate(date = floor_date(date, unit = "month")) |>
    rename(Date = date)
  
  # (rename this column to "Date" too)
  
  
  # 'pivotRO' almost has the required formatting
  
  # The only issue left is the cumulative nature of the flows
  
  # "RO" represents the cumulative flow in a subwatershed, inclusive of upstream
  # subwatersheds' contributions
  
  # For DWRAT, we specifically need the flow *generated* within a subwatershed
  
  # We will have to decumulate flows to accomplish that
  pivotRO <- pivotRO |>
    decumulate_flows(flowsTo)
  
  
  # After these updates, return 'pivotRO'
  return(pivotRO)
  
}



decumulate_flows <- function (pivotRO, flowsTo) {
  
  # 'pivotRO' contains monthly values in each row
  # Each column is a different subwatershed
  
  # Use 'flowsTo' to adjust these values and remove cumulative flow
  # (i.e., flow contributions from upstream subwatersheds)
  
  
  # Since each flow volume is cumulative, to decumulate the flow of a given 
  # subwatershed, simply subtract its immediately upstream subwatershed's
  # value from the target subwatershed's flow value
  
  
  # Before starting, create a copy of 'pivotRO' 
  # This one will have its flows adjusted
  adjustedRO <- pivotRO
  
  # (Since each subwatershed flow plays a role in its immediately downstream
  #  subwatershed's calculation, the original cumulative values must be preserved)
  
  
  # Iterate through the non-date columns of 'adjustedRO'
  for (j in 2:ncol(adjustedRO)) {
    
    # Get the name of the current subwatershed
    iterBasin <- names(adjustedRO)[j]
    
    
    # Identify which subwatersheds flow into 'iterBasin'
    immediateUpstream <- flowsTo$SUBWATERSHED[flowsTo$FLOWS_TO == iterBasin]
    
    
    # If 'immediateUpstream' is empty, there is no upstream sub-basin
    # Leave the values for this sub-basin as-is in 'adjustedRO'
    if (length(immediateUpstream) == 0) {
      
      next
      
      # If there is exactly one basin upstream of 'iterBasin',
      # take the difference between the two subwatersheds' outlet "RO" values
    } else if (length(immediateUpstream) == 1) {
      
      adjustedRO[, j] <- pivotRO[, j] - pivotRO[, names(pivotRO) == immediateUpstream]
      
      # Use 'pivotRO' for these calculations because its values are all cumulative
      # (In 'adjustedRO', the upstream basins' flows may have already been decumulated)
      
    } else {
      
      # If there are multiple immediately upstream subwatersheds, 
      # subtract all of their outlet values from the "RO" values for 'iterBasin'
      tempCalcs <- pivotRO[, j]
      
      
      for (k in 1:length(immediateUpstream)) {
        
        tempCalcs <- tempCalcs - pivotRO[, names(pivotRO) == immediateUpstream[k]]
        
      }
      
      
      # Replace the values for 'iterBasin' with 'tempCalcs'
      adjustedRO[, j] <- tempCalcs
      
    }
    
  } # End of 'j' loop through subwatersheds in 'adjustedRO'
  
  
  # The final check is for negative flows
  
  # Since subtraction is performed on the subwatershed flows, 
  # this can happen in some instances
  
  # If there are any negative values, another procedure must be applied
  if (any(adjustedRO[, -1] < 0)) {
    
    # Negative flows must be removed from 'adjustedRO'
    
    # However, to preserve the mass balance of the flow volumes, 
    # other values must be reduced to account for the negative values
    adjustedRO <- adjustedRO |>
      adjust_negative_flows(flowsTo)
    
  }
  
  
  # Return 'adjustedRO' after these updates
  return(adjustedRO)
  
}



adjust_negative_flows <- function (adjustedRO, flowsTo) {
  
  # Check each month-year pair in 'adjustedRO'
  
  # If there are any negative values for a subwatershed, zero it out
  
  # To preserve the balance of mass, however, "borrow" flows from 
  # upstream basins to account for these negative values
  
  
  # Iterate through every month
  for (i in 1:nrow(adjustedRO)) {
    
    # Skip the row if no values are negative
    if (!any(adjustedRO[i, -1] < 0)) {
      next
    }
    
    
    # Set up an infinite while loop while negative flow values are detected
    while (any(adjustedRO[i, -1] < 0)) {
      
      # Identify the locations of negative flow values
      colIndex <- which(names(adjustedRO) != "Date" & adjustedRO[i, ] < 0)
      
      
      # Get the names of these subwatersheds
      basinName <- names(adjustedRO)[colIndex]
      
      
      # Iterate through each of these basins and try to fix their negative flows
      for (j in 1:length(basinName)) {
        
        # Identify which subwatersheds are upstream of this iteration's basin
        immediateUpstream <- flowsTo$SUBWATERSHED[flowsTo$FLOWS_TO == basinName[j]]
        
        
        # If no subwatersheds flow into 'basinName', just zero out its negative flow
        if (length(immediateUpstream) == 0) {
          
          adjustedRO[i, colIndex[j]] <- 0
          
          # Or, if there is one subwatershed that is upstream of 'basinName'
        } else if (length(immediateUpstream) == 1) {
          
          # Add the negative flow in 'basinName' to this basin's flow value
          # This effectively "borrows" some of the upstream flow
          # Then, the value for 'basinName' is zeroed out
          adjustedRO[i, names(adjustedRO) == immediateUpstream] <- 
            adjustedRO[i, names(adjustedRO) == immediateUpstream] +
            adjustedRO[i, colIndex[j]]
          
          adjustedRO[i, colIndex[j]] <- 0
          
          
          # If the upstream flow value becomes negative because of this change,
          # that is no problem
          
          # The while loop will just have another iteration because a negative
          # flow value would still be present in the row
          
          
          # In the final scenario, if there are multiple upstream subwatersheds, 
          # more efforts will be required
        } else {
          
          # In this case, borrow flow from every immediately upstream subwatershed
          
          # The amount of flow to borrow from each basin will be based on
          # the total amount of flow available in their upstream paths
          # (That means considering all upstream subwatersheds in each flow path)
          
          # Calculate the total available 
          totalAvailability <- immediateUpstream |>
            map_dbl(~ calc_available_upstream_flow(flowsTo, adjustedRO[i, ], .))
          
          # Note: The original RO values are cumulative and could be used here
          #       instead, but if there are multiple iterations for a given date,
          #       this approach is better. This is because it reflects any 
          #       adjustments done from flow "borrowing". Using the original 
          #       cumulative flows in every case would not reflect the 
          #       completed adjustments for this row, 'i' 
          
          
          # If all upstream flow paths have zero flow available, just zero out 
          # the negative value for 'basinName'
          if (all(totalAvailability == 0)) {
            
            adjustedRO[i, colIndex[j]] <- 0
            
          } else {
            
            # Otherwise, calculate weights based on 'totalAvailability'
            contributionRatios <- totalAvailability / sum(totalAvailability)
            
            
            # From each of the subwatersheds that are upstream of 'basinNum',
            # borrow flow based on their corresponding ratios
            for (k in 1:length(immediateUpstream)) {
              
              # Locate one of the immediately upstream basins in 'adjustedRO'
              upstreamIndex <- which(names(adjustedRO) == immediateUpstream[k])
              
              
              # Double-check that the index is valid
              error_if(length(upstreamIndex) == 0,
                       paste0("Subwatershed ID Not Detected\n\n",
                              "For ", adjustedRO$Date[i], ", the script attempted ",
                              "to extract data related to Subwatershed \"", 
                              immediateUpstream[k], "\". However, the procedure ",
                              "failed to find a matching column in the flow dataset.\n\n",
                              "This could be an error in the flow data or in the ",
                              "basin connectivity table. Please investigate."))
              
              
              error_if(length(upstreamIndex) > 1,
                       paste0("Subwatershed ID Not Present More Than Once\n\n",
                              "For ", adjustedRO$Date[i], ", the script attempted ",
                              "to extract data related to Subwatershed \"", 
                              immediateUpstream[k], "\". However, the procedure ",
                              "encountered multiple columns with this name in ",
                              "the flow dataset.\n\n",
                              "This is likely a script error. Please investigate."))
              
              
              # Adjust the upstream basin and "borrow" some flow to offset
              # the negative value in this iteration's subwatershed
              adjustedRO[i, upstreamIndex] <- 
                adjustedRO[i, upstreamIndex] + 
                contributionRatios[k] * adjustedRO[i, colIndex[j]]
              
              
              # The contribution ratios consider the flow available in the 
              # entire upstream path for each immediately upstream sub-basin
              
              # So, it is possible that the immediately upstream sub-basin will
              # get a negative flow value after this operation
              
              # That's okay though, because the "while" loop will keep going
              # as long as there are any negative flows
              
            } # End of 'k' loop through immediate upstream subwatersheds
            
            
            # Zero out the negative sub-basin flow too
            adjustedRO[i, colIndex[j]] <- 0
            
            
          } # End of conditional for 'totalAvailability'
          
        } # End of conditional for multiple immediate upstream sub-basins
        
      } # End of 'j' loop through sub-basins with negative flows
      
    } # End of while loop for detecting negative flows for a certain month-year
    
  } # End of 'i' loop through dates in 'adjustedRO'
  
  
  # Return 'adjustedRO' at the end of this process
  return(adjustedRO)
  
}



calc_available_upstream_flow <- function (flowsTo, flowRow, startingBasin) {
  
  # Given 'flowRow', which contains flow values for each subwatershed,
  # and 'flowsTo', which expresses the connectivity between subwatersheds,
  # determine the total available flow from 'startingBasin' and ALL of 
  # its upstream basins
  
  
  # Define 'upstreamVec' to hold all upstream subwatersheds
  # (starting with the one defined in 'startingBasin')
  upstreamVec <- startingBasin
  
  
  # This variable will contain the total available upstream flow
  # Start with the sum of the flow available in 'startingBasin'
  totalFlow <- flowRow[1, which(names(flowRow) == startingBasin)] |>
    sum()
  
  
  # While 'upstreamVec' still contains basins
  while (length(upstreamVec) > 0) {
    
    # Identify which subwatersheds are upstream of the one(s) that appear 
    # currently in 'upstreamVec'
    upstreamVec <- flowsTo$SUBWATERSHED[which(flowsTo$FLOWS_TO %in% upstreamVec)]
    
    
    # Add to 'totalFlow' the flows of each of the sub-basins in 'upstreamVec'
    # (If 'upstreamVec' is empty, it will contribute "0" to 'totalFlow')
    totalFlow <- totalFlow +
      sum(flowRow[1, which(names(flowRow) %in% upstreamVec)])
    
  }
  
  
  # If 'totalFlow' is negative, return 0 as the total available upstream flow
  if (totalFlow < 0) {
    return(0)
  }
  
  
  # Otherwise, return 'totalFlow' if it's a positive number
  return(totalFlow)
  
}



format_basins <- function (flowsTo) {
  
  # Adjust the "immediate downstream" table for DWRAT
  
  
  # First, sort the rows and rename "SUBWATERSHED" to "BASIN"
  flowsTo <- flowsTo |>
    arrange(SUBWATERSHED) |> 
    rename(BASIN = SUBWATERSHED)
  
  
  # Then, to ensure that all values are read in as character strings,
  # use the same adjustment as done for the demand data and 'mdt'
  flowsTo <- flowsTo |>
    mutate(BASIN = ensure_correct_typing(BASIN),
           FLOWS_TO = ensure_correct_typing(FLOWS_TO))
  
  
  # Return this modified 'flowsTo'
  return(flowsTo)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
