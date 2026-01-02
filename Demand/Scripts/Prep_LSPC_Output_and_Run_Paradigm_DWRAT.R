# After the LSPC model has been run successfully, 
# this script can prepare that data for the Paradigm DWRAT 
# and then execute DWRAT



#### Setup ####


remove(list = ls())



require(tidyverse)
require(sf)
require(readxl)



cat("Starting 'Prep_LSPC_Output_and_Run_Paradigm_DWRAT.R'...\n\n")



#### Functions ####


mainProcedure <- function (hucBased = TRUE) {
  
  # The DWRAT run can be either based on HUC-12 sub-basins or catchments
  # If HUC-12 sub-basins are used, sometimes the sub-basin may be split
  # into smaller units if there are multiple outlets in that sub-basin
  
  
  
  startTime <- Sys.time()
  
  
  
  # Select a watershed
  source("Scripts/Watershed_Selection.R")
  
  
  
  # Output a message about the script run (HUC-12 or catchment-based)
  if (hucBased) {
    
    cat("\nRunning script with DWRAT sub-basins based on HUC-12 sub-basins!\n\n\n")
    
  } else {
    
    cat("\nRunning script with DWRAT sub-basins based on catchments!\n\n\n")
    
  }
  
  
  
  # Clear the "OutputData" folder of model files from previous script runs
  deleteOldOutputs(ws$ID)
  
  
  
  # Read in several important files
  # Start with the watershed boundaries
  wsBound <- getGIS(ws = ws, 
                    GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                    GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                    GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") %>%
    st_transform("epsg:3488")
  
  
  
  # Also get the watershed's sub-basins
  subWS <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                  GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME") %>%
    st_transform(st_crs(wsBound)) %>%
    rename(BASIN = ws$SUBBASIN_FIELD_ID_NAMES[1] %>% str_split(";") %>% unlist() %>% trimws() %>% head(1))
  
  
  
  # Get the Master Demand Table
  mdtDF <- makeSharePointPath(ws$MASTER_DEMAND_TABLE_CSV_PATH) %>%
    read_csv(show_col_types = FALSE)
  
  
  
  # Use the HUC-12 boundaries for modeling in DWRAT
  huc12 <- makeSharePointPath("Program Watersheds/1. Watershed Folders/Navarro River/Data/GIS Datasets/WBD_18_HU2_Shape/Shape/") %>%
    st_read(layer = "WBDHU12") %>%
    st_transform(st_crs(wsBound)) %>%
    select(huc12, name)
  
  
  
  # Shrink 'wsBound' by 1 meter and find relevant boundaries from 'huc12'
  huc12 <- huc12[lengths(st_intersects(huc12, st_buffer(wsBound, dist = -1))) > 0, ]
  
  
  
  stopifnot(nrow(huc12) == length(unique(huc12$huc12)))
  
  
  
  # Finally, read in the full catchment connectivity matrix
  connMat <- getXLSX(ws,
                     "IS_SHAREPOINT_PATH_CONNECTIVITY_MATRIX_SPREADSHEET",
                     "CONNECTIVITY_MATRIX_SPREADSHEET_PATH",
                     "CONNECTIVITY_MATRIX_WORKSHEET_NAME")
  
  
  
  # The first column will be referred to as "BASIN"
  names(connMat)[1] <- "BASIN"
  
  
  
  # Get the LSPC hydrology output as well
  RO <- makeSharePointPath(ws$LSPC_STREAM_OUTPUT_CSV_PATH) %>%
    read_csv(show_col_types = FALSE) %>%
    filter(parmname == "RO") %>%
    mutate(date = as.Date(date, "%m/%d/%Y"),
           value1_AF = value1 / 43559.9) # The CFS values are already using a monthly timestep
                                         # The only necessary conversion is ft^3 to AF
  
  
  
  # Now that the variables are all loaded in, first append HUC-12 information 
  # to the catchment and connectivity matrix variables
  
  
  
  # Assign a HUC-12 sub-basin to each catchment
  subWS <- assignHUC12toCatchment(subWS, huc12)
  
  
  
  # Assign a HUC-12 sub-basin to each catchment in the connectivity matrix
  # Use the assignments in 'subWS' to update 'connMat' 
  connMat <- connMat %>%
    left_join(subWS %>% st_drop_geometry() %>% 
                select(BASIN, huc12) %>% unique(),
              by = "BASIN", relationship = "one-to-one")
  
  
  
  # Append HUC-12 labels to 'RO'
  # Also rename "rchid" to "BASIN" (this column identifies the catchments)
  RO <- RO %>%
    rename(BASIN = rchid) %>%
    left_join(subWS %>% st_drop_geometry() %>% 
                select(BASIN, huc12) %>% unique(),
              by = "BASIN", relationship = "many-to-one")
  
  
  
  # After that, create a variable called 'lastCatch'
  # This identifies the outlet for each modeled sub-basin
  # (HUC-12 or catchment)
  if (hucBased) {
    
    lastCatch <- getSubbasinOutlets(huc12, connMat, subWS)
    
    
  } else {
    
    # For a catchment-only run, the outlet for each catchment is itself
    lastCatch <- subWS %>%
      st_drop_geometry() %>%
      select(BASIN) %>%
      mutate(DWRAT_SUBBASIN = BASIN)
    
  }
  
  
  
  # Using the catchment outlet assignments, create a new connectivity matrix
  # Each row and column will correspond to one DWRAT sub-basin
  # (For a catchment-only run, 'dwratConn' equals 'connMat')
  if (hucBased) {
    
    dwratConn <- getSubbasinConnectivity(connMat %>% select(-huc12), lastCatch)
    
  } else {
    
    dwratConn <- connMat %>% select(-huc12)
    
  }
  
  
  
  # Use 'dwratConn' to determine the immediate downstream sub-basin for each sub-basin
  flowsTo <- getDownstreamSubbasins(dwratConn, hucBased)
  
  
  
  # The next step is to prepare files that will be input into the Paradigm DWRAT model
  
  # "formatted_demand.csv"
  # "formatted_supply.csv"
  # "generated_basins.csv"
  
  
  
  # Prepare the demand dataset in another function
  mdtDF <- mdtDF %>%
    prepDemandData(lastCatch, connMat, subWS, hucBased, flowsTo)
  
  
  
  # Modify 'RO' next
  # Get the flow data at the end of each DWRAT sub-basin ("BASIN" in 'lastCatch')
  # Then, append "DWRAT_SUBBASIN" to 'RO' and simplify the dataset
  RO <- RO %>%
    filter(BASIN %in% lastCatch$BASIN) %>%
    left_join(lastCatch, by = "BASIN", relationship = "many-to-one") %>%
    select(date, value1_AF, DWRAT_SUBBASIN) %>%
    rename(BASIN = DWRAT_SUBBASIN, Date = date) %>%
    arrange(Date, BASIN) #%>%
  # mutate(BASIN = paste0("HUC_", BASIN))
  
  
  
  # Transform 'RO' so that each "Date" value marks a unique row and
  # each DWRAT sub-basin is its own column 
  # ("value1_AF" provides the values in this new table)
  # (Also make sure each date is in the "month/day/year" format, 
  #  with the day set to the first of the month)
  roSupply <- pivot_wider(RO, id_cols = Date, names_from = BASIN, values_from = value1_AF) %>%
    mutate(Date = format(Date, "%m/%d/%Y")) %>%
    mutate(Date = str_replace(Date, "/[0-9]{2}/", "/01/"))
  
  
  
  # The final input file lists the various sub-basins and their connectivity
  # (In a simplified one-to-one format)
  # (As with 'mdtDF', add "SUBBASIN_" to the sub-basin labels 
  #  to prevent issues with having numeric IDs as column names)
  flowsTo <- flowsTo %>%
    rename(BASIN = DWRAT_SUBBASIN) %>%
    arrange(BASIN) %>%
    mutate(BASIN = paste0("SUBBASIN_", BASIN)) %>%
    mutate(FLOWS_TO = paste0("SUBBASIN_", FLOWS_TO))
  
  
  
  # The variables are ready to write to a file
  # However, there is one more problem
  # The script can only handle one outlet at a time
  # If there are multiple outlets, DWRAT will need to be run separately for each grouping
  
  
  
  # In a separate function, produce output files that will be input into DWRAT 
  # Then, try to run the model as well
  # (And store their outputs into the "OutputData" folder too)
  outputAndRun(mdtDF, roSupply, flowsTo, ws, lastCatch)
  
  
  
  # Output a completion message
  cat("\n\nThe script has finished running!\n\n")
  
  
  
  # Output script run duration
  cat(paste0("The total script runtime was ", 
             round(difftime(Sys.time(), startTime, units = "secs"), digits = 2),
             " seconds\n\n"))
  
  
  
  # Return nothing
  return()
  
}



deleteOldOutputs <- function (wsID) {
  
  # Clear out script-related output files from the "OutputData" folder
  # Previous runs of this script may have introduced those files
  # If not removed, they can interfere with the file combination process at the end
  
  
  
  # Regex strings for DWRAT input and output files produced by this script
  # These will be used to located files in the "OutputData" folder and delete them
  removalStrings <- c("_formatted_demand",
                      "_formatted_supply",
                      "_generated_basins",
                      "_basin_appropriative_output",
                      "_basin_riparian_output",
                      "_user_appropriative_output",
                      "_user_riparian_output") %>%
    paste0("^", wsID, .)
  
  
  
  # Delete the input and output files
  removalStrings %>%
    map(~ list.files("OutputData", pattern = ., full.names = TRUE) %>% unlink())
  
  
  
  # Make sure the files were successfully deleted
  # Otherwise, output an error message
  if (TRUE %in% map_lgl(removalStrings, 
                        ~ length(list.files("OutputData", pattern = ., full.names = TRUE)) > 1)) {
    
    cat(paste0("\n\n\nPlease manually delete these files:\n\n",
               map(removalStrings, 
                   ~ list.files("OutputData", pattern = ., full.names = TRUE)) %>%
                 unlist() %>% paste0(collapse = "\n"),
               "\n\n"))
    
    stop(paste0("Could not delete all of the generated files from the previous run ",
                "of this script. This is a necessary step to avoid accidental errors.\n\n",
                "Please manually delete the files listed above.\n"))
    
  }
  
  
  
  # If there are no errors, return nothing
  return()
  
}



assignHUC12toCatchment <- function (subWS, huc12) {
  
  # Assign a HUC-12 sub-basin to each catchment
  # A new column will be added to 'subWS'
  
  
  
  # Get the centroids of the catchments and find an overlapping sub-basin
  # (Because of the varying catchment sizes, shrinking the boundaries does not work well)
  intersections <- st_intersects(st_centroid(subWS), huc12)
  
  
  
  # Only one HUC-12 sub-basin should overlap with each catchment
  stopifnot(max(lengths(intersections)) == 1)
  stopifnot(min(lengths(intersections)) == 1)
  
  
  
  # Create a new column titled "huc12"
  subWS <- subWS %>%
    mutate(huc12 = huc12$huc12[unlist(intersections)])
  
  
  
  # Return 'subWS'
  return(subWS)
  
}



getSubbasinOutlets <- function (huc12, connMat, subWS) {
  
  # For each HUC-12 sub-basin, find the outlet catchment
  # If a sub-basin has multiple outlets, 'lastCatch' will have the split sub-basins
  
  
  
  # Initialize 'lastCatch' with just the HUC-12 sub-basins first
  # "DWRAT_SUBBASIN" is the basin that will be modeled in DWRAT
  # "BASIN" is the outlet catchment 
  lastCatch <- huc12 %>%
    st_drop_geometry() %>%
    select(huc12, name) %>%
    mutate(BASIN = NA_real_)
  
  
  
  # Get the outlet catchment for each HUC-12 subbasin
  # (It should be the catchment that all other catchments in the subbasin drain to)
  # (For a watershed with coastal catchments that do not drain through the main outlet,
  #  this may not be the case)
  for (i in 1:nrow(lastCatch)) {
    
    # Get a subset of 'connMat' for just the current HUC-12 sub-basin
    subConn <- connMat %>%
      filter(huc12 == lastCatch$huc12[i]) %>%
      select(-huc12)
    
    
    
    subConn <- subConn %>%
      select(BASIN, all_of(as.character(subConn$BASIN)))
    
    
    
    # If a catchment is the sole outlet, every catchment will have a "1" in its 
    # connectivity matrix column 
    # Therefore, the column sum would be equal to the number of catchments 
    # in the sub-basin
    maxIndex <- which(colSums(subConn[, -1]) == nrow(subConn))
    
    
    
    # No single clear outlet --> Use sub-divisions of the HUC-12 sub-basin (but later)
    if (length(maxIndex) == 0) {
      
      cat(paste0("\n\n\n", huc12$huc12[i], " (", huc12$name[i], ") does not have ",
                 "a single clear outlet catchment!\n"))
      
      
      
      # For now, assign -999 to the "BASIN" column of 'lastCatch'
      lastCatch$BASIN[i] <- -999
      
      next
      
    }
    
    
    
    # For this part of the loop, there should be only one outlet here
    stopifnot(length(maxIndex) == 1)
    
    
    
    # If that is the case, get the catchment ID and assign it to the "BASIN" column
    lastCatch$BASIN[i] <- as.numeric(names(subConn)[maxIndex + 1])
    
  }
  
  
  
  # If there are HUC-12 sub-basins with multiple outlets, 
  # split them into distinct groups
  if (-999 %in% lastCatch$BASIN) {
    
    
    # Keep looping while "-999" is in 'lastCatch'
    while (-999 %in% lastCatch$BASIN) {
      
      # Get the first HUC-12 subbasin with no final catchment identified
      iterHUC <- lastCatch$huc12[lastCatch$BASIN == -999][1]
      
      
      
      # Filter the connectivity matrix to that HUC-12
      subConn <- connMat %>%
        filter(huc12 == iterHUC)
      
      
      
      subConn <- subConn %>%
        select(BASIN, all_of(as.character(subConn$BASIN)))
      
      
      
      # Get the number of connections for each catchment (i.e., the column sums)
      numConnections <- colSums(subConn[, -1])
      
      
      
      # Get a temporary vector of all the catchments in the HUC-12
      # This vector will be reduced in size as flow paths are noted
      catchVec <- subConn$BASIN
      
      
      
      # A counter to distinguish sub-divisions of the HUC-12 sub-basin
      iterCount <- 1
      
      
      
      # While there are unaccounted for catchments among the sub-divisions
      while (length(catchVec) > 0) {
        
        # Identify flow paths in the HUC-12 starting from the ones with the largest number of connections
        maxBasin <- names(numConnections)[numConnections == max(numConnections)][1] %>%
          as.numeric()
        
        
        
        stopifnot(length(maxBasin) == 1)
        
        
        
        # Get the catchments in this flow path
        iterPath <- subConn %>%
          select(BASIN, !! as.symbol(maxBasin)) %>%
          filter(!! as.symbol(maxBasin) == 1) %>%
          select(BASIN) %>% 
          unlist(use.names = FALSE)
        
        
        
        # Make sure that all of these catchments are in 'catchVec'
        # (Don't allow double-counting of any catchments in multiple flow paths)
        stopifnot(sum(iterPath %in% catchVec) == length(iterPath))
        
        
        
        # Add an entry to 'lastCatch' for this path
        lastCatch <- lastCatch %>%
          bind_rows(data.frame(huc12 = paste0(iterHUC, "_", iterCount, "_", maxBasin),
                               name = paste0(sort(iterPath), collapse = "_"),
                               BASIN = maxBasin))
        
        
        
        # Update the HUC-12 assignments in 'subWS' based on this flow path
        subWS$huc12[subWS$BASIN %in% iterPath] <- paste0(iterHUC, "_", iterCount)
        
        
        
        # Remove the catchments in 'iterPath' from 'catchVec' and 'numConnections'
        catchVec <- catchVec[!(catchVec %in% iterPath)]
        
        
        
        numConnections <- numConnections[!(names(numConnections) %in% iterPath)]
        
        
        
        # Increment 'iterCount'
        iterCount <- iterCount + 1
        
      }
      
      
      
      # Remove the "-999" entry from 'lastCatch' for this HUC-12 subbasin
      lastCatch <- lastCatch %>%
        filter(huc12 != iterHUC)
      
    }
    
  }
  
  
  
  # Return 'lastCatch'
  # For consistency with the alternate procedure, rename 'huc12' to "DWRAT_SUBBASIN"
  return(lastCatch %>%
           rename(DWRAT_SUBBASIN = huc12))
  
}



getSubbasinConnectivity <- function (connMat, lastCatch) {
  
  # Create a new connectivity matrix
  # 'connMat' is for all watershed catchments
  # This matrix is for the sub-basins used in the DWRAT run 
  
  
  
  # Create a connectivity matrix with just the catchments in 'lastCatch'
  dwratConn <- connMat %>%
    filter(BASIN %in% lastCatch$BASIN) %>%
    select(BASIN, all_of(as.character(lastCatch$BASIN)))
  
  
  
  # Replace the catchment IDs with HUC-12 IDs (the names in "DWRAT_SUBBASIN")
  dwratConn$BASIN <- dwratConn$BASIN %>%
    map_chr(~ lastCatch$DWRAT_SUBBASIN[which(lastCatch$BASIN == .)])
  
  
  colnames(dwratConn)[-1] <- colnames(dwratConn)[-1] %>%
    map_chr(~ lastCatch$DWRAT_SUBBASIN[which(lastCatch$BASIN == .)])
  
  
  
  # Return 'dwratConn'
  return(dwratConn %>%
           arrange(BASIN) %>%
           select(BASIN, sort(names(dwratConn)[-1])))
  
}



getDownstreamSubbasins <- function (dwratConn, hucBased) {
  
  # For each DWRAT sub-basin, identify the DWRAT sub-basin that is immediately downstream of 
  
  
  
  # Create a "FLOWS_TO" variable that indicates the immediate downstream sub-basin/catchment
  flowsTo <- dwratConn %>%
    select(BASIN) %>%
    arrange(BASIN) %>%
    rename(DWRAT_SUBBASIN = BASIN) %>%
    mutate(FLOWS_TO = NA_character_)
  
  
  
  # Use 'dwratConn' to make this determination
  for (i in 1:nrow(flowsTo)) {
    
    # Filter 'dwratConn' to the sub-basin
    iterRow <- dwratConn %>%
      filter(BASIN == flowsTo$DWRAT_SUBBASIN[i]) %>%
      select(-as.character(flowsTo$DWRAT_SUBBASIN[i]))
    
    
    
    # Identify which sub-basins' columns have a value of "1" in this row
    nonZeroBasins <- names(which(colSums(iterRow[, -1]) == 1))
    
    
    
    # The most downstream basins will not connect to anything
    # Skip the rest of this procedure if that is the case 
    if (length(nonZeroBasins) == 0) {
      next
    }
    
    
    
    # For the sub-basins downstream of this iteration's sub-basin,
    # get the number of connections for each sub-basin (column sums)
    # Sub-basins that are further downstream have a larger number
    # (since more sub-basins eventually drain into that sub-basin)
    downstreamSums <- colSums(dwratConn[, names(dwratConn) %in% nonZeroBasins])
    
    
    
    # The most immediate downstream sub-basin would have the minimum column sum
    immediateDownstream <- names(downstreamSums)[which(downstreamSums == min(downstreamSums))]
    
    
    
    # There should be exactly one match
    stopifnot(length(immediateDownstream) == 1)
    
    
    
    # Save that sub-basin ID to the "FLOWS_TO" column
    flowsTo$FLOWS_TO[i] <- immediateDownstream
    
  }
  
  
  
  # Only the most downstream basin(s) should have "NA" in "FLOWS_TO"
  # There are two possible cases:
  # (1) Only one sub-basin has no downstream basin (normal case for watersheds with one outlet)
  # (2) If a watershed has multiple outlets (coastal catchments)
  stopifnot(sum(is.na(flowsTo$FLOWS_TO)) > 0)
  
  
  
  # Replace those entries with 000
  flowsTo$FLOWS_TO[is.na(flowsTo$FLOWS_TO)] <- "000"
  
  
  
  # Return 'flowsTo'
  return(flowsTo)
  
}



prepDemandData <- function (mdtDF, lastCatch, connMat, subWS, hucBased, flowsTo) {
  
  # Add columns to the Master Demand Table
  # These new columns link water rights to HUC-12 boundaries and the DWRAT sub-basins
  
  
  
  # Use 'subWS' to add HUC-12 data to 'mdtDF'
  # Also add a new column to 'mdtDF' that identifies the last catchment in its flow path
  # (This will connect 'mdtDF' to 'lastCatch', which in turn connects to "DWRAT_SUBBASIN")
  mdtDF <- mdtDF %>%
    left_join(subWS %>% st_drop_geometry() %>% select(BASIN, huc12),
              by = "BASIN", relationship = "many-to-one") %>%
    mutate(LAST_CATCH = NA_character_)
  
  
  
  # The procedure for the HUC-based approach is slightly more complicated
  # A loop and the HUC-12 ID will have to be used to find proper matches
  if (hucBased) {
    
    # Sub-basin information must be appended to 'mdtDF'
    # Use 'lastCatch' and 'connMat' to fill in this information
    for (i in 1:nrow(lastCatch)) {
      
      # For this iteration's catchment, 
      # identify the catchments in 'connMat' who have "1" for its column
      subbasinList <- connMat %>%
        filter(huc12 == str_remove(lastCatch$DWRAT_SUBBASIN[i], "_.+$")) %>%
        filter(!! as.symbol(lastCatch$BASIN[i]) == 1) %>%
        select(BASIN) %>% unlist(use.names = FALSE)
      
      
      
      # In 'mdtDF', assign "LAST_CATCH" based on this connection
      # (Confirm that none of the rows that are going to be filled in were already modified)
      stopifnot(sum(is.na(mdtDF$LAST_CATCH[mdtDF$BASIN %in% subbasinList])) ==
                  length(mdtDF$LAST_CATCH[mdtDF$BASIN %in% subbasinList]))
      
      
      
      mdtDF$LAST_CATCH[mdtDF$BASIN %in% subbasinList] <- lastCatch$BASIN[i]
      
      
    }
    
  } else {
    
    # In the catchment procedure, "LAST_CATCH" is the catchment itself
    mdtDF$LAST_CATCH <- mdtDF$BASIN
    
  }
  
  
  
  # Make sure every record has "LAST_CATCH" filled in
  stopifnot(!anyNA(mdtDF$LAST_CATCH))
  
  
  
  # Use "LAST_CATCH" to join "DWRAT_SUBBASIN" to 'mdtDF'
  # Then, add "FLOWS_TO" from 'flowsTo' as well
  # (Treat "LAST_CATCH" as a character column in both cases to ensure consistency)
  mdtDF <- mdtDF %>%
    mutate(LAST_CATCH = as.character(LAST_CATCH)) %>%
    left_join(lastCatch %>%
                select(DWRAT_SUBBASIN, BASIN) %>%
                rename(LAST_CATCH = BASIN) %>%
                mutate(LAST_CATCH = as.character(LAST_CATCH)),
              by = "LAST_CATCH", relationship = "many-to-one") %>%
    left_join(flowsTo, by = "DWRAT_SUBBASIN", relationship = "many-to-one")
  
  
  
  # Make sure no data is missing
  stopifnot(!anyNA(mdtDF$DWRAT_SUBBASIN))
  stopifnot(!anyNA(mdtDF$FLOWS_TO))
  
  
  
  # Finally, rename "BASIN" to "CATCHMENT" (and "DWRAT_SUBBASIN" to "BASIN")
  # Append "SUBBASIN_" to the sub-basin IDs in "BASIN" and "FLOWS_TO"
  # (This will prevent the columns from being interpreted as numbers by mistake)
  mdtDF <- mdtDF %>%
    rename(CATCHMENT = BASIN) %>%
    rename(BASIN = DWRAT_SUBBASIN) %>%
    mutate(BASIN = paste0("SUBBASIN_", BASIN)) %>%
    mutate(FLOWS_TO = paste0("SUBBASIN_", FLOWS_TO))
  
  
  
  # Return 'mdtDF'
  return(mdtDF)
  
}



outputAndRun <- function (mdtDF, roSupply, flowsTo, ws, lastCatch) {
  
  # Produce the final output files
  # Then, attempt to run the model
  
  
  
  # First check if an Anaconda installation is present 
  # This R script can run DWRAT automatically if Anaconda Prompt is available
  useDWRAT <- checkForAnaconda()
  
  
  
  # After that, move on to preparing the output files
  
  
  
  # DWRAT requires there to be only one outlet
  # If there are multiple instances of "SUBBASIN_000",
  # that is a sign of multiple outlets
  if (sum(flowsTo$FLOWS_TO == "SUBBASIN_000") == 1) {
    
    # If there's only one outlet, write the files to the "OutputData" folder
    # without special modifications
    
    
    
    # There is one exception, however
    # DWRAT will fail if the watershed lacks at least one appropriative and one riparian user
    # Stop the script and alert the user if this occurs
    if (mdtDF %>% filter(!is.na(RIPARIAN) & RIPARIAN == "Y") %>%
        nrow() == 0 ||
        mdtDF %>% filter(!is.na(APPROPRIATIVE) & APPROPRIATIVE == "APPROPRIATIVE") %>%
        nrow() == 0) {
      
      stop(paste0("DWRAT will fail unless the watershed has at least one appropriative water right ",
                  "and at least one riparian water right. That should pretty much always be the case. ",
                  "There may be an error somewhere."))
      
    }
    
    
    
    # Define a vector with the planned filenames
    filePaths <- c(Demand = paste0("OutputData/", ws$ID, "_formatted_demand.csv"),
                   Supply = paste0("OutputData/", ws$ID, "_formatted_supply.csv"),
                   Basin = paste0("OutputData/", ws$ID, "_generated_basins.csv"))
    
    
    # Demand
    mdtDF %>%
      write_csv(filePaths[1])
    
    
    # Supply
    roSupply %>%
      write_csv(filePaths[2])
    
    
    # Basins
    flowsTo %>%
      write_csv(filePaths[3])
    
    
    
    if (useDWRAT) {
      
      # Try to run DWRAT as well
      runDWRAT(filePaths)
      
    }
    
    
    # If there are multiple outlets, create separate groupings of output files
  } else {
    
    
    # Identify each of the outlet catchments
    outletDF <- flowsTo %>%
      filter(FLOWS_TO == "SUBBASIN_000")
    
    
    
    # Construct a subset of 'flowsTo' for each outlet
    # Create a list to hold each of the different 'flowsTo' subsets
    flowsToSplitList <- splitFlowsTo(flowsTo, outletDF)
    
    
    
    # If there are no issues, start writing output files for each model run
    for (i in 1:length(flowsToSplitList)) {
      
      
      # If a flow path has no users, a DWRAT run is not necessary
      # Instead, generate the DWRAT outputs for this file path here
      if (mdtDF %>% filter(BASIN %in% flowsToSplitList[[i]]$BASIN) %>%
          nrow() == 0) {
        
        createModelBasinOutputs_NoUsers(flowsToSplitList, i, roSupply, ws)
        
        next
        
        
      # Similarly, if there's only one user in the flow path,
      # Running DWRAT is unnecessary
      } else if (mdtDF %>% filter(BASIN %in% flowsToSplitList[[i]]$BASIN) %>%
                 nrow() == 1) {
        
        createModelOutputs_OneUser(flowsToSplitList, i, 
                                   mdtDF %>% filter(BASIN %in% flowsToSplitList[[i]]$BASIN), 
                                   roSupply, ws)
        
        next
        
      } 
      
      
      
      # DWRAT requires there to be at least one appropriative user
      # and at least one riparian user
      
      # If the flow path lacks either of these water right types,
      # Add a dummy row to 'mdtDF'
      
      
      
      # Add a fake user row to 'mdtDF' if it lacks riparian rights
      if (mdtDF %>%
          filter(BASIN %in% flowsToSplitList[[i]]$BASIN) %>%
          filter(!is.na(RIPARIAN) & RIPARIAN == "Y") %>%
          nrow() == 0) {
        
        mdtDF <- mdtDF %>%
          addDummyRow(isRiparian = TRUE, flowsToSplitList[[i]], lastCatch)
        
      } 
      
      
      # Add a dummy user row to 'mdtDF' if it lacks appropriative rights
      if (mdtDF %>%
          filter(BASIN %in% flowsToSplitList[[i]]$BASIN) %>%
          filter(!is.na(APPROPRIATIVE) & APPROPRIATIVE == "APPROPRIATIVE") %>%
          nrow() == 0) {
        
        mdtDF <- mdtDF %>%
          addDummyRow(isRiparian = FALSE, flowsToSplitList[[i]], lastCatch)
        
      }
      
      
      
      # If there is only one sub-basin in the flow path, add a dummy sub-basin before it
      # There will be no flow contribution from that sub-basin, but it still needs to be there
      # (Otherwise, the DWRAT script curtails water rights for no reason)
      if (flowsToSplitList[[i]] %>% nrow() == 1) {
        
        # Add a new dummy sub-basin to 'flowsTo'
        flowsToSplitList[[i]] <- flowsToSplitList[[i]] %>%
          bind_rows(data.frame("BASIN" = "SUBBASIN_REMOVE_THIS",
                               "FLOWS_TO" = flowsToSplitList[[i]]$BASIN[1]))
        
        
        # Add a column of zeroes for this fake sub-basin to 'roSupply' too
        roSupply <- roSupply %>%
          mutate("REMOVE_THIS" = 0)
        
      }
      
      
      
      # Save the model input files to the "OutputData" folder
      
      # Define a vector with the planned filenames
      filePaths <- c(Demand = paste0("OutputData/", ws$ID, "_formatted_demand_", i, ".csv"),
                     Supply = paste0("OutputData/", ws$ID, "_formatted_supply_", i, ".csv"),
                     Basin = paste0("OutputData/", ws$ID, "_generated_basins_", i, ".csv"))
      
      # Note: If these filePaths are ever modified in the future, don't forget to update
      # the paths that appear in createModelBasinOutputs_NoUsers() and 
      # createModelOutputs_OneUser()
      
      
      # Demand
      mdtDF %>%
        filter(BASIN %in% flowsToSplitList[[i]]$BASIN) %>%
        write_csv(filePaths[1])
      
      
      # Supply
      roSupply %>%
        select(Date, all_of(as.character(str_remove(flowsToSplitList[[i]]$BASIN, "^SUBBASIN_")))) %>%
        write_csv(filePaths[2])
      
      
      # Basin
      flowsToSplitList[[i]] %>%
        arrange(BASIN) %>%
        write_csv(filePaths[3])
      
      
      
      if (useDWRAT) {
        
        # Try to run DWRAT as well
        useDWRAT <- runDWRAT(filePaths, i)
        
        
        # 'runDWRAT' returns either "TRUE" or "FALSE" and 'useDWRAT' will be updated with that result
        # If an error is encountered while running DWRAT, the function will return FALSE
        # If that occurs, don't try to use DWRAT in subsequent iterations
        
      }
      
    } # End of 'i' loop through 'flowsToSplitList'
    
    
    
    # If DWRAT was run successfully for all output files, 
    # combine the DWRAT outputs from different iterations into combined files
    if (useDWRAT) {
      
      combineOutputs(ws, length(flowsToSplitList))
      
    }
    
    
  }
  
  
  
  # Return nothing
  return()
  
}



splitFlowsTo <- function (flowsTo, outletDF) {
  
  # Split 'flowsTo' into separate data frames
  # Each flow path will have its own subset of this variable
  
  
  
  # Create a list to hold the data frames
  flowsToSplitList <- vector(mode = "list", length = nrow(outletDF))
  
  
  
  # Iterate through each outlet in 'outletDF'
  for (i in 1:nrow(outletDF)) {
    
    # Start the subset with the outlet
    flowsToSubset <- outletDF[i, ]
    
    
    
    # Prepare a variable to hold an upstream DWRAT sub-basin
    priorRow <- outletDF[i, ]
    
    
    
    # Keep adding rows to 'flowsToSubset' as long as there are upstream connections
    # (Upstream connections would have the "BASIN" value from 'priorRow' in the 'FLOWS_TO' column)
    while (nrow(priorRow) > 0) {
      
      # Get the row(s) of catchments/sub-basins that flow into this catchment/sub-basin
      priorRow <- flowsTo %>%
        filter(FLOWS_TO %in% priorRow$BASIN)
      
      
      
      # Add these rows to 'flowsToSubset'
      flowsToSubset <- flowsToSubset %>%
        bind_rows(priorRow) %>%
        unique()
      
    }
    
    
    
    # Add this subset to 'flowsToSplitList'
    flowsToSplitList[[i]] <- flowsToSubset
    
  } # End of 'i' loop through 'outletDF'
  
  
  
  # Verify that no duplicate assignments are made and no basins are missing
  # Every entry in 'flowsTo' should appear exactly once in 'flowsToSplitList'
  stopifnot(flowsToSplitList %>% map_dbl(nrow) %>% sum() == 
              nrow(flowsTo))
  
  
  
  # Return 'flowsToSplitList'
  return(flowsToSplitList)
  
}



createModelBasinOutputs_NoUsers <- function (flowsToSplitList, i, roSupply, ws) {
  
  # In the case where a flow path lacks any water rights,
  # Generate the DWRAT basin outputs here without running the model
  
  # The two output files will be:
  # "[ID]_basin_appropriative_output_i.csv"
  # "[ID]_basin_riparian_output_i.csv"
  
  
  
  # Create the empty files in another function
  # The list contains both tables
  # (Element 1 is the appropriative basin data frame)
  # (Element 2 is the riparian basin data frame)
  resList <- createEmptyBasinFiles(flowsToSplitList[[i]], roSupply)
  
  
  
  # Write the outputs to a file
  resList[[1]] %>%
    write_csv(paste0("OutputData/", ws$ID, "_basin_appropriative_output_", i, ".csv"))
  
  
  
  resList[[2]] %>%
    write_csv(paste0("OutputData/", ws$ID, "_basin_riparian_output_", i, ".csv"))
  
  
  
  # Return nothing
  return()
  
}



createEmptyBasinFiles <- function (flowsTo, roSupply, useNA = FALSE) {
  
  # Prepare two data frames that represent the appropriative and riparian
  # basin outputs from DWRAT
  # Both data frames will be returned in a list
  
  # The third variable ('useNA') is used to decide whether the data frame
  # contains 'NA' or zeroes
  
  
  
  # Start by defining a new data frame for appropriative output
  # The initial columns will be related to available flow
  appOutput <- getAvailableFlow(flowsTo, roSupply)
  
  
  
  # Add columns for ALLOCATIONS and DEMAND for each month as well
  appOutput[roSupply$Date %>% unique() %>% as.Date(format = "%m/%d/%Y") %>% format("%Y-%m") %>%
              paste0(., "_ALLOCATIONS")] <- if_else(useNA, NA_real_, 0)
  
  appOutput[roSupply$Date %>% unique() %>% as.Date(format = "%m/%d/%Y") %>% format("%Y-%m") %>%
              paste0(., "_DEMAND")] <- if_else(useNA, NA_real_, 0)
  
  
  
  # Sort the column names (with "BASIN" still at the start)
  # This gets the desired order of "ALLOCATIONS", "AVAILABLE_FLOW", and "DEMAND"
  appOutput <- appOutput %>%
    select(BASIN, all_of(sort(names(appOutput)[-1]))) %>%
    arrange(BASIN)
  
  
  
  # Prepare the basin riparian output next
  # The output is similar, but "AVAILABLE_FLOW" is now "FLOW"
  # In addition, there is a new "PROPORTIONS" column 
  # (that will be 0 in all cases because there is no demand)
  ripOutput <- appOutput
  
  
  
  names(ripOutput) <- names(ripOutput) %>%
    str_replace("_AVAILABLE_FLOW", "_FLOW")
  
  
  
  ripOutput[roSupply$Date %>% unique() %>% as.Date(format = "%m/%d/%Y") %>% format("%Y-%m") %>%
              paste0(., "_PROPORTIONS")] <- if_else(useNA, NA_real_, 0)
  
  
  
  # Sort the column names (aside from "BASIN")
  # This also gets the desired order of "ALLOCATIONS", "DEMAND", "FLOW", and "PROPORTIONS"
  ripOutput <- ripOutput %>%
    select(BASIN, all_of(sort(names(ripOutput)[-1]))) %>%
    arrange(BASIN)
  
  
  
  # Return 'appOutput' and 'ripOutput' as a list
  return(list("appOutput" = appOutput, "ripOutput" = ripOutput))
  
}



getAvailableFlow <- function (flowsTo, roSupply) {
  
  # Adjust the formatting of 'roSupply'
  # Each column will be a different date (year-month pair)
  # Each row will be a different DWRAT sub-basin
  
  # (This is the formatting of the "AVAILABLE_FLOW" columns in the DWRAT model output)
  
  
  
  # Initialize a new data frame
  availableFlow <- tibble()
  
  
  
  # Modify 'roSupply' to have each month in "Date" as a column
  # Each row will be a DWRAT sub-basin in the flow path
  # pivot_wider() is applied separately to each individual sub-basin
  # The result is then added to 'availableFlow'
  for (j in 1:nrow(flowsTo)) {
    
    subbasinName <- str_remove(flowsTo$BASIN[j], "^SUBBASIN_")
    
    availableFlow <- bind_rows(availableFlow,
                               roSupply %>%
                                 select(Date, all_of(subbasinName)) %>%
                                 pivot_wider(names_from = "Date", values_from = all_of(subbasinName)))
    
  }
  
  
  
  # Add a "BASIN" column to 'availableFlow' 
  availableFlow <- data.frame(BASIN = flowsTo$BASIN) %>%
    bind_cols(availableFlow)
  
  
  
  # Adjust the names of the columns appended from 'availableFlow'
  # Recognize the strings as dates, reformat them as "Year-Month", 
  # and then add "_AVAILABLE_FLOW" to their names
  names(availableFlow)[-1] <- names(availableFlow)[-1] %>% as.Date(format = "%m/%d/%Y") %>% format("%Y-%m") %>%
    paste0(., "_AVAILABLE_FLOW")
  
  
  
  # Return 'availableFlow'
  return(availableFlow)
  
}



createModelOutputs_OneUser <- function (flowsToSplitList, i, userDF, roSupply, ws) {
  
  # In the case where a flow path has only one water right,
  # Generate the DWRAT basin and user outputs here without running the model
  
  # The four output files will be:
  # "[ID]_basin_appropriative_output_[i].csv"
  # "[ID]_basin_riparian_output_[i].csv"
  # "[ID]_user_appropriative_output_[i].csv"
  # "[ID]_user_riparian_output_[i].csv"
  
  
  
  # Check if the user is a riparian or appropriative right
  if (!is.na(userDF$RIPARIAN[1]) && userDF$RIPARIAN[1] == "Y" && 
      is.na(userDF$APPROPRIATIVE[1])) {
    
    isRiparian <- TRUE
    
  } else if (!is.na(userDF$APPROPRIATIVE[1]) && userDF$APPROPRIATIVE[1] == "APPROPRIATIVE" &&
             userDF$RIPARIAN[1] == "N") {
    
    isRiparian <- FALSE
    
  } else {
    
    stop(paste0("The demand data for ", userDF$APPLICATION_NUMBER[1], " contains errors. ",
                "It should be either riparian ('Y' in 'RIPARIAN' and blank in 'APPROPRIATIVE') or ",
                "appropriative ('N' in 'RIPARIAN' and 'APPROPRIATIVE' in 'APPROPRIATIVE')."))
    
  }
  
  
  
  # First, set up the basin files
  resList <- createEmptyBasinFiles(flowsToSplitList[[i]], roSupply, useNA = TRUE)
  
  
  appBasinOutput <- resList[[1]]
  ripBasinOutput <- resList[[2]]
  
  
  
  # Use these files to make the user matrices as well
  
  
  # Start with the user matrix for appropriative rights
  # "ALLOCATIONS" and "DEMAND" can be copied from 'appBasinOutput'
  # "USER" is either NA or the one water right's application number 
  # (if the right isn't riparian)
  # "BASIN" should appear as well (including a column titled "PRIORITY")
  # "PRIORITY" is 1 if the one water right in the flow path is appropriative
  # (For riparian users, it's 10000000, but that will be set later)
  # If there are multiple basins in 'appBasinOutput', filter only to the sub-basin
  # that the user is present in
  appUserOutput <- appBasinOutput %>%
    filter(BASIN == userDF$BASIN[1]) %>%
    select(contains("_ALLOCATIONS"), contains("_DEMAND"), BASIN) %>%
    mutate(USER = if_else(isRiparian, NA_character_, userDF$APPLICATION_NUMBER[1]),
           PRIORITY = if_else(isRiparian, NA_real_, 1))
  
  
  
  # "CURTAILMENT" and "SHORTAGE_%" must be defined
  appUserOutput[names(appUserOutput) %>%
                  str_subset("_ALLOCATIONS") %>%
                  str_replace("ALLOCATIONS", "CURTAILMENT")] <- NA_real_
  
  
  
  appUserOutput[names(appUserOutput) %>%
                  str_subset("_ALLOCATIONS") %>%
                  str_replace("ALLOCATIONS", "SHORTAGE_%")] <- NA_real_
  
  
  
  # Sort the columns in 'appUserOutput'
  # The desired order is "ALLOCATIONS", "CURTAILMENT", "DEMAND", and "SHORTAGE_%"
  # with "USER" at the beginning and both "BASIN" and "PRIORITY" at the end
  appUserOutput <- appUserOutput %>%
    select(USER, sort(names(appUserOutput) %>% 
                        str_subset("USER", negate = TRUE) %>% 
                        str_subset("BASIN", negate = TRUE) %>% 
                        str_subset("PRIORITY", negate = TRUE)),
           BASIN, PRIORITY)
  
  
  
  # Prepare the riparian user matrix next
  # It has the exact same columns as 'appUserOutput'
  # However, "USER" and "PRIORITY" will have a different value
  # "USER" is either NA or the one water right's application number 
  # (if the right isn't appropriative)
  # The same applies to "PRIORITY" (with '10000000' as the priority date for riparian users)
  ripUserOutput <- appUserOutput %>%
    mutate(USER = if_else(isRiparian, userDF$APPLICATION_NUMBER[1], NA_character_),
           PRIORITY = if_else(isRiparian, 10000000, NA_real_))
  
  
  
  # Error Check
  stopifnot(nrow(appBasinOutput) > 0 && nrow(ripBasinOutput) > 0)
  stopifnot(nrow(appUserOutput) > 0 && nrow(ripUserOutput) > 0)
  
  
  
  # Iterate through the dates in 'roSupply'
  # Fill in the corresponding columns of 'appBasinOutput' and 'ripBasinOutput'
  for (j in 1:nrow(roSupply)) {
    
    # Convert the date entry in 'roSupply' into a Month-Year format
    # Use that to select the corresponding columns in the output data frames
    dateStr <- roSupply$Date[j] %>%
      as.Date(format = "%m/%d/%Y") %>%
      format("%Y-%m")
    
    
    
    # Extract the corresponding demand value for the month
    demandVal <- userDF[[grep(paste0("^", 
                                     month.abb[dateStr %>% str_extract("[0-9]{2}$") %>% as.numeric()],
                                     "_MEAN_DIV"),
                              names(userDF), 
                              ignore.case = TRUE)]]
    
    
    
    # Get the allocation value as well
    allocationVal <- min(demandVal,
                         ripBasinOutput[[names(ripBasinOutput) %>% 
                                           str_subset(dateStr) %>% str_subset("FLOW")]])
    # This code uses 'ripBasinOutput' regardless of what 'isRiparian' is
    # This is NOT a problem
    # Regardless of whether the water right is riparian or appropriative,
    # all tables have the iteration's flow value in their respective "FLOW" columns
    
    
    
    # Depending on the type of water right, 
    # Fill in the "DEMAND" and "ALLOCATIONS" columns with values
    if (isRiparian) {
      
      # Input 'demandVal' into the appropriate "DEMAND" column
      ripBasinOutput[[names(ripBasinOutput) %>% 
                        str_subset(dateStr) %>% str_subset("DEMAND")]] <- demandVal
      
      
      
      # Set "ALLOCATIONS" to be the minimum between "DEMAND" and "FLOW"
      ripBasinOutput[[names(ripBasinOutput) %>% 
                        str_subset(dateStr) %>% str_subset("ALLOCATIONS")]] <- allocationVal
      
      
      
      # The "PROPORTIONS" column must be set for riparian matrices too
      # PROPORTIONS = ALLOCATIONS / DEMAND
      ripBasinOutput[[names(ripBasinOutput) %>% 
                        str_subset(dateStr) %>% str_subset("PROPORTIONS")]] <- allocationVal / demandVal
      
      
      
      # Adjust 'ripUserOutput' too using the same values
      ripUserOutput[[names(ripUserOutput) %>% 
                       str_subset(dateStr) %>% str_subset("DEMAND")]] <- demandVal
      
      
      ripUserOutput[[names(ripUserOutput) %>% 
                       str_subset(dateStr) %>% str_subset("ALLOCATIONS")]] <- allocationVal
      
      
      
      # Calculate the "CURTAILMENT" and "SHORTAGE_%" columns as well
      # CURTAILMENT = IF (ALLOCATIONS < DEMAND, 1, 0)
      # SHORTAGE_% = 100 * (DEMAND - ALLOCATIONS) / DEMAND
      ripUserOutput[[names(ripUserOutput) %>% 
                       str_subset(dateStr) %>% str_subset("CURTAILMENT")]] <- if_else(allocationVal < demandVal, 1, 0)
      
      
      ripUserOutput[[names(ripUserOutput) %>% 
                       str_subset(dateStr) %>% str_subset("SHORTAGE")]] <- if_else(demandVal == 0, 0, 100 * (demandVal - allocationVal) / demandVal)
      
      
      
    } else {
      
      # Update the "DEMAND" column for this year-month pair using 'demandVal'
      appBasinOutput[[names(appBasinOutput) %>% 
                        str_subset(dateStr) %>% str_subset("DEMAND")]] <- demandVal
      
      
      # Set the allocations too
      appBasinOutput[[names(appBasinOutput) %>% 
                        str_subset(dateStr) %>% str_subset("ALLOCATIONS")]] <- allocationVal
      
      
      
      # Adjust 'appUserOutput' too using the same values
      appUserOutput[[names(appUserOutput) %>% 
                        str_subset(dateStr) %>% str_subset("DEMAND")]] <- demandVal
      
      
      appUserOutput[[names(appUserOutput) %>% 
                       str_subset(dateStr) %>% str_subset("ALLOCATIONS")]] <- allocationVal
      
      
      
      # Calculate the "CURTAILMENT" and "SHORTAGE_%" columns as well
      # CURTAILMENT = IF (ALLOCATIONS < DEMAND, 1, 0)
      # SHORTAGE_% = 100 * (DEMAND - ALLOCATIONS) / DEMAND
      appUserOutput[[names(appUserOutput) %>% 
                       str_subset(dateStr) %>% str_subset("CURTAILMENT")]] <- if_else(allocationVal < demandVal, 1, 0)
      
      
      appUserOutput[[names(appUserOutput) %>% 
                       str_subset(dateStr) %>% str_subset("SHORTAGE")]] <- if_else(demandVal == 0, 0, 100 * (demandVal - allocationVal) / demandVal)
      
    }
    
    
    
    # If the water right is riparian, the basin appropriative data frame just needs zeroes
    # If the water right is appropriative, the basin riparian data frame just needs zeroes
    # (The opposite user matrices should stay empty)
    if (isRiparian) {
      
      appBasinOutput[, grepl(dateStr, names(appBasinOutput))] <- appBasinOutput[, grepl(dateStr, names(appBasinOutput))] %>%
        map_dfc(~ if_else(is.na(.), 0, .))
      
    } else {
      
      ripBasinOutput[, grepl(dateStr, names(ripBasinOutput))] <- ripBasinOutput[, grepl(dateStr, names(ripBasinOutput))] %>%
        map_dfc(~ if_else(is.na(.), 0, .))
      
    }
    
  }
  
  
  
  # Write the four data frames to CSV files
  appBasinOutput %>%
    write_csv(paste0("OutputData/", ws$ID, "_basin_appropriative_output_", i, ".csv"))
  
  
  ripBasinOutput %>%
    write_csv(paste0("OutputData/", ws$ID, "_basin_riparian_output_", i, ".csv"))
  
  
  appUserOutput %>%
    write_csv(paste0("OutputData/", ws$ID, "_user_appropriative_output_", i, ".csv"))
  
  
  ripUserOutput %>%
    write_csv(paste0("OutputData/", ws$ID, "_user_riparian_output_", i, ".csv"))
  
  
  
  # Return nothing
  return()
  
}



addDummyRow <- function (mdtDF, isRiparian, flowsTo, lastCatch, demandVal = 1E-10) {
  
  # Create a fake water right with almost zero demand data
  # DWRAT requires at least one each of riparian and appropriative users to run,
  # but no flow should be allocated to them
  
  
  # Catchment and basin location information is set to the final DWRAT sub-basin in the flow path
  # (The one that drains into "SUBBASIN_000")
  
  
  # If the dummy right's demand is given as 0, the Paradigm DWRAT script sets their demand to 0.00002
  # By setting it beforehand here, a smaller demand value can be used
  
  
  
  # Start by determining the "CATCHMENT", "BASIN", and "FLOWS_TO" values
  FLOWS_TO <- flowsTo$FLOWS_TO %>%
    str_subset("_000$") %>%
    head(1)
  
  
  
  BASIN <- flowsTo$BASIN[flowsTo$FLOWS_TO == FLOWS_TO]
  
  
  
  CATCHMENT <- lastCatch$BASIN[lastCatch$DWRAT_SUBBASIN == BASIN %>%
                                 str_remove("^SUBBASIN_")]
  
  
  
  stopifnot(length(FLOWS_TO) == 1 && !is.na(FLOWS_TO))
  stopifnot(length(BASIN) == 1 && !is.na(BASIN))
  stopifnot(length(CATCHMENT) == 1 && !is.na(CATCHMENT))
  
  
  
  # Only initialize some of the columns
  # The rest will automatically be set to "NA" in the final step
  dummyFrame <- data.frame(APPLICATION_NUMBER = "REMOVE_THIS",
                           ORIGINAL_APPLICATION_NUMBER = "REMOVE_THIS",
                           # Diversion Data: All zeroes
                           TOTAL_EXPECTED_ANNUAL_DIVERSION = demandVal * 12,
                           TOTAL_MAY_SEPT_DIV = demandVal * 5,
                           # Priority Date: Always 1000-00-00 for Riparian
                           # For appropriative, set to 9999-12-31 so it's always the lowest priority
                           ASSIGNED_PRIORITY_DATE_SUB = if_else(isRiparian, 10000000, 99991231),
                           # No face value or initial diversion amount
                           INI_REPORTED_DIV_AMOUNT_AF = if_else(isRiparian, 0, NA_real_),
                           FACE_VALUE_AMOUNT_AF = if_else(isRiparian, NA_real_, 0),
                           # Riparian vs Appropriative 
                           RIPARIAN = if_else(isRiparian, "Y", "N"),
                           APPROPRIATIVE = if_else(isRiparian, NA_character_, "APPROPRIATIVE"), 
                           WATER_RIGHT_TYPE = if_else(isRiparian, "Statement of Div and Use", "Appropriative"), 
                           # Take the final DWRAT sub-basin in the flow path
                           CATCHMENT = CATCHMENT,
                           BASIN = BASIN,
                           FLOWS_TO = FLOWS_TO)
  
  
  
  # Make all of the monthly demand values equal to a very small value (almost 0)
  dummyFrame[paste0(toupper(month.abb), "_MEAN_DIV")] <- demandVal
  
  
  
  # Set several yes/no columns to "N"
  dummyFrame[c("FULLY NON-CONSUMPTIVE", "POWER_DEMAND_ZEROED",
               "PRE_1914", "ZERO_DEMAND", "NULL_DEMAND")] <- "N"
  
  
  
  # Append this new row to 'mdtDF'
  mdtDF <- bind_rows(mdtDF, dummyFrame)
  
  
  
  # Return 'mdtDF'
  return(mdtDF)
  
}



checkForAnaconda <- function () {
  
  # Check if the batch file for Anaconda Prompt is present on the user's computer
  # DWRAT can only be run if Anaconda is present
  
  # Return a boolean for whether DWRAT should be run by the script
  
  
  
  # Look for an Anaconda installation
  batchPath <- getAnacondaBatchPath()
  
  
  
  # Ensure that the Anaconda installation exists and the batch file can be located
  # If not, DWRAT will not be run automatically by this script
  if (!file.exists(batchPath)) {
    
    message(paste0("Could not find the Anaconda batch file that enables command usage",
                   " in Command Prompt. The 'Anaconda' installation should be in",
                   " the 'ProgramData' folder or the main C: Drive directory.\n\n",
                   "For that reason, DWRAT cannot be executed automatically."))
    
    
    # Return FALSE (don't run DWRAT using this script)
    return(FALSE)
    
  } else {
    
    # Return TRUE (attempt to run DWRAT using this script)
    return(TRUE)
    
  }
  
}



getAnacondaBatchPath <- function () {
  
  # Get the path to Anaconda Prompt's 'activate.bat' file
  # This file is used to run Anaconda via Command Prompt
  
  
  
  # Check the C: Drive and "ProgramData" folder for an Anaconda installation
  return(c(list.files("C:/", pattern = "[aA]naconda", full.names = TRUE),
           list.files("C:/ProgramData/", pattern = "[aA]naconda", full.names = TRUE)) %>%
           sort() %>%
           str_replace_all("/", "\\\\") %>%
           paste0(., "\\Scripts\\activate.bat") %>%
           head(1))
  
}



runDWRAT <- function (filePaths, i = NULL) {
  
  # Given the input filepaths for Paradigm DWRAT
  # Run DWRAT and save the basin and user outputs to the "OutputData" folder
  
  
  
  # First, modify the filepaths to be absolute filepaths
  # Include the initial output directory in this operation 
  # (the "output" folder in the "Paradigm_DWRAT" sub-repository)
  modifiedPaths <- c(filePaths,
                     "../Paradigm_DWRAT/dwrat/output/") %>%
    normalizePath(winslash = "/") 
  
  
  
  # For the output filepath, append a final forward slash to the end of the path
  modifiedPaths[4] <- paste0(modifiedPaths[4], "/")
  
  
  
  # Specify (as a variable) the location of the main DWRAT script
  scriptPath <- "../Paradigm_DWRAT/DWRAT_Mad.py"
  
  
  
  # Next, edit the Paradigm DWRAT script to contain these filepaths
  editScript(scriptPath, modifiedPaths)
  
  
  
  # After that, locate Anaconda Prompt 
  batchPath <- getAnacondaBatchPath()
  
  
  
  # If DWRAT will be run multiple times (i.e., the variable 'i' is not NULL),
  # Output a message about the run
  if (!is.null(i)) {
    
    cat(paste0("\n\n\nPreparing for DWRAT run on Watershed Flow Path #", i, "\n\n"))
    
  }
  
  
  
  # Send a series of commands to Command Prompt:
  # Start Anaconda Prompt
  # Then activate the "paradigm-dwrat" environment
  # Finally, run the DWRAT Python script
  iterRes <- paste0(batchPath, " && ",
                    "conda activate paradigm-dwrat &&",
                    "python \"", normalizePath(scriptPath), "\"") %>%
    system(intern = TRUE)
  
  
  
  # If there is an error when running the script, 
  # an error message will be present in 'iterRes'
  if (sum(grepl("Error", iterRes, ignore.case = TRUE)) > 0) {
    
    cat("There was an error while running DWRAT! See the message below:\n\n")
    print(iterRes)
    cat("\n\nPlease investigate and fix the error.\nFor the rest of this run, this script will not try to run DWRAT anymore.\n")
    
    return(FALSE)
    
  }
  
  
  
  # In all other cases, simply print out 'iterRes'
  cat("\n\n")
  print(iterRes)
  cat("\n\n")
  
  
  
  # Copy four output files from the Paradigm DWRAT "output" folder 
  # to the Demand "OutputData" folder
  file.copy("../Paradigm_DWRAT/dwrat/output/basin_appropriative_output_Paradigm_DWRAT.csv",
            filePaths[1] %>% str_replace("formatted_demand", "basin_appropriative_output"),
            overwrite = TRUE)
  
  
  file.copy("../Paradigm_DWRAT/dwrat/output/user_appropriative_output_Paradigm_DWRAT.csv",
            filePaths[1] %>% str_replace("formatted_demand", "user_appropriative_output"),
            overwrite = TRUE)
  
  
  file.copy("../Paradigm_DWRAT/dwrat/output/basin_riparian_output_Paradigm_DWRAT.csv",
            filePaths[1] %>% str_replace("formatted_demand", "basin_riparian_output"),
            overwrite = TRUE)
  
  
  file.copy("../Paradigm_DWRAT/dwrat/output/user_riparian_output_Paradigm_DWRAT.csv",
            filePaths[1] %>% str_replace("formatted_demand", "user_riparian_output"),
            overwrite = TRUE)
  
  
  
  # Then, return TRUE
  return(TRUE)
  
}



editScript <- function (scriptPath, modifiedPaths) {
  
  # Edit the filepaths that appear in the main DWRAT script
  
  
  
  # Read in the lines of code from this script
  dwratScript <- read_lines(scriptPath)
  
  
  
  # Replace the filepaths in 'dwratScript'
  dwratScript <- dwratScript %>%
    locateAndReplace("^demand_file", modifiedPaths[1]) %>%
    locateAndReplace("^supply_file", modifiedPaths[2]) %>%
    locateAndReplace("^basin_file ", modifiedPaths[3]) %>%
    locateAndReplace("directoryPath=", modifiedPaths[4])
  
  
  
  # Update the Python script with these changes
  write_lines(dwratScript, scriptPath)
  
  
  
  # Return nothing
  return()
  
}



locateAndReplace <- function (dwratScript, matchPattern, newPath) {
  
  # In the script text of 'dwratScript', locate a specific line of code
  # using the regex pattern in 'matchPattern'
  # Then, on that line, replace the filepath (encased in single quotes) 
  # with the path in 'newPath'
  
  
  
  # Try to find 'matchPattern' in 'dwratScript'
  matchIndex <- grep(matchPattern, dwratScript)
  
  
  
  # There should be exactly one match
  stopifnot(length(matchIndex) == 1)
  
  
  
  # Replace the filepath contained within single quotes on this line of code
  # Use 'newPath' as the replacement
  dwratScript[matchIndex] <- dwratScript[matchIndex] %>%
    str_replace("'.+'\\s*",
                paste0("'", newPath, "'"))
  
  
  
  # Return the updated 'dwratScript'
  return(dwratScript)
  
}



combineOutputs <- function (ws, numFlowPaths) {
  
  # If a watershed was split into multiple flow paths, it had a separate DWRAT run for each path
  # Four output files are saved from each DWRAT run
  # Since they all follow the same format, combine the files from different flow paths
  # into four output files
  
  
  
  # Initialize empty data frames for each of the output files
  combinedBasinApp <- tibble()
  
  combinedBasinRip <- tibble()
  
  combinedUserApp <- tibble()
  
  combinedUserRip <- tibble()
  
  
  
  # Iterate through every flow path
  for (i in 1:numFlowPaths) {
    
    # Read in this flow path's DWRAT basin outputs
    tempBasinApp <- read_csv(paste0("OutputData/", ws$ID, "_basin_appropriative_output_", i, ".csv"),
                             show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
      filter(BASIN != "SUBBASIN_REMOVE_THIS")
    
    tempBasinRip <- read_csv(paste0("OutputData/", ws$ID, "_basin_riparian_output_", i, ".csv"),
                             show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
      filter(BASIN != "SUBBASIN_REMOVE_THIS")
    
    
    
    # Append these temporary data frames to the combined variables
    combinedBasinApp <- bind_rows(combinedBasinApp, tempBasinApp)
    
    combinedBasinRip <- bind_rows(combinedBasinRip, tempBasinRip)
    
    
    
    # If the flow path had no users, there wouldn't be any user files for this iteration
    # Check first if those files exist
    if (!file.exists(paste0("OutputData/", ws$ID, "_user_appropriative_output_", i, ".csv")) ||
        !file.exists(paste0("OutputData/", ws$ID, "_user_riparian_output_", i, ".csv"))) {
      
      
      # If there really are no users, the sum of the demands in both of the basin files should be 0
      if (sum(tempBasinApp %>% select(contains("_DEMAND")) %>% mutate(across(everything(), as.numeric))) == 0 && 
          sum(tempBasinRip %>% select(contains("_DEMAND")) %>% mutate(across(everything(), as.numeric))) == 0) {
        
        # In that case, just skip to the next iteration
        next
      
        
      # If the basin files do contain demand values, that's an indication of an error
      # User files should exist in that scenario
      } else {
        
        stop(paste0("'OutputData/", ws$ID, "_user_appropriative_output_", i, ".csv' and/or ",
                    "'OutputData/", ws$ID, "_user_riparian_output_", i, ".csv' could not be found! ",
                    "\nSince there are non-zero demands in the basin files, there should be user files too."))
        
      }
      
      
    }
    
    
    
    # This code only runs if user files exist
    # Try to read them in
    tempUserApp <- read_csv(paste0("OutputData/", ws$ID, "_user_appropriative_output_", i, ".csv"),
                            show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
      filter(USER != "REMOVE_THIS")
    
    tempUserRip <- read_csv(paste0("OutputData/", ws$ID, "_user_riparian_output_", i, ".csv"),
                            show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
      filter(USER != "REMOVE_THIS")
    
    
    
    # Update the combined data frames using these user files too
    combinedUserApp <- bind_rows(combinedUserApp, tempUserApp)
    
    combinedUserRip <- bind_rows(combinedUserRip, tempUserRip)
    
  }
  
  
  
  # Make columns numeric
  # (Everything except the "BASIN" and "USER" columns)
  combinedBasinApp <- combinedBasinApp %>%
    mutate(across(!BASIN, as.numeric))
  
  combinedBasinRip <- combinedBasinRip %>%
    mutate(across(!BASIN, as.numeric))
  
  combinedUserApp <- combinedUserApp %>%
    mutate(across(!USER & !BASIN, as.numeric))
  
  combinedUserRip <- combinedUserRip %>%
    mutate(across(!USER & !BASIN, as.numeric))
  
  
  
  # Write the combined variables to files
  combinedBasinApp %>%
    write_csv(paste0("OutputData/", ws$ID, "_basin_appropriative_output_combined.csv"))
  
  combinedBasinRip %>%
    write_csv(paste0("OutputData/", ws$ID, "_basin_riparian_output_combined.csv"))
  
  combinedUserApp %>%
    write_csv(paste0("OutputData/", ws$ID, "_user_appropriative_output_combined.csv"))
  
  combinedUserRip %>%
    write_csv(paste0("OutputData/", ws$ID, "_user_riparian_output_combined.csv"))
  
  
  
  # Return nothing
  return()
  
}



#### Execution ####
mainProcedure(hucBased = TRUE)



# Clean the environment
remove(list = ls())
