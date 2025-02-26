# Assign a sub-basin to each water right
# For rights with PODs in multiple sub-basins,
# use the connectivity matrix to choose one


# This script reads in a shapefile of POD points and polygon data of Russian River subbasins
# Then, it uses the sf package's st_intersection() function to assign 

library(tidyverse)
library(readxl)
library(writexl)
library(sf)


cat("Starting 'Assign_Subbasin_to_POD.R'...\n")


#### Functions ####


mainProcedure <- function () {
  
  
  source("Scripts/Watershed_Selection.R")
  source("Scripts/Dataset_Year_Range.R")
  
  
  
  # Read in a spreadsheet with coordinate data and convert it into a spatial feature
  #   (Also, keep copies of the latitude and longitude coordinates in new columns)
  #   (Otherwise, when the geometry is dropped, the coordinate data is removed)
  POD <- getXLSX(ws = ws, 
                 SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
                 FILEPATH = "POD_COORDINATES_SPREADSHEET_PATH",
                 WORKSHEET_NAME = "POD_COORDINATES_WORKSHEET_NAME") %>%
    select(APPLICATION_NUMBER, POD_ID, LONGITUDE, LATITUDE) %>% unique() %>%
    mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
    st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = ws$POD_COORDINATES_REFERENCE_SYSTEM[1])
  
  
  
  # Also import a layer with the watershed's subbasins
  # (There should be one polygon per subbasin)
  subWS <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                  GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME")
  
  
  
  # Change the CRS of 'subWS' and 'POD' to the same projection
  subWS <- st_transform(subWS, "epsg:3488")
  POD <- st_transform(POD, "epsg:3488")
  
  
  
  # From 'ws', extract the field name(s) that will be carried over from 'subWS' to 'POD'
  # (They will help uniquely identify different subbasins)
  fieldNames <- ws$SUBBASIN_FIELD_ID_NAMES %>%
    str_split(";") %>% unlist() %>% trimws()
  
  
  
  # Check that each 'POD' intersects with a subbasin
  checkOverlap(POD, subWS)
  
  
  
  # Perform the intersections for 'POD' with both 'subWS'
  subbasinPOD <- st_intersection(POD,
                                 subWS %>% select(all_of(fieldNames)))
  
  
  
  # Extract a data frame of the attribute table from 'subbasinPOD'
  podTable <- st_drop_geometry(subbasinPOD)
  
  
  
  # Check for water rights with multiple subbasins assigned
  # (This can happen for rights with multiple PODs)
  # If that is the case, additional steps will be needed
  podTable <- checkForMultiBasinRights(podTable, fieldNames, subWS, ws, yearRange)
  
  
  
  # Make sure 'podTable' is sorted by "APPLICATION_NUMBER" and "POD_ID"
  podTable <- podTable %>%
    arrange(APPLICATION_NUMBER, POD_ID)
  
  
  
  # The next step is to export 'podTable' to a file
  podTable %>%
    write_xlsx(paste0("OutputData/", ws$ID, "_POD_Subbasin_Assignment.xlsx"))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkOverlap <- function (POD, subWS) {
  
  # Verify that each POD overlaps with one subbasin in 'subWS'
  
  
  
  # Check for intersections
  overlapCheck <- st_intersects(POD, subWS) %>% lengths()
  
  
  
  # Stop if there are any lengths greater than or less than 1 
  # (points present in more than one subbasin or points not present in any subbasin)
  # (No code has been written to handle either case)
  stopifnot(sum(overlapCheck > 1) == 0)
  stopifnot(sum(overlapCheck < 1) == 0)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkForMultiBasinRights <- function (podTable, fieldNames, subWS, ws, yearRange) {
  
  # Rights with more than one POD might have multiple subbasins assigned to them
  # These cases must be adjusted because DWRAT only accepts
  # one subbasin per water right/"APPLICATION_NUMBER" value
  
  
  
  # Create a table with unique combinations of "APPLICATION_NUMBER" and the subbasin ID field(s)
  appRecords <- podTable %>%
    select(APPLICATION_NUMBER, all_of(fieldNames)) %>%
    unique()
  
  
  
  # If each "APPLICATION_NUMBER" in 'appRecords' appears once, then no rights have 
  # multiple sub-basins assigned; in that case, return 'podTable' with no changes
  if (length(unique(appRecords$APPLICATION_NUMBER)) == nrow(appRecords)) {
    
    return(podTable)
    
  }
  
  
  
  # If the procedure reaches this code, then 
  # there are rights with multiple assigned sub-basins
  cat("\n\nThe dataset contains water rights with multiple assigned sub-basins\n")
  cat("This can happen when a right has multiple PODs, and they are located in different sub-basins\n")
  cat("This script will attempt to use the basin connectivity matrix to resolve these issues\n\n")
  
  
  
  # Use the basin connectivity matrix to analyze these rights
  # If the PODs are in the same flow path, choose the most downstream sub-basin
  
  
  
  # First read in the connectivity matrix
  connMat <- getXLSX(ws,
                     "IS_SHAREPOINT_PATH_CONNECTIVITY_MATRIX_SPREADSHEET",
                     "CONNECTIVITY_MATRIX_SPREADSHEET_PATH",
                     "CONNECTIVITY_MATRIX_WORKSHEET_NAME")
  
  
  # NOTE
  # This script assumes that the first column name in 'fieldNames' is the same
  # column used to identify different sub-basins in 'connMat'
  stopifnot(sum(unlist(connMat[, 1], use.names = FALSE) %in% podTable[[fieldNames[1]]]) > 0)
  
  
  
  # Get a list of rights with multiple entries
  appRecords <- appRecords %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(FREQ = n(), .groups = "drop") %>%
    filter(FREQ > 1) %>%
    select(APPLICATION_NUMBER)
  
  
  
  # Add fields to 'podTable' related to multiple sub-basins for the same right
  podTable <- podTable %>%
    mutate(ASSIGNED_MULTIPLE_SUBBASINS = APPLICATION_NUMBER %in% appRecords$APPLICATION_NUMBER,
           ORIGINAL_ASSIGNMENT = if_else(APPLICATION_NUMBER %in% appRecords$APPLICATION_NUMBER,
                                         !!sym(fieldNames[1]), NA))
  
  
  
  for (i in 1:nrow(appRecords)) {
    
    # Get a subset of 'podTable' with records related to this right only
    subsetDF <- podTable %>%
      filter(APPLICATION_NUMBER == appRecords$APPLICATION_NUMBER[i])
    
    
    
    # Keep only the first field of 'subsetDF' mentioned in 'fieldNames'
    # (The assumption is that this field distinguishes sub-basins
    #  in the connectivity matrix)
    subsetDF <- subsetDF %>%
      select(fieldNames[1]) %>%
      unique()
    
    
    
    # Get a subset of 'connMat'
    # It will contain the connectivity between the sub-basins in 'subsetDF'
    # The first filter keeps only rows whose values in the first column are in 'subsetDF'
    # (Remember, the first column contains the sub-basin identifiers used by 'connMat')
    # The column names in 'connMat' (other than the first column) are the same IDs
    # Therefore, the select() command uses the indices in 'connMat' whose IDs 
    # match the values in 'subsetDF' (and the first column is needed too)
    # The overall result of this code is just the connectivity between the sub-basins
    # listed in 'subsetDF'
    subsetConn <- connMat %>%
      filter(!!sym(names(connMat)[1]) %in% subsetDF[[1]]) %>%
      select(which(names(connMat) %in% c(names(connMat)[1], subsetDF[[1]])))
    
    
    
    # If the connectivity column sum for any of the sub-basins is equal to 
    # the number of rows in 'subsetDF'/'subsetConn', every sub-basin in 'subsetDF' 
    # eventually drains into that sub-basin
    # In that case, use that sub-basin for this iteration's water right
    if (nrow(subsetConn) %in% colSums(subsetConn[, -1])) {
      
      # In another function, implement updates into 'podTable'
      # Assign the chosen sub-basin's data to all rows belonging to this right
      podTable <- podTable %>%
        subbasinUpdate(appRecords$APPLICATION_NUMBER[i], subsetConn, fieldNames)
    
    # The alternative case is that no single sub-basin is connected to all of the
    # flagged sub-basins for this water right
    } else {
      
      # In this situation, the water right will be split into sub-rights
      # The sub-rights will be assigned to different sub-basins
      # In addition, the original water right's data will be split among these sub-rights
      # (The split will be based on sub-basin drainage area)
      
      
      
      # The procedure will differ depending on whether every sub-basin
      # in 'subsetConn' is disconnected from each other
      
      
      
      # If that is the case, the column sum for each sub-basin should be 1
      if (nrow(subsetConn) == sum(colSums(subsetConn[, -1]) == 1)) {
        
        # Easy Case
        # Make a separate right for every sub-basin in 'subsetConn'
        podTable <- podTable %>%
          splitWaterRight(appRecords$APPLICATION_NUMBER[i], 
                          subsetConn, fieldNames[1], 
                          subWS, ws, yearRange, connMat)
        
      } else {
        
        # Hard Case
        # Identify the minimum number of sub-basins that covers all 
        # disconnected flows 
        # Then, for each sub-basin in that group, all sub-basins in 
        # 'subsetConn' that drain into that sub-basin will have their
        # ID fields replaced with the data for that sub-basin
        
        
        
        # Get the names of the sub-basins
        # This vector will contain the minimum number of required sub-basins
        chosenSubbasins <- identifyMinimumSubbasinCombination(subsetConn)
        
        
        
        # Update 'podTable' based on this choice
        # Iterate through the sub-basins in 'chosenSubbasins'
        for (k in 1:length(chosenSubbasins)) {
          
          # Get the names of the sub-basins that drain into this chosen sub-basin
          nameVec <- subsetConn %>%
            filter(!!sym(as.character(chosenSubbasins[k])) == 1) %>%
            select(names(subsetConn)[1]) %>%
            unlist(use.names = FALSE)
          
          
          
          # Update 'podTable'
          podTable[podTable$APPLICATION_NUMBER == 
                     appRecords$APPLICATION_NUMBER[i] &
                     podTable[[fieldNames[1]]] %in% nameVec, ][fieldNames] <- podTable[podTable$APPLICATION_NUMBER == 
                                                                                         appRecords$APPLICATION_NUMBER[i] &
                                                                                         podTable[[fieldNames[1]]] == chosenSubbasins[k], ][1, fieldNames]
          
        }
        
        
        
        # After that, the water right will be divided into sub-rights
        # With one sub-right per sub-basin in 'chosenSubbasins'
        
        
        
        # Update 'subsetDF' and 'subsetConn'
        # (They likely contain more than the chosen minimum number of sub-basins)
        # Then, call splitWaterRight()
        subsetDF <- podTable %>%
          filter(APPLICATION_NUMBER == appRecords$APPLICATION_NUMBER[i])
        
        subsetDF <- subsetDF %>%
          select(fieldNames[1]) %>%
          unique()
        
        
        subsetConn <- connMat %>%
          filter(!!sym(names(connMat)[1]) %in% subsetDF[[1]]) %>%
          select(which(names(connMat) %in% c(names(connMat)[1], subsetDF[[1]])))
        
        
        
        # Update the flow dataset and 'podTable'
        podTable <- podTable %>%
          splitWaterRight(appRecords$APPLICATION_NUMBER[i], 
                          subsetConn, fieldNames[1], 
                          subWS, ws, yearRange, connMat)
        
      }
      
    }
    
  } # End of 'i' loop
  
  
  
  # Return 'podTable'
  return(podTable)
  
}



subbasinUpdate <- function (podTable, appNum, subsetConn, fieldNames) {
  
  # The sub-basins related to a right (identified by 'appNum')
  # are listed in 'subsetConn', which expresses their connectivity
  # This function is called if there is a sub-basin that all of these
  # sub-basins eventually drain into
  # That final sub-basin will be used as the assignment for the water right
  
  
  
  # Get the sub-basin ID that has a column sum equal to the row count
  subID <- names(subsetConn)[1 + which(colSums(subsetConn[, -1]) == nrow(subsetConn))] %>%
    as.numeric()
  
  
  
  # Error Check
  stopifnot(length(subID) == 1 & !is.na(subID))
  
  
  
  # Extract a row from 'podTable' that has this sub-basin's information
  selectedBasin <- podTable %>%
    filter(APPLICATION_NUMBER == appNum) %>%
    filter(!!sym(fieldNames[1]) == subID) %>%
    select(APPLICATION_NUMBER, all_of(fieldNames)) %>%
    unique()
  
  
  
  # Error Check
  stopifnot(nrow(selectedBasin) == 1)
  
  
  
  # Update the rows for this water right in 'podTable'
  # The basin-related fields should have their data replaced with 
  # the values in 'selectedBasin'
  podTable[podTable$APPLICATION_NUMBER == appNum, ][fieldNames] <- 
    selectedBasin[, ][fieldNames]
  
  
  
  # Return 'podTable'
  return(podTable)
  
}



splitWaterRight <- function (podTable, appNum, subsetConn, colName,
                             subWS, ws, yearRange, connMat) {
  
  # For each sub-basin in 'subsetConn', split the water right 
  # identified by 'appNum' into several sub-rights
  
  # This update will be applied both to 'podTable' and to
  # the flow spreadsheet for this watershed
  
  
  
  # Read in the flow spreadsheet
  flowDF <- read_xlsx(paste0("OutputData/", ws$ID, "_",
                             yearRange[1], "_", yearRange[2],
                             "_Monthly_Diversions.xlsx"))
  
  
  
  # Get the records related to this iteration's water right
  flowRecords <- flowDF %>%
    filter(APPLICATION_NUMBER == appNum)
  
  
  
  # Also get the area of every sub-basin
  # The right's flow values will be proportioned to 
  # each sub-right/sub-basin based on drainage area
  # For each sub-basin that will receive a split right, 
  # the drainage areas *upstream* of the sub-basin are also considered 
  # in the area calculations
  # (This is split into two steps so that the basin IDs can be used 
  #  in naming the results)
  subbasinAreas <- subWS %>% st_area() %>%
    as.numeric() %>%
    set_names(subWS[[colName]])
  
  
  
  # This vector will hold the total drainage areas to each of the
  # sub-basins in 'subsetConn'
  areaVec <- rep(NA_real_, nrow(subsetConn)) %>%
    setNames(subsetConn[[1]])
  
  
  
  # Get the total area of the sub-basins that flow into 
  # each sub-basin in 'subsetConn'
  # (Also include the target sub-basins themselves in these calculations)
  for (i in 1:length(areaVec)) {
    
    # In the connectivity matrix, the format is "row flows into column"
    # For each sub-basin row, columns will have a value of 1 if flow from the
    # sub-basin row eventually reaches the sub-basin identified by that column
    
    
    # Get a subset of 'connMat' that has the sub-basins that flow into
    # this iteration's sub-basin
    subbasinPath <- connMat[[1]][connMat[[which(names(connMat) == subsetConn[[1]][i])]] > 0]
    
    # Explaining the above line of code:
    # (1) "subsetConn[[1]][i]" is the target sub-basin in this iteration
    # (2) "which(names(connMat) == subsetConn[[1]][i])" gives the column index
    #     in 'connMat' that matches the target sub-basin
    #     (Rows with values of "1" in this column mean that those sub-basins 
    #      drain into the target sub-basin)
    # (3) "connMat[[which(names(connMat) == subsetConn[[1]][i])]] > 0" identifies
    #     the rows with a value of "1" in the target sub-basin's column
    # (4) "connMat[[1]][connMat[[which(names(connMat) == subsetConn[[1]][i])]] > 0]"
    #     is the names of the sub-basins with a value of "1" in the target 
    #     sub-basin's column. Therefore, this is a list of the sub-basins that
    #     drain into the target sub-basin (that target sub-basin is also included 
    #     in this list)
    
    
    
    areaVec[i] <- sum(subbasinAreas[names(subbasinAreas) %in% subbasinPath])
    
  }
  
  
  
  # Error Check
  stopifnot(!anyNA(areaVec))
  
  
  
  # Previous code that only considered the drainage areas of the sub-basins
  # that would house the split water rights: 
  
  #areaVec <- subWS[subWS[[colName]] %in% unlist(subsetConn[, 1]), ]
  
  # areaVec <- st_area(areaVec) %>%
  #   as.numeric() %>%
  #   set_names(areaVec[[colName]])
  
  
  
  # Iterate through each sub-basin
  # Proportion the right's flow data to each sub-right based on 'areaVec'
  for (j in 1:nrow(subsetConn)) {
    
    # Create a new sub-right for the water right
    # Its values will be a portion of the main right's total usage
    # (based on sub-basin drainage area)
    flowDF <- flowDF %>%
      bind_rows(flowRecords %>%
                  mutate(APPLICATION_NUMBER = paste0(APPLICATION_NUMBER, "_", j)) %>%
                  mutate(across(contains("DIVERSION"), ~ . * areaVec[which(names(areaVec) == subsetConn[[j, 1]])] / sum(areaVec))))
    
    
    
    # Update 'podTable' so that all of the right's PODs in this sub-basin
    # are assigned to this new sub-right
    podTable[podTable[[colName]] == subsetConn[[j, 1]] &
               podTable$APPLICATION_NUMBER == appNum, ]$APPLICATION_NUMBER <- paste0(podTable[podTable[[colName]] == subsetConn[[j, 1]] &
                                                                                                podTable$APPLICATION_NUMBER == appNum, ]$APPLICATION_NUMBER, 
                                                                                     "_", j)
    
  }
  
  
  
  # Remove the primary water right from 'flowDF'
  flowDF <- flowDF %>%
    filter(APPLICATION_NUMBER != appNum)
  
  
  
  # Then overwrite flowDF's file with these updates
  flowDF %>%
    write_xlsx(paste0("OutputData/", ws$ID, "_",
                      yearRange[1], "_", yearRange[2],
                      "_Monthly_Diversions.xlsx"))
  
  
  
  # Finally, return 'podTable'
  return(podTable)
  
}



createSubbasinCombinations <- function (subsetConn) {
  
  # Construct a data frame that contains all possible sub-basin combinations
  # 'groupOptions' will have 'n' columns 
  # (equal to the number of sub-basins in 'subsetConn')
  # But each row will have two or more sub-basins only (unused columns will be 'NA')
  # (And each row corresponds to a different combination of sub-basins)
  groupOptions <- data.frame()
  
  
  
  # Iterate through the sub-basins in 'subsetConn'
  for (j in 2:nrow(subsetConn)) {
    
    # Get all combinations with 'j' sub-basins in 'subsetConn'
    tempDF <- combn(subsetConn[[1]], j) %>% t() %>%
      as.data.frame()
    
    
    
    # If the number of sub-basins in each combination is less than
    # the total number of sub-basins in 'subsetConn', fill in the
    # remaining columns in 'tempDF' with 'NA'
    if (j < nrow(subsetConn)) {
      tempDF[, (j + 1):nrow(subsetConn)] <- NA
    }
    
    
    
    # For consistency, the names in 'tempDF' should be the
    # same in each iteration (this is vital for properly
    # defining 'groupOptions')
    names(tempDF) <- paste0("BASIN_", 1:nrow(subsetConn))
    
    
    
    # Add these sub-basin combinations to 'groupOptions'
    groupOptions <- rbind(groupOptions,
                          tempDF)
    
    
  }
  
  
  
  # Return 'groupOptions'
  return(groupOptions)
  
}



identifyMinimumSubbasinCombination <- function (subsetConn) {
  
  # Among the sub-basins in 'subsetConn', find the minimum number 
  # of sub-basins that covers all flow paths for this water right
  
  
  
  # Get a data frame with different combinations of sub-basins
  # (2 or more sub-basins per combination)
  groupOptions <- createSubbasinCombinations(subsetConn)
  
  
  
  # The data frame is defined to have the least number of sub-basins
  # in a combination at the beginning, and more sub-basins per
  # combination towards the end
  
  # This means that, by iterating from the beginning to end of
  # 'groupOptions', the minimum number of sub-basins will be
  # considered first
  
  
  
  # Iterate through 'groupOptions'
  # Find the first combination of sub-basins that covers all flows
  # for the water right
  for (j in 1:nrow(groupOptions)) {
    
    # Take a subset of columns in 'subsetConn'
    # These are the sub-basins that appear in this iteration of 'groupOptions'
    drainageCheck <- subsetConn %>%
      select(which(names(subsetConn) %in% unlist(groupOptions[j, ])))
    
    
    # The rows correspond to every sub-basin in 'subsetConn'
    # If every row contains at least one '1', then every sub-basin 
    # eventually drains into at least one of the sub-basins 
    # in 'groupOptions'
    if (sum(rowSums(drainageCheck) > 0) == nrow(subsetConn)) {
      
      return(groupOptions[j, 1:ncol(drainageCheck)] %>% 
               unlist(use.names = FALSE))
      
    }
    
  }
  
  
  
  # The code should never reach this point
  # The last row in 'groupOptions' should contain every sub-basin
  # Therefore, in the last iteration, every sub-basin would be considered
  # And every sub-basin drains into itself, at minimum
  # This means that the last iteration should always be successful
  stop("Sub-Basin Combination Error:\nThe code should never reach this point\nEvery sub-basin should drain into itself, at minimum")
  
}


#### Script Execution ####


# Run the script
mainProcedure()


cat("Done!\n")


# Remove the functions from the workspace
remove(mainProcedure, checkOverlap, checkForMultiBasinRights, subbasinUpdate,
       splitWaterRight, createSubbasinCombinations, identifyMinimumSubbasinCombination)
