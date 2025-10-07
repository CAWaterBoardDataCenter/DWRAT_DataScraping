# Prepare several spreadsheets for the watershed's PowerBI dashboard


remove(list = ls())


require(tidyverse)
require(readxl)
require(writexl)
require(sf)



mainProcedure <- function () {
  
  # This script will create several spreadsheets:
  #   (1) "[ID]_MDT.csv" (a modified CSV of the master demand table)
  #   (2) "[ID]_Monthly_Demand.csv" (a verticalized version of "[ID]_[YEAR]_[YEAR]_DemandDataset_MonthlyValues.csv")
  #   (3) "[ID]_DWRAT_Allocations.xlsx" (A spreadsheet with assigned allocations)
  #   (4) "[ID]_AppID_List.xlsx" (a spreadsheet with just application numbers)
  #   (5) "[ID]_PODs.xlsx" (a spreadsheet with geographic coordinates, sub-basin assignments, and HUC-12 assignments)
  
  # It will also generate a geopackage file with layers useful for the dashboard
  
  
  
  # Get the watershed and demand dataset range
  source("Scripts/Watershed_Selection.R")
  source("Scripts/Dataset_Year_Range.R")
  
  
  
  # Read in input files
  # (MDT, DemandDataset_MonthlyValues, catchment assignments)
  
  
  
  # Master Demand Table
  mdtDF <- list.files("OutputData/", 
                      pattern = paste0(ws$ID, "_", 
                                       yearRange[1], "_", yearRange[2], 
                                       "_MDT_"), 
                      full.names = "TRUE") %>%
    str_subset(if_else(is.na(ws$EXCLUDED_REPORTING_YEARS),
                       "_MDT_",
                       paste0("_Excluded_",
                              ws$EXCLUDED_REPORTING_YEARS %>%
                                str_split(";") %>% unlist() %>%
                                trimws() %>% 
                                as.numeric() %>% sort() %>% unique() %>%
                                paste0(collapse = "_")))) %>%
    sort() %>% tail(1) %>%
    read_csv()
  
  
  
  # "DemandDataset_MonthlyValues" CSV
  monthlyDF <- paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], 
                      "_DemandDataset_MonthlyValues",
                      if_else(is.na(ws$EXCLUDED_REPORTING_YEARS),
                              "",
                              paste0("_Excluded_",
                                     ws$EXCLUDED_REPORTING_YEARS %>%
                                       str_split(";") %>% unlist() %>%
                                       trimws() %>% 
                                       as.numeric() %>% sort() %>% unique() %>%
                                       paste0(collapse = "_"))),
                      ".csv") %>%
    read_csv()
  
  
  
  # Catchment/sub-basin assignments to each POD
  assignedDF <- getXLSX(ws, 
                     "IS_SHAREPOINT_PATH_SUBBASIN_ASSIGNMENT_SPREADSHEET",
                     "SUBBASIN_ASSIGNMENT_SPREADSHEET_PATH",
                     "SUBBASIN_ASSIGNMENT_WORKSHEET_NAME") %>% 
    filter(APPLICATION_NUMBER %in% mdtDF$APPLICATION_NUMBER) %>%
    select(APPLICATION_NUMBER, POD_ID, LONGITUDE, LATITUDE,
           all_of(ws$SUBBASIN_FIELD_ID_NAMES %>% 
                    str_split(";") %>%
                    unlist() %>% trimws() %>%
                    pluck(1)),
           ASSIGNED_MULTIPLE_SUBBASINS, ORIGINAL_ASSIGNMENT) %>%
    mutate(LONGITUDE2 = LONGITUDE, LATITUDE2 = LATITUDE) %>%
    st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = ws$POD_COORDINATES_REFERENCE_SYSTEM[1]) %>%
    st_transform("epsg:3488")
  
  
  
  # Read in two input layers as well
  # (HUC12 boundaries and watershed catchments)
  load("InputData/GIS_General/NHD_H_California_State_WBDHU12.RData") # Loads variable 'huc12'
  
  
  
  huc12 <- huc12 %>%
    st_transform(st_crs(assignedDF)) %>%
    mutate(name = name %>% str_remove("\\-.+$"))
  
  
  
  # Modified NHD Catchments (used in hydrologic model)
  catchDF <- getGIS(ws = ws, 
                    GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                    GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                    GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME") %>%
    st_transform(st_crs(assignedDF))
  
  
  
  
  # Also read in the eWRIMS Flat File and the watershed boundaries
  ewrimsDF <- read.csv("RawData/ewrims_flat_file.csv")
  
  
  
  wsBound <- getGIS(ws = ws, 
                    GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                    GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                    GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") %>%
    mutate(WATERSHED = ws$NAME) %>%
    select(WATERSHED) %>%
    st_transform(st_crs(assignedDF))
  
  
  
  # Start with initial edits 
  
  
  
  # Rename the catchment ID column in 'assignedDF" and 'catchDF'
  assignedDF <- assignedDF %>%
    rename(ASSIGNED_NHD_CAT = all_of(ws$SUBBASIN_FIELD_ID_NAMES %>% 
                                        str_split(";") %>% unlist() %>% pluck(1)))
  
  
  
  catchDF <- catchDF %>%
    rename(NHD_CAT = all_of(ws$SUBBASIN_FIELD_ID_NAMES %>% 
                              str_split(";") %>% unlist() %>% pluck(1)))
  
  
  
  # Do the same in 'mdtDF'
  mdtDF <- mdtDF %>%
    rename(ASSIGNED_NHD_CAT = BASIN)
  
  
  
  # Assign NHD catchments and HUC-12 sub-basins to each POD in 'assignedDF'
  assignedDF <- assignBasins(assignedDF, huc12, catchDF)
  
  
  
  # Reduce the size of 'huc12' to only ones relevant to the watershed
  huc12 <- huc12[st_intersects(st_buffer(wsBound, -5), huc12) %>%
          unlist(), ]
  
  
  
  # Clip the extent of the HUC12 boundaries to the watershed boundaries
  huc12 <- huc12 %>%
    st_intersection(wsBound)
  
  
  
  # Assign a HUC12 basin to 'mdtDF'
  mdtDF <- mdtDF %>%
    group_by(APPLICATION_NUMBER) %>%
    mutate(ASSIGNED_HUC12 = assignedDF$HUC12[assignedDF$ASSIGNED_NHD_CAT == ASSIGNED_NHD_CAT &
                                               assignedDF$APPLICATION_NUMBER == APPLICATION_NUMBER] %>% 
             unique() %>% pluck(1)) %>%
    ungroup()
  
  
  
  # Assign HUC12 sub-basins to 'catchDF' as well
  catchDF[["HUC12"]] <- huc12$huc12[st_intersects(st_centroid(catchDF), huc12) %>% 
                                      unlist()]
  
  
  
  catchDF[["HUC12_NAME"]] <- huc12$name[catchDF$HUC12 %>%
                                          map_int(~ which(. == huc12$huc12))]
    
  
  
  # Alter the formatting of 'monthlyDF'
  # Make each row contain data for only one water right/diversion type/month
  monthlyDF <- verticalizeData(monthlyDF)
  
  
  
  # Create data frames for the other two output files
  
  
  
  # One file contains just the "APPLICATION_NUMBER" values that appear in 'mdtDF'
  appDF <- mdtDF %>%
    select(APPLICATION_NUMBER) %>%
    unique() %>% arrange()
  
  
  
  # Create an allocations variable with dummy data
  message("\n\nThe allocations table contains dummy data!\n\n")
  allocationsDF <- mdtDF %>%
    select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE_SUB, ASSIGNED_HUC12,
           JAN_MEAN_DIV) %>%
    rename(PRIORITY = ASSIGNED_PRIORITY_DATE_SUB,
           BASIN = ASSIGNED_HUC12,
           DEMAND = JAN_MEAN_DIV) %>%
    mutate(Month = as.Date("2025-01-01", format = "%Y-%m-%d"),
           ALLOCATIONS = DEMAND,
           Curtailment = 0,
           `SHORTAGE %` = "0%") %>%
    select(APPLICATION_NUMBER, ALLOCATIONS, Curtailment, DEMAND,
           `SHORTAGE %`, BASIN, PRIORITY, Month)
  
  
  
  # Perform operations on the datasets to add potentially useful columns
  allocationsDF <- allocationsDF %>%
    mutate(`Demand - Allocations` = DEMAND - ALLOCATIONS,
           YYYYMM = if_else(is.na(ALLOCATIONS), NA, 
                            paste0(year(Month),
                                   if_else(month(Month) < 10, "0", ""),
                                   month(Month))),
           Current_Month = if_else(Month == max(Month), "Current", "Historical")) %>%
    mutate(Current_Allocations = if_else(Current_Month == "Current", ALLOCATIONS, NA_real_),
           Historical_Allocations = if_else(Current_Month == "Historical", ALLOCATIONS, NA_real_))
  
  
  
  monthlyDF <- monthlyDF %>%
    left_join(mdtDF %>% select(APPLICATION_NUMBER, PRIMARY_USE),
              by = "APPLICATION_NUMBER", relationship = "many-to-one") %>%
    rename(Primary_Use_Raw = PRIMARY_USE) %>%
    mutate(Primary_Use = if_else(is.na(Primary_Use_Raw), "Other",
                                       if_else(grepl("Municipal", Primary_Use_Raw, ignore.case = TRUE),
                                               "Municipal", Primary_Use_Raw)),
           Date = paste0(YEAR, "-", MONTH, "-01") %>%
             as.Date(format = "%Y-%m-%d"))
  
  
  
  # Add a label to 'huc12' that combines the ID and Name into one string
  huc12 <- huc12 %>%
    group_by(huc12) %>%
    mutate(Subwatershed_ID = paste0(huc12, " - ", name %>%
                                      str_split("\\-") %>% unlist() %>% 
                                      trimws() %>% pluck(1))) %>%
    ungroup()
  
  
  
  # In 'appDF', 
  # Add a version of "APPLICATION_NUMBER" without the split labels (e.g., "_1")
  appDF <- appDF %>%
    mutate(AppID_Trim = APPLICATION_NUMBER %>%
             str_remove("_[0-9]$"))
  
  
  
  # Append data to 'appDF' from 'eWRIMSDF'
  appDF <- appDF %>%
    left_join(ewrimsDF %>% select(APPLICATION_NUMBER, PRIMARY_OWNER_NAME, CURRENT_STATUS,
                                  SUB_TYPE) %>%
                rename(AppID_Trim = APPLICATION_NUMBER),
              by = "AppID_Trim", relationship = "many-to-one") %>%
    rename(Primary_Owner = PRIMARY_OWNER_NAME,
           Status = CURRENT_STATUS,
           SubType_FlatFile = SUB_TYPE)
  
  
  
  # Add data from 'allocationsDF' to 'appDF' as well
  appDF <- appDF %>%
    left_join(allocationsDF %>% filter(Current_Month == "Current") %>% 
                select(APPLICATION_NUMBER, PRIORITY, `Demand - Allocations`),
              by = "APPLICATION_NUMBER", relationship = "one-to-one") %>%
    rename(Priority_Pull = PRIORITY) %>%
    mutate(Concat_AppID_PrimaryOwner = paste0(AppID_Trim, "_", Primary_Owner),
           `Current Month Shortage` = round(`Demand - Allocations`, digits = 2)) %>%
    mutate(`Shorted?` = if_else(Status %in% c("Cancelled", "Inactive", "Revoked"),
                                              "Right Inactive",
                                              if_else(is.na(`Current Month Shortage`),
                                                      NA_character_,
                                                      if_else(`Current Month Shortage` > 0,
                                                              "Yes", "No"))))
  
  
  
  # Add water right information from 'mdtDF' to 'appDF' as well
  appDF <- appDF %>%
    left_join(mdtDF %>% select(APPLICATION_NUMBER, WATER_RIGHT_TYPE),
              by = "APPLICATION_NUMBER", relationship = "one-to-one") %>%
    rename(RightType_FlatFile = WATER_RIGHT_TYPE)
  
  
  
  # Define additional columns in 'appDF'
  appDF <- appDF %>%
    mutate(`App/Rip` = if_else(grepl("Statement", RightType_FlatFile, ignore.case = TRUE) &
                                 grepl("Riparian", SubType_FlatFile, ignore.case = TRUE),
                               "Riparian", "Appropriative"),
           Priority_Year = substr(Priority_Pull, 1, 4) %>% as.numeric(),
           Priority_Month = substr(Priority_Pull, 5, 6) %>% as.numeric(),
           Priority_Day = substr(Priority_Pull, 7, 8) %>% as.numeric()) %>%
    mutate(Priority_Date = if_else(is.na(Priority_Pull), NA,
                                   if_else(Priority_Pull == 10000000,
                                           as.Date("1000-01-01", format = "%Y-%m-%d"),
                                           paste(Priority_Year, 
                                                 Priority_Month, 
                                                 Priority_Day, sep = "-") %>%
                                             as.Date(format = "%Y-%m-%d"))))
  
  
  
  # Join Primary Use data form 'monthlyDF' to 'appDF' and 'assignedDF'
  appDF <- appDF %>%
    left_join(monthlyDF %>%
                select(APPLICATION_NUMBER, Primary_Use) %>%
                unique(),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  assignedDF <- assignedDF %>%
    left_join(monthlyDF %>%
                select(APPLICATION_NUMBER, Primary_Use) %>%
                unique(),
              by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  
  # Add HUC12 assignment information to 'assignedDF'
  assignedDF <- assignedDF %>%
    left_join(catchDF %>% 
                st_drop_geometry() %>%
                select(NHD_CAT, HUC12, HUC12_NAME) %>%
                rename(ASSIGNED_NHD_CAT = NHD_CAT,
                       ASSIGNED_HUC12 = HUC12,
                       ASSIGNED_HUC12_NAME = HUC12_NAME),
              by = "ASSIGNED_NHD_CAT",
              relationship = "many-to-one")
  
  
  
  # Add this information to 'appDF' and 'monthlyDF'
  appDF <- appDF %>%
    left_join(assignedDF %>%
                st_drop_geometry() %>%
                select(APPLICATION_NUMBER, ASSIGNED_HUC12, ASSIGNED_HUC12_NAME) %>%
                unique(),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  monthlyDF <- monthlyDF %>%
    left_join(appDF %>%
                select(APPLICATION_NUMBER, ASSIGNED_HUC12, ASSIGNED_HUC12_NAME),
              by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  
  # Create a catchment-focused spreadsheet
  # (The connectivity matrix spreadsheet is required for this)
  connMat <- getXLSX(ws,
                     "IS_SHAREPOINT_PATH_CONNECTIVITY_MATRIX_SPREADSHEET",
                     "CONNECTIVITY_MATRIX_SPREADSHEET_PATH",
                     "CONNECTIVITY_MATRIX_WORKSHEET_NAME")
  
  
  
  # Rename the first column of 'connMat' to "BASIN"
  names(connMat)[1] <- "BASIN"
  
  
  
  # For each catchment, identify its final outlet catchment (and HUC-12 sub-basin)
  outletDF <- findOutlets(catchDF, connMat)
  
  
  
  # Create output files
  write_csv(monthlyDF %>%
              select(APPLICATION_NUMBER, YEAR, MONTH, TYPE, DIVERSION),
            paste0("OutputData/", ws$ID, "_Monthly_Demand.csv"))
  
  
  
  write_xlsx(list("Sheet1" = allocationsDF %>%
                    select(APPLICATION_NUMBER, ALLOCATIONS,
                           Curtailment, DEMAND, `SHORTAGE %`,
                           BASIN, PRIORITY, Month) %>%
                    rename(`Application ID` = APPLICATION_NUMBER)),
             paste0("OutputData/", ws$ID, "_DWRAT_Allocations.xlsx"))
  
  
  
  write_xlsx(list("Sheet1" = appDF %>%
                    select(APPLICATION_NUMBER)),
             paste0("OutputData/", ws$ID, "_AppID_List.xlsx"))
  
  
  
  write_csv(mdtDF %>%
              mutate(BASIN = ASSIGNED_NHD_CAT) %>%
              select(APPLICATION_NUMBER, all_of(contains("_MEAN_DIV")), 
                     TOTAL_EXPECTED_ANNUAL_DIVERSION, TOTAL_MAY_SEPT_DIV,
                     WATER_RIGHT_TYPE, WATER_RIGHT_STATUS, PRIMARY_OWNER_TYPE,
                     APPLICATION_PRIMARY_OWNER, SOURCE_NAME, TRIB_DESC, WATERSHED,
                     PRIMARY_USE, `FULLY NON-CONSUMPTIVE`, POWER_DEMAND_ZEROED,
                     ASSIGNED_PRIORITY_DATE_SUB, ASSIGNED_PRIORITY_DATE_SOURCE,
                     PRE_1914, RIPARIAN, APPROPRIATIVE, FACE_VALUE_AMOUNT_AF,
                     INI_REPORTED_DIV_AMOUNT_AF, NULL_DEMAND, PERCENT_FACE,
                     ZERO_DEMAND, ORIGINAL_APPLICATION_NUMBER, BASIN),
            paste0("OutputData/", ws$ID, "_MDT.csv"))
  
  
  
  write_xlsx(list("TD_PODs" = assignedDF %>% 
                    st_drop_geometry() %>%
                    select(APPLICATION_NUMBER, POD_ID,
                           LATITUDE, LONGITUDE, HUC12,
                           HUC12_NAME, NHD_CAT)),
             paste0("OutputData/", ws$ID, "_PODs.xlsx"))
  
  
  
  write_xlsx(list("Out_Cat" = outletDF %>%
                    st_drop_geometry() %>%
                    select(NHD_CAT, HUC12, HUC12_NAME, NHD_OUTLET, HUC12_OUTLET,
                           NHD_DOWNSTREAM, HUC12_DOWNSTREAM)),
             paste0("OutputData/", ws$ID, "_Catchments.xlsx"))
  
  
  
  # Finally, generate a geopackage with layers useful to the visualization
  # (Metadata will appear in an accompanying HTML file)
  generateGPKG(ws, wsBound, assignedDF, huc12, catchDF, mdtDF)
  
  
}



assignBasins <- function (assignedDF, huc12, catchDF) {
  
  # Based on coordinate overlap, assign HUC12 values and NHD catchments to 'assignedDF'
  
  
  
  # Assign "NHD_CAT" and "HUC12" to 'assignedDF'
  # (The catchment and HUC12 IDs for sub-basins that the POD overlaps with)
  assignedDF <- assignedDF %>%
    mutate(NHD_CAT = catchDF$NHD_CAT[st_intersects(assignedDF, catchDF) %>% unlist()]) %>%
    mutate(HUC12 = huc12$huc12[st_intersects(assignedDF, huc12) %>% unlist()])
  
  
  
  # Use the "HUC12" column to also append the HUC12 name to 'assignedDF'
  assignedDF <- assignedDF %>%
    left_join(huc12 %>% 
                st_drop_geometry() %>%
                select(huc12, name) %>%
                rename(HUC12 = huc12, HUC12_NAME = name),
              by = "HUC12", relationship = "many-to-one")
  
  
  
  # Verify that catchments and HUC12 subbasins were successfully assigned
  # to every POD in 'assignedDF'
  stopifnot(!anyNA(assignedDF$NHD_CAT))
  stopifnot(!anyNA(assignedDF$HUC12))
  
  
  
  # Return 'assignedDF'
  return(assignedDF)
  
}



verticalizeData <- function (inputDF) {
  
  # Given the "DemandDataset_MonthlyValues" tibble,
  # reformat the data to have one month/diversion type per row
  
  
  
  # Get the names of the monthly storage and direct diversion columns
  colsToFlatten <- names(inputDF) %>%
    str_subset(paste0(toupper(month.abb), collapse = "|")) %>%
    str_subset("STORAGE|DIRECT")
  
  
  
  stopifnot(length(colsToFlatten) == 24)
  
  
  
  for (i in 1:length(colsToFlatten)) {
    
    extractDF <- inputDF %>%
      select(APPLICATION_NUMBER, YEAR, all_of(colsToFlatten[i])) %>%
      mutate(MONTH = if_else(i <= 12, i, i - 12),
             TYPE = if_else(grepl("DIRECT", colsToFlatten[i]), "DIRECT", "STORAGE")) %>%
      rename(DIVERSION = !! colsToFlatten[i])
    
    
    
    if (i == 1) {
      combinedDF <- extractDF
    } else {
      combinedDF <- bind_rows(combinedDF, extractDF)
    }
    
    
  }
  
  
  
  # Adjust the formatting of 'combinedDF'
  combinedDF <- combinedDF %>%
    relocate(DIVERSION, .after = TYPE) %>%
    arrange(APPLICATION_NUMBER, YEAR, MONTH, TYPE) %>%
    filter(!is.na(DIVERSION))
  
  
  
  return(combinedDF)
  
}



findOutlets <- function (catchDF, connMat) {
  
  # Find the outlet catchment for each catchment
  # Note that outlet catchment and its corresponding HUC-12 subbasin
  # Also prepare a string of every downstream catchment and downstream HUC-12 subbasin
  
  
  
  # Create new columns in 'catchDF' to identify the outlet catchment and HUC-12 IDs
  outletDF <- catchDF %>%
    mutate(NHD_OUTLET = NA_real_,
           HUC12_OUTLET = NA_real_,
           NHD_DOWNSTREAM = NA_character_,
           HUC12_DOWNSTREAM = NA_character_)
  
  
  
  # Iterate through the catchments in 'outletDF'
  for (i in 1:nrow(outletDF)) {
    
    
    # Find the row in 'connMat' that has this iteration's catchment ID
    rowIndex <- which(connMat$BASIN == outletDF$NHD_CAT[i])
    
    
    
    stopifnot(length(rowIndex) == 1)
    
    
    
    # Identify the columns with non-zero values in this row of 'connMat'
    # (Get their catchment IDs, which are stored as column names)
    # (Don't let the "BASIN" name appear in this vector)
    nonzeroCols <- names(connMat)[which(connMat[rowIndex, ] > 0)] %>%
      str_subset("^BASIN$", negate = TRUE) %>%
      as.numeric()
    
    
    
    # Get the row sum for each catchment
    catchRowSums <- connMat[, -1] %>% rowSums()
    
    
    
    # The catchment that is the outlet should not drain into any other catchment
    # That means that its row sum would be equal to 1 (since it only drains into itself)
    
    
    
    # Among the catchments in 'nonzeroCols' find the one with a row sum equal to 1
    # (There should only be 1)
    outCatch <- connMat$BASIN[which(connMat$BASIN %in% nonzeroCols & catchRowSums == 1)]
    
    
    
    # There should be exactly one match
    stopifnot(length(outCatch) == 1)
    
    
    
    # Update 'outletDF' accordingly
    outletDF$NHD_OUTLET[i] <- outCatch
    outletDF$HUC12_OUTLET[i] <- outletDF$HUC12[outletDF$NHD_CAT == outCatch]
    
    
    
    # Also get the HUC-12 subbasins of each of the catchments in 'nonzeroCols'
    nonzeroCols_HUC <- nonzeroCols %>%
      map(~ outletDF$HUC12[outletDF$NHD_CAT == .]) %>%
      unlist()
    
    
    
    # Input 'nonzeroCols' and 'nonzeroCols_HUC' as comma-separated strings into 'outletDF'
    outletDF$NHD_DOWNSTREAM[i] <- paste0("\"", nonzeroCols, "\"", collapse = ",")
    outletDF$HUC12_DOWNSTREAM[i] <- paste0("\"", nonzeroCols_HUC, "\"", collapse = ",")
    
  }
  
  
  
  # Return 'outletDF'
  return(outletDF)
  
}



generateGPKG <- function (ws, wsBound, assignedDF, huc12, catchDF, mdtDF) {
  
  # Create a geopackage file that contains the following layers:
  # (1) Watershed Boundary ('wsBound')
  # (2) Watershed PODs ('assingedDF')
  # (3) HUC-12 sub-basins ('huc12')
  # (4) Hydrologic Model NHD Catchments ('catchDF')
  # (5) Hydrologic Model NHD Flowlines (need to read in - watershed-specific!!!)
  # (6) Watershed Mask
  
  # (In an accompanying function, metadata will be generated for these layers)
  
  
  
  # Read in NHD Flowlines
  flowLines <- getGIS(ws,
                      "IS_SHAREPOINT_PATH_NHD_FLOWLINES",
                      "NHD_FLOWLINES_DATABASE_PATH",
                      "NHD_FLOWLINES_LAYER_NAME")
  
  
  #flowLines <- st_read("C:/Users/aprashar/Water Boards/Supply and Demand Assessment - Documents/Program Watersheds/1. Watershed Folders/Gualala River/Data/GIS/Delineations/Gualala_Flowlines/",
  #                     layer = "Gualala_Flowlines")
  
  #flowLines <- st_read("C:/Users/aprashar/Water Boards/Supply and Demand Assessment - Documents/Program Watersheds/1. Watershed Folders/Navarro River/Data/GIS Datasets/NHDPlus_Delineations/LSPC_Delineations",
  #                     layer = "NHDFlowline_EditedforLSPC")
  
  
  
  # Get the name of the column that uniquely identifies each catchment
  # (It's usually called "COMID")
  fieldName <- if_else("COMID" %in% names(flowLines),
                       "COMID",
                       if_else("reachcode" %in% names(flowLines),
                               "reachcode",
                               ws$SUBBASIN_FIELD_ID_NAMES %>%
                                 str_split(";") %>% unlist() %>%
                                 trimws() %>% head(1)))
  
    
  
  # Keep only the column that uniquely identifies different flowlines
  flowLines <- flowLines %>%
    st_zm() %>%
    select(all_of(fieldName)) %>%
    rename(NHD_CAT := !!fieldName) %>%
    st_transform(st_crs(assignedDF))
  
  
  
  # Prepare the watershed mask layer next
  # Use 'wsBound' and a generic rectangle that covers California to create a mask layer
  # (The mask layer will be the rectangle with the watershed's polygon subtracted out)
  
  
  
  # Create the generic rectangle layer
  universalMask <- c(-131.8766, 50.95556, 
                     -105.8087, 50.89833, 
                     -105.8659, 24.83043, 
                     -131.9338, 24.88766, 
                     -131.8766, 50.95556) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    data.frame() %>%
    st_as_sf(coords = 1:2, crs = "epsg:4269") %>%
    summarize(geometry = st_combine(geometry)) %>% 
    st_cast("POLYGON") %>%
    st_transform(st_crs(wsBound))
  
  
  
  # Create the mask layer for the watershed
  # (The mask layer will contain no fields except for "geometry" and an ID column)
  wsMask <- st_difference(universalMask, wsBound) %>%
    select(geometry) %>%
    mutate(FEATUREID = 1) %>%
    select(FEATUREID, geometry)
  
  
  
  # Write all of these layers to a file
  st_write(wsBound,
           paste0("OutputData/", ws$ID, "_GIS_Layers.gpkg"), 
           layer = "Watershed_Boundary",
           append = FALSE)
  
  
  
  # "ASSIGNED_HUC12" and "ASSIGNED_NHD_CAT" are appended before writing 'assignedDF'
  st_write(assignedDF %>%
             select(POD_ID, APPLICATION_NUMBER,
                    HUC12, HUC12_NAME, NHD_CAT, 
                    ASSIGNED_HUC12, ASSIGNED_HUC12_NAME,
                    ASSIGNED_NHD_CAT),
           paste0("OutputData/", ws$ID, "_GIS_Layers.gpkg"),
           layer = "Water_Rights",
           append = FALSE)
  
  
  
  st_write(huc12 %>%
             select(huc12, name) %>%
             rename(HUC12 = huc12,
                    HUC12_NAME = name),
           paste0("OutputData/", ws$ID, "_GIS_Layers.gpkg"),
           layer = "HUC12_Subbasins",
           append = FALSE)
  
  
  
  # Ensure that the catchments layer is valid
  if (sum(st_is_valid(catchDF)) != nrow(catchDF)) {

    catchDF <- catchDF %>%
      st_make_valid()

  }
  
  
  
  st_write(catchDF %>%
             select(NHD_CAT, HUC12, HUC12_NAME),
           paste0("OutputData/", ws$ID, "_GIS_Layers.gpkg"),
           layer = "Hydro_Model_NHD_Catchments",
           append = FALSE)
  
  
  
  st_write(flowLines,
           paste0("OutputData/", ws$ID, "_GIS_Layers.gpkg"),
           layer = "Hydro_Model_NHD_Flowlines",
           append = FALSE)
  
  
  
  st_write(wsMask,
           paste0("OutputData/", ws$ID, "_GIS_Layers.gpkg"),
           layer = "Watershed_Mask",
           append = FALSE)
  
  
  
  # Generate metadata about these layers next
  generateMetadata(ws)
  
  
  
  # Output a completion message
  cat("\n\n")
  print("Check the 'OutputData' folder for the files to be used in the PowerBI dashboard!")
  
}



generateMetadata <- function (ws) {
  
  # Produce metadata for the resultant geopackage of this script
  # (This information will be useful for making the dataset public)
  # The output will be an HTML file
  
  
  
  # Check first if the metadata document already exists
  # (Delete it if this is the case)
  if (file.exists(paste0("OutputData/", ws$ID, "_GIS_Layers_Metadata.html"))) {
    
    unlink(paste0("OutputData/", ws$ID, "_GIS_Layers_Metadata.html"))
    
  }
  
  
  
  # Initiate a new HTML document
  htmlVec <- c("<!DOCTYPE html>",
               "",
               "<html lang=\"en\">",
               "",
               "<head>",
               "",
               "<meta charset=\"UTF-8\">",
               "",
               "<title>Watershed GIS Metadata</title>",
               "",
               "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
               "",
               "<style>",
               "",
               "body {",
               "  font-family: \"Avenir Next W01\", \"Avenir Next W00\", \"Avenir Next\", \"Avenir\", \"Helvetica Neue\", sans-serif;",
               "  font-size: 12pt;", 
               "}",
               "",
               "h2 {",
               "  margin-top: 36pt;",
               "}",
               "",
               ".hintText {",
               "  color: blue;",
               "}",
               "",
               "</style>",
               "",
               "</head>",
               "",
               "<body>",
               "",
               "<h1>Metadata for ArcGIS Portal</h1>",
               paste0("<p>",
                      paste0("Instructions: Copy the text below into each of the ",
                             "corresponding sections on Portal!"),
                      "</p>"),
               paste0("<a href = ",
                      "\"https://gispublic.waterboards.ca.gov/portal/home/content.html#my\" ",
                      "target = \"_blank\">",
                      "Link to Portal Content",
                      "</a>"),
               # Title
               "<h2>Title</h2>",
               paste0("<p>", paste0(ws$NAME, " Watershed GIS Layers"), "</p>"),
               "",
               # Summary
               "<h2>Summary</h2>",
               paste0("<p>", 
                      paste0("This GIS dataset includes a comprehensive set of ",
                             "spatial layers prepared for modeling and analysis of ",
                             "water supply, demand, and availability within the ",
                             ws$NAME, " ",
                             "watershed. The dataset integrates surface hydrology, ",
                             "water right diversion points, and watershed boundaries ",
                             "to support technical assessments by the California State ",
                             "Water Board’s Supply & Demand Assessment Unit (SDA)."),
                      "</p>"),
               "",
               # Description
               "<h2>Description</h2>",
               paste0("<p>",
                      paste0("This dataset comprises the core spatial components ",
                             "used in the ", ws$NAME, " watershed modeling framework."),
                      "</p>"),
               paste0("<p>",
                      paste0("It includes: "),
                      "</p>"),
               # Ordered List of Layer Descriptions
               paste0("<ol>",
                      paste0("<li>",
                             "<b>Water Rights Point of Diversion</b>: ",
                             "Points of diversion for water rights that are currently ",
                             "active in the watershed. The original source is ",
                             "the Electronic Water Rights Information Management System ",
                             "(eWRIMS), which is maintained by the State Water Board.",
                             "</li>"),
                      paste0("<li>",
                             "<b>NHD Flowlines</b>: ",
                             "Modified surface water features (rivers, streams, ",
                             "channels) from the National Hydrography Dataset (NHD). ",
                             "In the SDA hydrologic modeling procedure, one major ",
                             "flowline is assigned to each catchment in the watershed.",
                             "</li>"),
                      paste0("<li>",
                             "<b>HUC-12 Subbasins</b>: ",
                             "Subdivisions from the Hydrologic Unit Code (HUC) dataset ",
                             "that enable a more granular regional analysis within a ",
                             "watershed. Wherever applicable, the HUC-12 subbasins ",
                             "are clipped to the extent of the California land mass.",
                             "</li>"),
                      paste0("<li>",
                             "<b>NHD Catchments</b>: ",
                             "Fine-scale hydrologic units that support water balance ",
                             "modeling and water availability analysis. The catchments ",
                             "are sometimes split or combined to better suit the ",
                             "watershed's modeling needs, so this layer may differ from ",
                             "the original NHD source.",
                             "</li>"),
                      paste0("<li>",
                             "<b>Watershed Boundary</b>: ",
                             "Delineation of the modeled watershed boundaries. This ",
                             "polygon is derived from the official HUC boundaries ",
                             "maintained by the United States Geological Survey (USGS).",
                             "</li>"),
                      paste0("<li>",
                             "<b>Watershed Mask</b>: ",
                             "A polygon used to spatially constrain modeling and ",
                             "analysis to the watershed basin extent.",
                             "</li>"),
                      "</ol>"),
               paste0("<p>",
                      paste0("This suite of data is used by the California State Water ",
                             "Resources Control Board (SWRCB) for scenario modeling, ",
                             "hydrologic assessment, water rights evaluation, and drought ",
                             "response planning. It supports coordination across State ",
                             "agencies, local stakeholders, and the public in efforts to ",
                             "manage limited surface water resources."),
                      "</p>"),
               paste0("<p>",
                      paste0("For more information, please contact the Supply & Demand ",
                             "Assessment Unit (",
                             paste0("<a href = \"mailto:DWR-SDA@waterboards.ca.gov\">",
                                    "DWR-SDA@waterboards.ca.gov",
                                    "</a>"),
                             ")."),
                      "</p>"),
               "",
               # Topic Categories
               "<h2>Topic Categories</h2>",
               paste0("<ol>",
                      paste0("<li>",
                             "Division of Water Rights",
                             "</li>"),
                      paste0("<li>",
                             "Boundaries",
                             "</li>"),
                      paste0("<li>",
                             "Inland Waters",
                             paste0("<ul>",
                                    "<li>",
                                    "Choose \"Rivers and Streams\"!",
                                    "</li>",
                                    "</ul>"),
                             "</li>"),
                      "</ol>"),
               paste0("<p class = \"hintText\">",
                      paste0("<i>Hint</i>: You'll have to input each category ",
                             "one-by-one. Click on the label that appears after ",
                             "pasting a text string into the \"Assign ",
                             "Category\" box!"),
                      "</p>"),
               "",
               # Tags
               "<h2>Tags</h2>",
               paste0("<p>",
                      paste0(ws$NAME, ", SDA, Water, Watershed, Surface Water, ",
                             "NHD, HUC, eWRIMS, ",
                             "California, CA, State Water Resources Control Board, SWRCB, ",
                             "Division of Water Rights, DWR, Supply, Demand"),
                      "</p>"),
               paste0("<p class = \"hintText\">",
                      paste0("<i>Hint</i>: These can be entered all at once! Triple-click ",
                             "the above line to select all of the tags at once! Press ",
                             "\"Enter\" after pasting the above tags into ",
                             "the \"Edit Tags\" section!"),
                      "</p>"),
               "",
               # Credits
               "<h2>Credits (Attribution)</h2>",
               paste0("<p>",
                      paste0("The USGS is the original source of the boundaries ",
                             "and NHD data. The SWRCB Division of Water Rights is ",
                             "the original source for information on California's water ",
                             "rights and their PODs. Modifications to the NHD catchments ",
                             "and flowlines were performed in conjunction with Paradigm ",
                             "Environmental."),
                      "</p>"),
               "",
               # Terms of Use
               "<h2>Terms of Use</h2>",
               paste0("<p>",
                      paste0("This dataset is provided for planning, assessment, and coordination purposes only. ",
                      "It is not a legal survey product and cannot be used to establish legal standing. ",
                      "While reasonable efforts have been made to ensure the accuracy of the data, ",
                      "no warranty is made regarding its completeness, timeliness, or accuracy."),
                      "</p>"),
               paste0("<p>",
                      paste0("By using this data, the user acknowledges these limitations and ",
                             "assumes full responsibility for any conclusions or decisions derived from its use."),
                      "</p>"),
               "",
               "<h2>HTML Info (Don't Copy This)</h2>",
               paste0("<p>",
                      paste0("This file was generated on: ", Sys.time()),
                      "</p>"),
               "</body>",
               "",
               "</html>")
  
  
  
  # Save the HTML to a file
  write_lines(htmlVec, paste0("OutputData/", ws$ID, "_GIS_Layers_Metadata.html"))
  
}



#### Script Execution ####

mainProcedure()



# Cleanup
remove(list = ls())
