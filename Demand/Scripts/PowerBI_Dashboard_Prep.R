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
    sort() %>% tail(1) %>%
    read_csv()
  
  
  
  # "DemandDataset_MonthlyValues" CSV
  monthlyDF <- paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], 
                      "_DemandDataset_MonthlyValues.csv") %>%
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
  
  
  
  # Create output files
  write_xlsx(list("Monthly_Demand" = monthlyDF),
             paste0("OutputData/", ws$ID, "_Monthly_Demand.xlsx"))
  
  
  
  write_xlsx(list("DWRAT_Allocations" = allocationsDF),
             paste0("OutputData/", ws$ID, "_DWRAT_Allocations.xlsx"))
  
  
  
  write_xlsx(list("AppID_List" = appDF),
             paste0("OutputData/", ws$ID, "_AppID_List.xlsx"))
  
  
  
  write_xlsx(list("MDT" = mdtDF),
             paste0("OutputData/", ws$ID, "_MDT.xlsx"))
  
  
  
  write_xlsx(list("PODs" = assignedDF %>% st_drop_geometry()),
             paste0("OutputData/", ws$ID, "_PODs.xlsx"))
  
  
  
  # Finally, generate a geopackage with layers useful to the visualization
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



generateGPKG <- function (ws, wsBound, assignedDF, huc12, catchDF, mdtDF) {
  
  # Create a geopackage file that contains the following layers:
  # (1) Watershed Boundary ('wsBound')
  # (2) Watershed PODs ('assingedDF')
  # (3) HUC-12 sub-basins ('huc12')
  # (4) Hydrologic Model NHD Catchments ('catchDF')
  # (5) Hydrologic Model NHD Flowlines (need to read in - watershed-specific!!!)
  
  
  
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
                       ws$SUBBASIN_FIELD_ID_NAMES %>%
                         str_split(";") %>% unlist() %>%
                         trimws() %>% head(1))
  
    
  
  # Keep only the column that uniquely identifies different flowlines
  flowLines <- flowLines %>%
    st_zm() %>%
    select(all_of(fieldName)) %>%
    rename(NHD_CAT := !!fieldName) %>%
    st_transform(st_crs(assignedDF))
  
  
  
  # Write these layers to a file
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
  
  
  
  # Output a completion message
  cat("\n\n")
  print("Check the 'OutputData' folder for the files to be used in the PowerBI dashboard!")
  
}



#### Script Execution ####

mainProcedure()



# Cleanup
remove(list = ls())
