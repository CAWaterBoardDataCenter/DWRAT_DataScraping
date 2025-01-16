# This script is a recreation of the Excel file "QAQC_Working_File.xlsx"


# Data from the previous Excel modules will be compiled here
# The end result will be a spreadsheet with two worksheets ("DiversionData" and "MasterDemandTable")


cat("Starting 'MasterDemandTable.R'...\n")


source("Scripts/Watershed_Selection.R")
source("Scripts/Dataset_Year_Range.R")


#### Dependencies ####

require(tidyverse)
require(openxlsx)
require(readxl)


# Spreadsheet Adjustment function----

# Remove the first two rows from 'sheetDF'
# The second removed row will also be used as the column names of the DF
spreadsheetAdjustment <- function (sheetDF) {
  
  sheetDF <- sheetDF[-c(1:2), ] %>%
    set_names(sheetDF[2, ] %>% unlist() %>% as.vector())
  
  # Return 'sheetDF'
  return(sheetDF)
}


# Assign Basin Data Function----
assignBasinData_RR <- function (ewrimsDF) {
  
  # 'ewrimsDF' will be updated to contain columns related to the right's subbasin and lat/long coordinates:
      # BASIN, MAINSTEM, LATITUDE, LONGITUDE
  
  # More than one input source will be used to supply this data in 'ewrimsDF'
  
  
  # Start by using "RUSSIAN_RIVER_DATABASE_2022.xlsx"
  # This file originated from the DWRAT GitHub repository as "RUSSIAN_RIVER_DATABASE_2022.csv"
  # It contains data used with the original demand dataset and methodology
  # (Note: This spreadsheet has one row per unique "APPLICATION_NUMBER" value)
  
  rrDF <- read_xlsx("InputData/RUSSIAN_RIVER_DATABASE_2022.xlsx", sheet = "in") %>%
    select(APPLICATION_NUMBER, BASIN, MAINSTEM, LONGITUDE, LATITUDE)
  
  
  # Based on the value of "APPLICATION_NUMBER", join this data to 'ewrimsDF'
  ewrimsDF <- ewrimsDF %>%
    left_join(rrDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  # Several rights had multiple PODs but only one POD's data has to be selected
  # All but one case already had a selection made (in "RUSSIAN_RIVER_DATABASE_2022.xlsx")
  # That case (and others with missing data) were manually assigned data in the 
  # manual review spreadsheet "Missing_MainStem_GIS_Manual_Assignment.xlsx"
  
  
  # Create the manual review spreadsheet and then import it into ArcGIS Pro and visually inspect which PODs
  # fall on or near the main stem of the Russian River
  
  ## Requires new code snippet --Payman to add later; the entire main stem assignment process can be done in R
  # if we import the appropriate layers and projections; side project for Payman
  
  #After filling the manual review spreadsheet, import it back into R
  manualDF <- getXLSX(ws = ws, 
                      SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_MANUAL_ASSIGNMENT",
                      FILEPATH ="SUBBASIN_MANUAL_ASSIGNMENT_SPREADSHEET_PATH",
                      WORKSHEET_NAME = "SUBBASIN_MANUAL_ASSIGNMENT_WORKSHEET_NAME") %>%
    filter(APPLICATION_NUMBER %in% ewrimsDF$APPLICATION_NUMBER[is.na(ewrimsDF$MAINSTEM)])
  
  
  # Iterate through 'manualDF' and apply these values to 'ewrimsDF'
  for (i in 1:nrow(manualDF)) {
    
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$BASIN <- manualDF$BASIN[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$LATITUDE <- manualDF$LATITUDE[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$LONGITUDE <- manualDF$LONGITUDE[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$MAINSTEM <- manualDF$MAINSTEM[i]
  }
  
  # Finally, rely on the output of "Assign_Subbasin_to_POD.R" and the initial POD spreadsheet
  # The initial spreadsheet has mainstem information about the PODs
  # The POD Subbasin Assignment spreadsheet has subbasin information
  podDF <- getXLSX(ws = ws, 
                   SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
                   FILEPATH = "POD_COORDINATES_SPREADSHEET_PATH",
                   WORKSHEET_NAME = "POD_COORDINATES_WORKSHEET_NAME") %>%
    filter(APPLICATION_NUMBER %in% ewrimsDF$APPLICATION_NUMBER[is.na(ewrimsDF$MAINSTEM)]) %>%
    left_join(read_xlsx("OutputData/RR_POD_Subbasin_Assignment.xlsx") %>%
                select(-LONGITUDE, -LATITUDE), by = c("APPLICATION_NUMBER", "POD_ID"),
              relationship = "one-to-one")
  
  # This procedure will not work if the remaining rights have multiple PODs
  stopifnot(nrow(podDF) == length(unique(podDF$APPLICATION_NUMBER)))
  
  
  # Iterate through 'podDF' and apply these values to 'ewrimsDF'
  for (i in 1:nrow(podDF)) {
    
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i], ]$BASIN <- paste0("R_", 
                                                                                           if_else(podDF$Basin_Num[i] < 10, "0", ""), 
                                                                                           podDF$Basin_Num[i], 
                                                                                           if_else(podDF$MAIN_STEM[i] == "Y", "_M", ""))
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i], ]$LATITUDE <- podDF$LATITUDE[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i], ]$LONGITUDE <- podDF$LONGITUDE[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i], ]$MAINSTEM <- podDF$MAIN_STEM[i]
  }
  
  
  # Check for errors
  stopifnot(!anyNA(ewrimsDF$BASIN))
  stopifnot(!anyNA(ewrimsDF$MAINSTEM))
  stopifnot(!anyNA(ewrimsDF$LONGITUDE))
  stopifnot(!anyNA(ewrimsDF$LATITUDE))
  
  
  # Return 'ewrimsDF'
  return(ewrimsDF)
  
}



colAdd <- function (col1, col2) {
  
  # This function adds two numeric columns together
  # However, it handles NA values in specific ways
  
  # If both column values are not NA, a simple sum is returned
  # If one column value is NA, the other column value is returned
  # If both column values are NA, NA is returned
  
  
  
  # If 'col1' is NA, return the value of 'col2' 
  # (This is fine regardless of whether 'col2' is NA or not because the above requirements
  #  will still be satisfied either way)
  # If 'col1' is NOT NA, check 'col2'
  # If 'col2' is NA, return 'col1' (the same reasoning from above applies here)
  # If 'col2' is NOT NA, then both column values are not NA, so their sum should be returned
  return(if_else(is.na(col1), col2,
                 if_else(is.na(col2), col1, col1 + col2)))
  
}



colMean <- function (colData) {
  
  # Given the data for a numeric column, compute the mean
  # This is written as a custom function to ensure that NA values are handled in a specific way
  
  
  # If all of a right's diversion values for this month are NA, return NA
  if (sum(is.na(colData)) == length(colData)) {
    return(NA_real_)
  }
  
  
  
  # Otherwise, return the mean, removing NA values from the calculation
  return(mean(colData, na.rm = TRUE))
  
}




# Import the data from the Expected Demand module----
#expectedDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx",
                        #col_types = "text") #%>%
#  spreadsheetAdjustment()


# Make one table with diversion information
# It will have the APPLICATION_NUMBER and both DIRECT and STORAGE diversion data
# (The other columns in 'expectedDF' are not needed here)
#diverDF <- #expectedDF[, c(which(names(expectedDF) == "APPLICATION_NUMBER")[3],
          #                grep("^[A-Z]{3}_DIRECT_DIVERSION$", names(expectedDF)),
           #               grep("^[A-Z]{3}_STORAGE_DIVERSION$", names(expectedDF)))] %>%
  #mutate(across(ends_with("DIVERSION"), as.numeric))

diverDF <- read_xlsx(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], 
                            "_Monthly_Diversions.xlsx"))



# Add a new column for each month that is the total diversion (DIRECT + STORAGE)
diverDF <- diverDF %>%
  mutate(JAN_TOTAL_DIVERSION = colAdd(JAN_DIRECT_DIVERSION, JAN_STORAGE_DIVERSION),
         FEB_TOTAL_DIVERSION = colAdd(FEB_DIRECT_DIVERSION, FEB_STORAGE_DIVERSION),
         MAR_TOTAL_DIVERSION = colAdd(MAR_DIRECT_DIVERSION, MAR_STORAGE_DIVERSION),
         APR_TOTAL_DIVERSION = colAdd(APR_DIRECT_DIVERSION, APR_STORAGE_DIVERSION),
         MAY_TOTAL_DIVERSION = colAdd(MAY_DIRECT_DIVERSION, MAY_STORAGE_DIVERSION),
         JUN_TOTAL_DIVERSION = colAdd(JUN_DIRECT_DIVERSION, JUN_STORAGE_DIVERSION),
         JUL_TOTAL_DIVERSION = colAdd(JUL_DIRECT_DIVERSION, JUL_STORAGE_DIVERSION),
         AUG_TOTAL_DIVERSION = colAdd(AUG_DIRECT_DIVERSION, AUG_STORAGE_DIVERSION),
         SEP_TOTAL_DIVERSION = colAdd(SEP_DIRECT_DIVERSION, SEP_STORAGE_DIVERSION),
         OCT_TOTAL_DIVERSION = colAdd(OCT_DIRECT_DIVERSION, OCT_STORAGE_DIVERSION),
         NOV_TOTAL_DIVERSION = colAdd(NOV_DIRECT_DIVERSION, NOV_STORAGE_DIVERSION),
         DEC_TOTAL_DIVERSION = colAdd(DEC_DIRECT_DIVERSION, DEC_STORAGE_DIVERSION)) %>%
  ungroup()


# Generate Monthly Demand Dataset for every year in your timeframe; uncomment these lines for the PowerBI 
# Demand Analysis
diverDF %>%
  write_csv(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], 
                   "_DemandDataset_MonthlyValues.csv"))

# Create a separate variable with expected total diversion values
# (There are columns in 'expectedDF' with this name, but they are calculated differently)
# (Averages of sums vs sums of averages)
sumDF <- diverDF %>%
  group_by(APPLICATION_NUMBER) %>%
  summarize(JAN_MEAN_DIV = colMean(JAN_TOTAL_DIVERSION),
            FEB_MEAN_DIV = colMean(FEB_TOTAL_DIVERSION),
            MAR_MEAN_DIV = colMean(MAR_TOTAL_DIVERSION),
            APR_MEAN_DIV = colMean(APR_TOTAL_DIVERSION),
            MAY_MEAN_DIV = colMean(MAY_TOTAL_DIVERSION),
            JUN_MEAN_DIV = colMean(JUN_TOTAL_DIVERSION),
            JUL_MEAN_DIV = colMean(JUL_TOTAL_DIVERSION),
            AUG_MEAN_DIV = colMean(AUG_TOTAL_DIVERSION),
            SEP_MEAN_DIV = colMean(SEP_TOTAL_DIVERSION),
            OCT_MEAN_DIV = colMean(OCT_TOTAL_DIVERSION),
            NOV_MEAN_DIV = colMean(NOV_TOTAL_DIVERSION),
            DEC_MEAN_DIV = colMean(DEC_TOTAL_DIVERSION),
            .groups = "drop") %>%
  mutate(TOTAL_ANNUAL_EXPECTED_DIVERSION = replace_na(JAN_MEAN_DIV, 0) + 
           replace_na(FEB_MEAN_DIV, 0) + replace_na(MAR_MEAN_DIV, 0) + 
           replace_na(APR_MEAN_DIV, 0) + replace_na(MAY_MEAN_DIV, 0) + 
           replace_na(JUN_MEAN_DIV, 0) + replace_na(JUL_MEAN_DIV, 0) +
           replace_na(AUG_MEAN_DIV, 0) + replace_na(SEP_MEAN_DIV, 0) +
           replace_na(OCT_MEAN_DIV, 0) + replace_na(NOV_MEAN_DIV, 0) + 
           replace_na(DEC_MEAN_DIV, 0),
         MAY_TO_SEPT_EXPECTED_DIVERSION = replace_na(MAY_MEAN_DIV, 0) + 
           replace_na(JUN_MEAN_DIV, 0) + replace_na(JUL_MEAN_DIV, 0) +
           replace_na(AUG_MEAN_DIV, 0) + replace_na(SEP_MEAN_DIV, 0))


if (anyNA(sumDF)) {
  
  cat("Warning: There are rights with 'NA' monthly averages\n")
  
}



# Import the ewrims_flat_file_working_file.csv----
  # Will be the basis of the Master Demand Table
  # (In the master table, "PRIMARY_OWNER_ENTITY_TYPE" is called "PRIMARY_OWNER_TYPE")
ewrimsDF <- read.csv(paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], 
                            "_ewrims_flat_file_Working_File.csv")) %>%
  rename(PRIMARY_OWNER_TYPE = PRIMARY_OWNER_ENTITY_TYPE) %>%
  #filter(APPLICATION_NUMBER %in% diverDF$APPLICATION_NUMBER)
  filter(APPLICATION_NUMBER %in% str_remove_all(diverDF$APPLICATION_NUMBER, "_[0-9]+$"))



# Add in columns from the beneficial use module
beneficialUse <- read_xlsx(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], 
                                  "_Beneficial_Use_Return_Flow_Scripted.xlsx")) %>%
  spreadsheetAdjustment()


# Narrow the table down to the four desired columns
beneficialUse <- beneficialUse[, c(which(names(beneficialUse) == "APPLICATION_NUMBER")[2],
                                   which(names(beneficialUse) %in% c("ASSIGNED_BENEFICIAL_USE",
                                                                     "FULLY NON-CONSUMPTIVE",
                                                                     "POWER_DEMAND_ZEROED")))] %>%
  rename(PRIMARY_USE = ASSIGNED_BENEFICIAL_USE)

# Join those columns to 'ewrimsDF'
ewrimsDF <- ewrimsDF %>%
  left_join(beneficialUse, by = "APPLICATION_NUMBER", relationship = "one-to-one")



#Join Priority Date Module data to ewrimsDF----
priorityDF <- read_xlsx(paste0("OutputData/", ws$ID, "_Priority_Date_Scripted.xlsx"), col_types = "text") %>%
  rename(ASSIGNED_PRIORITY_DATE_SOURCE = APPROPRIATIVE_DATE_SOURCE) %>%
  select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE, ASSIGNED_PRIORITY_DATE_SOURCE, 
         PRE_1914, RIPARIAN, APPROPRIATIVE) %>%
  unique()


#Change RIPARIAN values in RIPARIAN column to Y or N values
priorityDF$RIPARIAN <- if_else(priorityDF$RIPARIAN == "RIPARIAN", "Y", "N")


# Replace NA values in Riparian column with N
priorityDF$RIPARIAN[is.na(priorityDF$RIPARIAN)] <- "N"


# Use a left join once again
ewrimsDF <- ewrimsDF %>%
  left_join(priorityDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")



# Import the expected demand module----
#expectedDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx",
#                        col_types = "text") %>%
#  spreadsheetAdjustment()
expectedDF <- read_xlsx(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], 
                               "_ExpectedDemand_FV.xlsx"), col_types = "text")

# Get two sub-tables from the main dataset
# (Rename some columns too)
# faceVars <- expectedDF[, c(which(names(expectedDF) == "APPLICATION_NUMBER")[3],
#                            which(names(expectedDF) == "FACE_VALUE_AMOUNT")[2],
#                            which(names(expectedDF) == "IniDiv_Converted_to_AF"))] %>%
#   unique() %>%
#   rename(INI_REPORTED_DIV_AMOUNT_AF = IniDiv_Converted_to_AF,
#          FACE_VALUE_AMOUNT_AF = FACE_VALUE_AMOUNT)
faceVars <- expectedDF %>% 
  select(APPLICATION_NUMBER, FACE_VALUE_AMOUNT, IniDiv_Converted_to_AF) %>%
  unique() %>%
  rename(INI_REPORTED_DIV_AMOUNT_AF = IniDiv_Converted_to_AF,
         FACE_VALUE_AMOUNT_AF = FACE_VALUE_AMOUNT)


# For the second sub-table, add a new indicator variable for whether the 
# APPLICATION_NUMBER column is NA
nullVar <- #expectedDF[, which(names(expectedDF) == "APPLICATION_NUMBER")[4]] %>%
  expectedDF %>% select(APPLICATION_NUMBER) %>% unique() %>%
  mutate(NULL_DEMAND = if_else(!is.na(APPLICATION_NUMBER), "N", "Y")) %>%
  unique()


# Join both of these datasets to 'ewrimsDF'
ewrimsDF <- ewrimsDF %>%
  left_join(faceVars, by = "APPLICATION_NUMBER", relationship = "one-to-one") %>%
  left_join(nullVar, by = "APPLICATION_NUMBER", relationship = "one-to-one")



# Assign basin information to 'ewrimsDF' using information output by "Assign_Subbasin_to_POD.R"----
# For other sub-basins, use a different procedure later in the script
if (grepl("^Russian", ws$NAME)) {
  ewrimsDF <- ewrimsDF %>%
    assignBasinData_RR()
}



# Error Check
if ("BASIN" %in% names(ewrimsDF)) {
  stopifnot(!anyNA(ewrimsDF$BASIN))
}

if ("MAINSTEM" %in% names(ewrimsDF)) {
  stopifnot(!anyNA(ewrimsDF$MAINSTEM))
}

if ("LONGITUDE" %in% names(ewrimsDF)) {
  stopifnot(!anyNA(ewrimsDF$LONGITUDE))
}

if ("LATITUDE" %in% names(ewrimsDF)) {
  stopifnot(!anyNA(ewrimsDF$LATITUDE))
}






# Add UPPER_RUSSIAN Field (Russian River only)
  #For basins 01 to 13, UPPER_RUSSIAN should be "Y". This includes basins with an "_M" 
  #suffix for "main stem". For the remaining basins, 14 to 28, the UPPER_RUSSIAN field should be "N."
  #the str_sub looks at the 3rd and 4th characters of the Basin column which contain the 2-digit 
  #basin number. 
if (grepl("^Russian", ws$NAME)) {
  ewrimsDF <- ewrimsDF %>%
    mutate(UPPER_RUSSIAN = if_else(str_sub(BASIN, 3, 4) %in% c("01", "02", "03", "04", "05", 
                                                               "06", "07", "08", "09", "10", "11", 
                                                               "12", "13"), "Y", "N"))
}


# Convert columns to appropriate data types
  # convert from character to integer
ewrimsDF$ASSIGNED_PRIORITY_DATE = as.integer(ewrimsDF$ASSIGNED_PRIORITY_DATE) 

# Rename a few more columns----
ewrimsDF = rename(ewrimsDF, ASSIGNED_PRIORITY_DATE_SUB = ASSIGNED_PRIORITY_DATE)

if ("MAINSTEM" %in% names(ewrimsDF) && grepl("^Russian", ws$NAME)) {
  ewrimsDF = rename(ewrimsDF, MAINSTEM_RR = MAINSTEM)
}




# Append COUNTY to 'ewrimsDF'

if (grepl("^Russian", ws$NAME)) {
  
  # Read in "RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx, which is now hosted on SharePoint"
  podDF <- getXLSX(ws = ws, 
                   SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_POD_COORDINATES_SPREADSHEET",
                   FILEPATH = "POD_COORDINATES_SPREADSHEET_PATH",
                   WORKSHEET_NAME = "POD_COORDINATES_WORKSHEET_NAME")
  
  
  # Create a tibble with "APPLICATION_NUMBER" values that have only one unique county for their POD(s) 
  countyDF <- podDF %>%
    select(APPLICATION_NUMBER, COUNTY) %>% unique() %>%
    group_by(APPLICATION_NUMBER) %>%
    filter(n() == 1)
  
  
  # Join this data to 'ewrimsDF'
  ewrimsDF <- ewrimsDF %>%
    left_join(countyDF, by = "APPLICATION_NUMBER")
  
  
  
  # If there are still NA values in "COUNTY", try to use the "LATITUDE" and "LONGITUDE" to help
  if (anyNA(ewrimsDF$COUNTY)) {
    
    # Iterate through 'ewrimsDF'
    for (i in 1:nrow(ewrimsDF)) {
      
      # Skip rows with a non-NA "COUNTY"
      if (!is.na(ewrimsDF$COUNTY[i])) {
        next
      }
      
      # Assign a county to 'ewrimsDF' based on the right's POD with matching APPLICATION_NUMBER and approximately equal LONGITUDE coordinate
      ewrimsDF$COUNTY[i] <- podDF[podDF$APPLICATION_NUMBER == ewrimsDF$APPLICATION_NUMBER[i] &
                                    round(podDF$LONGITUDE, 2) == round(ewrimsDF$LONGITUDE[i], 2), ]$COUNTY %>%
        unique()
      
    }
    
    
  }
  
  
  # Error Check
  # Every entry should have a non-NA "COUNTY" value
  stopifnot(!anyNA(ewrimsDF$COUNTY))
  
}



# Add the diversion data to eWRIMSDF----


# If some rights were split, the information in 'ewrimsDF' will need to be adjusted
if (sum(grepl("_[0-9]+$", sumDF$APPLICATION_NUMBER)) > 0) {
  
  
  # Get a list of rights that have been split
  splitRights <- sumDF$APPLICATION_NUMBER %>%
    str_subset("_[0-9]+$") %>% str_remove_all("_[0-9]+$") %>% 
    unique() %>% sort()
  
  
  
  # Iterate through the split rights
  for (i in 1:length(splitRights)) {
    
    # Get the location of this water right in 'ewrimsDF' 
    matchIndex <- grep(splitRights[i], ewrimsDF$APPLICATION_NUMBER)
    
    
    stopifnot(length(matchIndex) == 1)
    
    
    
    # Get the number of sub-rights for this split right
    splitCounts <- sumDF$APPLICATION_NUMBER %>%
      str_subset(splitRights[i]) %>% length()
    
    
    
    # For each sub-right (other than the first one), 
    # add a copy of the original row to 'ewrimsDF'
    for (j in 2:splitCounts) {
      
      ewrimsDF <- ewrimsDF %>%
        bind_rows(ewrimsDF[matchIndex, ] %>%
                    mutate(APPLICATION_NUMBER = paste0(APPLICATION_NUMBER, "_", j)))
      
    } # End of 'j' loop
    
    
    
    # Finally, the first instance of this right in 'ewrimsDF' 
    # will be updated to be the first sub-right
    ewrimsDF$APPLICATION_NUMBER[matchIndex] <- paste0(ewrimsDF$APPLICATION_NUMBER[matchIndex],
                                                      "_1")
    
  } # End of 'i' loop
  
}



# With split water rights now reflected in 'ewrimsDF', merge it with 'sumDF'
ewrimsDF <- sumDF %>%
  select(APPLICATION_NUMBER,
         JAN_MEAN_DIV, FEB_MEAN_DIV, 
         MAR_MEAN_DIV, APR_MEAN_DIV, 
         MAY_MEAN_DIV, JUN_MEAN_DIV, 
         JUL_MEAN_DIV, AUG_MEAN_DIV, 
         SEP_MEAN_DIV, OCT_MEAN_DIV, 
         NOV_MEAN_DIV, DEC_MEAN_DIV, 
         TOTAL_ANNUAL_EXPECTED_DIVERSION, MAY_TO_SEPT_EXPECTED_DIVERSION) %>%
  rename(TOTAL_EXPECTED_ANNUAL_DIVERSION = TOTAL_ANNUAL_EXPECTED_DIVERSION,
         TOTAL_MAY_SEPT_DIV = MAY_TO_SEPT_EXPECTED_DIVERSION) %>%
  right_join(ewrimsDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")



# Calculate two new columns: "PERCENT_FACE" and "ZERO_DEMAND"----
# The former will be the "TOTAL_EXPECTED_ANNUAL_DIVERSION" divided by the larger value
# between the "INI_REPORTED_DIV_AMOUNT_AF" and the "FACE_VALUE_AMOUNT_AF"
# The latter will be a Y/N column for whether "TOTAL_EXPECTED_ANNUAL_DIVERSION"
# is equal to 0
ewrimsDF <- ewrimsDF %>%
  mutate(TOTAL_EXPECTED_ANNUAL_DIVERSION = as.numeric(TOTAL_EXPECTED_ANNUAL_DIVERSION),
         INI_REPORTED_DIV_AMOUNT_AF = as.numeric(INI_REPORTED_DIV_AMOUNT_AF),
         FACE_VALUE_AMOUNT_AF = as.numeric(FACE_VALUE_AMOUNT_AF)) %>%
  rowwise() %>%
  mutate(PERCENT_FACE = 
           TOTAL_EXPECTED_ANNUAL_DIVERSION / max(INI_REPORTED_DIV_AMOUNT_AF, FACE_VALUE_AMOUNT_AF, -Inf, na.rm = TRUE),
         ZERO_DEMAND = if_else(TOTAL_EXPECTED_ANNUAL_DIVERSION == 0, "Y", "N")) %>%
  ungroup()



# For all watersheds, regardless of whether split water rights are present,
# add a column that identifies the original "APPLICATION_NUMBER" value
# ("APPLICATION_NUMBER" and "ORIGINAL_APPLICATION_NUMBER" are only different for split rights)
ewrimsDF <- ewrimsDF %>%
  mutate(ORIGINAL_APPLICATION_NUMBER = APPLICATION_NUMBER %>%
           str_remove_all("_[0-9]+$"))



# For watersheds other than the Russian River, 
# append water rights' sub-basins to 'ewrimsDF' here
if (!grepl("^Russian", ws$NAME) & !is.na(ws$SUBBASIN_ASSIGNMENT_SPREADSHEET_PATH)) {
  
  
  # Read in the sub-basin assignments and the name of the column that 
  # distinguishes between different sub-basins 
  basinDF <- getXLSX(ws, 
                     "IS_SHAREPOINT_PATH_SUBBASIN_ASSIGNMENT_SPREADSHEET",
                     "SUBBASIN_ASSIGNMENT_SPREADSHEET_PATH",
                     "SUBBASIN_ASSIGNMENT_WORKSHEET_NAME")
  
  
  basinColName <- ws[["SUBBASIN_FIELD_ID_NAMES"]] %>%
    str_split(";") %>% unlist() %>%
    pluck(1) %>% trimws()
  
  
  
  # Keep just "APPLICATION_NUMBER" and the sub-basin column
  # Rename the sub-basin column to "BASIN" for consistency
  basinDF <- basinDF %>%
    select(APPLICATION_NUMBER, all_of(basinColName)) %>%
    unique() %>%
    rename(BASIN = all_of(basinColName))
  
  
  
  # Join 'basinDF' to 'ewrimsDF'
  ewrimsDF <- ewrimsDF %>%
    left_join(basinDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  # Ensure that there are no "NA" values in this sub-basin column
  stopifnot(!anyNA(ewrimsDF[["BASIN"]]))
  
}



#Write the MasterDemandTable to a CSV----
#dataset that includes 2021 and 2022 curtailment reporting years
write.csv(ewrimsDF, file = paste0("OutputData/", ws$ID, "_",
                                         yearRange[1], "_", yearRange[2],
                                         "_MDT_", format(Sys.Date(), "%Y-%m-%d"), ".csv"), row.names = FALSE)

#just the 2017-2020 reporting years
#write.csv(ewrimsDF, file = "OutputData/2017-2020_RR_MasterDemandTable.csv", row.names = FALSE)


#Compare 2023_RRMasterDemandTable to Russian_River_Database_2022.csv----
# MasterDemandTable = read.csv(file = "OutputData/2023_RR_MasterDemandTable.csv")
# RussianRiverDatabase2022 = read.csv(file = "InputData/RUSSIAN_RIVER_DATABASE_2022.csv")
# 
# # Structure of 2023_RRMasterDemandTable
# structure_MDT = data.frame(
#   MDT_ColumnName = colnames(MasterDemandTable),
#   MDT_VariableType = sapply(MasterDemandTable, class)
# )
# 
# 
# # Structure of Russian_River_Database_2022
# structure_RR2022 = data.frame(
#   RR2022_ColumnName = colnames(RussianRiverDatabase2022),
#   RR2022_VariableType = sapply(RussianRiverDatabase2022,class)
# )
# 
# library(openxlsx)
# 
# MDT_Comparison <-createWorkbook()
#   addWorksheet(MDT_Comparison, "MDT2023")
#   writeDataTable(MDT_Comparison, "MDT2023", structure_MDT)
#   addWorksheet(MDT_Comparison,"RR2022")
#   writeDataTable(MDT_Comparison, "RR2022", structure_RR2022)
#   saveWorkbook(MDT_Comparison, file = paste0("OutputData/MDT2023_RR2022_Comparison.xlsx"), overwrite =  TRUE)
# 
print("The MasterDemandTable.R script has finished running")



remove(list = ls())
