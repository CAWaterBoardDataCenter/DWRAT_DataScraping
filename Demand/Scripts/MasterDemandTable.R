# This script is a recreation of the Excel file "QAQC_Working_File.xlsx"


# Data from the previous Excel modules will be compiled here
# The end result will be a spreadsheet with two worksheets ("DiversionData" and "MasterDemandTable")


cat("Starting 'MasterDemandTable.R'...\n")


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
assignBasinData <- function (ewrimsDF) {
  
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
  
  
  # Also consult a manual review spreadsheet
  manualDF <- read_xlsx("InputData/Missing_MainStem_GIS_Manual_Assignment.xlsx") %>%
    filter(APPLICATION_NUMBER %in% ewrimsDF$APPLICATION_NUMBER[is.na(ewrimsDF$MAINSTEM)])
  
  
  # Iterate through 'manualDF' and apply these values to 'ewrimsDF'
  for (i in 1:nrow(manualDF)) {
    
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$BASIN <- manualDF$BASIN[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$LATITUDE <- manualDF$LATITUDE[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$LONGITUDE <- manualDF$LONGITUDE[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == manualDF$APPLICATION_NUMBER[i], ]$MAINSTEM <- manualDF$MAINSTEM[i]
  }
  
  
  # Finally, rely on the output of "Assign_Subbasin_to_POD.R"
  podDF <- read_xlsx("OutputData/POD_Subbasin_Assignment.xlsx") %>%
    filter(APPLICATION_NUMBER %in% ewrimsDF$APPLICATION_NUMBER[is.na(ewrimsDF$MAINSTEM)])
  
  # This procedure will not work if the remaining rights have multiple PODs
  stopifnot(nrow(podDF) == length(unique(podDF$APPLICATION_NUMBER)))
  
  # Iterate through 'podDF' and apply these values to 'ewrimsDF'
  for (i in 1:nrow(podDF)) {
    
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i], ]$BASIN <- paste0("R_", 
                                                                                           if_else(podDF$Basin_Num[i] < 10, "0", ""), 
                                                                                           podDF$Basin_Num[i], 
                                                                                           if_else(podDF$MAIN_STEM[i] == "Y", "_M", ""))
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i], ]$LATITUDE <- podDF$LATITUDE2[i]
    ewrimsDF[ewrimsDF$APPLICATION_NUMBER == podDF$APPLICATION_NUMBER[i], ]$LONGITUDE <- podDF$LONGITUDE2[i]
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
diverDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx")


# Add a new column for each month that is the total diversion (DIRECT + STORAGE)
diverDF <- diverDF %>%
  mutate(JAN_TOTAL_DIVERSION = replace_na(JAN_DIRECT_DIVERSION, 0) + replace_na(JAN_STORAGE_DIVERSION, 0),
         FEB_TOTAL_DIVERSION = replace_na(FEB_DIRECT_DIVERSION, 0) + replace_na(FEB_STORAGE_DIVERSION, 0),
         MAR_TOTAL_DIVERSION = replace_na(MAR_DIRECT_DIVERSION, 0) + replace_na(MAR_STORAGE_DIVERSION, 0),
         APR_TOTAL_DIVERSION = replace_na(APR_DIRECT_DIVERSION, 0) + replace_na(APR_STORAGE_DIVERSION, 0),
         MAY_TOTAL_DIVERSION = replace_na(MAY_DIRECT_DIVERSION, 0) + replace_na(MAY_STORAGE_DIVERSION, 0),
         JUN_TOTAL_DIVERSION = replace_na(JUN_DIRECT_DIVERSION, 0) + replace_na(JUN_STORAGE_DIVERSION, 0),
         JUL_TOTAL_DIVERSION = replace_na(JUL_DIRECT_DIVERSION, 0) + replace_na(JUL_STORAGE_DIVERSION, 0),
         AUG_TOTAL_DIVERSION = replace_na(AUG_DIRECT_DIVERSION, 0) + replace_na(AUG_STORAGE_DIVERSION, 0),
         SEP_TOTAL_DIVERSION = replace_na(SEP_DIRECT_DIVERSION, 0) + replace_na(SEP_STORAGE_DIVERSION, 0),
         OCT_TOTAL_DIVERSION = replace_na(OCT_DIRECT_DIVERSION, 0) + replace_na(OCT_STORAGE_DIVERSION, 0),
         NOV_TOTAL_DIVERSION = replace_na(NOV_DIRECT_DIVERSION, 0) + replace_na(NOV_STORAGE_DIVERSION, 0),
         DEC_TOTAL_DIVERSION = replace_na(DEC_DIRECT_DIVERSION, 0) + replace_na(DEC_STORAGE_DIVERSION, 0)) %>%
  ungroup()


# Create a separate variable with expected total diversion values
# (There are columns in 'expectedDF' with this name, but they are calculated differently)
# (Averages of sums vs sums of averages)
sumDF <- diverDF %>%
  group_by(APPLICATION_NUMBER) %>%
  summarize(JAN_MEAN_DIV = mean(JAN_TOTAL_DIVERSION, na.rm = TRUE),
            FEB_MEAN_DIV = mean(FEB_TOTAL_DIVERSION, na.rm = TRUE),
            MAR_MEAN_DIV = mean(MAR_TOTAL_DIVERSION, na.rm = TRUE),
            APR_MEAN_DIV = mean(APR_TOTAL_DIVERSION, na.rm = TRUE),
            MAY_MEAN_DIV = mean(MAY_TOTAL_DIVERSION, na.rm = TRUE),
            JUN_MEAN_DIV = mean(JUN_TOTAL_DIVERSION, na.rm = TRUE),
            JUL_MEAN_DIV = mean(JUL_TOTAL_DIVERSION, na.rm = TRUE),
            AUG_MEAN_DIV = mean(AUG_TOTAL_DIVERSION, na.rm = TRUE),
            SEP_MEAN_DIV = mean(SEP_TOTAL_DIVERSION, na.rm = TRUE),
            OCT_MEAN_DIV = mean(OCT_TOTAL_DIVERSION, na.rm = TRUE),
            NOV_MEAN_DIV = mean(NOV_TOTAL_DIVERSION, na.rm = TRUE),
            DEC_MEAN_DIV = mean(DEC_TOTAL_DIVERSION, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(TOTAL_ANNUAL_EXPECTED_DIVERSION = JAN_MEAN_DIV + 
           FEB_MEAN_DIV + MAR_MEAN_DIV + 
           APR_MEAN_DIV + MAY_MEAN_DIV + 
           JUN_MEAN_DIV + JUL_MEAN_DIV +
           AUG_MEAN_DIV + SEP_MEAN_DIV +
           OCT_MEAN_DIV + NOV_MEAN_DIV + 
           DEC_MEAN_DIV,
         MAY_TO_SEPT_EXPECTED_DIVERSION = MAY_MEAN_DIV + 
           JUN_MEAN_DIV + JUL_MEAN_DIV +
           AUG_MEAN_DIV + SEP_MEAN_DIV)


# Import the ewrims_flat_file_working_file.csv----
  # Will be the basis of the Master Demand Table
  # (In the master table, "PRIMARY_OWNER_ENTITY_TYPE" is called "PRIMARY_OWNER_TYPE")
ewrimsDF <- read.csv("IntermediateData/ewrims_flat_file_Working_File.csv") %>%
  rename(PRIMARY_OWNER_TYPE = PRIMARY_OWNER_ENTITY_TYPE)

# Add in columns from the beneficial use module
beneficialUse <- read_xlsx("OutputData/Beneficial_Use_Return_Flow_Scripted.xlsx") %>%
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
priorityDF <- read_xlsx("OutputData/Priority_Date_Scripted.xlsx", col_types = "text") %>%
  rename(ASSIGNED_PRIORITY_DATE_SOURCE = APPROPRIATIVE_DATE_SOURCE) %>%
  select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE, ASSIGNED_PRIORITY_DATE_SOURCE, 
         PRE_1914, RIPARIAN, APPROPRIATIVE) %>%
  unique()

#Change RIPARIAN values in RIPARIAN column to Y or N values
priorityDF$RIPARIAN <- ifelse(test =priorityDF$RIPARIAN == "RIPARIAN", 
                              yes = "Y", 
                              no = "N")

# Replace NA values in Riparian column with N
priorityDF$RIPARIAN[is.na(priorityDF$RIPARIAN)] <- "N"

# Use a left join once again
ewrimsDF <- ewrimsDF %>%
  left_join(priorityDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")



# Import the expected demand module----
#expectedDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx",
#                        col_types = "text") %>%
#  spreadsheetAdjustment()
expectedDF <- read_xlsx("OutputData/ExpectedDemand_FV.xlsx", col_types = "text")

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


# Add the diversion data to eWRIMSDF----
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


# Assign basin information to 'ewrimsDF' using information output by "Assign_Subbasin_to_POD.R"----
ewrimsDF <- ewrimsDF %>%
  assignBasinData()

# Add UPPER_RUSSIAN Field
  #For basins 01 to 13, UPPER_RUSSIAN should be "Y". This includes basins with an "_M" 
  #suffix for "main stem". For the remaining basins, 14 to 28, the UPPER_RUSSIAN field should be "N."
  #the str_sub looks at the 3rd and 4th characters of the Basin column which contain the 2-digit 
  #basin number. 

ewrimsDF <- ewrimsDF %>%
  mutate(UPPER_RUSSIAN = if_else(str_sub(BASIN, 3, 4) %in% c("01", "02", "03", "04", "05", 
                                                            "06", "07", "08", "09", "10", "11", 
                                                            "12", "13"), "Y", "N"))

# Convert columns to appropriate data types
  # convert from character to integer
ewrimsDF$ASSIGNED_PRIORITY_DATE = as.integer(ewrimsDF$ASSIGNED_PRIORITY_DATE) 

# Rename a few more columns----
ewrimsDF = rename(ewrimsDF, ASSIGNED_PRIORITY_DATE_SUB = ASSIGNED_PRIORITY_DATE)
ewrimsDF = rename(ewrimsDF, MAINSTEM_RR = MAINSTEM)



# Append COUNTY to 'ewrimsDF'

# Read in "RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx"
podDF <- read_xlsx("OutputData/POD_Subbasin_Assignment.xlsx")


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
                                  round(podDF$LONGITUDE2, 2) == round(ewrimsDF$LONGITUDE[i], 2), ]$COUNTY %>%
      unique()
    
  }
  
  
}


# Error Check
# Every entry should have a non-NA "COUNTY" value
stopifnot(!anyNA(ewrimsDF$COUNTY))



#Write the MasterDemandTable to a CSV----
#dataset that includes 2021 and 2022 curtailment reporting years
#write.csv(ewrimsDF, file = "OutputData/2023_RR_MasterDemandTable.csv", row.names = FALSE)
#just the 2017-2020 reporting years
write.csv(ewrimsDF, file = "OutputData/2020_RR_MasterDemandTable.csv", row.names = FALSE)


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
# print("The MasterDemandTable.R script has finished running")



remove(assignBasinData, spreadsheetAdjustment, beneficialUse, diverDF,
       ewrimsDF, expectedDF, faceVars, nullVar, priorityDF, sumDF,
       MasterDemandTable, RussianRiverDatabase2022, countyDF, podDF, i)
