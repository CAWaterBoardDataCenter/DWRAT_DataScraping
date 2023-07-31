# This script is a recreation of the Excel file "QAQC_Working_File.xlsx"


# Data from the previous Excel modules will be compiled here
# The end result will be a spreadsheet with two worksheets ("DiversionData" and "MasterDemandTable)


#### Dependencies ####


require(tidyverse)
require(openxlsx)
require(readxl)


#### Script Procedure ####


mainProcedure <- function () {
  
  # Main script procedure
  
  
  
  # Create an Excel workbook object
  wb <- createWorkbook()
  
  
  
  # Begin with the "DiversionData" worksheet
  # Add its worksheet and construct the two data frames that make up its data
  addWorksheet(wb, "DiversionData")
  
  
  # Read in the data from the Expected Demand module
  expectedDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx",
                          col_types = "text") %>%
    spreadsheetAdjustment()
  
  
  # Make one table with diversion information
  diverDF <- expectedDF[, c(which(names(expectedDF) == "APPLICATION_NUMBER")[2],
                            grep("^[A-Z]{3}_DIRECT_DIVERSION$", names(expectedDF)),
                            grep("^[A-Z]{3}_STORAGE_DIVERSION$", names(expectedDF)))] %>%
    mutate(across(ends_with("DIVERSION"), as.numeric))
  
  
  # Add a new column for each month that is the total diversion (DIRECT + STORAGE)
  # Also, include three QA/QC columns at the end
  diverDF <- diverDF %>%
    mutate(JAN_TOTAL_DIVERSION = JAN_DIRECT_DIVERSION + JAN_STORAGE_DIVERSION,
           FEB_TOTAL_DIVERSION = FEB_DIRECT_DIVERSION + FEB_STORAGE_DIVERSION,
           MAR_TOTAL_DIVERSION = MAR_DIRECT_DIVERSION + MAR_STORAGE_DIVERSION,
           APR_TOTAL_DIVERSION = APR_DIRECT_DIVERSION + APR_STORAGE_DIVERSION,
           MAY_TOTAL_DIVERSION = MAY_DIRECT_DIVERSION + MAY_STORAGE_DIVERSION,
           JUN_TOTAL_DIVERSION = JUN_DIRECT_DIVERSION + JUN_STORAGE_DIVERSION,
           JUL_TOTAL_DIVERSION = JUL_DIRECT_DIVERSION + JUL_STORAGE_DIVERSION,
           AUG_TOTAL_DIVERSION = AUG_DIRECT_DIVERSION + AUG_STORAGE_DIVERSION,
           SEP_TOTAL_DIVERSION = SEP_DIRECT_DIVERSION + SEP_STORAGE_DIVERSION,
           OCT_TOTAL_DIVERSION = OCT_DIRECT_DIVERSION + OCT_STORAGE_DIVERSION,
           NOV_TOTAL_DIVERSION = NOV_DIRECT_DIVERSION + NOV_STORAGE_DIVERSION,
           DEC_TOTAL_DIVERSION = DEC_DIRECT_DIVERSION + DEC_STORAGE_DIVERSION,
           QAQC_ACTION_TAKEN_Y_N = NA_character_,
           CHANGE_MADE = NA_character_,
           REASON_FOR_CHANGE = NA_character_)
  
  
  
  # Create a separate variable with expected total diversion values
  # (There are columns in 'expectedDF' with this name, but they are calculated differently)
  # (Averages of sums vs sums of averages)
  # (Include the QA/QC columns as well)
  sumDF <- diverDF %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(JAN_EXPECTED_TOTAL_DIVERSION = mean(JAN_TOTAL_DIVERSION, na.rm = TRUE),
              FEB_EXPECTED_TOTAL_DIVERSION = mean(FEB_TOTAL_DIVERSION, na.rm = TRUE),
              MAR_EXPECTED_TOTAL_DIVERSION = mean(MAR_TOTAL_DIVERSION, na.rm = TRUE),
              APR_EXPECTED_TOTAL_DIVERSION = mean(APR_TOTAL_DIVERSION, na.rm = TRUE),
              MAY_EXPECTED_TOTAL_DIVERSION = mean(MAY_TOTAL_DIVERSION, na.rm = TRUE),
              JUN_EXPECTED_TOTAL_DIVERSION = mean(JUN_TOTAL_DIVERSION, na.rm = TRUE),
              JUL_EXPECTED_TOTAL_DIVERSION = mean(JUL_TOTAL_DIVERSION, na.rm = TRUE),
              AUG_EXPECTED_TOTAL_DIVERSION = mean(AUG_TOTAL_DIVERSION, na.rm = TRUE),
              SEP_EXPECTED_TOTAL_DIVERSION = mean(SEP_TOTAL_DIVERSION, na.rm = TRUE),
              OCT_EXPECTED_TOTAL_DIVERSION = mean(OCT_TOTAL_DIVERSION, na.rm = TRUE),
              NOV_EXPECTED_TOTAL_DIVERSION = mean(NOV_TOTAL_DIVERSION, na.rm = TRUE),
              DEC_EXPECTED_TOTAL_DIVERSION = mean(DEC_TOTAL_DIVERSION, na.rm = TRUE),
              .groups = "drop") %>%
    rowwise() %>%
    mutate(TOTAL_ANNUAL_EXPECTED_DIVERSION = JAN_EXPECTED_TOTAL_DIVERSION + 
             FEB_EXPECTED_TOTAL_DIVERSION + MAR_EXPECTED_TOTAL_DIVERSION + 
             APR_EXPECTED_TOTAL_DIVERSION + MAY_EXPECTED_TOTAL_DIVERSION + 
             JUN_EXPECTED_TOTAL_DIVERSION + JUL_EXPECTED_TOTAL_DIVERSION +
             AUG_EXPECTED_TOTAL_DIVERSION + SEP_EXPECTED_TOTAL_DIVERSION +
             OCT_EXPECTED_TOTAL_DIVERSION + NOV_EXPECTED_TOTAL_DIVERSION + 
             DEC_EXPECTED_TOTAL_DIVERSION,
           MAY_TO_SEPT_EXPECTED_DIVERSION = MAY_EXPECTED_TOTAL_DIVERSION + 
             JUN_EXPECTED_TOTAL_DIVERSION + JUL_EXPECTED_TOTAL_DIVERSION +
             AUG_EXPECTED_TOTAL_DIVERSION + SEP_EXPECTED_TOTAL_DIVERSION,
           QAQC_ACTION_TAKEN_Y_N = NA_character_,
           CHANGE_MADE = NA_character_,
           REASON_FOR_CHANGE = NA_character_) %>%
    ungroup()
  
  
  
  # Write these data frames to the worksheet
  writeData(wb, "DiversionData", startCol = 1, startRow = 3, diverDF)
  writeData(wb, "DiversionData", startCol = 42, startRow = 3, sumDF)
  
  
  # Add accompanying text as well
  writeData(wb, "DiversionData", startCol = 1, startRow = 1, "INFO:")
  writeData(wb, "DiversionData", startCol = 1, startRow = 2, "ACTION:")
  
  
  writeData(wb, "DiversionData", startCol = 2, startRow = 1, 
            "REPORTED DIRECT DIVERSION BY YEAR")
  writeData(wb, "DiversionData", startCol = 2, startRow = 2, 
            "IF CHANGES TO DIVERSION DATA ARE NEEDED, DIRECTLY OVERWRITE THE RECORD, AND UPDATE THE QAQC COLUMNS")
  
  
  writeData(wb, "DiversionData", startCol = 14, startRow = 1, 
            "REPORTED DIVERSION TO STORAGE BY YEAR")
  writeData(wb, "DiversionData", startCol = 14, startRow = 2, 
            "IF CHANGES TO DIVERSION DATA ARE NEEDED, DIRECTLY OVERWRITE THE RECORD, AND UPDATE THE QAQC COLUMNS")
  
  
  writeData(wb, "DiversionData", startCol = 26, startRow = 1, 
            "REPORTED TOTAL DIVERSION BY YEAR")
  writeData(wb, "DiversionData", startCol = 26, startRow = 2, 
            "DO NOT MAKE CHANGES HERE")
  
  
  writeData(wb, "DiversionData", startCol = 42, startRow = 1, 
            "MONTHLY AVERAGE TOTAL DIVERSIONS (STORAGE + DIRECT)")
  writeData(wb, "DiversionData", startCol = 42, startRow = 2, 
            "IF CHANGES TO DIVERSION DATA ARE NEEDED, DIRECTLY OVERWRITE THE RECORD, AND UPDATE THE QAQC COLUMNS")
  
  
  
  # Next, prepare the master data table
  
  
  # Add a worksheet for the master table
  addWorksheet(wb, "MasterDemandTable")
  
  
  # Construct the table that will be input into this worksheet
  
  
  # The basis of the table will be "ewrims_flat_file_Working_File.csv"
  # (In the master table, "PRIMARY_OWNER_ENTITY_TYPE" is called "PRIMARY_OWNER_TYPE")
  ewrimsDF <- read.csv("InputData/ewrims_flat_file_Working_File.csv") %>%
    rename(PRIMARY_OWNER_TYPE = PRIMARY_OWNER_ENTITY_TYPE)
  
  
  
  # Add in columns from the beneficial use module
  beneficialUse <- read_xlsx("OutputData/Beneficial_Use_Return_Flow_Scripted.xlsx") %>%
    spreadsheetAdjustment()
  
  
  # Narrow the table down to the four desired columns
  beneficialUse <- beneficialUse[, c(which(names(beneficialUse) == "APPLICATION_NUMBER")[2],
                                     which(names(beneficialUse) %in% c("ASSIGNED_BENEFICIAL_USE",
                                                                       "FULLY NON-CONSUMPTIVE",
                                                                       "POWER_DEMAND_ZEROED")))] %>%
    rename(PRIMARY_BENEFICIAL_USE = ASSIGNED_BENEFICIAL_USE)
  
  
  # Join those columns to 'ewrimsDF'
  ewrimsDF <- ewrimsDF %>%
    left_join(beneficialUse, by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  # Next, data from the priority date module will be added to 'ewrimsDF'
  priorityDF <- read_xlsx("OutputData/Priority_Date_Scripted.xlsx", col_types = "text") %>%
    rename(ASSIGNED_PRIORITY_DATE_SOURCE = APPROPRIATIVE_DATE_SOURCE) %>%
    select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE, ASSIGNED_PRIORITY_DATE_SOURCE, 
           PRE_1914, RIPARIAN, APPROPRIATIVE) %>%
    unique()
  
  
  # Use a left join once again
  ewrimsDF <- ewrimsDF %>%
    left_join(priorityDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  # After that, read in the spreadsheet table for the expected demand module
  expectedDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx",
                          col_types = "text") %>%
    spreadsheetAdjustment()
  
  
  # Get two sub-tables from the main dataset
  # (Rename some columns too)
  faceVars <- expectedDF[, c(which(names(expectedDF) == "APPLICATION_NUMBER")[3],
                             which(names(expectedDF) == "FACE_VALUE_AMOUNT")[2],
                             which(names(expectedDF) == "IniDiv_Converted_to_AF"))] %>%
    unique() %>%
    rename(INI_REPORTED_DIV_AMOUNT_AF = IniDiv_Converted_to_AF,
           FACE_VALUE_AMOUNT_AF = FACE_VALUE_AMOUNT)
  
  
  # For the second sub-table, add a new indicator variable for whether the 
  # APPLICATION_NUMBER column is NA
  nullVar <- expectedDF[, which(names(expectedDF) == "APPLICATION_NUMBER")[4]] %>%
    mutate(NULL_DEMAND = if_else(!is.na(APPLICATION_NUMBER), "N", "Y")) %>%
    unique()
  
  
  
  # Join both of these datasets to 'ewrimsDF'
  ewrimsDF <- ewrimsDF %>%
    left_join(faceVars, by = "APPLICATION_NUMBER", relationship = "one-to-one") %>%
    left_join(nullVar, by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  # Data from the "DiversionData" sheet is needed next
  ewrimsDF <- sumDF %>%
    select(APPLICATION_NUMBER,
           JAN_EXPECTED_TOTAL_DIVERSION, FEB_EXPECTED_TOTAL_DIVERSION, 
           MAR_EXPECTED_TOTAL_DIVERSION, APR_EXPECTED_TOTAL_DIVERSION, 
           MAY_EXPECTED_TOTAL_DIVERSION, JUN_EXPECTED_TOTAL_DIVERSION, 
           JUL_EXPECTED_TOTAL_DIVERSION, AUG_EXPECTED_TOTAL_DIVERSION, 
           SEP_EXPECTED_TOTAL_DIVERSION, OCT_EXPECTED_TOTAL_DIVERSION, 
           NOV_EXPECTED_TOTAL_DIVERSION, DEC_EXPECTED_TOTAL_DIVERSION, 
           TOTAL_ANNUAL_EXPECTED_DIVERSION, MAY_TO_SEPT_EXPECTED_DIVERSION) %>%
    rename(TOTAL_EXPECTED_ANNUAL_DIVERSION = TOTAL_ANNUAL_EXPECTED_DIVERSION,
           TOTAL_MAY_SEPT_DIV = MAY_TO_SEPT_EXPECTED_DIVERSION) %>%
    right_join(ewrimsDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  # Calculate two new columns: "PERCENT_FACE" and "ZERO_DEMAND"
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
  
  
  
  # Finally, add several empty columns, including QA/QC columns, to 'ewrimsDF'
  ewrimsDF <- ewrimsDF %>%
    mutate(BASIN = NA_character_, 
           MAINSTEM = NA_character_, 
           LATITUDE = NA_character_, 
           LONGITUDE = NA_character_,
           QAQC_ACTION_TAKEN_Y_N = NA_character_,
           CHANGE_MADE = NA_character_,
           REASON_FOR_CHANGE = NA_character_)
  
  
  
  # Write 'ewrimsDF' to a sheet in 'wb'
  writeData(wb, "MasterDemandTable", startCol = 1, startRow = 1,
            ewrimsDF %>%
              select(APPLICATION_NUMBER, WATER_RIGHT_TYPE, WATER_RIGHT_STATUS, 
                     PRIMARY_OWNER_TYPE, APPLICATION_PRIMARY_OWNER, PRIMARY_BENEFICIAL_USE, 
                     ASSIGNED_PRIORITY_DATE, ASSIGNED_PRIORITY_DATE_SOURCE, PRE_1914, 
                     RIPARIAN, APPROPRIATIVE, NULL_DEMAND, ZERO_DEMAND, 
                     `FULLY NON-CONSUMPTIVE`, POWER_DEMAND_ZEROED, 
                     JAN_EXPECTED_TOTAL_DIVERSION, FEB_EXPECTED_TOTAL_DIVERSION, 
                     MAR_EXPECTED_TOTAL_DIVERSION, APR_EXPECTED_TOTAL_DIVERSION, 
                     MAY_EXPECTED_TOTAL_DIVERSION, JUN_EXPECTED_TOTAL_DIVERSION, 
                     JUL_EXPECTED_TOTAL_DIVERSION, AUG_EXPECTED_TOTAL_DIVERSION, 
                     SEP_EXPECTED_TOTAL_DIVERSION, OCT_EXPECTED_TOTAL_DIVERSION, 
                     NOV_EXPECTED_TOTAL_DIVERSION, DEC_EXPECTED_TOTAL_DIVERSION, 
                     TOTAL_EXPECTED_ANNUAL_DIVERSION, TOTAL_MAY_SEPT_DIV, PERCENT_FACE, 
                     INI_REPORTED_DIV_AMOUNT_AF, FACE_VALUE_AMOUNT_AF, BASIN, MAINSTEM, 
                     LATITUDE, LONGITUDE, SOURCE_NAME, TRIB_DESC, WATERSHED, 
                     QAQC_ACTION_TAKEN_Y_N, CHANGE_MADE, REASON_FOR_CHANGE))
  
  
  
  # Save 'wb' to a file
  saveWorkbook(wb, "OutputData/QAQC_Working_File_Scripted.xlsx", overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


spreadsheetAdjustment <- function (sheetDF) {
  
  # Remove the first two rows from 'sheetDF'
  # The second removed row will also be used as the column names of the DF
  
  sheetDF <- sheetDF[-c(1:2), ] %>%
    set_names(sheetDF[2, ] %>% unlist() %>% as.vector())
  
  
  # Return 'sheetDF'
  return(sheetDF)

}



#### Original Code ####


# Code from the original procedure that attempted to create all sheets in the QA/QC workbook


# mainProcedure <- function () {
#   
#   # The main body of the script
#   
#   
#   # Initialize the Excel workbook
#   wb <- createWorkbook()
#   
#   
#   
#   #### Sheet 1: MissingRMS ####
#   
#   
#   # The last five columns from the "Missing_RMS_Reports" module will be written
#   
#   
#   # First read in that module's data
#   missingDF <- read_xlsx("OutputData/Missing_RMS_Reports_Scripted.xlsx", 
#                          col_types = "text")
#   
#   
#   # Save only the last five columns
#   missingDF <- missingDF[, c((ncol(missingDF) - 4):ncol(missingDF))]
#   
#   
#   # Then, use the generic function to update 'wb'
#   # (The data has already been read into R, so 'dfIsFilePath' is FALSE)
#   # (Also, the top header must be removed and QA/QC columns must be added)
#   wb <- sheetDataImporter(missingDF, wb, "MissingRMS", dfIsFilePath = FALSE, 
#                           removeTopHeader = TRUE, includeQAQC = TRUE)
#   
#   
#   
#   #### Sheet 2: BeneficialUse_ReturnFlow ####
#   
#   
#   # Add the entirety of the "Beneficial_Use_Return_Flow" module results to 'wb'
#   wb <- sheetDataImporter("OutputData/Beneficial_Use_Return_Flow_Scripted.xlsx", 
#                           wb, "BeneficialUse_ReturnFlow",dfIsFilePath = TRUE, 
#                           sheetNum = 1, removeTopHeader = FALSE, includeQAQC = FALSE)
#   
#   
#   
#   #### Sheet 3: DiversionOutofSeasonA ####
#   
#   
#   # For this sheet, the generic function will not be used
#   # Create the worksheet for Part A of the "Diversion_Out_Of_Season" module
#   addWorksheet(wb, "DiversionOutofSeasonA")
#   
#   
#   # Read in the data for this module (and remove the first two rows)
#   oosDF_A <- read_xlsx("OutputData/Diversion_Out_of_Season_Part_A_Scripted.xlsx",
#                        col_types = "text") %>%
#     spreadsheetAdjustment()
#   
#   
#   # Select data from the second table stored within 'oosDF_A'
#   # (This one has the second instance of "APPLICATION_NUMBER" as well as the
#   # final monthly "DIRECT" and "STORAGE" columns)
#   extracted_oos <- oosDF_A[, c(which(names(oosDF_A) == "APPLICATION_NUMBER")[2],
#                                grep("[A-Z]_DIRECT", names(oosDF_A)), 
#                                grep("[A-Z]_STORAGE", names(oosDF_A)))]
#   
#   
#   # Add 24 empty columns (manual override columns) to 'extracted_oos'
#   # There should be two for each month ("STORAGE" and "DIRECT" have separate columns)
#   extracted_oos <- rep(NA_character_, 24 * nrow(extracted_oos)) %>%
#     matrix(ncol = 24) %>% data.frame() %>% tibble() %>%
#     set_names(paste0(names(extracted_oos)[-1], "_Manual")) %>%
#     cbind(extracted_oos)
#   
#   
#   # Write 'extracted_oos' to the spreadsheet
#   writeData(wb, "DiversionOutofSeasonA", startCol = 1, startRow = 1,
#             extracted_oos %>%
#               select(APPLICATION_NUMBER, JAN_DIRECT, FEB_DIRECT, MAR_DIRECT,
#                      APR_DIRECT, MAY_DIRECT, JUN_DIRECT, JUL_DIRECT, AUG_DIRECT,
#                      SEP_DIRECT, OCT_DIRECT, NOV_DIRECT, DEC_DIRECT,
#                      JAN_DIRECT_Manual, FEB_DIRECT_Manual, MAR_DIRECT_Manual,
#                      APR_DIRECT_Manual, MAY_DIRECT_Manual, JUN_DIRECT_Manual,
#                      JUL_DIRECT_Manual, AUG_DIRECT_Manual, SEP_DIRECT_Manual,
#                      OCT_DIRECT_Manual, NOV_DIRECT_Manual, DEC_DIRECT_Manual,
#                      JAN_STORAGE, FEB_STORAGE, MAR_STORAGE, APR_STORAGE, MAY_STORAGE,
#                      JUN_STORAGE, JUL_STORAGE, AUG_STORAGE, SEP_STORAGE, OCT_STORAGE,
#                      NOV_STORAGE, DEC_STORAGE, JAN_STORAGE_Manual, FEB_STORAGE_Manual, 
#                      MAR_STORAGE_Manual, APR_STORAGE_Manual, MAY_STORAGE_Manual, 
#                      JUN_STORAGE_Manual, JUL_STORAGE_Manual, AUG_STORAGE_Manual, 
#                      SEP_STORAGE_Manual, OCT_STORAGE_Manual, NOV_STORAGE_Manual, 
#                      DEC_STORAGE_Manual))
#   
#   
#   
#   #### Sheet 4: DiversionOutofSeasonB ####
#   
#   
#   # Create a worksheet for Part B of the "Diversion_Out_Of_Season" module
#   wb <- sheetDataImporter("OutputData/Diversion_Out_of_Season_Part_B_Scripted.xlsx", 
#                           wb, "DiversionOutofSeasonB", dfIsFilePath = TRUE, 
#                           sheetNum = 2, removeTopHeader = TRUE, includeQAQC = TRUE)
#   
#   
#   
#   #### Sheet 5: Duplicate_SameOwnerMultiWR ####
#   
#   
#   # Create a worksheet for the "DuplicateReport_SameOwner" module
#   wb <- sheetDataImporter("OutputData/DuplicateReport_SameOwner_Scripted.xlsx", 
#                           wb, "Duplicate_SameOwnerMultiWR", dfIsFilePath = TRUE, 
#                           sheetNum = 1, removeTopHeader = TRUE, includeQAQC = TRUE)
#   
#   
#   
#   #### Sheet 6: Duplicate_MonthsYears ####
#   
#   
#   # Create a worksheet for the "DuplicateReport_SameOwner" module
#   wb <- sheetDataImporter("OutputData/DuplicateMonths_Years_Scripted.xlsx", 
#                           wb, "Duplicate_MonthsYears", dfIsFilePath = TRUE, 
#                           sheetNum = 1, removeTopHeader = FALSE, includeQAQC = TRUE)
#   
#   
#   #### Sheet 7: Overlapping_Water_Rights ####
#   
#   
#   # Add a worksheet for the "Overlapping_Water_Rights.csv" 
#   wb <- sheetDataImporter("InputData/Overlapping_Water_Rights.csv", 
#                           wb, "Overlapping_Water_Rights", dfIsFilePath = TRUE, 
#                           sheetNum = 1, removeTopHeader = FALSE, includeQAQC = TRUE)
#   
#   
#   #### Sheet 8: PriorityDate ####
#   
#   
#   # Read in the "Priority_Date" module and extract 13 of the columns
#   priorityDF <- read_xlsx("OutputData/Priority_Date_Scripted.xlsx", col_types = "text") %>%
#     select(APPLICATION_NUMBER, WATER_RIGHT_TYPE, PRIORITY_DATE, APPLICATION_RECD_DATE, 
#            APPLICATION_ACCEPTANCE_DATE, SUB_TYPE, YEAR_DIVERSION_COMMENCED,
#            ASSIGNED_PRIORITY_DATE, PRE_1914, RIPARIAN, APPROPRIATIVE, 
#            APPROPRIATIVE_DATE_SOURCE, STATEMENT_PRIORITY_SOURCE)
#   
#   
#   # Add that data to 'wb'
#   wb <- sheetDataImporter(priorityDF, 
#                           wb, "PriorityDate", dfIsFilePath = FALSE,
#                           removeTopHeader = FALSE, includeQAQC = TRUE)
#   
#   
#   
#   #### Sheet 9: ExpectedDemand_ExceedsFV ####
#   
#   
#   # Create a new worksheet with data from the "ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics" module
#   wb <- sheetDataImporter("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx", 
#                           wb, "ExpectedDemand_ExceedsFV", dfIsFilePath = TRUE,
#                           sheetNum = 1, removeTopHeader = FALSE, includeQAQC = FALSE)
#   
#   
#   #### Sheet 10: DiversionData ####
#   
#   
#   # This is also a sheet based on the "Expected Demand" data 
#   # Prepare its data in a separate function
#   wb <- diversionData(wb)
#   
#   
#   #### Sheet 11: ewrims_flat_file_Working_File ####
#   
#   
#   # Import the eWRIMS working file directly into a sheet
#   wb <- sheetDataImporter("InputData/ewrims_flat_file_Working_File.csv", 
#                           wb, "ewrims_flat_file_Working_File", dfIsFilePath = TRUE, 
#                           sheetNum = 1, removeTopHeader = FALSE, includeQAQC = FALSE)
#   
#   
#   #### Sheet 12: MasterDemandTable ####
#   
#   
#   # The final sheet is a combination of different datasets
#   # Prepare that worksheet in a separate function
#   wb <- masterTableCreator(wb)
#   
#   
#   
#   
#   # Return nothing
#   return(invisible(NULL))
#   
# }
# 
# 
# sheetDataImporter <- function (scriptedDF, wb, wsName, dfIsFilePath = FALSE, sheetNum = 1, 
#                                removeTopHeader = TRUE, includeQAQC = FALSE) {
#   
#   # This is a generic function for adding data to worksheets in the workbook 'wb'
#   
#   # Often, the QAQC sheet simply contains the corresponding module's columns
#   # This function contains the procedure to update 'wb' with that data 
#   # (in a worksheet called 'wsName')
#   
#   # Some cases require additional customization
#   # The three boolean variables assist with applying those specific changes while 
#   # maintaining a generic procedure
#   
#   # If 'dfIsFilePath' is FALSE, 'scriptedDF' should be a data frame
#   # Otherwise, it will be the path to a spreadsheet that must be read into R
#   
#   # There is also an option to remove the first three lines from the data frame 
#   # (3 if including the column names in the count)
#   # This is determined by the variable 'removeTopHeader'
#   
#   # 'includeQAQC' is a boolean for whether three additional QA/QC columns should
#   # be written to the worksheet
#   
#   
#   
#   # Start by reading in the data frame (if needed)
#   if (dfIsFilePath == TRUE) {
#     
#     # The filepath should be stored in 'scriptedDF' right now
#     
#     # If it is a CSV file, use read.csv()
#     # Otherwise, use read_xlsx()
#     if (grepl("\\.csv$", scriptedDF)) {
#     
#       scriptedDF <- read.csv(scriptedDF)
#     
#     # Otherwise, use read_xlsx() 
#     # The sheet that will be read into R is specified by 'sheetNum'
#     } else {
#       scriptedDF <- read_xlsx(scriptedDF, sheet = sheetNum, col_types = "text")
#     }
#     
#     
#   }
#   
#   
#   
#   # If 'removeTopHeader' is TRUE, perform that operation now (in a separate function)
#   if (removeTopHeader == TRUE) {
#     scriptedDF <- scriptedDF %>% spreadsheetAdjustment()
#   }
#   
#   
#   
#   # Add a worksheet to 'wb' with the name 'wsName'
#   addWorksheet(wb, wsName)
#   
#   
#   
#   # Write 'scriptedDF' to the sheet
#   writeData(wb, wsName, startCol = 1, startRow = 1, scriptedDF)
#   
#   
#   
#   # If 'includeQAQC' is TRUE, use qaColumns() to include QA/QC columns
#   # The value for 'startRow' will depend on 'removeTopHeader'
#   if (includeQAQC == TRUE) {
#     
#     # If 'removeTopHeader' is TRUE, to be on the same line as the rest of the data,
#     # 'startRow' should be 1
#     writeData(wb, wsName, startCol = ncol(scriptedDF) + 1, startRow = 1, colNames = FALSE,
#               qaColumns())
#     
#   # And if it's FALSE, it should start on Row 3
#   } else {
#     
#     writeData(wb, wsName, startCol = ncol(scriptedDF) + 1, startRow = 3, colNames = FALSE,
#               qaColumns())
#     
#   }
#   
#   
#   
#   # Return 'wb' after these updates
#   return(wb)
#   
# }
# 
# 
# qaColumns <- function () {
#   
#   # Return a data frame with a single row
#   # It will contain the QAQC column headers that are present in a few sheets
#   
#   return(c("QAQC_ACTION_TAKEN_Y_N", "CHANGE_MADE", "REASON_FOR_CHANGE") %>%
#            matrix(nrow = 1) %>% data.frame())
#   
# }
# 
# 
# spreadsheetAdjustment <- function (sheetDF) {
#   
#   # Remove the first two rows from 'sheetDF'
#   # The second removed row will also be used as the column names of the DF
#   
#   
#   sheetDF <- sheetDF[-c(1:2), ] %>%
#     set_names(sheetDF[2, ] %>% unlist() %>% as.vector())
#   
#   
#   # Return 'sheetDF'
#   return(sheetDF)
#   
# }
# 
# 
# diversionData <- function (wb) {
#   
#   # Create the data in Sheet 10 (Diversion Data)
#   # Update 'wb' with this information
#   
#   
#   
#   # Add the worksheet to 'wb'
#   addWorksheet(wb, "DiversionData")
#   
#   
#   
#   # Read in the data from the Expected Demand module
#   expectedDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx",
#                           col_types = "text") %>%
#     spreadsheetAdjustment()
#   
#   
#   
#   # Make one table with diversion information
#   diverDF <- expectedDF[, c(which(names(expectedDF) == "APPLICATION_NUMBER")[2],
#                             grep("^[A-Z]{3}_DIRECT_DIVERSION$", names(expectedDF)),
#                             grep("^[A-Z]{3}_STORAGE_DIVERSION$", names(expectedDF)))] %>%
#     mutate(across(ends_with("DIVERSION"), as.numeric))
#   
#   
#   
#   # Add a new column for each month that is the total diversion (DIRECT + STORAGE)
#   # Also, include three QA/QC columns at the end
#   diverDF <- diverDF %>%
#     mutate(JAN_TOTAL_DIVERSION = JAN_DIRECT_DIVERSION + JAN_STORAGE_DIVERSION,
#            FEB_TOTAL_DIVERSION = FEB_DIRECT_DIVERSION + FEB_STORAGE_DIVERSION,
#            MAR_TOTAL_DIVERSION = MAR_DIRECT_DIVERSION + MAR_STORAGE_DIVERSION,
#            APR_TOTAL_DIVERSION = APR_DIRECT_DIVERSION + APR_STORAGE_DIVERSION,
#            MAY_TOTAL_DIVERSION = MAY_DIRECT_DIVERSION + MAY_STORAGE_DIVERSION,
#            JUN_TOTAL_DIVERSION = JUN_DIRECT_DIVERSION + JUN_STORAGE_DIVERSION,
#            JUL_TOTAL_DIVERSION = JUL_DIRECT_DIVERSION + JUL_STORAGE_DIVERSION,
#            AUG_TOTAL_DIVERSION = AUG_DIRECT_DIVERSION + AUG_STORAGE_DIVERSION,
#            SEP_TOTAL_DIVERSION = SEP_DIRECT_DIVERSION + SEP_STORAGE_DIVERSION,
#            OCT_TOTAL_DIVERSION = OCT_DIRECT_DIVERSION + OCT_STORAGE_DIVERSION,
#            NOV_TOTAL_DIVERSION = NOV_DIRECT_DIVERSION + NOV_STORAGE_DIVERSION,
#            DEC_TOTAL_DIVERSION = DEC_DIRECT_DIVERSION + DEC_STORAGE_DIVERSION,
#            QAQC_ACTION_TAKEN_Y_N = NA_character_,
#            CHANGE_MADE = NA_character_,
#            REASON_FOR_CHANGE = NA_character_)
#   
#   
#   
#   # Create a separate variable with expected total diversion values
#   # (There are columns in 'expectedDF' with this name, but they are calculated differently)
#   # (Averages of sums vs sums of averages)
#   # (Include the QA/QC columns as well)
#   sumDF <- diverDF %>%
#     group_by(APPLICATION_NUMBER) %>%
#     summarize(JAN_EXPECTED_TOTAL_DIVERSION = mean(JAN_TOTAL_DIVERSION, na.rm = TRUE),
#               FEB_EXPECTED_TOTAL_DIVERSION = mean(FEB_TOTAL_DIVERSION, na.rm = TRUE),
#               MAR_EXPECTED_TOTAL_DIVERSION = mean(MAR_TOTAL_DIVERSION, na.rm = TRUE),
#               APR_EXPECTED_TOTAL_DIVERSION = mean(APR_TOTAL_DIVERSION, na.rm = TRUE),
#               MAY_EXPECTED_TOTAL_DIVERSION = mean(MAY_TOTAL_DIVERSION, na.rm = TRUE),
#               JUN_EXPECTED_TOTAL_DIVERSION = mean(JUN_TOTAL_DIVERSION, na.rm = TRUE),
#               JUL_EXPECTED_TOTAL_DIVERSION = mean(JUL_TOTAL_DIVERSION, na.rm = TRUE),
#               AUG_EXPECTED_TOTAL_DIVERSION = mean(AUG_TOTAL_DIVERSION, na.rm = TRUE),
#               SEP_EXPECTED_TOTAL_DIVERSION = mean(SEP_TOTAL_DIVERSION, na.rm = TRUE),
#               OCT_EXPECTED_TOTAL_DIVERSION = mean(OCT_TOTAL_DIVERSION, na.rm = TRUE),
#               NOV_EXPECTED_TOTAL_DIVERSION = mean(NOV_TOTAL_DIVERSION, na.rm = TRUE),
#               DEC_EXPECTED_TOTAL_DIVERSION = mean(DEC_TOTAL_DIVERSION, na.rm = TRUE),
#               .groups = "drop") %>%
#     rowwise() %>%
#     mutate(TOTAL_ANNUAL_EXPECTED_DIVERSION = JAN_EXPECTED_TOTAL_DIVERSION + 
#              FEB_EXPECTED_TOTAL_DIVERSION + MAR_EXPECTED_TOTAL_DIVERSION + 
#              APR_EXPECTED_TOTAL_DIVERSION + MAY_EXPECTED_TOTAL_DIVERSION + 
#              JUN_EXPECTED_TOTAL_DIVERSION + JUL_EXPECTED_TOTAL_DIVERSION +
#              AUG_EXPECTED_TOTAL_DIVERSION + SEP_EXPECTED_TOTAL_DIVERSION +
#              OCT_EXPECTED_TOTAL_DIVERSION + NOV_EXPECTED_TOTAL_DIVERSION + 
#              DEC_EXPECTED_TOTAL_DIVERSION,
#            MAY_TO_SEPT_EXPECTED_DIVERSION = MAY_EXPECTED_TOTAL_DIVERSION + 
#              JUN_EXPECTED_TOTAL_DIVERSION + JUL_EXPECTED_TOTAL_DIVERSION +
#              AUG_EXPECTED_TOTAL_DIVERSION + SEP_EXPECTED_TOTAL_DIVERSION,
#            QAQC_ACTION_TAKEN_Y_N = NA_character_,
#            CHANGE_MADE = NA_character_,
#            REASON_FOR_CHANGE = NA_character_) %>%
#     ungroup()
#   
#   
#   
#   # Write these data frames to the worksheet
#   writeData(wb, "DiversionData", startCol = 1, startRow = 3, diverDF)
#   writeData(wb, "DiversionData", startCol = 42, startRow = 3, sumDF)
#   
#   
#   
#   # Add accompanying text as well
#   writeData(wb, "DiversionData", startCol = 1, startRow = 1, "INFO:")
#   writeData(wb, "DiversionData", startCol = 1, startRow = 2, "ACTION:")
#   
#   
#   writeData(wb, "DiversionData", startCol = 2, startRow = 1, 
#             "REPORTED DIRECT DIVERSION BY YEAR")
#   writeData(wb, "DiversionData", startCol = 2, startRow = 2, 
#             "IF CHANGES TO DIVERSION DATA ARE NEEDED, DIRECTLY OVERWRITE THE RECORD, AND UPDATE THE QAQC COLUMNS")
#   
#   
#   writeData(wb, "DiversionData", startCol = 14, startRow = 1, 
#             "REPORTED DIVERSION TO STORAGE BY YEAR")
#   writeData(wb, "DiversionData", startCol = 14, startRow = 2, 
#             "IF CHANGES TO DIVERSION DATA ARE NEEDED, DIRECTLY OVERWRITE THE RECORD, AND UPDATE THE QAQC COLUMNS")
#   
#   
#   writeData(wb, "DiversionData", startCol = 26, startRow = 1, 
#             "REPORTED TOTAL DIVERSION BY YEAR")
#   writeData(wb, "DiversionData", startCol = 26, startRow = 2, 
#             "DO NOT MAKE CHANGES HERE")
#   
#   
#   writeData(wb, "DiversionData", startCol = 42, startRow = 1, 
#             "MONTHLY AVERAGE TOTAL DIVERSIONS (STORAGE + DIRECT)")
#   writeData(wb, "DiversionData", startCol = 42, startRow = 2, 
#             "IF CHANGES TO DIVERSION DATA ARE NEEDED, DIRECTLY OVERWRITE THE RECORD, AND UPDATE THE QAQC COLUMNS")
#   
#   
#   
#   # Return 'wb'
#   return(wb)
#   
# }
# 
# 
# masterTableCreator <- function (wb) {
#   
#   # Create the table featured in the "MasterDemandTable" worksheet and write it to 'wb'
#   
#   
#   
#   # Add a worksheet for the master table
#   addWorksheet(wb, "MasterDemandTable")
#   
#   
#   
#   # Next, construct the table that will be input into this worksheet
#   
#   
#   
#   # The basis of the table will be "ewrims_flat_file_Working_File.csv"
#   # (In the master table, "PRIMARY_OWNER_ENTITY_TYPE" is called "PRIMARY_OWNER_TYPE")
#   ewrimsDF <- read.csv("InputData/ewrims_flat_file_Working_File.csv") %>%
#     rename(PRIMARY_OWNER_TYPE = PRIMARY_OWNER_ENTITY_TYPE)
#   
#   
#   
#   # Add in columns from the beneficial use module
#   beneficialUse <- read_xlsx("OutputData/Beneficial_Use_Return_Flow_Scripted.xlsx") %>%
#     spreadsheetAdjustment()
#   
#   
#   # Narrow the table down to the four desired columns
#   beneficialUse <- beneficialUse[, c(which(names(beneficialUse) == "APPLICATION_NUMBER")[2],
#                                      which(names(beneficialUse) %in% c("ASSIGNED_BENEFICIAL_USE",
#                                                                        "FULLY NON-CONSUMPTIVE",
#                                                                        "POWER_DEMAND_ZEROED")))]
#   
#   
#   # Join those columns to 'ewrimsDF'
#   ewrimsDF <- ewrimsDF %>%
#     left_join(beneficialUse, by = "APPLICATION_NUMBER", relationship = "one-to-one")
#   
#   
#   
#   # Next, data from the priority date module will be added to 'ewrimsDF'
#   priorityDF <- read_xlsx("OutputData/Priority_Date_Scripted.xlsx", col_types = "text") %>%
#     rename(ASSIGNED_PRIORITY_DATE_SOURCE = APPROPRIATIVE_DATE_SOURCE) %>%
#     select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE, ASSIGNED_PRIORITY_DATE_SOURCE, 
#            PRE_1914, RIPARIAN, APPROPRIATIVE) %>%
#     unique()
#   
#   
#   # Use a left join once again
#   ewrimsDF <- ewrimsDF %>%
#     left_join(priorityDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")
#   
#   
#   
#   # After that, read in the spreadsheet table for the expected demand module
#   expectedDF <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx",
#                           col_types = "text") %>%
#     spreadsheetAdjustment()
#   
#   
#   # Get two sub-tables from the main dataset
#   faceVars <- expectedDF[, c(which(names(expectedDF) == "APPLICATION_NUMBER")[3],
#                              which(names(expectedDF) == "FACE_VALUE_AMOUNT")[2],
#                              which(names(expectedDF) == "IniDiv_Converted_to_AF"))] %>%
#     unique()
#   
#   
#   # For the second sub-table, add a new indicator variable for whether the 
#   # APPLICATION_NUMBER column is NA
#   nullVar <- expectedDF[, which(names(expectedDF) == "APPLICATION_NUMBER")[4]] %>%
#     mutate(NULL_DEMAND = if_else(!is.na(APPLICATION_NUMBER), "N", "Y")) %>%
#     unique()
#   
#   
#   
#   # Join both of these datasets to 'ewrimsDF'
#   ewrimsDF <- ewrimsDF %>%
#     left_join(faceVars, by = "APPLICATION_NUMBER", relationship = "one-to-one") %>%
#     left_join(nullVar, by = "APPLICATION_NUMBER", relationship = "one-to-one")
#   
#   
#   
#   # Data from the "DiversionData" sheet is needed next
#   diverDF <- readWorkbook(wb, "DiversionData", startRow = 3, sep.names = "_")
#   
#   names(diverDF)
#   
#   
# }


#### Script Execution ####

mainProcedure()