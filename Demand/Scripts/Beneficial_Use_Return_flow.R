# Calculate and summarize data on each right holder's beneficial use and return flow


# This script is a recreation of the Excel module Beneficial_Use_Return_Flow.xlsx"


#### Dependencies ####


require(tidyverse)
require(openxlsx)


#### Script Procedure ####


mainProcedure <- function () {
  
  # The main body of the script
  
  
  # Read in the input CSV file for this analysis
  inputDF <- read.csv("InputData/Beneficial_Use_and_Return_Flow_FINAL.csv")
  
  
  
  # Based on the value of "USE_CODE", assign a ranking to each row
  # (The rankings for each use type will be accessible through a function)
  inputDF <- inputDF %>%
    left_join(useRanking(), by = "USE_CODE", relationship = "many-to-one")
  
  # NOTE
  # Each use type should match with exactly one ranking
  # And multiple rows in 'inputDF' will have the same use type
  # Therefore, "many-to-one" is specified for "relationship"
  
  
  # Error Check
  # Every row in 'inputDF' should have a value in "USE_RANKING"
  stopifnot(!anyNA(inputDF$USE_RANKING))
  
  
  
  # The next column to add to 'inputDF' is "HIGHEST_RANK"
  # For each unique application number, get the smallest rank value (1 is "highest")
  
  
  # Create a temporary data frame that contains the smallest rank for each right holder
  tempDF <- inputDF %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(HIGHEST_RANK = min(USE_CODE_RANK))
  
  
  # Join 'tempDF' to 'inputDF' (it should be a "many-to-one" relationship)
  inputDF <- inputDF %>%
    left_join(tempDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  
  # A similar procedure is needed for the next three columns
  # ("IRRIGATION", "MUNICIPAL", and "DOMESTIC_COUNT")
  
  # Create a summarized data frame of 'inputDF' with a single row per application number
  # Then, join that data frame to 'inputDF'
  
  # These three columns are counts of "USE_CODE_RANK" for specific ranks
  # ("DOMESTIC_COUNT" also only counts for "Government (State/Municipal)" entity types)
  # (Also, "DOMESTIC_COUNT" was renamed because the name "DOMESTIC" is also used later)
  
  
  # Create a temporary data frame with counts by use code
  tempDF <- inputDF %>%
    group_by(APPLICATION_NUMBER, USE_CODE) %>%
    summarize(COUNTS = n(), .groups = "drop")
  
  
  # Add "IRRIGATION" to 'inputDF' (the rank for "Irrigation" is 1)
  # Then, rename "COUNTS" to "IRRIGATION"
  # After that, replace NA entries in "IRRIGATION" with 0
  inputDF <- left_join(x = inputDF,
                       y = tempDF %>% filter(USE_CODE == "Irrigation") %>% select(-USE_CODE),
                       by = "APPLICATION_NUMBER", relationship = "many-to-one") %>%
    rename(IRRIGATION = COUNTS) %>%
    mutate(IRRIGATION = replace_na(IRRIGATION, 0))
  
  
  # Repeat a similar procedure for "MUNICIPAL" (the rank for "Municipal" is 2)
  inputDF <- left_join(x = inputDF,
                       y = tempDF %>% filter(USE_CODE == "Municipal") %>% select(-USE_CODE),
                       by = "APPLICATION_NUMBER", relationship = "many-to-one") %>%
    rename(MUNICIPAL = COUNTS) %>%
    mutate(MUNICIPAL = replace_na(MUNICIPAL, 0))
  
  
  # 'tempDF' needs to be redefined for "DOMESTIC_COUNT"
  # A filter must be added so that "PRIMARY_OWNER_ENTITY_TYPE" is equal to 
  # "Government (State/Municipal)"
  tempDF <- inputDF %>%
    filter(PRIMARY_OWNER_ENTITY_TYPE == "Government (State/Municipal)") %>%
    group_by(APPLICATION_NUMBER, USE_CODE) %>%
    summarize(COUNTS = n(), .groups = "drop")
  
  
  # Then repeat the join procedure for "DOMESTIC_COUNT" (its use code ranking is 3)
  inputDF <- left_join(x = inputDF,
                       y = tempDF %>% filter(USE_CODE == "Domestic") %>% select(-USE_CODE),
                       by = "APPLICATION_NUMBER", relationship = "many-to-one") %>%
    rename(DOMESTIC_COUNT = COUNTS) %>%
    mutate(DOMESTIC_COUNT = replace_na(DOMESTIC_COUNT, 0))
  
  
  
  # The next group of columns can be defined using if_else() statements:
  # "IRRIGATION_AND_MUNICIPAL"
  #   If "IRRIGATION" and "MUNICIPAL" have non-zero counts, this column is 
  #   "IRRIGATION_AND_MUNICIPAL"; otherwise, it is NA
  # "IRRIGATION_AND_DOMESTIC"
  #   If "IRRIGATION" and "DOMESTIC_COUNT" have non-zero counts, this column is 
  #   "IRRIGATION_AND_DOMESTIC"; otherwise, it is NA
  # "STOCKWATERING"
  #   If "WATER_RIGHT_TYPE" contains the fragment "Stock", this column is
  #   "Stockwatering"; otherwise, it is NA
  # "DOMESTIC"
  #   If "WATER_RIGHT_TYPE" is "Registration Domestic", this column is
  #   "Domestic"; otherwise, it is NA
  # "REGISTRATION_IRRIGATION"
  #   If "WATER_RIGHT_TYPE" is "Registration Irrigation", this column is
  #   "Irrigation"; otherwise, it is NA
  # "REGISTRATION_CANNABIS"
  #   If "WATER_RIGHT_TYPE" is "Registration Cannabis", this column is
  #   "Irrigation"; otherwise, it is NA
  # "MANUAL_OVERRIDE_BU"
  #   This column would contain manually-specified assignments for the
  #   right holder's primary beneficial use type
  #   For now, this column will be left completely blank (NA)
  #   However, if needed, an input spreadsheet (with columns for "APPLICATION_NUMBER"
  #   and "MANUAL_OVERRIDE_BU") could be created and joined to 'inputDF'
  inputDF <- inputDF %>%
    mutate(IRRIGATION_AND_MUNICIPAL = if_else(IRRIGATION > 0 & MUNICIPAL > 0,
                                              "IRRIGATION_AND_MUNICIPAL", NA_character_),
           IRRIGATION_AND_DOMESTIC = if_else(IRRIGATION > 0 & DOMESTIC_COUNT > 0,
                                             "IRRIGATION_AND_DOMESTIC", NA_character_),
           STOCKWATERING = if_else(grepl("Stock", WATER_RIGHT_TYPE, ignore.case = TRUE),
                                   "Stockwatering", NA_character_),
           DOMESTIC = if_else(WATER_RIGHT_TYPE == "Registration Domestic",
                              "Domestic", NA_character_),
           REGISTRATION_IRRIGATION = if_else(WATER_RIGHT_TYPE == "Registration Irrigation",
                                             "Irrigation", NA_character_),
           REGISTRATION_CANNABIS = if_else(WATER_RIGHT_TYPE == "Registration Cannabis",
                                           "Irrigation", NA_character_),
           MANUAL_OVERRIDE_BU = NA_character_)
  
  
  
  # The next column to add is "ASSIGNED_BENEFICIAL_USE_BY_WR"
  
  # The value of this column is based on the data in the previously created columns
  # Going backwards from the most recently added column, check if the column is not NA
  # If that is the case, use that column's value (or a similar string) in this column
  
  # (There is an exception in the ordering; "IRRIGATION_AND_MUNICIPAL" is checked
  # before "IRRIGATION_AND_DOMESTIC")
  
  # If no value is present in any of the previously created columns,
  # use the "USE_CODE" that corresponds with the row's value for "HIGHEST_RANKING" 
  
  inputDF <- inputDF %>%
    mutate(ASSIGNED_BENEFICIAL_USE_BY_WR = 
             if_else(!is.na(MANUAL_OVERRIDE_BU), 
                     MANUAL_OVERRIDE_BU,
                     if_else(!is.na(REGISTRATION_CANNABIS),
                             REGISTRATION_CANNABIS,
                             if_else(!is.na(REGISTRATION_IRRIGATION),
                                     REGISTRATION_IRRIGATION,
                                     if_else(!is.na(DOMESTIC),
                                             DOMESTIC,
                                             if_else(!is.na(STOCKWATERING),
                                                     STOCKWATERING,
                                                     if_else(!is.na(IRRIGATION_AND_MUNICIPAL),
                                                             "Irrigation and Municipal",
                                                             if_else(!is.na(IRRIGATION_AND_DOMESTIC),
                                                                     "Irrigation and Domestic Municipality",
                                                                     useRanking()[["USE_CODE"]][HIGHEST_RANK]))))))))
  
  
  
  # The next columns require a new data frame
  # There should be only one row per "APPLICATION_NUMBER"
  
  # It will be accompanied by the column "ASSIGNED_BENEFICIAL_USE"
  # This will contain the primary beneficial use of the right holder
  
  # In the Excel module, the primary beneficial use is selected by picking the
  # first match in "ASSIGNED_BENEFICIAL_USE_BY_WR" for each application number
  # This does not seem to be an accurate method for identifying the primary beneficial use
  # Therefore, a different approach will be used
  
  
  # Filter 'inputDF' to rows where "USE_CODE_RANK" and "HIGHEST_RANK" are equal
  # Select "APPLICATION_NUMBER" and "ASSIGNED_BENEFICIAL_USE_BY_WR" (and rename the latter)
  # Keep only unique matches after that
  resDF <- inputDF %>%
    filter(USE_CODE_RANK == HIGHEST_RANK) %>%
    select(APPLICATION_NUMBER, ASSIGNED_BENEFICIAL_USE_BY_WR) %>%
    rename(ASSIGNED_BENEFICIAL_USE = ASSIGNED_BENEFICIAL_USE_BY_WR) %>%
    unique()
  
  
  # As an error check, verify that each application number appears only once
  stopifnot(nrow(resDF) == length(unique(resDF$APPLICATION_NUMBER)))
  stopifnot(nrow(resDF) == length(unique(inputDF$APPLICATION_NUMBER)))
  
  
  # The next two columns, "FULLY NON-CONSUMPTIVE" and "POWER_DEMAND_ZEROED",
  # are Y/N columns based on the value of "ASSIGNED_BENEFICIAL_USE"
  resDF <- resDF %>%
    mutate(`FULLY NON-CONSUMPTIVE` = 
             if_else(ASSIGNED_BENEFICIAL_USE %in% 
                       c("Power", "Recreational", "Aquaculture", "Fish and Wildlife Preservation and Enhancement"),
                     "Y",
                     "N"),
           POWER_DEMAND_ZEROED = if_else(ASSIGNED_BENEFICIAL_USE == "Power", "Y", "N"))
  
  
  
  # For the next groups of columns, the exact values will depend on whether
  # each right has a standard or manually-input return flow percentage
  
  # For now, every row will be assigned to "STANDARD" 
  # (Meaning that their return flow percentages will follow the standard
  # values given in the table of standardReturnFlow())
  
  # Support for manually-entered percentages will be decided upon later
  
  
  # So, to start, create the column "STD_OR_MANUAL_RETURN_FLOW"
  # Every row will have a value of "STANDARD"
  resDF <- resDF %>%
    mutate(STD_OR_MANUAL_RETURN_FLOW = "STANDARD")
  
  
  # Initialize the manual override columns for each month
  # They will all be NA
  resDF <- resDF %>%
    mutate(JAN_MANUAL_OVERRIDE_RETURN = NA_real_, FEB_MANUAL_OVERRIDE_RETURN = NA_real_,
           MAR_MANUAL_OVERRIDE_RETURN = NA_real_, APR_MANUAL_OVERRIDE_RETURN = NA_real_,
           MAY_MANUAL_OVERRIDE_RETURN = NA_real_, JUN_MANUAL_OVERRIDE_RETURN = NA_real_,
           JUL_MANUAL_OVERRIDE_RETURN = NA_real_, AUG_MANUAL_OVERRIDE_RETURN = NA_real_,
           SEP_MANUAL_OVERRIDE_RETURN = NA_real_, OCT_MANUAL_OVERRIDE_RETURN = NA_real_,
           NOV_MANUAL_OVERRIDE_RETURN = NA_real_, DEC_MANUAL_OVERRIDE_RETURN = NA_real_)
  
  
  # Also add the standard percentage columns to all rows
  # By using a many-to-one join, the standard percentages for each month can be added to each row
  # "ASSIGNED_BENEFICIAL_USE" is matched with "USE_CODE" (though the latter must be renamed for the join)
  resDF <- resDF %>%
    left_join(standardReturnFlow() %>% rename(ASSIGNED_BENEFICIAL_USE = USE_CODE),
              by = "ASSIGNED_BENEFICIAL_USE", relationship = "many-to-one") %>%
    rename(JAN_STD_PERCENT_USE_RETURN = Jan, FEB_STD_PERCENT_USE_RETURN = Feb,
           MAR_STD_PERCENT_USE_RETURN = Mar, APR_STD_PERCENT_USE_RETURN = Apr,
           MAY_STD_PERCENT_USE_RETURN = May, JUN_STD_PERCENT_USE_RETURN = Jun,
           JUL_STD_PERCENT_USE_RETURN = Jul, AUG_STD_PERCENT_USE_RETURN = Aug,
           SEP_STD_PERCENT_USE_RETURN = Sep, OCT_STD_PERCENT_USE_RETURN = Oct,
           NOV_STD_PERCENT_USE_RETURN = Nov, DEC_STD_PERCENT_USE_RETURN = Dec)
  
  
  # Error Check
  # Verify that a standard percentage is found for each month for all rows
  stopifnot(!anyNA(resDF$JAN_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$FEB_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$MAR_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$APR_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$MAY_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$JUN_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$JUL_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$AUG_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$SEP_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$OCT_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$NOV_STD_PERCENT_USE_RETURN))
  stopifnot(!anyNA(resDF$DEC_STD_PERCENT_USE_RETURN))
  
  
  
  # Finally, create a column for each month that displays the chosen percentages for each row
  # (It will be either the standard or manual values based on the value of "STD_OR_MANUAL_RETURN_FLOW")
  resDF <- resDF %>%
    mutate(JAN_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             JAN_STD_PERCENT_USE_RETURN, JAN_MANUAL_OVERRIDE_RETURN),
           FEB_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             FEB_STD_PERCENT_USE_RETURN, FEB_MANUAL_OVERRIDE_RETURN),
           MAR_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             MAR_STD_PERCENT_USE_RETURN, MAR_MANUAL_OVERRIDE_RETURN),
           APR_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             APR_STD_PERCENT_USE_RETURN, APR_MANUAL_OVERRIDE_RETURN),
           MAY_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             MAY_STD_PERCENT_USE_RETURN, MAY_MANUAL_OVERRIDE_RETURN),
           JUN_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             JUN_STD_PERCENT_USE_RETURN, JUN_MANUAL_OVERRIDE_RETURN),
           JUL_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             JUL_STD_PERCENT_USE_RETURN, JUL_MANUAL_OVERRIDE_RETURN),
           AUG_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             AUG_STD_PERCENT_USE_RETURN, AUG_MANUAL_OVERRIDE_RETURN),
           SEP_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             SEP_STD_PERCENT_USE_RETURN, SEP_MANUAL_OVERRIDE_RETURN),
           OCT_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             OCT_STD_PERCENT_USE_RETURN, OCT_MANUAL_OVERRIDE_RETURN),
           NOV_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             NOV_STD_PERCENT_USE_RETURN, NOV_MANUAL_OVERRIDE_RETURN),
           DEC_PERCENT_RETURN_FLOW = if_else(STD_OR_MANUAL_RETURN_FLOW == "STANDARD",
                                             DEC_STD_PERCENT_USE_RETURN, DEC_MANUAL_OVERRIDE_RETURN))
  
  
  
  # Finally, prepare a spreadsheet with the output data
  writeSpreadsheet(inputDF, resDF)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


useRanking <- function () {
  
  # Return a data frame containing the ranking for different use codes
  
  # Convert a vector into a matrix and then a data frame
  # Add column names and convert the rankings into a numeric column
  return(
    c("Irrigation", 1,
      "Municipal", 2,
      "Domestic", 3,
      "Power", 4,
      "Stockwatering", 5,
      "Industrial", 6,
      "Frost Protection", 7,
      "Heat Control", 8,
      "Recreational", 9,
      "Dust Control", 10,
      "Mining", 11,
      "Fire Protection", 12,
      "Aquaculture", 13,
      "Fish and Wildlife Preservation and Enhancement", 14,
      "Incidental Power", 15,
      "Milling", 16,
      "Snow Making", 17,
      "Water Quality", 18,
      "Aesthetic", 19,
      "Other", 20) %>%
      matrix(ncol = 2, byrow = TRUE) %>% data.frame() %>%
      set_names(c("USE_CODE", "USE_CODE_RANK")) %>%
      mutate(USE_CODE_RANK = as.numeric(USE_CODE_RANK))
  )
  
}


standardReturnFlow <- function () {
  
  # Return a data frame containing standard return flow percentages
  # by month and use type
  
  
  # Convert a vector into a matrix and then a data frame
  # Add column names and convert the percentages into numeric columns
  return(
    c("Dust Control", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Domestic", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Irrigation", 0, 0, 20, 20, 10, 10, 0, 0, 0, 20, 20, 0,
      "Power", 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
      "Municipal", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Fish and Wildlife Preservation and Enhancement", 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
      "Stockwatering", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Industrial", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Mining", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Recreational", 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
      "Fire Protection", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Incidental Power", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Frost Protection", 20, 20, 20, 20, 0, 0, 0, 0, 0, 0, 0, 0,
      "Aquaculture", 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
      "Snow Making", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Milling", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Heat Control", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Other", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Aesthetic", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Irrigation and Domestic Municipality", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Irrigation and Municipal", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      "Water Quality", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) %>%
      matrix(ncol = 13, byrow = TRUE) %>% data.frame() %>%
      set_names(c("USE_CODE", month.abb)) %>%
      mutate(across(month.abb, as.numeric))
  )
  
}


writeSpreadsheet <- function (inputDF, resDF) {
  
  # Create a spreadsheet with similar formatting to the Excel module
  
  
  # Initialize a new workbook
  wb <- createWorkbook()
  
  
  
  # Add a sheet for "Beneficial_Use_Return_Flow"
  addWorksheet(wb, "Beneficial_Use_Return_Flow")
  
  
  
  # First write 'inputDF' into the spreadsheet
  writeData(wb, "Beneficial_Use_Return_Flow", inputDF, startRow = 3, startCol = 1)
  
  
  # Also include the descriptive text in the adjacent cells 
  writeData(wb, "Beneficial_Use_Return_Flow", "INFO:", startRow = 1, startCol = 1)
  
  writeData(wb, "Beneficial_Use_Return_Flow", "ACTION:", startRow = 2, startCol = 1)
  
  writeData(wb, "Beneficial_Use_Return_Flow", 
            "INPUT DATA FOR SPREADSHEET - FROM FLAT FILES", 
            startRow = 1, startCol = 2)
  
  writeData(wb, "Beneficial_Use_Return_Flow", 
            "PASTE NEW INPUT DATA FROM [NAME OF SCRIPT] SCRIPT HERE - DELETE SAMPLE DATA BELOW", 
            startRow = 2, startCol = 2)
  
  writeData(wb, "Beneficial_Use_Return_Flow", 
            "FORMULA - INTERMEDIATE CALCULATION", 
            startRow = 1, startCol = 9)
  
  writeData(wb, "Beneficial_Use_Return_Flow", 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA", 
            startRow = 2, startCol = 9)
  
  writeData(wb, "Beneficial_Use_Return_Flow", 
            "MANUALLY ENTER BU IF OTHER", 
            startRow = 2, startCol = 20)
  
  writeData(wb, "Beneficial_Use_Return_Flow", 
            "MULTIPLE RECORD PER WR", 
            startRow = 1, startCol = 21)
  
  
  
  # Add in 'resDF' next
  
  # The column ordering required for the spreadsheet 
  # is different from the ordering in 'resDF'
  # select() will be used to get the proper order
  resDF %>%
    select(APPLICATION_NUMBER, ASSIGNED_BENEFICIAL_USE, `FULLY NON-CONSUMPTIVE`,
           POWER_DEMAND_ZEROED, JAN_PERCENT_RETURN_FLOW, FEB_PERCENT_RETURN_FLOW,
           MAR_PERCENT_RETURN_FLOW, APR_PERCENT_RETURN_FLOW, MAY_PERCENT_RETURN_FLOW,
           JUN_PERCENT_RETURN_FLOW, JUL_PERCENT_RETURN_FLOW, AUG_PERCENT_RETURN_FLOW,
           SEP_PERCENT_RETURN_FLOW, OCT_PERCENT_RETURN_FLOW, NOV_PERCENT_RETURN_FLOW,
           DEC_PERCENT_RETURN_FLOW, STD_OR_MANUAL_RETURN_FLOW, JAN_MANUAL_OVERRIDE_RETURN,
           FEB_MANUAL_OVERRIDE_RETURN, MAR_MANUAL_OVERRIDE_RETURN, APR_MANUAL_OVERRIDE_RETURN,
           MAY_MANUAL_OVERRIDE_RETURN, JUN_MANUAL_OVERRIDE_RETURN, JUL_MANUAL_OVERRIDE_RETURN,
           AUG_MANUAL_OVERRIDE_RETURN, SEP_MANUAL_OVERRIDE_RETURN, OCT_MANUAL_OVERRIDE_RETURN,
           NOV_MANUAL_OVERRIDE_RETURN, DEC_MANUAL_OVERRIDE_RETURN, JAN_STD_PERCENT_USE_RETURN,
           FEB_STD_PERCENT_USE_RETURN, MAR_STD_PERCENT_USE_RETURN, APR_STD_PERCENT_USE_RETURN,
           MAY_STD_PERCENT_USE_RETURN, JUN_STD_PERCENT_USE_RETURN, JUL_STD_PERCENT_USE_RETURN,
           AUG_STD_PERCENT_USE_RETURN, SEP_STD_PERCENT_USE_RETURN, OCT_STD_PERCENT_USE_RETURN,
           NOV_STD_PERCENT_USE_RETURN, DEC_STD_PERCENT_USE_RETURN) %>%
    writeData(wb = wb, sheet = "Beneficial_Use_Return_Flow",
              startCol = 23, startRow = 3)
  
  
  # Add the extra text as well
  writeData(wb, "Beneficial_Use_Return_Flow",
            "1 RECORD PER WR",
            startCol = 23, startRow = 1)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "DO NOT FILL DOWN",
            startCol = 23, startRow = 2)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "FINAL RESULTS - PRIMARY BENEFICIAL USE",
            startCol = 24, startRow = 1)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA",
            startCol = 24, startRow = 2)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "FINAL RESULTS - RETURN FLOW PERCENT",
            startCol = 27, startRow = 1)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA - USE THESE RESULTS FOR QAQC - REFER TO DESCRIPTION",
            startCol = 27, startRow = 2)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "MONTHLY MANUAL OVERRIDE FOR NON-STANDARD RETRUN FLOWS",
            startCol = 39, startRow = 1)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            'FOR WATER RIGHTS WHOSE RETURN FLOW PERCENTAGES DIFFER FROM THE STANDARD, TYPE IN "MANUAL" AND UPDATE THE MONTHLY RETURN FLOW PERCENTAGES FOR ALL 12 MONTHS',
            startCol = 39, startRow = 2)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "FORMULA - INTERMEDIATE CALCULATION",
            startCol = 52, startRow = 1)
  
  writeData(wb, "Beneficial_Use_Return_Flow",
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA",
            startCol = 52, startRow = 2)
  
  
  
  # Save 'wb' to a file
  saveWorkbook(wb, "OutputData/Beneficial_Use_Return_Flow_Scripted.xlsx", overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

#mainProcedure()