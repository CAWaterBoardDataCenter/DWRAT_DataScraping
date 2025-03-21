# Verify that every flagging script was run successfully
# The flag table should contain columns related to every prior script


#### Setup ####


remove(list = ls())



require(crayon)
require(data.table)
require(tidyverse)



source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


#### Procedure ####


print("Starting '[HELPER]_2_Verify_Flag_Table.R'...")



# This data frame contains all of the expected column names in the flag table
expectedColumns <- c("APPLICATION_NUMBER", "Extended_Flat_File",
                     "YEAR", "Extended_Flat_File",
                     "MONTH", "Extended_Flat_File",
                     "AMOUNT", "Extended_Flat_File",
                     "DIVERSION_TYPE", "Extended_Flat_File",
                     "PARTY_ID", "Extended_Flat_File",
                     "ASSIGNED_PRIORITY_DATE", "Priority_Date_Script",
                     "APPROPRIATIVE_DATE_SOURCE", "Priority_Date_Script", 
                     "STATEMENT_PRIORITY_SOURCE", "Priority_Date_Script",
                     "PRE_1914", "Priority_Date_Script",
                     "RIPARIAN", "Priority_Date_Script",
                     "APPROPRIATIVE", "Priority_Date_Script",
                     "DUP_REPORT_SAME_RIGHT_YEAR_DIFFERENT_OWNER", "Duplicate_Reporting_Script",
                     "DUP_REPORT_SAME_OWNER_YEAR_DIFFERENT_RIGHT_OR_DIVERSION_TYPE", "Duplicate_Reporting_Script",
                     "MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV", "Expected_Demand_Script",
                     "EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT", "Expected_Demand_Script",
                     "EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL", "Expected_Demand_Script",
                     "REPORT_IS_MISSING_DIVERSION_DATA", "Empty_Reports_Script",
                     "CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION", "Face_Value_Substitution_Script",
                     "PRIMARY_BENEFICIAL_USE", "Beneficial_Use_Script") %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  data.frame() %>%
  set_names(c("COLUMN_NAME", "SOURCE"))



# Read in 'flagDF'
flagDF <- readFlagTable()



# Get a list of columns that are missing in this 'flagDF'
missingDF <- expectedColumns %>%
  filter(!(COLUMN_NAME %in% names(flagDF)))



# If there are missing columns, stop the script and state which scripts were not executed 
if (nrow(missingDF) > 0) {
  
  
  cat("\n\n")
  cat(paste0("Missing Columns") %>% bold() %>% silver())
  cat("\n")
  cat(missingDF %>% mutate(LIST = paste0("   ", COLUMN_NAME, " (", red(SOURCE), ")")) %>%
        select(LIST) %>% unlist(use.names = FALSE) %>%
        paste0(collapse = "\n"))
  cat("\n")
  
  
  stop(paste0("The flag table is missing columns. One or more [CA] scripts was not executed.",
              " The list of missing column(s) was printed above this error message") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("missing", red("missing")) %>%
         str_replace("not", red("not")))
  
  
}



# Check if additional columns are in 'flagDF'
extraDF <- names(flagDF)[!(names(flagDF) %in% expectedColumns$COLUMN_NAME)]



# Output a warning message if there are extra columns in 'flagDF'
if (length(extraDF) > 0) {
  
  
  cat("\n\n")
  cat(paste0("Unrecognized Columns") %>% bold() %>% silver())
  cat("\n   ")
  cat(extraDF %>%
        paste0(collapse = "\n   "))
  cat("\n")
  
  
  message(paste0("The flag table has extra columns. Was there an error or an update to the procedure?",
              " In the latter case, this script should be updated to include the names of the",
              " new columns (add to 'expectedColumns'). In addition, this script should include some checks to",
              " verify the formatting of the new column data.") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("extra", red("extra")) %>%
           str_replace("expectedColumns", blue("expectedColumns")) %>%
         str_replace("updated", green("updated")))
  
  
}



# Next, perform checks on the columns to verify data integrity
problemColumns <- c(if_else(anyNA(flagDF$APPLICATION_NUMBER), "APPLICATION_NUMBER", ""),
                    if_else(anyNA(flagDF$YEAR), "YEAR", ""),
                    if_else(anyNA(flagDF$MONTH), "MONTH", ""),
                    if_else(anyNA(flagDF$AMOUNT), "AMOUNT", ""),
                    if_else(anyNA(flagDF$DIVERSION_TYPE), "DIVERSION_TYPE", ""),
                    if_else(anyNA(flagDF$PARTY_ID), "PARTY_ID", ""),
                    if_else(anyNA(flagDF$ASSIGNED_PRIORITY_DATE), "ASSIGNED_PRIORITY_DATE", ""),
                    if_else(anyNA(flagDF$DUP_REPORT_SAME_OWNER_YEAR_DIFFERENT_RIGHT_OR_DIVERSION_TYPE), "DUP_REPORT_SAME_OWNER_YEAR_DIFFERENT_RIGHT_OR_DIVERSION_TYPE", ""),
                    if_else(anyNA(flagDF$DUP_REPORT_SAME_RIGHT_YEAR_DIFFERENT_OWNER), "DUP_REPORT_SAME_RIGHT_YEAR_DIFFERENT_OWNER", ""),
                    if_else(anyNA(flagDF$MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV[flagDF$DIVERSION_TYPE %in% c("DIRECT", "STORAGE", "Combined (Direct + Storage)")]), "MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV", ""),
                    if_else(anyNA(flagDF$EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT[flagDF$DIVERSION_TYPE %in% c("DIRECT", "STORAGE", "Combined (Direct + Storage)")]), "EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT", ""),
                    if_else(anyNA(flagDF$EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL[flagDF$DIVERSION_TYPE %in% c("DIRECT", "STORAGE", "Combined (Direct + Storage)")]), "EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL", ""),
                    if_else(anyNA(flagDF$REPORT_IS_MISSING_DIVERSION_DATA), "REPORT_IS_MISSING_DIVERSION_DATA", ""),
                    if_else(anyNA(flagDF$CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION), "CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION", ""))



if (sum(expectedColumns$COLUMN_NAME %in% problemColumns) > 0) {
  
  
  
  cat("\n\n")
  
  
  
  if (sum(c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE", "PARTY_ID") %in% problemColumns) > 0) {
    
    message(paste0("[*] At least one of the columns from the extended flat file contains 'NA' values.",
                   " The file should be checked for data corruption.") %>%
              strwrap(width = 0.98 * getOption("width")) %>%
              paste0(collapse = "\n") %>%
              str_replace("NA", red("NA")) %>%
              str_replace("data", red("data")) %>%
              str_replace("corruption", red("corruption")))
    
    cat("\n")
    
  }
  
  
  
  if ("ASSIGNED_PRIORITY_DATE" %in% problemColumns) {
    
    message(paste0("[*] One or more water rights has a missing priority date ('NA').",
                   " There may be issues with the dataset or the Priority Date script.") %>%
              strwrap(width = 0.98 * getOption("width")) %>%
              paste0(collapse = "\n") %>%
              str_replace("NA", red("NA")) %>%
              str_replace("missing", red("missing")) %>%
              str_replace("issues", red("issues")))
    
    cat("\n")
    
  }
  
  
  
  if (sum(c("DUP_REPORT_SAME_OWNER_YEAR_DIFFERENT_RIGHT_OR_DIVERSION_TYPE", "DUP_REPORT_SAME_RIGHT_YEAR_DIFFERENT_OWNER") %in% problemColumns) > 0) {
    
    message(paste0("[*] At least one of the columns from the Duplicate Reporting procedure contains 'NA' values.",
                   " There may be issues with the dataset or the script.") %>%
              strwrap(width = 0.98 * getOption("width")) %>%
              paste0(collapse = "\n") %>%
              str_replace("NA", red("NA")) %>%
              str_replace("issues", red("issues")))
    
    cat("\n")
    
  }
  
  
  
  if (sum(c("MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV", "EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT", "EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL") %in% problemColumns) > 0) {
    
    message(paste0("[*] At least one of the columns from the Expected Demand procedure contains 'NA' values",
                   " (for a diversion type other than 'USE'). There may be issues with the dataset or the script.") %>%
              strwrap(width = 0.98 * getOption("width")) %>%
              paste0(collapse = "\n") %>%
              str_replace("NA", red("NA")) %>%
              str_replace("issues", red("issues")))
    
    cat("\n")
    
  }
  
  
  
  if ("REPORT_IS_MISSING_DIVERSION_DATA" %in% problemColumns) {
    
    message(paste0("[*] There may be issues with the dataset or the Empty Reports script.",
                   " There should not be any 'NA' values in its data column.") %>%
              strwrap(width = 0.98 * getOption("width")) %>%
              paste0(collapse = "\n") %>%
              str_replace("NA", red("NA")) %>%
              str_replace("issues", red("issues")))
    
    cat("\n")
    
  }
  
  
  
  if ("CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION" %in% problemColumns) {
    
    message(paste0("[*] There may be issues with the dataset or the Face Value script.",
                   " There should not be any 'NA' values in its column.") %>%
              strwrap(width = 0.98 * getOption("width")) %>%
              paste0(collapse = "\n") %>%
              str_replace("NA", red("NA")) %>%
              str_replace("issues", red("issues")))
    
    cat("\n")
    
  }
  
  
  
  cat(paste0("Columns with Data Issues") %>% bold() %>% silver())
  cat("\n   ")
  cat(expectedColumns %>%
        filter(COLUMN_NAME %in% problemColumns) %>%
        mutate(RES = paste0(COLUMN_NAME, " (", SOURCE, ")")) %>%
        select(RES) %>% unlist(use.names = FALSE) %>%
        paste0(collapse = "\n   "))
  cat("\n\n")
  
  
  
  stop("One or more columns contains data issues. Please see the above messages and column list.")
  
}



# Output a completion message
cat("\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())
