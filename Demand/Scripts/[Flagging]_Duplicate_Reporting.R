
#Load libraries----
require(dplyr) #for numerous functions
require(tidyverse) #for read_csv
require(data.table) #for fread function
require(janitor) # for get_dupes function
require(writexl) # for write_xlsx function
require(readxl) # for read_xlsx function



flagDuplicateReports <- function () {
  
  # Add flags related to duplicate reporting
  
  
  source("Scripts/Watershed_Selection.R")
  source("Scripts/Dataset_Year_Range.R")
  
  
  
  # Add flags for whether a right has more than one submission 
  # (from different owners) in the same year 
  sameYearDiffOwners(ws, yearRange)
  
  
  
  # Next, add flags related to different rights in the same year with the same owner
  # that have the same annual total
  sameTotalDiffRights(ws, yearRange)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



sameYearDiffOwners <- function (ws, yearRange) {
  
  # Check for instances where there is more than one report (from different owners)
  # for the same right and reporting year
  
  
  
  # Get the flag table
  # The water rights in this table will be a necessary filter
  flagDF <- paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv") %>%
    fread()
  
  
  # From the extended CSV, extract party information
  # Then apply some filters: 
  #   (1) Filter the dataset to years within the desired dataset range
  #   (2) The diversion type should be either "STORAGE" or "DIRECT" only
  #   (3) Consider only rights that appear within 'flagDF'
  partyDF <- fread("RawData/Snowflake_water_use_report_extended.csv",
                   select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", 
                              "DIVERSION_TYPE", "APPLICATION_PRIMARY_OWNER", "PARTY_ID")) %>%
    filter(YEAR >= yearRange[1] & YEAR <= yearRange[2],
           DIVERSION_TYPE %in% c("STORAGE", "DIRECT"),
           APPLICATION_NUMBER %in% flagDF$APPLICATION_NUMBER)
  
  
  
  # Group by "APPLICATION_NUMBER" and "YEAR"
  # Then, get the unique counts for each grouping
  # (Ideally, there should be one unique report per right/year)
  # The concern is whether multiple parties submitted a report for the same year
  # for a right (e.g., during the year when it changed ownership)
  partyCounts <- partyDF %>%
    select(APPLICATION_NUMBER, YEAR, APPLICATION_PRIMARY_OWNER, PARTY_ID) %>%
    unique() %>%
    group_by(APPLICATION_NUMBER, YEAR) %>%
    summarize(PARTY_COUNTS = n(), .groups = "drop")
  
  
  
  # Create a flagging column related to whether "PARTY_COUNTS" is greater than 1
  # Append this column to 'flagDF'
  flagDF <- flagDF %>%
    left_join(partyCounts %>%
                mutate(DIFFERENT_OWNERS_SUBMITTED_FOR_THE_SAME_RIGHT_AND_YEAR = PARTY_COUNTS > 1) %>%
                select(-PARTY_COUNTS),
              by = c("APPLICATION_NUMBER", "YEAR"), 
              relationship = "many-to-one")
  
  
  
  # Update the flag CSV file
  write_csv(flagDF,
            paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv"))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



sameTotalDiffRights <- function (ws, yearRange) {
  
  # Check for instances where multiple rights have the same total for a year
  # (as well as the same owner)
  
  
  
  # Get the flag table
  # The water rights in this table will be a necessary filter
  flagDF <- paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv") %>%
    fread()
  
  
  
  # From the extended CSV, extract party information
  # Then apply some filters: 
  #   (1) Filter the dataset to years within the desired dataset range
  #   (2) The diversion type should be either "STORAGE" or "DIRECT" only
  #   (3) Consider only rights that appear within 'flagDF'
  #   (4) The reported volume ("AMOUNT") should be non-zero
  partyDF <- fread("RawData/Snowflake_water_use_report_extended.csv",
                   select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", 
                              "DIVERSION_TYPE", "PARTY_ID")) %>%
    filter(YEAR >= yearRange[1] & YEAR <= yearRange[2],
           DIVERSION_TYPE %in% c("STORAGE", "DIRECT"),
           APPLICATION_NUMBER %in% flagDF$APPLICATION_NUMBER,
           AMOUNT > 0)
  
  
  
  # Get an annual total for each water right's report
  sumDF <- partyDF %>%
    group_by(APPLICATION_NUMBER, YEAR, PARTY_ID, DIVERSION_TYPE) %>%
    summarize(ANNUAL_TOTAL = sum(AMOUNT, na.rm = TRUE), .groups = "drop")
  
  
  
  # Create a primary key based on "PARTY_ID", "YEAR", and "ANNUAL_TOTAL"
  sumDF <- sumDF %>%
    mutate(DUPLICATE_REPORTING_PRIMARY_KEY = paste(PARTY_ID, YEAR, ANNUAL_TOTAL, sep = "_"))
  
  
  
  # Get the number of unique rights with each "DUPLICATE_REPORTING_PRIMARY_KEY" value
  countDF <- sumDF %>%
    select(APPLICATION_NUMBER, DUPLICATE_REPORTING_PRIMARY_KEY) %>%
    unique() %>%
    group_by(DUPLICATE_REPORTING_PRIMARY_KEY) %>%
    summarize(DUPLICATE_COUNTS = n(), .groups = "drop")
  
  
  
  # Append these counts back to 'sumDF'
  sumDF <- sumDF %>%
    left_join(countDF, by = "DUPLICATE_REPORTING_PRIMARY_KEY", relationship = "many-to-one")
  
  
  
  # Create a flag variable based on whether "DUPLICATE_COUNTS" is greater than 1
  sumDF <- sumDF %>%
    mutate(REPORTED_SAME_ANNUAL_TOTAL = DUPLICATE_COUNTS > 1) %>%
    select(APPLICATION_NUMBER, YEAR, PARTY_ID, DIVERSION_TYPE, 
           DUPLICATE_REPORTING_PRIMARY_KEY, REPORTED_SAME_ANNUAL_TOTAL)
  
  
  
  # Add "DUPLICATE_REPORTING_PRIMARY_KEY" and "REPORTED_SAME_ANNUAL_TOTAL" to 'flagDF'
  flagDF <- flagDF %>%
    left_join(sumDF,
              by = c("APPLICATION_NUMBER", "YEAR", "DIVERSION_TYPE", "PARTY_ID"),
              relationship = "many-to-one")
  
  
  
  # Write the updated 'flagDF' to a file
  write_csv(flagDF,
            paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv"))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


# 
# # Before exporting the table, remove entries that were already manually reviewed by SDA in Fall 2022; duplicate
# # primary keys will be removed and this saves SDA from reviewing the same records over and over again;
# # primary key is a concatenation of
# # Reporting Year ("YEAR"), 
# # PARTY_ID (unique ID assigned to owners in eWRIMS)
# # DIVERSION_TYPE (Direct Diversion or Diversion to Storage),
# # AnnualTotal (sum of direct diversion and diversion to storage in a given reporting year for a given right)
# 
# # Newer reporting data will not be removed because even if all the other fields are identical, the YEAR will 
# # be the new reporting year, 2023, 2024, and so on.
# 
# # A similar protection
# 
# if (!is.na(ws$QAQC_DUPLICATE_REPORTING_SPREADSHEET_PATH)) {
#   
#   reviewDF <- getXLSX(ws = ws, 
#                       SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_QAQC_DUPLICATE_REPORTING_SPREADSHEET", 
#                       FILEPATH = "QAQC_DUPLICATE_REPORTING_SPREADSHEET_PATH", 
#                       WORKSHEET_NAME = "QAQC_DUPLICATE_REPORTING_WORKSHEET_NAME")
#   
#   
#   Duplicate_Reports <- Duplicate_Reports %>%
#     filter(!(PK %in% reviewDF$PK))
#   
#   
#   remove(reviewDF)
#   
# }
# 
# 
# 
# # Add review columns as well
# Duplicate_Reports <- Duplicate_Reports %>%
#   mutate(QAQC_Action_Taken = NA_character_,
#          QAQC_Reason = NA_character_)
# 
# 
# 
# # Sort the columns too
# Duplicate_Reports <- Duplicate_Reports %>%
#   arrange(PARTY_ID, ADJ_YEAR, AnnualTotal, APPLICATION_NUMBER, DIVERSION_TYPE)
# 
# 
# 
# writexl::write_xlsx(x= Duplicate_Reports, path = paste0("OutputData/", ws$ID, "_Duplicate_Reports_Manual_Review.xlsx"), col_names = TRUE)

print("The Multiple_Owner_Analysis.R script is done running!")



remove(flagDuplicateReports, sameTotalDiffRights, sameYearDiffOwners)