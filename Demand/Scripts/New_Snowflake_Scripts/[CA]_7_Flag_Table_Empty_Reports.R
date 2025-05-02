# Add a column to the flag table related to reports 
# that contain no "DIRECT" or "STORAGE" diversions

# (1) REPORT_IS_MISSING_DIVERSION_DATA


#### Setup ####


remove(list = ls())


require(cli)
require(data.table)
require(tidyverse)



source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


#### Procedure ####


print("Starting '[CA]_7_Flag_Table_Empty_Reports.R'...")



cat("\n\n")
paste0("This script will check for RMS report submissions in the dataset that lack both ",
       "storage and direct diversion data") %>%
  wrapStr() %>%
  str_replace("check", col_blue("check")) %>%
  str_replace("lack", col_silver("lack")) %>%
  str_replace("both", col_red("both")) %>%
  cat()
cat("\n\n\n")



cat("Reading in the extended dataset and checking for problematic reports..." %>%
      wrapStr() %>%
      str_replace("problematic", col_magenta("problematic")) %>%
      str_replace("reports", col_magenta("reports")))



# Read in the extended flat file
reportDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", 
                      "DIVERSION_TYPE")) %>%
  unique() %>%
  mutate(KEY = paste0(APPLICATION_NUMBER, "_", YEAR))



# Get combinations of "APPLICATION_NUMBER" and "YEAR" that do not have "DIRECT" or "STORAGE" usage data
emptyDiversionReports <- reportDF %>%
  filter(!(KEY %in% reportDF$KEY[reportDF$DIVERSION_TYPE %in% c("DIRECT", "STORAGE", "Combined (Direct + Storage)")])) %>%
  select(KEY) %>% unique() %>%
  unlist(use.names = FALSE) %>% sort()



cat("Done!\n\n\n")
cat("Loading in the flag table and creating a new flag column...")



# Read in the flag table
flagDF <- readFlagTable()



# Create a variable for whether the report is missing diversion data
flagDF <- flagDF %>%
  mutate(REPORT_IS_MISSING_DIVERSION_DATA = APPLICATION_NUMBER %in% emptyDiversionReports)



# Write the updated 'flagDF' to a file
writeFlagTable(flagDF)



# Output a completion message
cat("Loading in the flag table and creating a new flag column...Done!")
cat("\n\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())

