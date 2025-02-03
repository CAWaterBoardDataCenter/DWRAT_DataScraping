# Add columns to the flag table related to duplicate reporting:
# (1) DUP_REPORT_SAME_RIGHT_YEAR_DIFFERENT_OWNER
# (2) DUP_REPORT_SAME_OWNER_YEAR_DIFFERENT_RIGHT_OR_DIVERSION_TYPE


# Note whether reports for different rights in the same year with the same owner have the same annual total
# Also note when the same report has the same total volume for different diversion types



#### Setup ####


remove(list = ls())


require(crayon)
require(data.table)
require(tidyverse)



source("Scripts/Shared_Functions_Demand.R")


#### Procedure ####


print("Starting '[CA]_5_Flag_Table_Duplicate_Reporting.R'...")



dupDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", 
                      "DIVERSION_TYPE", "PARTY_ID")) %>%
  unique() %>%
  mutate()



# There should be no missing ("NA") entries in any of these columns
if (anyNA(dupDF)) {
  
  cat("\n\n")
  stop(paste0("There is a problem with the extended CSV. None of the table's columns ",
              " ('APPLICATION_NUMBER', 'PARTY_ID', 'YEAR', 'MONTH', 'DIVERSION_TYPE',",
              "  and 'AMOUNT') should contain 'NA' values.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("problem", red("problem")) %>%
         str_replace("None", bold("None")) %>%
         str_replace("NA", red("NA")))
  
}



# If there are no issues, proceed to the flagging stage

# There are two checks to perform:

# (1) No duplicate reports (different party IDs, same water right, same year)

# (2) Check for the same total year amount, same year, same party ID, different water rights (or diversion type)



# Create a data frame with an annual total for each report/diversion type
annualDF <- dupDF %>%
  select(PARTY_ID, APPLICATION_NUMBER, YEAR, AMOUNT, DIVERSION_TYPE) %>%
  group_by(PARTY_ID, APPLICATION_NUMBER, YEAR, DIVERSION_TYPE) %>%
  summarize(YEAR_TOTAL = sum(AMOUNT), .groups = "drop")



# Task 1: No submissions for the same right+year by different parties
sameRightYear_differentOwner <- annualDF %>%
  select(PARTY_ID, APPLICATION_NUMBER, YEAR) %>%
  unique() %>%
  group_by(APPLICATION_NUMBER, YEAR) %>%
  summarize(PARTY_COUNTS = n(), .groups = "drop") %>%
  filter(PARTY_COUNTS > 1) %>%
  mutate(ID_KEY_1 = paste(APPLICATION_NUMBER, YEAR, sep = "_"))



# Verify the results of the previous operation
if (anyNA(sameRightYear_differentOwner)) {
  
  cat("\n\n")
  stop(paste0("The script may need to be revised. The portion that checks for submissions",
              " from different owners in the same year for the same right is producing",
              " 'NA' values. None of these columns ('APPLICATION_NUMBER', 'PARTY_ID', and 'YEAR')",
              " should contain 'NA' values, though it is okay for the final data frame to be empty",
              " (i.e., zero rows).") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("revised", red("revised")) %>%
         str_replace("None", bold("None")) %>%
         str_replace("NA", red("NA")))
  
}



# Prepare for the next check
# Task 2: Same total amount+year+owner, different right and/or diversion type
# (Nonzero volumes only)
# (Direct and Storage diversions only)
sameOwnerTotalYear_differentRightOrDiversionType <- annualDF %>%
  filter(YEAR_TOTAL > 0) %>%
  filter(DIVERSION_TYPE != "USE") %>%
  group_by(PARTY_ID, YEAR, YEAR_TOTAL) %>% 
  summarize(COUNT = n(), .groups = "drop") %>%
  filter(COUNT > 1) %>%
  mutate(ID_KEY_2 = paste(PARTY_ID, YEAR, YEAR_TOTAL, sep = "_"))



# "APPLICATION_NUMBER" is not present in the previous result
# (and the flag table will not have a column for annual totals)
# Therefore, use 'annualDF' with the previous result to get 
# a list of relevant water rights
sameOwnerTotalYear_differentRightOrDiversionType <- annualDF %>%
  mutate(ID_KEY_2 = paste(PARTY_ID, YEAR, YEAR_TOTAL, sep = "_")) %>%
  filter(ID_KEY_2 %in% sameOwnerTotalYear_differentRightOrDiversionType$ID_KEY_2) %>%
  mutate(ID_KEY_3 = paste(PARTY_ID, YEAR, APPLICATION_NUMBER, DIVERSION_TYPE, sep = "_"))



# Verify the results of the previous operation
if (anyNA(sameOwnerTotalYear_differentRightOrDiversionType)) {
  
  cat("\n\n")
  stop(paste0("The script may need revisions. The portion that identifies submissions",
              " with the same owner in the same year with the same non-zero annual totals is producing",
              " 'NA' values. None of these columns ('APPLICATION_NUMBER', 'PARTY_ID', 'YEAR',",
              " 'DIVERSION_TYPE', and 'AMOUNT') should contain 'NA' values. It is okay for the",
              "  final data frame to be empty (i.e., zero rows).") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("revisions", red("revisions")) %>%
         str_replace("None", bold("None")) %>%
         str_replace("NA", red("NA")))
  
}



# Read in 'flagDF' and prepare to append new columns
flagDF <- makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                                    makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                                      list.files() %>% sort() %>% tail(1) %>% 
                                      str_replace("water_use_report_extended", "Flag_Table"))) %>%
  fileRead("read_csv") %>%
  mutate(ID_KEY_1 = paste(APPLICATION_NUMBER, YEAR, sep = "_"),
         ID_KEY_3 = paste(PARTY_ID, YEAR, APPLICATION_NUMBER, DIVERSION_TYPE, sep = "_"))



# Add these columns to the flag table:
# (1) DUP_REPORT_SAME_RIGHT_YEAR_DIFFERENT_OWNER
# (2) DUP_REPORT_SAME_OWNER_YEAR_DIFFERENT_RIGHT_OR_DIVERSION_TYPE
flagDF <- flagDF %>%
  mutate(DUP_REPORT_SAME_RIGHT_YEAR_DIFFERENT_OWNER = ID_KEY_1 %in% sameRightYear_differentOwner$ID_KEY_1,
         DUP_REPORT_SAME_OWNER_YEAR_DIFFERENT_RIGHT_OR_DIVERSION_TYPE = ID_KEY_3 %in% sameOwnerTotalYear_differentRightOrDiversionType$ID_KEY_3) %>%
  select(-ID_KEY_1, -ID_KEY_3)



# Write the updated 'flagDF' to a file
makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                          makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                            list.files() %>% sort() %>% tail(1) %>% 
                            str_replace("water_use_report_extended", "Flag_Table"))) %>%
  write_csv(x = flagDF)



# Output a completion message
cat("\n\n")
print("The script is complete!")



# Clear out the environment
remove(list = ls())
