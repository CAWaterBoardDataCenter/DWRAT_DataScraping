# Add a column to the flag table related to relatively new appropriative water rights
# This column will mark recent reports that lack usage volumes
# Eventually, there would be an option to add values to these reports such that 
# the report's total equals their face value

# (1) CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION


#### Setup ####


remove(list = ls())


require(crayon)
require(data.table)
require(tidyverse)



source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


#### Procedure ####


print("Starting '[CA]_8_Flag_Table_Face_Value_Substitution.R'...")



# Read in the flag table
flagDF <- readFlagTable()



# Look for appropriative water rights initiated in the past three years
rightDF <- flagDF %>%
  filter(grepl("^A", APPLICATION_NUMBER)) %>%
  filter(substr(ASSIGNED_PRIORITY_DATE, 1, 4) > (year(Sys.Date()) - 3)) %>%
  select(APPLICATION_NUMBER, YEAR) %>% unique() %>%
  mutate(KEY = paste0(APPLICATION_NUMBER, "|", YEAR))



# For the water rights in 'rightDF',
# get a data frame that summarizes annual totals
annualDF <- flagDF %>%
  mutate(KEY = paste0(APPLICATION_NUMBER, "|", YEAR)) %>%
  filter(KEY %in% rightDF$KEY) %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
  select(APPLICATION_NUMBER, YEAR, AMOUNT) %>%
  group_by(APPLICATION_NUMBER, YEAR) %>%
  summarize(YEAR_TOTAL = sum(AMOUNT), .groups = "drop")



# If 'annualDF' is not empty and there are reports with a "YEAR_TOTAL" of 0 AF,
# mark these reports as eligible for face value substitution
annualDF <- annualDF %>%
  mutate(CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION = (YEAR_TOTAL == 0)) %>%
  select(-YEAR_TOTAL)



# Update 'flagDF' with this column ("CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION")
# Don't let there be "NA" values in "CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION"
# Replace "NA" with "FALSE" instead
flagDF <- flagDF %>%
  left_join(annualDF, by = c("APPLICATION_NUMBER", "YEAR"),
            relationship = "many-to-one") %>%
  mutate(CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION = replace_na(CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION, FALSE))



# Write the updated 'flagDF' to a file
writeFlagTable(flagDF)



# Output a completion message
cat("\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())
