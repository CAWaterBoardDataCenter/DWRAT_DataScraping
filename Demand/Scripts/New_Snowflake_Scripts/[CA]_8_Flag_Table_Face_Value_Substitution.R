# Add a column to the flag table related to relatively new appropriative water rights
# This column will mark recent reports that lack usage volumes
# Eventually, there would be an option to add values to these reports such that 
# the report's total equals their face value

# (1) CANDIDATE_FOR_FACE_VALUE_SUBSTITUTION


#### Setup ####


remove(list = ls())


require(cli)
require(data.table)
require(tidyverse)



source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


#### Procedure ####


print("Starting '[CA]_8_Flag_Table_Face_Value_Substitution.R'...")



cat("\n\n")
cat("This script will flag water rights that could be considered for 'Face Value Substitution'" %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("considered", col_green("considered")) %>%
      str_replace("Face", col_blue("Face")) %>%
      str_replace("Value", col_blue("Value")) %>%
      str_replace("Substitution", col_blue("Substitution")))
cat("\n\n")
cat(paste0("Relatively new appropriative water rights (with a priority date within ",
           "the last three years) may have submitted a recent report that contains only zeroes") %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("new", col_blue("new")) %>%
      str_replace("appropriative", col_magenta("appropriative")) %>%
      str_replace("last", col_green("last")) %>%
      str_replace("three", col_green("three")) %>%
      str_replace("years", col_green("years")) %>%
      str_replace("only", col_red("only")) %>%
      str_replace("zeroes", col_red("zeroes")))
cat("\n\n")
cat(paste0("This may be due to needing time to 'wind up' their project or system, and so ",
           "that report may not accurately reflect what their typical demand would really be") %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("wind", col_red("wind")) %>%
      str_replace("up", col_red("up")) %>%
      str_replace("accurately", col_silver("accurately")) %>%
      str_replace("reflect", col_silver("reflect")) %>%
      str_replace("typical", col_blue("typical")) %>%
      str_replace("demand", col_blue("demand")))
cat("\n\n")
cat(paste0("If desired, these reports could later be modified to have the right's face value ",
           "distributed across their diversion season (so that the total would no longer be zero)") %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("If", col_green("If")) %>%
      str_replace("desired", col_green("desired")) %>%
      str_replace("later", col_magenta("later")) %>%
      str_replace("face", col_red("face")) %>%
      str_replace("value", col_red("value")) %>%
      str_replace("across", col_silver("across")) %>%
      str_replace("diversion", col_blue("diversion")) %>%
      str_replace("season", col_blue("season")))
cat("\n\n\n")



cat("Identifying new appropriative rights with recent zero reports...")



# Read in the flag table
flagDF <- readFlagTable()



# Look for appropriative water rights initiated in the past three years
rightDF <- flagDF %>%
  filter(grepl("^A", APPLICATION_NUMBER)) %>%
  filter(substr(ASSIGNED_PRIORITY_DATE, 1, 4) > (year(Sys.Date()) - 3)) %>%
  select(APPLICATION_NUMBER, YEAR) %>% unique() %>%
  mutate(KEY = paste0(APPLICATION_NUMBER, "|", YEAR))



# For the water rights in 'rightDF',
# get a data frame that summarizes annual totals in recent reports
annualDF <- flagDF %>%
  filter(YEAR > (year(Sys.Date() - 3))) %>%
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



cat("Identifying new appropriative rights with recent zero reports...Done!")
cat("\n\n\n")
cat("Adding a new column to the flag table...")



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
cat("Done!\n\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())
