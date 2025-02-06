# Create columns related to a water right's priority date

# These columns will be added to the flag table:
#   (*) ASSIGNED_PRIORITY_DATE
#   (*) APPROPRIATIVE_DATE_SOURCE
#   (*) STATEMENT_PRIORITY_SOURCE
#   (*) PRE_1914
#   (*) RIPARIAN
#   (*) APPROPRIATIVE


#### Setup ####


require(crayon)
require(data.table)
require(tidyverse)



source("Scripts/Shared_Functions_Demand.R")


#### Procedure ####


print("Starting '[CA]_4_Flag_Table_Priority_Date.R'...")



priorityDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "WATER_RIGHT_TYPE", "PRIORITY_DATE", "APPLICATION_RECD_DATE", 
                      "APPLICATION_ACCEPTANCE_DATE", "SUB_TYPE", "YEAR_DIVERSION_COMMENCED")) %>%
  unique()



# Recreate the following columns:
#   (1)  PRE_1914_1
#   (2)  PRE14_DIV_COMMENCED
#   (3)  PRE14_DATE
#   (4)  RIPARIAN_DATE
#   (5)  APPROPRIATIVE_DATE
#   (6)  APP_YEAR
#   (7)  APP_MON
#   (8)  APP_DAY
#   (9)  APPROPRIATIVE_DATE_STRING
#   (10) ASSIGNED_PRIORITY_DATE
#   (11) PRE_1914
#   (12) RIPARIAN
#   (13) APPROPRIATIVE
#   (14) APPROPRIATIVE_DATE_SOURCE
#   (15) STATEMENT_PRIORITY_SOURCE



# (1)  PRE_1914_1
# The column "PRE_1914_1" contains the string "PRE_1914" if the string "14" 
# is found in the "SUB_TYPE" column (meaning that "PRE_1914" is listed there)
# Otherwise, it is an empty string ("")
priorityDF <- priorityDF %>%
  mutate(PRE_1914_1 = if_else(!is.na(SUB_TYPE) & grepl("14", SUB_TYPE), "PRE_1914", ""))



# (2)  PRE14_DIV_COMMENCED
# The column "PRE14_DIV_COMMENCED" checks if "YEAR_DIVERSION_COMMENCED" is numeric
# It is empty if that is not the case
# Otherwise, it checks if "YEAR_DIVERSION_COMMENCED" is before 1915 and that 
# "WATER_RIGHT_TYPE" is "Statement of Div and Use"
# If that is true, then "PRE14_DIV_COMMENCED" is equal to "YEAR_DIVERSION_COMMENCED"
# If not, then it is empty
priorityDF <- priorityDF %>%
  mutate(PRE14_DIV_COMMENCED = if_else(YEAR_DIVERSION_COMMENCED %>% map_lgl(~ !is.na(.) & is.numeric(.)), 
                                       if_else(YEAR_DIVERSION_COMMENCED < 1915 & WATER_RIGHT_TYPE == "Statement of Div and Use", 
                                               as.character(YEAR_DIVERSION_COMMENCED), 
                                               ""), 
                                       ""))


# (3)  PRE14_DATE
# The column "PRE14_DATE" first checks if "PRE_1914_1" is not empty and "YEAR_DIVERSION_COMMENCED" is blank
# If yes, the column is set as "11111111"
# If no, the column checks if "PRE_1914_1" is not empty
# If no, the column will be empty ("")
# If yes, "PRE14_DATE" checks if "YEAR_DIVERSION_COMMENCED" is numeric
# If that is the case, "PRE14_DATE" will be a concatenation of that year and "0101"
# Otherwise, it would output "FALSE" (not sure if this behavior is desired)
priorityDF <- priorityDF %>%
  mutate(PRE14_DATE = if_else(PRE_1914_1 == "PRE_1914" & is.na(YEAR_DIVERSION_COMMENCED), 
                              "11111111",
                              if_else(PRE_1914_1 == "PRE_1914",
                                      if_else(YEAR_DIVERSION_COMMENCED %>% map_lgl(~ !is.na(.) & is.numeric(.)),
                                              paste0(YEAR_DIVERSION_COMMENCED, "0101"),
                                              "FALSE"),
                                      "")))


# (11) PRE_1914
# If the column "PRE_1914_1" is "PRE_1914" or the string length of "PRE14_DIV_COMMENCED" is 4,
# this column will be "PRE_1914", otherwise 
priorityDF <- priorityDF %>%
  mutate(PRE_1914 = if_else(PRE_1914_1 == "PRE_1914" | nchar(PRE14_DIV_COMMENCED) == 4, 
                            "PRE_1914", ""))


# (13) APPROPRIATIVE
# If "WATER_RIGHT_TYPE" is neither "Federal Claims" nor "Statement of Div and Use",
# this column will be "APPROPRIATIVE"
# Otherwise, this column is empty
priorityDF <- priorityDF %>%
  mutate(APPROPRIATIVE = if_else(WATER_RIGHT_TYPE != "Federal Claims" & WATER_RIGHT_TYPE != "Statement of Div and Use",
                                 "APPROPRIATIVE", 
                                 ""))


# (12) RIPARIAN
# If "PRE_1914_1" and "APPROPRIATIVE" are both empty, 
# this column will be "RIPARIAN"
# In all other cases, it is empty
priorityDF <- priorityDF %>%
  mutate(RIPARIAN = if_else(PRE_1914_1 == "",
                            if_else(APPROPRIATIVE == "", "RIPARIAN", ""),
                            ""))


# (4)  RIPARIAN_DATE
# If the "RIPARIAN" column contains "RIPARIAN", this column is set to "10000000"
# Otherwise, it is empty ("")
priorityDF <- priorityDF %>%
  mutate(RIPARIAN_DATE = if_else(RIPARIAN == "RIPARIAN", "10000000", ""))


# (5)  APPROPRIATIVE_DATE
# This column first checks if the "APPROPRIATIVE" column is "APPROPRIATIVE"
# If this is not true, this column will be empty
# Otherwise, it checks if "PRIORITY_DATE" is empty
# If this is false, "APPROPRIATIVE_DATE" will be "PRIORITY_DATE"
# If this is true, this column checks if "APPLICATION_RECD_DATE" is empty
# If this is false, "APPROPRIATIVE_DATE" will be "APPLICATION_RECD_DATE"
# Otherwise, this column checks if "APPLICATION_ACCEPTANCE_DATE" is empty
# If this is true, "APPROPRIATIVE_DATE" will be "99999999"
# If this is false, "APPROPRIATIVE_DATE" will be "APPLICATION_ACCEPTANCE_DATE"
priorityDF <- priorityDF %>%
  mutate(APPROPRIATIVE_DATE = if_else(APPROPRIATIVE == "APPROPRIATIVE",
                                      if_else(is.na(PRIORITY_DATE),
                                              if_else(is.na(APPLICATION_RECD_DATE),
                                                      if_else(is.na(APPLICATION_ACCEPTANCE_DATE),
                                                              "99999999",
                                                              as.character(APPLICATION_ACCEPTANCE_DATE)),
                                                      as.character(APPLICATION_RECD_DATE)),
                                              as.character(PRIORITY_DATE)),
                                      ""))


# (6)  APP_YEAR
# This column first checks if the "APPROPRIATIVE" column is "APPROPRIATIVE"
# If this is true, the year is extracted from "APPROPRIATIVE_DATE" (which must be converted into a date temporarily)
# If this is false, the column will be empty ""
priorityDF <- priorityDF %>%
  mutate(APP_YEAR = if_else(APPROPRIATIVE == "APPROPRIATIVE" & str_detect(APPROPRIATIVE_DATE, "/"),
                            APPROPRIATIVE_DATE %>% as.Date(format = "%m/%d/%Y") %>% 
                              year() %>% as.character(),
                            ""))



# Verify the results of the previous operation
if (anyNA(priorityDF$APP_YEAR)) {
  
  cat("\n\n")
  stop(paste0("The script must be revised. The portion that extracts the numeric year from the",
              " 'APPROPRIATIVE_DATE' column is missing some cases and is producing",
              " 'NA' values in the 'APP_YEAR' column. The code must be updated",
              " to identify these cases and replace them with the empty string ('').") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("APPROPRIATIVE_DATE", red("APPROPRIATIVE_DATE")) %>%
         str_replace("NA", red("NA")) %>%
         str_replace("APP_YEAR", red("APP_YEAR")) %>%
         str_replace("updated", blue("updated")) %>%
         str_replace("identify", blue("identify")) %>%
         str_replace("replace", blue("replace")) %>%
         str_replace("empty string", green("empty string")))
  
}



# (7)  APP_MON
# This column first checks if the "APPROPRIATIVE" column is "APPROPRIATIVE"
# If this is false, the column will be empty ""
# Otherwise, the column extracts the month from "APPROPRIATIVE_DATE"
# This is followed by a length check for "APP_MON"
# If the string length is 1 (i.e., a month between 1 and 9, inclusive), 
# a "0" will be added to the beginning of the string
# Otherwise, the month string is unchanged
priorityDF <- priorityDF %>%
  mutate(APP_MON = if_else(APPROPRIATIVE == "APPROPRIATIVE" & str_detect(APPROPRIATIVE_DATE, "/"),
                           APPROPRIATIVE_DATE %>% as.Date(format = "%m/%d/%Y") %>% 
                             month() %>% as.character(),
                           "")) %>%
  mutate(APP_MON = if_else(nchar(APP_MON) == 1, paste0("0", APP_MON), APP_MON))



# Verify the results of the previous operation
if (anyNA(priorityDF$APP_MON)) {
  
  cat("\n\n")
  stop(paste0("The script must be revised. The portion that extracts the numeric month from the",
              " 'APPROPRIATIVE_DATE' column is missing some cases and is producing",
              " 'NA' values in the 'APP_MON' column. The code must be updated",
              " to identify these cases and replace them with the empty string ('').") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("APPROPRIATIVE_DATE", red("APPROPRIATIVE_DATE")) %>%
         str_replace("NA", red("NA")) %>%
         str_replace("APP_MON", red("APP_MON")) %>%
         str_replace("updated", blue("updated")) %>%
         str_replace("identify", blue("identify")) %>%
         str_replace("replace", blue("replace")) %>%
         str_replace("empty string", green("empty string")))
  
}



# (8)  APP_DAY
# This column first checks if the "APPROPRIATIVE" column is "APPROPRIATIVE"
# If this is false, the column will be empty ""
# Otherwise, the column extracts the day from "APPROPRIATIVE_DATE"
# This is followed by a length check for "APP_DAY"
# If the string length is 1 (i.e., a day between 1 and 9, inclusive), 
# a "0" will be added to the beginning of the string
# Otherwise, the day string is unchanged
priorityDF <- priorityDF %>%
  mutate(APP_DAY = if_else(APPROPRIATIVE == "APPROPRIATIVE" & str_detect(APPROPRIATIVE_DATE, "/"),
                           APPROPRIATIVE_DATE %>% as.Date(format = "%m/%d/%Y") %>% 
                             day() %>% as.character(),
                           "")) %>%
  mutate(APP_DAY = if_else(nchar(APP_DAY) == 1, paste0("0", APP_DAY), APP_DAY))



# Verify the results of the previous operation
if (anyNA(priorityDF$APP_DAY)) {
  
  cat("\n\n")
  stop(paste0("The script must be revised. The portion that extracts the numeric month from the",
              " 'APPROPRIATIVE_DATE' column is missing some cases and is producing",
              " 'NA' values in the 'APP_DAY' column. The code must be updated",
              " to identify these cases and replace them with the empty string ('').") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("APPROPRIATIVE_DATE", red("APPROPRIATIVE_DATE")) %>%
         str_replace("NA", red("NA")) %>%
         str_replace("APP_DAY", red("APP_DAY")) %>%
         str_replace("updated", blue("updated")) %>%
         str_replace("identify", blue("identify")) %>%
         str_replace("replace", blue("replace")) %>%
         str_replace("empty string", green("empty string")))
  
}



# (9)  APPROPRIATIVE_DATE_STRING
# This column is simply a concatenation of "APP_YEAR", "APP_MON", and "APP_DAY"
priorityDF <- priorityDF %>%
  mutate(APPROPRIATIVE_DATE_STRING = paste0(APP_YEAR, APP_MON, APP_DAY))



# (10) ASSIGNED_PRIORITY_DATE
# This column checks if "PRE14_DIV_COMMENCED" contains a number
# If yes, "ASSIGNED_PRIORITY_DATE" will be the concatenation of 
# "PRE14_DIV_COMMENCED" and "0101"
# Otherwise, this column will be the concatenation of "PRE14_DATE",
# "RIPARIAN_DATE", and "APPROPRIATIVE_DATE_STRING"
priorityDF <- priorityDF %>%
  mutate(ASSIGNED_PRIORITY_DATE = if_else(PRE14_DIV_COMMENCED %>% map_lgl(~ !is.na(as.numeric(.))), 
                                          paste0(PRE14_DIV_COMMENCED, "0101"),
                                          paste0(PRE14_DATE, RIPARIAN_DATE, APPROPRIATIVE_DATE_STRING)))



# (14) APPROPRIATIVE_DATE_SOURCE
# The column first checks if the "APPROPRIATIVE" column has a value of "APPROPRIATIVE"
# If not, this column is empty ("")
# Otherwise, it checks first if "PRIORITY_DATE" is empty
# If that column is not empty, this column shows "PRIORITY_DATE"
# If it is empty, this column checks "APPLICATION_RECD_DATE" next
# If that column is not empty, "APPROPRIATIVE_DATE_SOURCE" is "APPLICATION_RECD_DATE"
# If it is empty, "APPROPRIATIVE_DATE_SOURCE" checks "APPLICATION_ACCEPTANCE_DATE" next
# If that column is not empty, this column is "APPLICATION_ACCEPTANCE_DATE"
# Otherwise, it has a value of "NO_PRIORITY_DATE_INFORMATION"
priorityDF <- priorityDF %>%
  mutate(APPROPRIATIVE_DATE_SOURCE = if_else(APPROPRIATIVE == "APPROPRIATIVE",
                                             if_else(is.na(PRIORITY_DATE) | PRIORITY_DATE == "", 
                                                     if_else(is.na(APPLICATION_RECD_DATE),
                                                             if_else(is.na(APPLICATION_ACCEPTANCE_DATE),
                                                                     "NO_PRIORITY_DATE_INFORMATION",
                                                                     "APPLICATION_ACCEPTANCE_DATE"),
                                                             "APPLICATION_RECD_DATE"),
                                                     "PRIORITY_DATE"),
                                             ""))



# (15) STATEMENT_PRIORITY_SOURCE
# This column first checks if "WATER_RIGHT_TYPE" is "Statement of Div and Use"
# If that is false, "STATEMENT_PRIORITY_SOURCE" is an empty string
# Otherwise, it then checks the column "PRE_1914"
# If that column has a value of "PRE_1914", "STATEMENT_PRIORITY_SOURCE" is "YEAR_DIVERSION_COMMENCED"
# If not, then "STATEMENT_PRIORITY_SOURCE" is "SUB_TYPE"
priorityDF <- priorityDF %>%
  mutate(STATEMENT_PRIORITY_SOURCE = if_else(WATER_RIGHT_TYPE == "Statement of Div and Use",
                                             if_else(PRE_1914 == "PRE_1914", 
                                                     "YEAR_DIVERSION_COMMENCED", "SUB_TYPE"),
                                             ""))



# Read in 'flagDF' and append new columns
flagDF <- makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                                    makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                                      list.files() %>% sort() %>% tail(1) %>% 
                                      str_replace("water_use_report_extended", "Flag_Table"))) %>%
  fileRead("read_csv")



# Join some columns of 'priorityDF' to 'flagDF' using "APPLICATION_NUMBER"
# The joining relationship should be "many-to-one", meaning that multiple rows
# of 'flagDF' will match with the same row in 'priorityDF'
flagDF <- flagDF %>%
  left_join(priorityDF %>%
              select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE, 
                     APPROPRIATIVE_DATE_SOURCE, STATEMENT_PRIORITY_SOURCE, 
                     PRE_1914, RIPARIAN, APPROPRIATIVE),
            by = "APPLICATION_NUMBER", relationship = "many-to-one")



# Write the updated 'flagDF' to a file
makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                          makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                            list.files() %>% sort() %>% tail(1) %>% 
                            str_replace("water_use_report_extended", "Flag_Table"))) %>%
  write_csv(x = flagDF)



# Output a completion message
cat("\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())
