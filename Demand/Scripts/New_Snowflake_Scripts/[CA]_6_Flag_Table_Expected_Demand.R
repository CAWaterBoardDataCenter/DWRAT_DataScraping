# Add columns to the flag table related to unusual demand volumes
# (Unit conversion errors are the most common reason for this)

# (1) MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV
# (2) EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT
# (3) EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOLUMES


#### Setup ####


remove(list = ls())


require(cli)
require(data.table)
require(tidyverse)



source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


#### Procedure ####


print("Starting '[CA]_6_Flag_Table_Expected_Demand.R'...")



cat("\n\n")
cat("This script compares water rights' report totals to several reference values. Volumes that are 'different enough' from these reference values are flagged." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("report", col_blue("report")) %>%
      str_replace("totals", col_blue("totals")) %>%
      str_replace("reference", col_green("reference")) %>%
      str_replace("values", col_green("values")) %>%
      str_replace("different", col_silver("different")) %>%
      str_replace("enough", col_silver("enough")) %>%
      str_replace("flagged", col_red("flagged")))
cat("\n\n\n")



cat("Gathering and preparing the first set of reference values (Face Value and Initial Diversion Amounts)..." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("Gathering", col_magenta("Gathering")) %>%
      str_replace("and", col_magenta("and")) %>%
      str_replace("preparing", col_magenta("preparing")))



# Read in diversion data
useDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", 
                      "DIVERSION_TYPE")) %>%
  unique()



# There should be no missing ("NA") entries in any of these columns
if (anyNA(useDF)) {
  
  cat("\n\n")
  stop(paste0("There is a problem with the extended CSV. None of the table's columns ",
              " ('APPLICATION_NUMBER', 'YEAR', 'MONTH', 'DIVERSION_TYPE',",
              "  and 'AMOUNT') should contain 'NA' values.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("problem", col_red("problem")) %>%
         str_replace("None", style_bold("None")) %>%
         str_replace("NA", col_red("NA")))
  
}



# Convert 'useDF' into a data frame with totals summarized by year 
# Only consider "DIRECT" and "STORAGE" diversion types
annualDF <- useDF %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE", "Combined (Direct + Storage)")) %>%
  group_by(APPLICATION_NUMBER, YEAR) %>%
  summarize(YEAR_TOTAL = sum(AMOUNT), .groups = "drop")



# Also read in the face value amounts and initial diversion volumes
maxDivDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "YEAR", "FACE_VALUE_AMOUNT",
                      "FACE_VALUE_UNITS", "INI_REPORTED_DIV_AMOUNT", 
                      "INI_REPORTED_DIV_UNIT")) %>%
  unique()



# Some of the face value or initial diversion volumes may use units of gallons instead of acre-feet
# Convert them if they exist
maxDivDF$FACE_VALUE_AMOUNT[!is.na(maxDivDF$FACE_VALUE_AMOUNT) & maxDivDF$FACE_VALUE_UNITS == "Gallons"] <- maxDivDF$FACE_VALUE_AMOUNT[!is.na(maxDivDF$FACE_VALUE_AMOUNT) & maxDivDF$FACE_VALUE_UNITS == "Gallons"] / 325851
maxDivDF$FACE_VALUE_UNITS[!is.na(maxDivDF$FACE_VALUE_AMOUNT) & maxDivDF$FACE_VALUE_UNITS == "Gallons"] <- "Acre-feet per Year"



maxDivDF$INI_REPORTED_DIV_AMOUNT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT) & maxDivDF$INI_REPORTED_DIV_UNIT == "Gallons"] <- maxDivDF$INI_REPORTED_DIV_AMOUNT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT) & maxDivDF$INI_REPORTED_DIV_UNIT == "Gallons"] / 325851
maxDivDF$INI_REPORTED_DIV_UNIT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT) & maxDivDF$INI_REPORTED_DIV_UNIT == "Gallons"] <- "Acre-feet"



# Verify that all face value amounts are in acre-feet

# First check that there's only one unit referenced in the column 
if (maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
    select(FACE_VALUE_UNITS) %>% unique() %>% nrow() != 1)  {
  
  if (maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
      select(FACE_VALUE_UNITS) %>% unique() %>% nrow() == 0) {
    
    cat("\n\n")
    stop(paste0("The 'FACE_VALUE_UNITS' column should contain only",
                " one non-NA type of units: acre-feet. However,",
                " there are zero non-NA entries.") %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", col_blue("one")) %>%
           str_replace("acre.feet", col_green("acre-feet")) %>%
           str_replace("zero", col_red("zero")))
    
  } else {
    
    cat("\n\n")
    stop(paste0("The 'FACE_VALUE_UNITS' column should contain only",
                " one non-NA type of units (acre-feet). However,",
                " it contains ",
                maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                  select(FACE_VALUE_UNITS) %>% unique() %>% nrow(),
                " unit strings: ",
                maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                  select(FACE_VALUE_UNITS) %>% unique() %>% unlist() %>%
                  paste0(collapse = "; ")) %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", col_blue("one")) %>%
           str_replace("acre.feet", col_green("acre-feet")) %>%
           str_replace(maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                         select(FACE_VALUE_UNITS) %>% unique() %>% nrow() %>%
                         paste0(" ", ., " "), 
                       col_red(maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                                 select(FACE_VALUE_UNITS) %>% unique() %>% nrow() %>%
                                 paste0(" ", ., " "))))
    
  }
  
}



# Then, confirm that the specified unit is acre-feet (per year)
if (unique(maxDivDF$FACE_VALUE_UNITS[!is.na(maxDivDF$FACE_VALUE_AMOUNT)])[1] != "Acre-feet per Year") {
  
  cat("\n\n")
  stop(paste0("The expected units for the face value is acre-feet. To identify this unit,",
              " the 'FACE_VALUE_UNITS' column should contain 'Acre-feet per Year'.",
              " However, it instead contains the string '",
              unique(maxDivDF$FACE_VALUE_UNITS[!is.na(maxDivDF$FACE_VALUE_AMOUNT)])[1], "'.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("acre.feet", col_green("acre-feet")) %>%
         str_replace("Acre.feet per Year", col_green("Acre-feet per Year")) %>%
         str_replace("However", col_red("However")))
  
}



# Perform the same checks for the initial reported diversion amount

# First check that there's only one unit referenced in the column 
if (maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
    select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() != 1)  {
  
  if (maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
      select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() == 0) {
    
    cat("\n\n")
    stop(paste0("The 'INI_REPORTED_DIV_UNIT' column should contain only",
                " one non-NA type of units: acre-feet. However,",
                " there are zero non-NA entries.") %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", col_blue("one")) %>%
           str_replace("acre.feet", col_green("acre-feet")) %>%
           str_replace("zero", col_red("zero")))
    
  } else {
    
    cat("\n\n")
    stop(paste0("The 'INI_REPORTED_DIV_UNIT' column should contain only",
                " one non-NA type of units (acre-feet). However,",
                " it contains ",
                maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                  select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow(),
                " unit strings: ",
                maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                  select(INI_REPORTED_DIV_UNIT) %>% unique() %>% unlist() %>%
                  paste0(collapse = "; ")) %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", col_blue("one")) %>%
           str_replace("acre.feet", col_green("acre-feet")) %>%
           str_replace(maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                         select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() %>%
                         paste0(" ", ., " "), 
                       col_red(maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                                 select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() %>%
                                 paste0(" ", ., " "))))
    
  }
  
}



# Then, confirm that the specified unit is acre-feet 
if (unique(maxDivDF$INI_REPORTED_DIV_UNIT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT)])[1] != "Acre-feet") {
  
  cat("\n\n")
  stop(paste0("The expected units for the face value is acre-feet. To identify this unit,",
              " the 'INI_REPORTED_DIV_UNIT' column should contain 'Acre-feet'.",
              " However, it instead contains the string '",
              unique(maxDivDF$INI_REPORTED_DIV_UNIT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT)])[1], "'.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("acre.feet", col_green("acre-feet")) %>%
         str_replace("Acre.feet per Year", col_green("Acre-feet per Year")) %>%
         str_replace("However", col_red("However")))
  
}



cat("Done!\n\n\n    ")
cat("Adding a flag for rights that are missing both a Face Value and an Initial Diversion Amount..." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n"))



# Make a note in 'maxDivDF' if a water right has "NA" for both 
# "FACE_VALUE_AMOUNT" and "INI_REPORTED_DIV_AMOUNT"
# (This column will be added to the flag table later)
maxDivDF <- maxDivDF %>%
  mutate(MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV = is.na(FACE_VALUE_AMOUNT) & is.na(INI_REPORTED_DIV_AMOUNT))



cat("Done!\n\n\n    ")
cat("Creating the initial set of reference-comparison flags..." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n"))



# Join 'maxDivDF' to 'annualDF'
# This should be a "many-to-one" join operation
# (multiple rows in 'annualDF' for the same row in 'maxDivDF')
annualDF <- annualDF %>%
  left_join(maxDivDF, by = c("APPLICATION_NUMBER", "YEAR"), 
            relationship = "many-to-one")



# Create a flag for unusual "YEAR_TOTAL" values
# The variable should be "TRUE" if one of the following is "TRUE":
#   (1) "YEAR_TOTAL" / "FACE_VALUE_AMOUNT" > X
#   (2) "YEAR_TOTAL" / "FACE_VALUE_AMOUNT" < 1/X
#   (3) "YEAR_TOTAL" / "INI_REPORTED_DIV_AMOUNT" > X
#   (4) "YEAR_TOTAL" / "INI_REPORTED_DIV_AMOUNT" < 1/X
# The value of "X" depends on "YEAR_TOTAL"
annualDF <- annualDF %>%
  mutate(EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT = 
           (YEAR_TOTAL > 100 & 
              ((!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT > 10) |
                 (!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT < 1/10) |
                 (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT > 10) |
                 (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT < 1/10))) |
           (YEAR_TOTAL > 10 & 
              ((!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT > 100) |
                 (!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT < 1/100) |
                 (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT > 100) |
                 (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT < 1/100))) |
           (YEAR_TOTAL <= 10 & 
              ((!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT > 1000) |
                 (!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT < 1/1000) |
                 (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT > 1000) |
                 (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT < 1/1000))))



cat("Done!\n\n\n\n")
cat("Gathering and preparing the second set of reference values (Average and Median totals)..." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("Gathering", col_magenta("Gathering")) %>%
      str_replace("and", col_magenta("and")) %>%
      str_replace("preparing", col_magenta("preparing")))



# Create a variable that has averages and medians for each water right
summaryDF <- annualDF %>%
  group_by(APPLICATION_NUMBER) %>%
  summarize(AVERAGE = mean(YEAR_TOTAL), MEDIAN = median(YEAR_TOTAL), .groups = "drop")



# Check for missing values in 'summaryDF'
if (anyNA(summaryDF)) {
  
  cat("\n\n")
  stop(paste0(paste0("When calculating the average and median annual volume for each water right,",
                     " some 'NA' values were calculated. This suggests that there are 'NA' values",
                     " in the 'YEAR_TOTAL' column. That should not have happened, so the script",
                     " may need changes to ensure that these 'NA' values do not occur again. Here",
                     " are the 'APPLICATION_NUMBER' values of the water rights that had a 'NA' average",
                     " and/or median value:") %>%
                strwrap(width = getOption("width")) %>%
                paste0(collapse = "\n") %>%
                str_replace("average", col_green("average")) %>%
                str_replace("median", col_green("median")) %>%
                str_replace("NA", col_red("NA")) %>%
                str_replace("YEAR_TOTAL", col_blue("YEAR_TOTAL")) %>%
                str_replace("may need", col_blue("may need")) %>%
                str_replace("changes", col_green("changes")),
              "\n    ",
              summaryDF %>% filter(is.na(AVERAGE) | is.na(MEDIAN)) %>%
                select(APPLICATION_NUMBER) %>% unlist(use.names = FALSE) %>%
                paste0(collapse = "\n    ")))
  
}



# Append these columns to 'annualDF'
# This will be another "many-to-one" join
annualDF <- annualDF %>%
  left_join(summaryDF, by = "APPLICATION_NUMBER",
            relationship = "many-to-one")



cat("Done!\n\n\n    ")
cat("Generating the second set of reference-comparison flags..." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n"))



# Add another flag for unusual "YEAR_TOTAL" values
# The variable should be "TRUE" if one of the following is "TRUE":
#  (1) "YEAR_TOTAL" / "AVERAGE" > X
#  (2) "YEAR_TOTAL" / "AVERAGE" < 1/X
#  (3) "YEAR_TOTAL" / "MEDIAN" > X
#  (4) "YEAR_TOTAL" / "MEDIAN" < 1/X
#  (5) abs("YEAR_TOTAL" - AVERAGE) > X
#  (6) abs("YEAR_TOTAL" - MEDIAN) > X
# The value of "X" depends on "YEAR_TOTAL"
annualDF <- annualDF %>%
  mutate(EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL = 
           (YEAR_TOTAL > 100 & ((AVERAGE > 0 & YEAR_TOTAL / AVERAGE > 10) |
                                  (AVERAGE > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / AVERAGE < 1/10) |
                                  (MEDIAN > 0 & YEAR_TOTAL / MEDIAN > 10) |
                                  (MEDIAN > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / MEDIAN < 1/10) |
                                  (YEAR_TOTAL > 0 & abs(YEAR_TOTAL - AVERAGE) > 10) |
                                  (YEAR_TOTAL > 0 & abs(YEAR_TOTAL - MEDIAN) > 10))) |
           (YEAR_TOTAL > 10 & ((AVERAGE > 0 & YEAR_TOTAL / AVERAGE > 100) |
                                 (AVERAGE > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / AVERAGE < 1/100) |
                                 (MEDIAN > 0 & YEAR_TOTAL / MEDIAN > 100) |
                                 (MEDIAN > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / MEDIAN < 1/100) |
                                 (YEAR_TOTAL > 0 & abs(YEAR_TOTAL - AVERAGE) > 100) |
                                 (YEAR_TOTAL > 0 & abs(YEAR_TOTAL - MEDIAN) > 100))) |
           (YEAR_TOTAL <= 10 & ((AVERAGE > 0 & YEAR_TOTAL / AVERAGE > 1000) |
                                  (AVERAGE > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / AVERAGE < 1/1000) |
                                  (MEDIAN > 0 & YEAR_TOTAL / MEDIAN > 1000) |
                                  (MEDIAN > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / MEDIAN < 1/1000) |
                                  (YEAR_TOTAL > 0 & abs(YEAR_TOTAL - AVERAGE) > 1000) |
                                  (YEAR_TOTAL > 0 & abs(YEAR_TOTAL - MEDIAN) > 1000))))



# Ensure that there are no NA values in the three flags
if (anyNA(annualDF$MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV)) {
  
  cat("\n\n")
  stop(paste0("There should be no 'NA' values when calculating the variable",
              " 'MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV'. It should contain",
              " only 'TRUE' or 'FALSE'. There may be a problem with the extended dataset or script.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("NA", col_red("NA")) %>%
         str_replace("problem", col_red("problem")) %>%
         str_replace("dataset", col_red("dataset")) %>%
         str_replace("script", col_red("script")))
  
} else if (anyNA(annualDF$EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT)) {
  
  cat("\n\n")
  stop(paste0("There should be no 'NA' values when calculating the variable",
              " 'EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT'. It should contain",
              " only 'TRUE' or 'FALSE'. There may be a problem with the extended dataset or script.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("NA", col_red("NA")) %>%
         str_replace("problem", col_red("problem")) %>%
         str_replace("dataset", col_red("dataset")) %>%
         str_replace("script", col_red("script")))
  
} else if (anyNA(annualDF$EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL)) {
  
  cat("\n\n")
  stop(paste0("There should be no 'NA' values when calculating the variable",
              " 'EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL'. It should contain",
              " only 'TRUE' or 'FALSE'. There may be a problem with the extended dataset or script.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("NA", col_red("NA")) %>%
         str_replace("problem", col_red("problem")) %>%
         str_replace("dataset", col_red("dataset")) %>%
         str_replace("script", col_red("script")))
  
}



# Remove columns from 'annualDF' to help prepare for appending these flags to the main flag table
annualDF <- annualDF %>%
  select(APPLICATION_NUMBER, YEAR,
         MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV,
         EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT,
         EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL)



cat("Done!\n\n\n")
cat("Adding new columns to the flag table..." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n"))



# Read in the flag table 
flagDF <- readFlagTable()



# Add these columns to the flag table:
# (1) MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV
# (2) EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT
# (3) EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL
#flagDF <- flagDF %>%
#  left_join(annualDF, by = c("APPLICATION_NUMBER", "YEAR"), relationship = "many-to-one")



# Check again to ensure that there are no missing values in these flags
if (flagDF %>%
    filter(is.na(MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV) |
           is.na(EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT) |
           is.na(EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL)) %>%
    filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE", "Combined (Direct + Storage)")) %>%
    nrow() > 0) {
  
  cat("\n\n")
  stop(paste0(paste0("There should be no 'NA' values in these flags (except for reports",
                     " that lack direct diversion or storage diversion data). However,",
                     " there may be a problem with the extended dataset or script. 'NA'",
                     " values were detected in additional cases. See these water rights:") %>%
                strwrap(width = getOption("width")) %>%
                paste0(collapse = "\n") %>%
                str_replace("NA", col_red("NA")) %>%
                str_replace("problem", col_red("problem")),
              "\n   ",
              flagDF %>%
                filter(is.na(MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV) |
                         is.na(EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT) |
                         is.na(EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOL)) %>%
                filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE", "Combined (Direct + Storage)")) %>% 
                select(APPLICATION_NUMBER, YEAR) %>% unique() %>% 
                mutate(KEY = paste0(APPLICATION_NUMBER, " (", YEAR, ")")) %>% 
                select(KEY) %>% unlist(use.names = FALSE) %>% sort() %>% 
                paste0(collapse = "\n   ")))
  
}



# Write the updated 'flagDF' to a file
#writeFlagTable(flagDF)



# Output a completion message
cat("Adding new columns to the flag table...Done!" %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n"))
cat("\n\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())
