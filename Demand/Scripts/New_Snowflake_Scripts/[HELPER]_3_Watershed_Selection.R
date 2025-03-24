# Use this script to select a watershed for the demand data analysis
# Change the number on Line 6 to choose a watershed



watershed_index <- 2    # <-- INPUT THE INDEX HERE!



# USE THE "INDEX" NUMBER THAT CORRESPONDS TO YOUR WATERSHED IN
# "Snowflake_Watershed_Demand_Dataset_Paths.xlsx"



# NO OTHER EDITS ARE NEEDED TO THIS SCRIPT



# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #



# Next Step:
# Based on the user's input, select a watershed



# Important required packages
require(cli)
require(tidyverse)
require(readxl)



# Validate the user's selection
if (length(watershed_index) != 1 || is.na(watershed_index) || is.infinite(watershed_index) ||
    !is.numeric(watershed_index) || round(watershed_index) != watershed_index) {
  
  stop(paste0("Please correct the value input on Line 6. A single integer number ",
              "should be assigned to the variable 'watershed_index'.\n\nStrings, 'NA', ",
              "etc. are not acceptable inputs.") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("correct", col_blue("correct")) %>%
         str_replace("Line", col_red("Line")) %>%
         str_replace("6", col_red("6")) %>%
         str_replace("single", col_green("single")) %>%
         str_replace("integer", col_green("integer")) %>%
         str_replace("assigned", col_blue("assigned")) %>%
         str_replace("watershed_index", col_magenta("watershed_index")))
  
}



# Load generic functions that are useful in multiple scripts
source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")



# Read in "Snowflake_Watershed_Demand_Dataset_Paths.xlsx" 
"Program Watersheds/4. Demand Data Tracking/Snowflake_Watershed_Demand_Dataset_Paths.xlsx" %>%
  makeSharePointPath() %>%
  fileRead("read_xlsx") -> wsSpreadsheet



# The "INDEX" values are located in the first row of 'wsSpreadsheet'
# Identify the watershed column with a matching index integer 
colIndex <- which(wsSpreadsheet[1, ] %>% map_dfc(trimws) == watershed_index)



# Verify that a single index was identified 
if (length(colIndex) > 1) {
  
  stop(paste0("The 'INDEX' row of 'Snowflake_Watershed_Demand_Dataset_Paths.xlsx' ",
              "contains multiple columns with a value that corresponds to ",
              watershed_index, ". The indices should be unique for each watershed. ",
              "Please correct the error in the spreadsheet.") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("INDEX", col_blue("INDEX")) %>%
         str_replace("multiple", col_red("multiple")) %>%
         str_replace("indices", col_red("indices")) %>%
         str_replace("unique", col_red("unique")) %>%
         str_replace("correct", col_blue("correct")))
  
} else if (length(colIndex) < 1) {
  
  stop(paste0("No match was found. The 'INDEX' row of ",
              "'Snowflake_Watershed_Demand_Dataset_Paths.xlsx' does not contain ",
              watershed_index, ". Please check both the spreadsheet and ",
              "'watershed_index' for errors.") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("No", col_red("No")) %>%
         str_replace("match", col_red("match")) %>%
         str_replace("INDEX", col_blue("INDEX")) %>%
         str_replace("does", col_red("does")) %>%
         str_replace("not", col_red("not")) %>%
         str_replace("contain", col_red("contain")) %>%
         str_replace("both", col_silver("both")) %>%
         str_replace("spreadsheet", col_blue("spreadsheet")) %>%
         str_replace("watershed_index", col_blue("watershed_index")) %>%
         str_replace("errors", col_red("errors")))
  
}



# If there was no error, select the watershed column that 
# corresponds to 'colIndex' (as well as the "WATERSHED" column, which
# contains labels for each row entry)
ws <- wsSpreadsheet %>%
  select(WATERSHED, names(wsSpreadsheet)[colIndex])



# Remove all variables other than 'ws'
remove(list = base::setdiff(ls(), "ws"))



# Finally, output a message noting the watershed that was selected
cat(paste0("Running scripts for ", names(ws)[2], "\n\n"))
