# Scripts that require the start and end dates for data scraping should source 
# this file

# It validates the user's input while importing these dates
# Also, users input strings, but this script will turn those variables into dates


#### Dependencies ####

require(tidyverse)


#### Script Procedure ####

# For ease of editing this script, specify the relative path to the date script here
controlPath <- "Scripts/CTR_001_Set_Start_and_End_Dates.R"


# Read in the date range script
# This script contains two user-defined variables: "startDate" and "endDate"
source(controlPath)


# If either variable is missing, output an error message
if (!exists("startDate") || !exists("endDate")) {
  
  stop(paste0("Date Range Script Issue\n\n",
              "This script expects the user input script to define two ",
              "variables, 'startDate' and 'endDate'. However, there appears ",
              "to be a corruption issue with either this script or the user ",
              "input script.\n\n",
              "Please ensure this script is up-to-date. Also, please obtain ",
              "a fresh copy of '", controlPath, "' from the repository and ",
              "restart the process with that script.") |>
         strwrap(width = 0.99 * getOption("width")) |>
         paste0(collapse = "\n"))
  
}


# Verify that both 'startDate' and 'endDate' are properly formatted strings
# Both should be single-length character variables
if (length(startDate) != 1 || !is.character(startDate)) {
  
  stop(paste0("Start Date Formatting Issue\n\n",
              "The start and end dates should be strings containing dates ",
              "in the format 'YYYY-MM-DD' (e.g., \"2025-10-01\")\n\n",
              "Please revise 'startDate' in '", controlPath, "'") |>
         strwrap(width = 0.99 * getOption("width")) |>
         paste0(collapse = "\n"))
  
} else if (length(endDate) != 1 || !is.character(endDate)) {
  
  stop(paste0("End Date Formatting Issue\n\n",
              "The start and end dates should be strings containing dates ",
              "in the format 'YYYY-MM-DD' (e.g., \"2026-09-30\")\n\n",
              "Please revise 'endDate' in '", controlPath, "'") |>
         strwrap(width = 0.99 * getOption("width")) |>
         paste0(collapse = "\n"))
  
}


# Next, verify the formatting of 'startDate' and 'endDate'
# Try to convert them into date variables
startDate <- as.Date(startDate, format = "%Y-%m-%d")
endDate <- as.Date(endDate, format = "%Y-%m-%d")


# Make sure that neither variable is 'NA'
# If that occurs, it means that the variable is not in the expected format
if (is.na(startDate)) {
  
  stop(paste0("Start Date Formatting Issue\n\n",
              "The start and end dates should have a 'YYYY-MM-DD' format ",
              "(e.g., \"2025-10-01\")\n\n",
              "Please revise 'startDate' in '", controlPath, "'") |>
         strwrap(width = 0.99 * getOption("width")) |>
         paste0(collapse = "\n"))
  
} else if (is.na(endDate)) {
  
  stop(paste0("End Date Formatting Issue\n\n",
              "The start and end dates should have a 'YYYY-MM-DD' format ",
              "(e.g., \"2026-09-30\")\n\n",
              "Please revise 'endDate' in '", controlPath, "'") |>
         strwrap(width = 0.99 * getOption("width")) |>
         paste0(collapse = "\n"))
  
}


# Next, confirm that 'endDate' is at least two days prior to today
# It should not be equal to yesterday or today (or any future date, for that matter)
if (endDate >= Sys.Date() - 1) {
  
  # This issue can be addressed automatically by the script
  # The user will only receive a warning message about the issue
  
  
  # Set 'endDate' to two days prior to today
  endDate <- Sys.Date() - 2
  
  
  # Update "CTR_001_Set_Start_and_End_Dates.R" with this change
  read_lines(controlPath) |>
    str_replace("^\\s*endDate .+$", paste0("endDate <- \"", endDate, "\"")) |>
    write_lines(controlPath)
  
  
  message(paste0("Warning: End Date Value Issue\n\n",
                 "The end date should be, at most, two days prior to today ",
                 "(there is sometimes a lag in data being posted online)\n\n",
                 "This script has automatically updated 'endDate' in '",
                 controlPath, "' to enforce this restriction.") |>
            strwrap(width = 0.99 * getOption("width")) |>
            paste0(collapse = "\n"))
  
}


# The final validation step is to confirm that 'endDate' is not 
# an earlier date than 'startDate'
if (endDate < startDate) {
  
  stop(paste0("Date Value Issue\n\n",
              "'", endDate, "' is set as the end date, while '",
              startDate, "' is the start date\n\n", 
              "However, the end date cannot be a date before the start date\n\n",
              "Please revise the values in 'CTR_001_Set_Start_and_End_Dates.R'") |>
         strwrap(width = 0.99 * getOption("width")) |>
         paste0(collapse = "\n"))
  
}


# If there are no issues with the input dates, output a message
cat(paste0("\n\nDate Range: ", startDate, " to ", endDate, "\n\n"))


# Remove 'controlPath' from the environment
# (It does not need to be passed to later procedures)
remove(controlPath)
