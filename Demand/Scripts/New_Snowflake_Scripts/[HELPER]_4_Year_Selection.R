# Use this script to choose the years included in the demand data analysis
# Adjust the numbers listed in 'yearSelection' (defined starting on Line 11)



# DO NOT COMMIT YOUR SELECTION TO GITHUB UNLESS IT'S RELEVANT TO THE ENTIRE GROUP



# SPECIFY THE INCLUDED YEARS AS A VECTOR OF NUMBERS HERE
yearSelection <- c(2017,
                   2018,
                   2019,
                   2020,
                   2021,
                   2022,
                   2023)



# Numbers should be comma-separated within the vector
# Input values as numbers, not strings (e.g., 2017 instead of "2017")



# NO OTHER EDITS ARE NEEDED TO THIS SCRIPT



# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #



# Next Step:
# Validate the user's input



# Import required packages
require(cli)
require(tidyverse)



# Keep unique values only (and sort them)
yearSelection <- yearSelection %>%
  unique() %>%
  sort(na.last = TRUE)



# Check that input was given
if (length(yearSelection) < 1) {
  
  stop(paste0("At least one value must be input into the vector; ", 
              "it should not be empty\n\n",
              "The format should be 'c(####, ####, ..., ####)'") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("At", col_red("At")) %>%
         str_replace("least", col_red("least")) %>%
         str_replace("one", col_red("one")) %>%
         str_replace("not", col_red("not")) %>%
         str_replace("empty", col_blue("empty")) %>%
         str_replace("format", col_blue("format")) %>%
         str_replace("c\\((.+)\\)", col_green("c(\\1)")))
  
}



# 'yearSelection' should be a numeric vector, not a character vector
if (sum(is.character(yearSelection)) > 0) {
  
  stop(paste0("'yearSelection' should be a numeric vector. As it is now, the ",
              "variable is being interpreted as a character vector. Make sure ", 
              "there are no quoted numbers or strings ", 
              "(e.g., \"2021\" instead of 2021).\n\n",
              "The format should be 'c(####, ####, ..., ####)'") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("numeric", col_red("numeric")) %>%
         str_replace("vector", col_red("vector")) %>%
         str_replace("character", col_magenta("character")) %>%
         str_replace("format", col_blue("format")) %>%
         str_replace("c\\((.+)\\)", col_green("c(\\1)")))
  
}



# Check that "Inf" and 'NA' are not present in the vector
if (sum(is.infinite(yearSelection)) > 0 || anyNA(yearSelection)) {
  
  stop(paste0("'yearSelection' contains infinity (Inf) and/or 'NA' values. This ", 
              "vector should contain only finite integer years.\n\n",
              "The format should be 'c(####, ####, ..., ####)'") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("Inf", col_red("Inf")) %>%
         str_replace("NA", col_red("NA")) %>%
         str_replace("finite", col_blue("finite")) %>%
         str_replace("integer", col_blue("integer")) %>%
         str_replace("years", col_blue("years")) %>%
         str_replace("c\\((.+)\\)", col_green("c(\\1)")))
  
}



# Confirm that every year is an integer
if (yearSelection %>% map_lgl(~ round(.) != .) %>% sum() > 0) {
  
  stop(paste0("'yearSelection' contains one or more non-integer values. This ", 
              "vector should contain only finite integer years.\n\n",
              "The format should be 'c(####, ####, ..., ####)'") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("non.integer", col_red("non-integer")) %>%
         str_replace("finite", col_blue("finite")) %>%
         str_replace(" integer", col_blue(" integer")) %>%
         str_replace("years", col_blue("years")) %>%
         str_replace("c\\((.+)\\)", col_green("c(\\1)")))
  
}



# Ensure that every number in 'yearSelection' is four digits
if (min(yearSelection) < 1000 || max(yearSelection) > 9999) {
  
  stop(paste0("'yearSelection' contains one or more values that ", 
              "are not four digits long.\n\n",
              "The format should be 'c(####, ####, ..., ####)'") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("not", col_red("not")) %>%
         str_replace("four", col_blue("four")) %>%
         str_replace("digits", col_blue("digits")) %>%
         str_replace("c\\((.+)\\)", col_green("c(\\1)")))
  
}




# Ensure that years from 2017 onwards are specified
if (min(yearSelection) < 2017) {
  
  stop(paste0("'yearSelection' contains a year that is earlier than 2017. This ", 
              "procedure was not written to incorporate older data. The Snowflake ", 
              "download script acquires data from 2017 onwards, so both that script ",
              "and this script would need to be adjusted to allow usage of older data.") %>%
         strwrap(width = 0.98 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("earlier", col_red("earlier")) %>%
         str_replace("than", col_red("than")) %>%
         str_replace("2017", col_red("2017")) %>%
         str_replace("not", col_red("not")) %>%
         str_replace("both", col_magenta("both")) %>%
         str_replace("adjusted", col_magenta("adjusted")))
  
}



# The final step is to determine the *label* that will be included in filenames
# related to this year selection



# Get the sequence of years between the minimum and maximum years in 'yearSelection'
fullRange <- seq(from = min(yearSelection), to = max(yearSelection), by = 1)



# This should not ever happen, but check for it, just in case
# The length of 'fullRange' should always be greater than or equal to the length of 'yearSelection'
if (length(yearSelection) > length(fullRange)) {
  
  stop("An unhandled error occurred. Script revisions are needed.")
  
}



# If the entire sequence between these years is present in 'yearSelection',
# the file label can simply be the minimum and maximum years 
if (length(yearSelection) == length(fullRange) &&
    sum(fullRange %in% yearSelection) == length(yearSelection)) {
  
  
  yearLabel <- paste0(min(yearSelection), "_", max(yearSelection))
  
  
# Otherwise, identify the missing years in the sequence
# The label will mention the bounds *as well as* the excluded years
} else {
  
  
  missingYears <- base::setdiff(fullRange, yearSelection)
  
  
  
  yearLabel <- paste0(min(yearSelection), "_", max(yearSelection),
                      "_(sans_",
                      paste0(sort(missingYears), collapse = "_"),
                      ")")
  
  
}



# After that, output a message to the console about the select years
if (grepl("sans", yearLabel)) {
  
  cat(paste0("Running scripts with reporting data from ", 
             yearLabel %>% str_replace("_", " to ") %>% str_remove("_.+$"), 
             " (sans ",
             if_else(length(missingYears) == 1, 
                     as.character(missingYears[1]),
                     if_else(length(missingYears) == 2, 
                             paste0(missingYears[1], " and ", missingYears[2]),
                             paste0(missingYears, collapse = ", ") %>% 
                               str_replace(", ([0-9]{4})$", ", and \\1"))),
             ")\n\n"))
  
} else {
  
  cat(paste0("Running scripts with reporting data from ", 
             yearLabel %>% str_replace("_", " to "), 
             "\n\n"))
  
}



# Other than 'yearSelection' and 'yearLabel', remove all other variables introduced by this script
remove(list = base::intersect(ls(), c("fullRange", "missingYears")))




