# Create a column related to a water right's primary beneficial use

# A new column will be added to the flag table:
#  "PRIMARY_BENEFICIAL_USE"


#### Setup ####


remove(list = ls())


require(cli)
require(data.table)
require(tidyverse)



source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


#### Procedure ####


print("Starting '[CA]_9_Flag_Table_Primary_Beneficial_Use.R'...")



cat("\n\n")
cat(paste0("Using the 'USE_CODE' rankings specified in this script, ",
           "a 'primary' beneficial use will be assigned to each water right") %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n") %>%
      str_replace("in ", col_blue("in ")) %>%
      str_replace("this", col_blue("this")) %>%
      str_replace("script", col_blue("script")) %>%
      str_replace("primary", col_green("primary")))
cat("\n\n\n")



# Define the different use codes that can appear in the dataset
# Assign a ranking value to each one (use codes that are more important have a lower number)
# (The numbers are assigned based on the order in which the use codes appear in this vector)
# (The codes at the top of the vector have a lower ranking => greater importance)
useCodeRanking <- c("Irrigation", 
                    "Municipal",
                    "Domestic",
                    "Power",
                    "Stockwatering",
                    "Industrial",
                    "Frost Protection",
                    "Heat Control",
                    "Recreational",
                    "Dust Control",
                    "Mining", 
                    "Fire Protection",
                    "Aquaculture", 
                    "Fish and Wildlife Preservation and Enhancement", 
                    "Incidental Power", 
                    "Milling", 
                    "Snow Making",
                    "Water Quality",
                    "Aesthetic",
                    "Other") %>%
  matrix(ncol = 1, byrow = TRUE) %>% data.frame() %>%
  set_names("USE_CODE") %>%
  mutate(USE_CODE_RANK = row_number())



cat("Gathering the beneficial uses of each water right..." %>%
      strwrap(width = 0.98 * getOption("width")) %>%
      paste0(collapse = "\n"))



# Read in the primary uses for each of the water rights
useDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "YEAR", "USE_CODE")) %>%
  unique()



# Verify that every non-NA use code that appears in 'useDF' is present in 'useCodeRanking'
if (sum(useDF$USE_CODE %in% c(NA_character_, useCodeRanking$USE_CODE)) != nrow(useDF)) {
  
  cat("\n\n")
  stop(paste0("The dataset contains at least one 'USE_CODE' value that does not appear in this",
              " script's table of uses:\n\n",
              paste0(useDF$USE_CODE[!is.na(useDF$USE_CODE) & !(useDF$USE_CODE %in% useCodeRanking$USE_CODE)], 
                     collapse = "; "), 
              "\n\nPlease update the variable titled 'useCodeRanking' in this script.",
              " Add the new 'USE_CODE' string(s) to the initial vector.\n\nThe actual numeric rankings are",
              " automatically assigned based on the order of the strings in the vector.",
              " More important uses have a lower ranking value and appear earlier in the initial vector.",
              " Also, the strings must exactly match what appears in the eWRIMS dataset.") %>%
         strwrap(width = 0.99 * getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("does", col_red("does")) %>%
         str_replace("not", col_red("not")) %>%
         str_replace("appear", col_red("appear")) %>%
         str_replace("update", col_green("update")) %>%
         str_replace("useCodeRanking", col_green("useCodeRanking")) %>%
         str_replace("string\\(s\\)", style_bold("string(s)")) %>%
         str_replace("initial", col_blue("initial")) %>%
         str_replace("vector", col_blue("vector")))
  
}



cat("Done!\n\n")
cat("Determining the primary use of each water right...")



# Append the rankings for different use codes to 'useDF'
useDF <- useDF %>%
  left_join(useCodeRanking, by = "USE_CODE", relationship = "many-to-one")



# Get the minimum-ranked use type for 
# each pair of "APPLICATION_NUMBER" and "YEAR"
useDF <- useDF %>%
  group_by(APPLICATION_NUMBER, YEAR) %>%
  summarize(USE_CODE_RANK = sort(unique(USE_CODE_RANK), na.last = TRUE) %>% head(1),
            .groups = "drop")



# Append again the rankings for different use codes to 'useDF'
useDF <- useDF %>%
  left_join(useCodeRanking, by = "USE_CODE_RANK", relationship = "many-to-one")



cat("Done!\n\n")
cat("Adding a 'PRIMARY_BENEFICIAL_USE' column to the flag table...")



# Read in 'flagDF' and append a new column
flagDF <- readFlagTable()



# Join some columns of 'useDF' to 'flagDF' using "APPLICATION_NUMBER" and "YEAR"
flagDF <- flagDF %>%
  left_join(useDF %>%
              select(APPLICATION_NUMBER, YEAR, USE_CODE) %>%
              rename(PRIMARY_BENEFICIAL_USE = USE_CODE),
            by = c("APPLICATION_NUMBER", "YEAR"), relationship = "many-to-one")



# Write the updated 'flagDF' to a file
writeFlagTable(flagDF)



# Output a completion message
cat("Adding a 'PRIMARY_BENEFICIAL_USE' column to the flag table...Done!\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())
