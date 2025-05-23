# FLAGGING SCRIPT

#Install if you do not have in your current packages or are not up to date.----
# install.packages("tidyverse")
#Load Packages- This step must be done each time the project is opened. ----
library(tidyverse)
library(readxl)


# Output a message to the console
cat("Starting 'Priority_Date_Preprocessing.R'...\n")


source("Scripts/Watershed_Selection.R")


######################################################################## List of Application from GIS Step ####################################################################################

# Read in the results of the GIS Pre-Processing steps
# The filename will differ depending on its location (i.e., whether or not it's on SharePoint)
Application_Number <- getXLSX(ws = ws, 
                              SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_POD_APPLICATION_NUMBER_SPREADSHEET", 
                              FILEPATH = "POD_APPLICATION_NUMBER_SPREADSHEET_PATH", 
                              WORKSHEET_NAME ="POD_APPLICATION_NUMBER_WORKSHEET_NAME")


# Keep only the "APPLICATION_NUMBER" column
Application_Number <- Application_Number %>%
  select(APPLICATION_NUMBER) %>%
  unique()


# Read in the eWRIMS Flat File
ewrims_flat_file <- read.csv("RawData/ewrims_flat_file.csv") %>%
  unique()

# Perform an inner join using "APPLICATION_NUMBER"; 
  # force a one_to_one relationship to avoid duplicate application_numbers

ewrims_flat_file_Combined <- inner_join(Application_Number, ewrims_flat_file, by = "APPLICATION_NUMBER",
                                        relationship = "one-to-one")

# Output 'ewrims_flat_file_Combined' to a folder
write_csv(ewrims_flat_file_Combined, paste0("IntermediateData/", ws$ID, "_ewrims_flat_file_WITH_FILTERS.csv"))
################################################################### Priority Date ############################################################

# Produce the input file for the Priority Date module

# First, read in a flat file
Priority_Date <- read.csv(paste0("IntermediateData/", ws$ID, "_ewrims_flat_file_WITH_FILTERS.csv"))

# Extract a subset of the columns
Priority_Date_FINAL <- Priority_Date %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_TYPE, PRIORITY_DATE, APPLICATION_RECD_DATE, APPLICATION_ACCEPTANCE_DATE, SUB_TYPE, YEAR_DIVERSION_COMMENCED)

# Output that variable to a CSV file
write_csv(Priority_Date_FINAL, paste0("IntermediateData/", ws$ID, "_Priority_Date_FINAL.csv"))


# Remove variables that are no longer needed
remove(Priority_Date, Priority_Date_FINAL, ewrims_flat_file, ewrims_flat_file_Combined)


# Output a completion message to the console
cat("The Priority_Date_Processing.R script is done running!\n")
