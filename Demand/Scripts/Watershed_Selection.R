# DATA ACQUISITION SCRIPT
# Use this script to select a watershed for the demand data analysis
# Change the row index on Line 22 to choose a watershed



require(tidyverse)
require(readxl)


# Generic functions that are used in multiple scripts
source("Scripts/Shared_Functions_Demand.R")



# Get watershed names and identifiers
ws <- makeSharePointPath(filePathFragment = "Watershed Folders/Watershed_Demand_Dataset_Paths.xlsx") %>%
  read_xlsx(sheet = "Main_Sheet", skip = 1)



# IMPORTANT!! CHOOSE A WATERSHED
ws <- ws[5, ] # Change the row index to your desired watershed


# No other edits are needed to this file!



# Error Check
stopifnot(nrow(ws) == 1)



cat(paste0("Running script for ", ws$NAME, "\n"))
