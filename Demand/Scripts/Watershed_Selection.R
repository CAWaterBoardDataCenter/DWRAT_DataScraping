# Use this script to select a watershed for the demand data analysis
# Change the row index on Line 18 to choose a watershed



require(tidyverse)
require(readxl)



# Generic functions that are used in multiple scripts
source("Scripts/Shared_Functions_Demand.R")




# IMPORTANT!! CHOOSE A WATERSHED
index <- 6 # Change the index to your desired watershed's corresponding "INDEX" value




# No other edits are needed to this file!



# Get watershed names and identifiers
if (file.exists(makeSharePointPath("Program Watersheds/4. Demand Data Tracking/Watershed_Demand_Dataset_Paths.xlsx"))) {
  
  ws <- makeSharePointPath("Program Watersheds/4. Demand Data Tracking/Watershed_Demand_Dataset_Paths.xlsx") %>%
    read_xlsx(sheet = "Main_Sheet", skip = 1)
  
} else {
  
  ws <- read_xlsx("InputData/Watershed_Demand_Dataset_Paths.xlsx",
                  sheet = "Main_Sheet", skip = 1)
  
}



# Select the row index of the chosen watershed
ws <- ws[index, ] 



# Error Check
stopifnot(nrow(ws) == 1)



cat(paste0("Running script for ", ws$NAME, "\n"))
