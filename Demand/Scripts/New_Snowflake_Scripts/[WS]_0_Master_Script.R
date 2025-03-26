# Given the eWRIMS flag table (generated using "[CA]_0_Master_Script.R"),
# Develop a QA/QC'd demand dataset for a given watershed


remove(list = ls())



require(cli)
require(tidyverse)
require(readxl)



#### IMPORTANT! ####

# CHOOSE YOUR WATERSHED #
source("Scripts/New_Snowflake_Scripts/[HELPER]_3_Watershed_Selection.R")



# CHOOSE THE BOUNDS OF YOUR DEMAND DATASET #
source("Scripts/New_Snowflake_Scripts/[HELPER]_4_Year_Selection.R")



#### GIS PREPROCESSING ####

source("Scripts/New_Snowflake_Scripts/[WS]_1_GIS_Preprocessing.R")


