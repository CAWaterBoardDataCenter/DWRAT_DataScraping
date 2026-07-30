# This script contains scripts that automate some of the steps in the LSPC modeling
# workflow

# Please note that this is an interim script 
# for a process that is still being built out


#### Setup ####

# Clear the environment first
base::remove(list = ls())


# Check the working directory
if (!grepl("[/\\\\]DWRAT_DataScraping$", getwd())) {
  stop("Please use \"DWRAT_DataScraping.Rproj\"")
}


# Import packages next

# Install 'renv' and all required R packages
source("Shared_Scripts/Project_Setup.R")


# Load in tidyverse packages
require(tidyverse)


#### Scripts ####


# Move finished weather files from the "W3" workflow folder to 
# corresponding watershed folders in the LSPC model directory
source("W3_LSPC_Watershed/Export_Weather_Files.R")


# Update the inp files of all watershed LSPC models to match the end dates
# in their weather files
source("W3_LSPC_Watershed/Update_End_Date.R")


# Clean-up
base::remove(list = ls())
