# This workflow involves downloading weather data for one or more watersheds

# That data is QC'd (with a manual review) and reformatted into LSPC weather files

# These weather files are input into an LSPC executable to generate streamflow
# estimates for a watershed

# The resultant files are submitted alongside demand data to run DWRAT


# This is the "Part 2" portion of the workflow 

# After a manual review has been completed, this script contains the remainder
# of the procedure (producing weather files, running LSPC, and running DWRAT)


#### Setup ####

# Clear the environment first
base::remove(list = ls())


# Check the working directory
if (!grepl("[/\\\\]DWRAT_DataScraping$", getwd())) {
  stop("Please use \"DWRAT_DataScraping.Rproj\"")
}


# Import packages next

# Install 'renv' if it's not already present
source("Additional_Scripts/Project_Setup.R")


source("Additional_Scripts/Load_Packages.R")


#### Scripts ####


# Confirm that the "Part 1" script has been run
# Check for QC spreadsheets, the archive text file
source("W3_LSPC_Watershed/scripts/HLP_003_Confirm_Part_1_Completion.R")


# Run the final set of Python scripts to prepare the LSPC weather files
source("W3_LSPC_Watershed/scripts/LSPC_014a_Generate_Weather_Files.R")


# Archive the files


# Adjust the weather files to prevent QA/QC issues


# Set up the inp files


# Run LSPC


# Archive files


# Run DWRAT


# Archive files
