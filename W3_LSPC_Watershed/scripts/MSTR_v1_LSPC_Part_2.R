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


# Adjust the weather files to prevent issues with newer versions of LSPC
source("W3_LSPC_Watershed/scripts/LSPC_015_Add_Dummy_Weather_Entries.R")


# Archive the files
source("W3_LSPC_Watershed/scripts/LSPC_016_Archive_Candidate_and_Curated_Files.R")


# Validate the LSPC model folder's contents
source("W3_LSPC_Watershed/scripts/LSPC_017_Check_LSPC_Model_Directory.R")


# Export the weather files to the LSPC model folder
source("W3_LSPC_Watershed/scripts/LSPC_018_Migrate_Weather_Files.R")


# Set up the inp files
source("W3_LSPC_Watershed/scripts/LSPC_019_Finalize_LSPC_Inputs.R")


# Run LSPC
source("W3_LSPC_Watershed/scripts/LSPC_020_Run_LSPC.R")


# Archive LSPC files 
source("W3_LSPC_Watershed/scripts/LSPC_021_Archive_LSPC_Outputs.R")


# Ensure that DWRAT is available
source("W3_LSPC_Watershed/LSPC_021_DWRAT_Precheck.R")


# Set up DWRAT files
source("W3_LSPC_Watershed/LSPC_022_Finalize_DWRAT_Inputs.R")


# Run DWRAT
source("W3_LSPC_Watershed/LSPC_023_Run_DWRAT.R")


# Archive DWRAT files
source("W3_LSPC_Watershed/LSPC_024_DWRAT_Cleanup.R")
