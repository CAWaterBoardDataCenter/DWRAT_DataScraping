# This workflow involves downloading weather data for one or more watersheds

# That data is QC'd (with a manual review) and reformatted into LSPC weather files

# These weather files are input into an LSPC executable to generate streamflow
# estimates for a watershed

# The resultant files are submitted alongside demand data to run DWRAT


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


##### User Inputs #####

# Please open the following script and update it:
"CTR_001_Set_Start_and_End_Dates.R"


##### Web Scraping #####

# Update the LSPC weather control file
source("W3_LSPC_Watershed/scripts/LSPC_001_Update_Control_File.R")


# Setup the Anaconda environment (plus SSL configuration)
source("W3_LSPC_Watershed/scripts/LSPC_002_Setup_Anaconda_Environment.R")


# Make sure project directories exist
source("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R")


# Check for previous downloaded data
# Remove data from the last two water years


# Setup archive directory


# Read credential file for `env` values (NLDAS only)


# Download weather data


# Archive weather data and QC scripts


# Copy data from previous manual reviews






