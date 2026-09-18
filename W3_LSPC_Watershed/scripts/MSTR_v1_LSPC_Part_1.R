# This workflow involves downloading weather data for one or more watersheds

# That data is QC'd (with a manual review) and reformatted into LSPC weather files

# These weather files are input into an LSPC executable to generate streamflow
# estimates for a watershed

# The resultant files are submitted alongside demand data to run DWRAT


# This is the "Part 1" portion of the workflow 

# It covers much of the initial setup as well as the gathering of weather data


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
"W3_LSPC_Watershed/scripts/CTR_001_Set_Start_and_End_Dates.R"


##### Web Scraping #####

# Update the LSPC weather control file
source("W3_LSPC_Watershed/scripts/LSPC_001_Update_Control_File.R")


# Setup the Anaconda environment (plus SSL configuration)
source("W3_LSPC_Watershed/scripts/LSPC_002_Setup_Anaconda_Environment.R")


# Make sure project directories exist 
# Clear out previously downloaded data too, if present
source("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R")


# Download weather data from "shared" sources (PRISM, CIMIS, and NLDAS)
source("W3_LSPC_Watershed/scripts/LSPC_004a_Download_Shared_Climate_Data.R")

source("W3_LSPC_Watershed/scripts/LSPC_004c_Download_PRISM_Data.R")


# Copy old weather data into the "shared" weather folder
source("W3_LSPC_Watershed/scripts/LSPC_005_Copy_Raw_Historic_Weather_Data.R")


# Update the LSPC weather control file to have a start date from the beginning
# of the model run period
source("W3_LSPC_Watershed/scripts/LSPC_006_Update_Control_File_Start_Date.R")


# Download gage data for each watershed
source("W3_LSPC_Watershed/scripts/LSPC_007a_Download_Watershed_Data.R")

source("W3_LSPC_Watershed/scripts/LSPC_007c_Download_RAWS_Data.R")


# Setup an archive directory
source("W3_LSPC_Watershed/scripts/LSPC_008_Setup_Archive_Directory.R")


# To Do: 
# Update the 100 yr return period from NOAA in each watershed's project control file


# Pre-process PRISM data for the staging step
source("W3_LSPC_Watershed/scripts/LSPC_009_Prep_Candidate_PRISM_Data.R")


# Pre-process RAWS data before staging as well
source("W3_LSPC_Watershed/scripts/LSPC_010_Prep_RAWS_Data.R")


# Stage climate data next
source("W3_LSPC_Watershed/scripts/LSPC_011a_Stage_Climate_Data.R")


# Archive files before proceeding
source("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R")


# Adjust the manual review spreadsheets before users perform the actual review
source("W3_LSPC_Watershed/scripts/LSPC_013_Adjust_Manual_Review_Sheets.R")


# End of Part 1

# Please complete the manual reviews
# Then, proceed to the Part 2 master script
"W3_LSPC_Watershed/scripts/MSTR_v1_LSPC_Part_2.R"
