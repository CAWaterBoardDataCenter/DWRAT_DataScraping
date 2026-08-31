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
"CTR_001_Set_Start_and_End_Dates.R"


##### Web Scraping #####

# Update the LSPC weather control file
source("W3_LSPC_Watershed/scripts/LSPC_001_Update_Control_File.R")


# Setup the Anaconda environment (plus SSL configuration)
source("W3_LSPC_Watershed/scripts/LSPC_002_Setup_Anaconda_Environment.R")


# Make sure project directories exist 
# Clear out previously downloaded data too, if present
source("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R")


# Setup archive directory
source("W3_LSPC_Watershed/scripts/LSPC_004_Setup_Archive_Directory.R")


# To Do: 
# Update the 100 yr return period from NOAA in each watershed's project control file


# Use Python scripts to download and process weather data
source("W3_LSPC_Watershed/scripts/LSPC_005a_Download_and_Stage_Climate_Data.R")


# The next step of the workflow is a manual review
# Provide the user with instructions about this
cat("\n\n")
paste0("Manual review spreadsheets have been generated for each watershed.\n\n",
       "They are located in \"W3_LSPC_Watershed/data/projects/[Watershed]/candidate/gage/QCSpreadsheets\".\n\n",
       "Please review the two spreadsheets that contain QC Flags 1, 2, 3, and 4. Delete the ",
       "entries of values that should be removed. Later scripts will fill in ",
       "all blank entries with data from PRISM.") |>
  errWrap() |>
  cat()
cat("\n\n")
