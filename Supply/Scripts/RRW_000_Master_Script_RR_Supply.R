# This script contains the fully automated procedure for the supply and demand
# components of the Russian River watershed's modeling workflow


#### Setup ####

# Clear the environment first
base::remove(list = ls())


# Import packages
require(renv)

restore()

source("Scripts/HLP_000_Load_Packages.R")


#### Scripts ####


##### User Inputs #####

# Please open the following scripts and update them:
"CTR_001_Set_Start_and_End_Dates.R"


##### Process Pre-Check #####

# Check that the directory is correctly set
source("Scripts/HLP_004_Check_Working_Directory.R")


# Check if updates are required for the models' core DAT and Precipitation files
source("Scripts/HLP_008_Update_Main_DAT_and_Historic_Precip_Files.R")

##### Web Scraping #####

# Specify the start and end dates of the data scraping procedure in this script:
# "CTR_001_Set_Start_and_End_Dates.R"


# Once that script has been set, data from the following sources will be scraped:
#   (*) PRISM
#   (*) NOAA
#   (*) RAWS
#   (*) CIMIS


source("Scripts/RRW_001_PRISM_HTTP_Scraper.R")

source("Scripts/RRW_002_NOAA_API_Scraper.R")

source("Scripts/RRW_003_RAWS_HTTP_Scraper.R")

source("Scripts/RRW_004_CIMIS_API_Scraper.R")


##### PRMS ####

# Process the downloaded weather files
source("Scripts/RRW_005_Process_PRMS_Weather_Data.R")


# Setup the output directory for the PRMS and SRP model runs
source("Scripts/RRW_006_Setup_Output_Directory.R")


# Setup the temporary PRMS model location
source("Scripts/RRW_007_Setup_PRMS_Model.R")


# Generate the input DAT file for PRMS
source("Scripts/RRW_008_Finalize_PRMS_Input.R")


# Run PRMS
source("Scripts/RRW_009_Run_PRMS.R")


# Store key outputs and clear out the copied model files
source("Scripts/RRW_010_PRMS_Cleanup.R")


##### SRP #####

# Process the downloaded weather file for SRP
source("Scripts/RRW_011_Process_SRP_Weather_Data.R")


# Setup the temporary SRP model location
source("Scripts/RRW_012_Setup_SRP_Model.R")


# Generate the input DAT file for SRP
source("Scripts/RRW_013_Finalize_SRP_Input.R")


# Run SRP
source("Scripts/RRW_014_Run_SRP.R")


# Store key outputs and clear out the copied model files
source("Scripts/RRW_015_SRP_Cleanup.R")


##### Raw Flows #####

# Process the PRMS and SRP model results into raw flows for each sub-basin
# (This is the final result of the "supply" part of the process)
source("Scripts/RRW_016_Generate_Raw_Flows.R")


##### DWRAT #####

# Ensure that Anaconda and a "paradigm-dwrat" environment are present
source("Scripts/RRW_017_DWRAT_Precheck.R")


# Prepare the input files for DWRAT and setup the Paradigm DWRAT script
source("Scripts/RRW_018_Finalize_DWRAT_Inputs.R")


# Run DWRAT
source("Scripts/RRW_019_Run_DWRAT.R")


# Perform final post-processing steps
source("Scripts/RRW_020_DWRAT_Cleanup.R")


#### Extra Scripts ####

# Generate a batch file for ease of future use
source("Scripts/HLP_006_Generate_RR_Workflow_Bat.R")


# Data analysis steps



#### Maintenance Scripts ####

# Generating new long-running DAT file

# Generating new historic precipitation averages 
# for the PRMS and SRP model domains

