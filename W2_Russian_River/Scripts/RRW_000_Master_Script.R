# This script contains the fully automated procedure for the supply and demand
# components of the Russian River watershed's modeling workflow


#### Setup ####

# Clear the environment first
base::remove(list = ls())


# Check the working directory
if (!grepl("[/\\\\]DWRAT_DataScraping$", getwd())) {
  stop("Please use \"DWRAT_DataScraping.Rproj\"")
}


# Import packages next

# Install 'renv' if it's not already present
source("Shared_Scripts/Project_Setup.R")


source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


#### Scripts ####


##### User Inputs #####

# Please open the following script and update it:
"CTR_001_Set_Start_and_End_Dates.R"


##### Process Pre-Check #####


# Check if updates are required for the models' core DAT and Precipitation files
source("W2_Russian_River/Scripts/HLP_008_Update_Main_DAT_and_Historic_Precip_Files.R")

##### Web Scraping #####

# Specify the start and end dates of the data scraping procedure in this script:
# "CTR_001_Set_Start_and_End_Dates.R"


# Once that script has been set, data from the following sources will be scraped:
#   (*) PRISM
#   (*) NOAA
#   (*) RAWS
#   (*) CIMIS
#   (*) CDEC


source("W2_Russian_River/Scripts/RRW_001_PRISM_HTTP_Scraper.R")

source("W2_Russian_River/Scripts/RRW_002_NOAA_API_Scraper.R")

source("W2_Russian_River/Scripts/RRW_003_RAWS_HTTP_Scraper.R")

source("W2_Russian_River/Scripts/RRW_004_CIMIS_API_Scraper.R")

source("W2_Russian_River/Scripts/RRW_005_CDEC_API_Scraper.R")


##### PRMS #####

# Process the downloaded weather files
source("W2_Russian_River/Scripts/RRW_006_Process_PRMS_Weather_Data.R")


# Setup the output directory for the PRMS and SRP model runs
source("W2_Russian_River/Scripts/RRW_007_Setup_Output_Directory.R")


# Setup the temporary PRMS model location
source("W2_Russian_River/Scripts/RRW_008_Setup_PRMS_Model.R")


# Generate the input DAT file for PRMS
source("W2_Russian_River/Scripts/RRW_009_Finalize_PRMS_Input.R")


# Run PRMS
source("W2_Russian_River/Scripts/RRW_010_Run_PRMS.R")


# Store key outputs and clear out the copied model files
source("W2_Russian_River/Scripts/RRW_011_PRMS_Cleanup.R")


# Before proceeding, compare the precipitation in PRMS's model output 
# to the basin-averaged precipitation data from PRISM (as well as to the 
# average precipitation data in the PRMS input DAT file)
source("W2_Russian_River/Scripts/HLP_009_Compare_PRMS_Precip.R")


# Check streamflow data in PRMS's output too
source("W2_Russian_River/Scripts/HLP_010_Plot_PRMS_Streamflow.R")


# Before proceeding, compare PRMS's streamflow output to USGS gages' datasets
source("W2_Russian_River/Scripts/HLP_012_Compare_PRMS_Output_to_USGS_Gage.R")


##### SRP #####

# Process the downloaded weather file for SRP
source("W2_Russian_River/Scripts/RRW_012_Process_SRP_Weather_Data.R")


# Setup the temporary SRP model location
source("W2_Russian_River/Scripts/RRW_013_Setup_SRP_Model.R")


# Generate the input DAT file for SRP
source("W2_Russian_River/Scripts/RRW_014_Finalize_SRP_Input.R")


# Run SRP
source("W2_Russian_River/Scripts/RRW_015_Run_SRP.R")


# Store key outputs and clear out the copied model files
source("W2_Russian_River/Scripts/RRW_016_SRP_Cleanup.R")


# Before proceeding, compare SRP's streamflow output to USGS gages' datasets
source("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R")


##### Raw Flows #####

# Process the PRMS and SRP model results into raw flows for each sub-basin
# (This is the final result of the "supply" part of the process)
source("W2_Russian_River/Scripts/RRW_017_Generate_Raw_Flows.R")


##### DWRAT #####

# Ensure that Anaconda and a "paradigm-dwrat" environment are present
source("W2_Russian_River/Scripts/RRW_018_DWRAT_Precheck.R")


# Prepare the input files for DWRAT and setup the Paradigm DWRAT script
source("W2_Russian_River/Scripts/RRW_019_Finalize_DWRAT_Inputs.R")


# Run DWRAT
source("W2_Russian_River/Scripts/RRW_020_Run_DWRAT.R")


# Perform final post-processing steps
source("W2_Russian_River/Scripts/RRW_021_DWRAT_Cleanup.R")


#### Extra Scripts ####

# Generate a batch file for ease of future use
source("W2_Russian_River/Scripts/HLP_006_Generate_RR_Workflow_Bat.R")


# Data analysis steps



#### Maintenance Scripts ####

# Generating new long-running DAT file

# Generating new historic precipitation averages 
# for the PRMS and SRP model domains

