# This script contains the fully automated procedure for the supply and demand
# components of the Russian River watershed's modeling workflow

# This is the "v2" process that relies on the newer SRP and RRIHM models


#### Setup ####

# Clear the environment first
base::remove(list = ls())


# Import packages
require(renv)

restore()

source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


#### Scripts ####



##### User Inputs #####

# Please open the following scripts and update them:
"CTR_001_Set_Start_and_End_Dates.R"


##### Process Pre-Check #####

# Check that the directory is correctly set
source("W2_Russian_River/Scripts/HLP_004_Check_Working_Directory.R")


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

source("W2_Russian_River/Scripts/RRW_v2_005_CDEC_API_Scraper.R")


##### SRP #####

# Process PRISM data for SRP
source("W2_Russian_River/Scripts/RRW_v2_006_Process_SRP_Weather_Data.R")


# Setup the output directory for the workflow model runs
source("W2_Russian_River/Scripts/RRW_v2_007_Setup_Output_Directory.R")


# Setup the temporary SRP model location
source("W2_Russian_River/Scripts/RRW_v2_008_Setup_SRP_Model.R")


# Generate the input DAT file for SRP
source("W2_Russian_River/Scripts/RRW_v2_009_Finalize_SRP_Input.R")


# Run SRP
source("W2_Russian_River/Scripts/RRW_v2_010_Run_SRP.R")


# Store key outputs and clear out the copied model files
source("W2_Russian_River/Scripts/RRW_v2_011_SRP_Cleanup.R")


#### RRIHM ####

# Setup the temporary RRIHM model location
source("W2_Russian_River/Scripts/RRW_v2_012_Setup_RRIHM_Model.R")







