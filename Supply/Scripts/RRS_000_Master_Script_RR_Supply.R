# This script contains the fully automated procedure for the "supply side" 
# of the Russian River watershed's modeling workflow


#### Setup ####

# Clear the environment first
remove(list = ls())


# Import packages
require(data.table)
require(tidyverse)
require(readxl)
require(cli)
require(httr)
require(rvest)
require(fs)


#### Scripts ####


##### User Inputs ####

# Please open the following scripts and update them:
"CTR_001_Set_Start_and_End_Dates.R"



##### Web Scraping #####

# Specify the start and end dates of the data scraping procedure in this script:
# "CTR_001_Set_Start_and_End_Dates.R"


# Once that script has been set, data from the following sources will be scraped:
#   (*) PRISM
#   (*) NOAA
#   (*) RAWS
#   (*) CIMIS


source("Scripts/RRS_001_PRISM_HTTP_Scraper.R")

source("Scripts/RRS_002_NOAA_API_Scraper.R")

source("Scripts/RRS_003_RAWS_HTTP_Scraper.R")

source("Scripts/RRS_004_CIMIS_API_Scraper.R")


##### PRMS ####

# Process the downloaded weather files
source("Scripts/RRS_005_Process_PRMS_Weather_Data.R")


# Setup the output directory for the PRMS and SRP model runs
source("Scripts/RRS_006_Setup_Output_Directory.R")


# Setup the temporary PRMS model location
source("Scripts/RRS_007_Setup_PRMS_Model.R")


# Generate the input DAT file for PRMS
source("Scripts/RRS_008_Finalize_PRMS_Input.R")


# Run PRMS
source("Scripts/RRS_009_Run_PRMS.R")


# Store key outputs and clear out the copied model files
source("Scripts/RRS_010_PRMS_Cleanup.R")


#### SRP ####

# source("Scripts/RRS_011_Process_SRP_Weather_Data.R")

# source("Scripts/RRS_012_Setup_SRP_Model.R")

# source("Scripts/RRS_013_Finalize_SRP_Input.R")



#### Hydro ####

# Process PRMS output (copies model outputs to GitHub folder)

# SRP Processor (PRMS output + aggregated/processed gag files --> Raw Flows + Datestamp in name)

# Copy SRP inputs and outputs + Raw Flows to SharePoint

# Prepare CSV to help with new rows for DWRAT Run Tracker

# Copy Raw Flows into Paradigm DWRAT

# Edit RR_Connected Paradigm DWRAT script

# Run DWRAT

# Model Post-processing and data analysis steps






# Maintenance scripts

# Generating SPI dataset
# Generating new long-running DAT file



