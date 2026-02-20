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
source("Scripts/RRS_005_Process_Weather_Data.R")


# Generate a DAT file for PRMS
#


##### Hydrology Directory Setup #####

# Generate a new folder to store files related to the model

# Have a script-controlled control file that contains the folder name
# (Automatically update this in the procedure)


# Setup model run folder for PRMS

# Edit PRMS control file 

# Running PRMS

# SPI/Similar Water Year (+ Rerun Dat PRMS and PRMS)

# Copy inputs and outputs to Hydrology folder

# Process PRMS output (copies model outputs to GitHub folder)

# SRP Processor (PRMS output + aggregated/processed gag files --> Raw Flows + Datestamp in name)

# Copy SRP inputs and outputs + Raw Flows to SharePoint

# Prepare CSV to help with new rows for DWRAT Run Tracker

# Copy Raw Flows into Paradigm DWRAT

# Edit RR_Connected Paradigm DWRAT script

# Run DWRAT

# Model Post-processing and data analysis steps



