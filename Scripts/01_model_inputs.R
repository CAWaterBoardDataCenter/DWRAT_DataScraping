# load packages -----------------------------------------------------------
library(tidyverse)
library(RSelenium)
library(netstat)
library(lubridate)
library(here)

# RUNS SCRAPING & PROCESSING SCRIPTS IN ORDER TO GENERATE FINAL DAT FILE
# BEFORE running, download Downsizer data

# set start and end dates -------------------------------------------------
StartDate <- as.Date("2023-04-01") # 1-2 months before previous end date
EndDate <- as.Date("2023-05-21") # set to yesterday's date
End_Date <- Sys.Date() + 5 # forecast end date for DAT_Shell_Generation.R

# generate PRMS model input -----------------------------------------------
source(here("Scripts/PRISM_Scraper.R"))
source(here("Scripts/PRISM_Processor.R"))
source(here("Scripts/CNRFC_Scraper.R"))
source(here("Scripts/CNRFC_RR_Processor.R"))
# change input file name for Downsizer data
source(here("Scripts/Downsizer_Processor.R"))
source(here("Scripts/RAWS_Scraper.R"))
source(here("Scripts/CIMIS_Scraper.R"))
source(here("Scripts/DAT_Shell_Generation.R"))
# change output file name for DAT File
source(here("Scripts/DAT_File_Manipulation.R"))

# generate SRP model input ------------------------------------------------
source(here("Scripts/CNRFC_SRP_Processor.R"))
source(here("Scripts/PRISM_SRP_Processor.R"))
