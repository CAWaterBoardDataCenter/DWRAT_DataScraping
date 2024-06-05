#install.packages ("tinytex")
# load packages -----------------------------------------------------------
library(tidyverse)
library(RSelenium)
library(netstat)
library(lubridate)
library(here)
library(tinytex)
require(rvest)
require(httr)
require(writexl)
#
# RUNS SCRAPING & PROCESSING SCRIPTS IN ORDER TO GENERATE FINAL DAT FILE

# Include forecasted data from CNRFC in the datasets? ----
# (This should be either "TRUE" or "FALSE")
includeForecast <- FALSE


# set start and end dates -------------------------------------------------
## Set start dates----

StartDate <- as.Date("2023-10-01") # 1-2 months before previous end date; serves as the metereological start date
Hydro_StartDate = as.Date("2023-10-01", format = "%Y-%m-%d") #serves as the start date of the hydro simulation, 

  #usually the 1st day of the following month

#Serves as the start date for the observed data forecast and the DAT_Shell

# Extract Day, Month, and Year from StartDate; functions require lubridate package
StartDay <- day(StartDate) 
StartMonth <- month(StartDate)
StartYear <- year(StartDate)
StartDate <- data.frame(date = StartDate, day = StartDay, month = StartMonth, year = StartYear)

print(StartDate)

## set end date----
EndDate <- as.Date("2024-05-31")# set to desired end date for observed meteorological data range
EndDay <- day(EndDate) 
EndMonth <- month(EndDate)
EndYear <- year(EndDate)
EndDate <- data.frame(date = EndDate, day = EndDay, month = EndMonth, year = EndYear)

print(EndDate)

TimeFrame = seq(from = StartDate$date, to = EndDate$date, by = 'day') 
End_Date <- as.Date("2024-09-30", format = "%Y-%m-%d") # End of current Water Year

Hydro_EndDate = as.Date("2024-04-30", format = "%Y-%m-%d") #serves as the end date for the hydrological flows;

  # usually the last day of the next month

# generate PRMS model input -----------------------------------------------
source(here("Scripts/PRISM_HTTP_Scraper.R")) #downloads PRISM climate data for both PRMS and SRP stations simultaneously
source(here("Scripts/PRISM_Processor.R"))
print(Prism_Processed)
source(here("Scripts/NOAA_API_Scraper.R"))
#source(here("Scripts/CNRFC_API_Scraper.R")) #downloads CNRFC data for both PRMS and SRP stations simultaneously
#source(here("Scripts/CNRFC_PRMS_Processor.R")) #Formats CRNFC station data that are used by the PRMS model so 
  # they can be appended to the raw observed datasets from RAWS, CIMIS, and NOAA

#print(CNRFC_Processed)
# change input file name for Downsizer data; you need to run Downsizer and  
# move the Downsizer file to the WebData folder prior to running Downsizer_Processor.R
# Downsizer filename should match the filename given by Downsizer_Processor.R
source(here("Scripts/NOAA_Processor.R")) #Ignore the warning message: Expected 252 pieces...


source(here("Scripts/RAWS_API_Scraper.R"))
source(here("Scripts/CIMIS_API_Scraper.R"))

# Generate PRMS Dat File
source(here("Scripts/Dat_PRMS.R"))


# generate SRP model input ------------------------------------------------
#source(here("Scripts/CNRFC_SRP_Processor.R")) #Formats already downloaded CNRFC forecast data for SRP
source(here("Scripts/PRISM_SRP_Processor.R")) #Formats already downloaded PRISM observed data for SRP

# generate SRP Dat File
source(here("Scripts/Dat_SRP.R"))


# Model Post-Processing ------------------------------------------------

# PRMS Post-Processing Script
source(here("Scripts/PRMS_Processor.R"))


# SRP Post-Processing Script
source(here("Scripts/SRP_Post_Processing.R"))