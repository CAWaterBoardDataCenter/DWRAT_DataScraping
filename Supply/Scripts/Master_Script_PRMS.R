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
#jjkljldsdkl
# RUNS SCRAPING & PROCESSING SCRIPTS IN ORDER TO GENERATE FINAL DAT FILE
# BEFORE running, download Downsizer data

# set start and end dates -------------------------------------------------
## Set start date----
StartDate <- as.Date("2023-10-01") # 1-2 months before previous end date
#Serves as the start date for the observed data forecast and the DAT_Shell

# Extract Day, Month, and Year from StartDate; functions require lubridate package
StartDay <- day(StartDate) 
StartMonth <- month(StartDate)
StartYear <- year(StartDate)
StartDate <- data.frame(date = StartDate, day = StartDay, month = StartMonth, year = StartYear)

print(StartDate)

## set end date----
EndDate <- Sys.Date() - 1 # set to yesterday's date; serves as the end date for the observed data range
EndDay <- day(EndDate) 
EndMonth <- month(EndDate)
EndYear <- year(EndDate)
EndDate <- data.frame(date = EndDate, day = EndDay, month = EndMonth, year = EndYear)

print(EndDate)

TimeFrame = seq(from = StartDate$date, to = EndDate$date, by = 'day') #Timeframe is necessary for Downsizer_Processor.R
End_Date <- Sys.Date() + 5 # forecast end date for DAT_Shell_Generation.R

# generate PRMS model input -----------------------------------------------
source(here("Scripts/NOAA_API_Scraper.R"))
source(here("Scripts/PRISM_Scraper.R")) #downloads PRISM climate data for both PRMS and SRP stations simultaneously
source(here("Scripts/PRISM_Processor.R"))
print(Prism_Processed)
source(here("Scripts/CNRFC_Static_Scraper.R")) #downloads CNRFC data for both PRMS and SRP stations simultaneously
source(here("Scripts/CNRFC_RR_Processor.R"))
print(CNRFC_Processed)
# change input file name for Downsizer data; you need to run Downsizer and  
# move the Downsizer file to the WebData folder prior to running Downsizer_Processor.R
# Downsizer filename should match the filename given by Downsizer_Processor.R
source(here("Scripts/Downsizer_Processor.R")) #Ignore the warning message: Expected 252 pieces...
source(here("Scripts/RAWS_API_Scraper.R"))
source(here("Scripts/CIMIS_Static_Scraper.R"))
#source(here("Scripts/DAT_Shell_Generation.R")) #Ignore the warning message:In eval(e, x, parent.frame()) :...
# change output file name for DAT File
source(here("Scripts/DAT_File_Manipulation.R"))

# generate SRP model input ------------------------------------------------
source(here("Scripts/CNRFC_SRP_Processor.R")) #Formats already downloaded CNRFC forecast data for SRP
source(here("Scripts/PRISM_SRP_Processor.R")) #Formats already downloaded PRISM observed data for SRP

