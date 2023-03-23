#SCRIPT LAST UPDATED:
    #BY: Payman Alemi
    #ON: 3/20/2023

#install packages----
  #you should only have to do this once ever on your computer; then comment
  #out this portion of the script
# install.packages('RSelenium')
# install.packages('rvest')
# install.packages('tidyverse')
# install.packages('netstat')
# install.packages('here')
# install.packages('dplyr')
# install.packages('readr')

#load packages ----
library(RSelenium)
library(rvest)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)

#Define Date Range----
StartDate <- as.Date("2023-01-11")
EndDate <- as.Date("2023-03-20")


#Set up RSelenium----
##Set Default download folder ----
eCaps <- list(
  chromeOptions =
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = here("WebData") # download.dir
    )
    )
)
default_folder = here("WebData")
## Open browser ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever ='111.0.5563.64',
  port = free_port(),
)

remDr <- rs_driver_object$client

#PRISM Precipitation Bulk Download----
URL = "https://prism.oregonstate.edu/explorer/bulk.php"
remDr$navigate(URL)

#Import Precipitation Stations
file_path <- here("InputData/prism_rr_precip_stations.csv")
file_input <- remDr$findElement(using = "xpath", "//input[@type='file']")
file_input$sendKeysToElement(list(file_path))

#Wait for the file to be selected
Sys.sleep(5)

#Open Locations File
Locations <-remDr$findElement(using = "id", value = "upload_locations")
Locations$clickElement()

#Uncheck Mean temperature checkbox
MeanTemp <- remDr$findElement(using = "id", value = "cvar_tmean")
MeanTemp$clickElement()

#SI Units
Units <-remDr$findElement(using = "id", value = "units_si")
Units$clickElement()

#Interpolate Grid Cell Values
Grid <- remDr$findElement(using = "id", value = "loc_interp")
Grid$clickElement()

#Select Daily Values----
#Select Daily values checkbox
Daily <- remDr$findElement(using = "id", value = "tper_daily")
Daily$clickElement()

#Set start and end date ranges
StartDay <- remDr$findElement(using = "id", value = "tper_daily_start_day")
StartDay$sendKeysToElement(list(format(StartDate, "%d")))

StartMonth <- remDr$findElement(using = "id", value = "tper_daily_start_month")
StartMonth$sendKeysToElement(list(format(StartDate, "%B")))

StartYear <- remDr$findElement(using = "id", value = "tper_daily_start_year")
StartYear$sendKeysToElement(list(format(StartDate, "%Y")))

Sys.sleep(7)

EndDay <-remDr$findElement(using = "id", value = "tper_daily_end_day")
EndDay$sendKeysToElement(list(format(EndDate, "%d")))
  
EndMonth <-remDr$findElement(using = "id", value = "tper_daily_end_month")
EndMonth$sendKeysToElement(list(format(EndDate, "%B")))

EndYear <- remDr$findElement(using = "id", value = "tper_daily_end_year")
EndYear$sendKeysToElement(list(format(EndDate, "%Y")))

#Prepare and Download Time Series
Download <- remDr$findElement(using = "id", value = "submitdown_button")
Download$clickElement()
