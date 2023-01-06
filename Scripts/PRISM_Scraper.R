# Set up RSelenium -------------------------------------------------------
## load packages
library(RSelenium)
library(rvest)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)

##Set Default download folder----
eCaps <- list(
  chromeOptions =
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = gsub(pattern = '/', replacement = '\\\\', x = here("WebData")) # download.dir
    )
    )
)
default_folder = here("WebData")

##Open a chrome browser session with RSelenium----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever ='108.0.5359.71',
  port = free_port(),
)

remDr <- rs_driver_object$client

#Input Data for both datasets----
#Define Dates
StartDate = data.frame("December", "01", "2022", as.Date("2022-12-01"))
EndDate = data.frame("January", "04", "2023", as.Date("2023-01-04"))

colnames(StartDate) = c("month", "day", "year", "date")
colnames(EndDate) = c("month", "day", "year", "date")
ndays = seq(from = StartDate$date, to = EndDate$date, by = 'day')%>% length()
ndays

#Precipitation Data----
##Input Locations File
PrecipitationFile <- gsub(pattern = "/", replacement = "\\\\", x = here("InputData/prism_rr_precip_stations.csv"))

##Navigate to PRISM Explorer webpage----
remDr <- rs_driver_object$client
URL <- "https://prism.oregonstate.edu/explorer/bulk.php"
remDr$navigate(URL)

##Fill out PRISM Explorer webpage----
#Select precipitation checkbox
# Precipitation <-remDr$findElement(using = "id", value = "cvar_ppt")
# Precipitation$clickElement()

#Deselect all other data settings checkboxes
MeanTemp <- remDr$findElement(using = "id", value = "cvar_tmean")
MeanTemp$clickElement()

#Select daily values
Daily <- remDr$findElement(using = "id", value = "tper_daily")
Daily$clickElement()

#Set the Starting Date
StartDay <- remDr$findElement(using = "id", value = "tper_daily_start_day")
StartDay$sendKeysToElement(list(StartDate$day))

StartMonth <- remDr$findElement(using = "id", value = "tper_daily_start_month")
StartMonth$sendKeysToElement(list(StartDate$month))

StartYear <- remDr$findElement(using = "id", value = "tper_daily_start_year")
StartYear$sendKeysToElement(list(StartDate$year))

#Set the Ending Date
EndDay <- remDr$findElement(using = "id", value = "tper_daily_end_day")
EndDay$sendKeysToElement(list(EndDate$day))

EndMonth <- remDr$findElement(using = "id", value = "tper_monthly_end_month")
EndMonth$sendKeysToElement(list(EndDate$month))

EndYear <- remDr$findElement(using = "id", value = "tper_daily_end_year")
EndYear$sendKeysToElement(list(EndDate$year))

#Select SI (metric) units
Units <- remDr$findElement(using = "id", value = "units_si")
Units$clickElement()

#Check Interpolate grid cell values
Grid <- remDr$findElement(using = "id", value = "loc_interp")
Grid$clickElement()

#Select Open Locations File
UploadBtn <- remDr$findElement(using = "id", value = "upload_locations")
#UploadBtn$sendKeysToElement(list(PrecipitationFile))
UploadBtn$sendKeysToElement(list("C:\\Users\\palemi\\Water Boards\\Supply and Demand Assessment - Documents\\DWRAT_DataScraping\\InputData\\prism_rr_precip_stations.csv"))
UploadBtn$clickElement()

#Click Prepare & Download Time Series
SubmitBtn <- remDr$findElement(using = "id", value = "submitdown_button")
SubmitBtn$clickElement()

#Temperature Data ----
#Import Temperature Stations
TempFile <- gsub(pattern = "/", replacement = "\\\\", x = here("InputData/temp_fill_stations.csv"))

##Navigate to PRISM Explorer webpage----
remDr <- rs_driver_object$client
URL <- "https://prism.oregonstate.edu/explorer/bulk.php"
remDr$navigate(URL)

#Deselect precipitation checkbox
Precipitation <-remDr$findElement(using = "id", value = "cvar_ppt")
Precipitation$clickElement()

#Select Temperature checkboxes
MinTemp <-remDr$findElement(using = "id", value = "cvar_tmin")
MinTemp$clickElement()

MaxTemp <- remDr$findElement(using = "id", value = "cvar_tmax")
MaxTemp$clickElement()


#Select daily values
Daily <- remDr$findElement(using = "id", value = "tper_daily")
Daily$clickElement()

#Set the Starting Date
StartDay <- remDr$findElement(using = "id", value = "tper_daily_start_day")
StartDay$sendKeysToElement(list(StartDate$day))

StartMonth <- remDr$findElement(using = "id", value = "tper_daily_start_month")
StartMonth$sendKeysToElement(list(StartDate$month))

StartYear <- remDr$findElement(using = "id", value = "tper_daily_start_year")
StartYear$sendKeysToElement(list(StartDate$year))

#Set the Ending Date
EndDay <- remDr$findElement(using = "id", value = "tper_daily_end_day")
EndDay$sendKeysToElement(list(EndDate$day))

EndMonth <- remDr$findElement(using = "id", value = "tper_monthly_end_month")
EndMonth$sendKeysToElement(list(EndDate$month))

EndYear <- remDr$findElement(using = "id", value = "tper_daily_end_year")
EndYear$sendKeysToElement(list(EndDate$year))

#Select SI (metric) units
Units <- remDr$findElement(using = "id", value = "units_si")
Units$clickElement()

#Check Interpolate grid cell values
Grid <- remDr$findElement(using = "id", value = "loc_interp")
Grid$clickElement()

#Select Open Locations File
UploadBtn <- remDr$findElement(using = "id", value = "upload_locations")
UploadBtn$sendKeysToElement(list(TempFile))
UploadBtn$clickElement()

#Click Prepare & Download Time Series
SubmitBtn <- remDr$findElement(using = "id", value = "submitdown_button")
SubmitBtn$clickElement()


