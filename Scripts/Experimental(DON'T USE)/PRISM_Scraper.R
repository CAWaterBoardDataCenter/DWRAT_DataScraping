#SCRIPT LAST UPDATED:
    #BY: Marshall Knox
    #ON: 4/06/2023

#load packages ----
library(tidyverse)
library(RSelenium)
library(netstat)
library(here)

#Define Date Range---- 
StartDate <- as.Date("2023-01-11")
EndDate <- as.Date("2023-04-05")

#Set up RSelenium----
##Set Default download folder ----
eCaps <- list(
  chromeOptions =
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = gsub(pattern = '/', replacement = '\\\\', x = here("WebData")) # download.dir
    )
  )
)
default_folder <- eCaps$chromeOptions$prefs$download.default_directory

## Open a chrome browser session with RSelenium ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever ='111.0.5563.64', #set to the version on your PC that most closely matches the chrome browser version
  port = free_port(),
  extraCapabilities = eCaps
)

remDr <- rs_driver_object$client

#PRISM Precipitation Bulk Download----
URL = "https://prism.oregonstate.edu/explorer/bulk.php"
remDr$navigate(URL)

#Import Precipitation Stations
file_path_precip <- here("InputData/prism_rr_precip_stations.csv")
file_input <- remDr$findElement(using = "xpath", "//input[@type='file']")
file_input$sendKeysToElement(list(file_path_precip))

#Wait for the file to be selected
Sys.sleep(2)

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
StartMonth <- remDr$findElement(using = "id", value = "tper_daily_start_month")
StartMonth$sendKeysToElement(list(format(StartDate, "%B")))

StartYear <- remDr$findElement(using = "id", value = "tper_daily_start_year")
StartYear$sendKeysToElement(list(format(StartDate, "%Y")))

EndDay <-remDr$findElement(using = "id", value = "tper_daily_end_day")
EndDay$sendKeysToElement(list(format(EndDate, "%d")))

EndMonth <-remDr$findElement(using = "id", value = "tper_daily_end_month")
EndMonth$sendKeysToElement(list(format(EndDate, "%B")))

StartDay <- remDr$findElement(using = "id", value = "tper_daily_start_day")
StartDay$sendKeysToElement(list(format(StartDate, "%d")))

#Do not need to input EndYear (if in current year) Just breaks date range
# EndYear <- remDr$findElement(using = "id", value = "tper_daily_end_year")
# EndYear$sendKeysToElement(list(format(EndDate, "%Y")))

Sys.sleep(2)

#Prepare and Download Time Series
Download <- remDr$findElement(using = "id", value = "submitdown_button")
Download$clickElement()

Sys.sleep(2)

#Input Temperature Stations
file_path_temp <- here("InputData/temp_fill_stations.csv")
file_input$sendKeysToElement(list(file_path_temp))

Sys.sleep(2)

#Uncheck Precipitation checkbox
Precip_box <- remDr$findElement(using = "id", value = "cvar_ppt")
Precip_box$clickElement()

#Check Minimum and Maximum temp
MinTemp <- remDr$findElement(using = "id", value = "cvar_tmin")
MinTemp$clickElement()
MaxTemp <- remDr$findElement(using = "id", value = "cvar_tmax")
MaxTemp$clickElement()

Sys.sleep(2)

#Prepare and Download Time Series
Download <- remDr$findElement(using = "id", value = "submitdown_button")
Download$clickElement()

Sys.sleep(4)
#End RSelenium Process
remDr$closeWindow()
system("taskkill /im java.exe /f")

#Rename scraped PRISM raw files----
#Get the list of file names in WebData
web_data <- here("Webdata")
file_list <- list.files(path = web_data)

#Loop to find and rename "ppt" and "tm" files
for (file_name in file_list) {
  if (grepl("ppt", file_name)) {
    new_ppt_name <- "PRISM_Precip_Raw.csv"
    file.rename(file.path(web_data, file_name), file.path(web_data, new_ppt_name))
  } else if (grepl("tmin_tmax", file_name)) {
    new_tm_name <- "PRISM_Temp_Raw.csv"
    file.rename(file.path(web_data, file_name), file.path(web_data, new_tm_name))
  }
}
