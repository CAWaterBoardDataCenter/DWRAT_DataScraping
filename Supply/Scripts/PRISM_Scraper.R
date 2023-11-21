#SCRIPT LAST UPDATED:
    #BY: Payman Alemi
    #ON: 7/12/2023

# Load packages
library(tidyverse)
library(RSelenium)
library(netstat)
library(here)
library(binman)

# Define Date Range
# StartDate <- as.Date("2023-04-01")
# EndDate <- as.Date("2023-05-21")

# Set up RSelenium----
##Set Default download folder----
eCaps <- list(
  chromeOptions = list(
    prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = gsub(pattern = '/', replacement = '\\\\', x = here("WebData")) # download.dir
    )
  )
)
default_folder <- eCaps$chromeOptions$prefs$download.default_directory

## Download latest Chrome drivers---
temp <- wdman::chrome()
temp$stop()

## Set version of Chrome----
### Get current version of chrome browser----
chrome_browser_version <- system2(
  command = "wmic",
  args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
  stdout = TRUE,
  stderr = TRUE
) %>%
  str_extract(pattern = "(?<=Version=)(\\d+\\.){3}")

if (sum(!is.na(chrome_browser_version)) == 0) {
  chrome_browser_version <- system2(
    command = "wmic",
    args = 'datafile where name="C:\\\\Program Files\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
    stdout = TRUE,
    stderr = TRUE
  ) %>%
    str_extract(pattern = "(?<=Version=)(\\d+\\.){3}")
}

### List the versions of chromedriver on this PC
chrome_driver_versions <- list_versions("chromedriver")

### Match drivers to version----
chrome_driver_current <- chrome_browser_version %>%
  magrittr::extract(!is.na(.)) %>%
  str_replace_all(pattern = "\\.", replacement = "\\\\.") %>%
  paste0("^", .) %>%
  str_subset(string = last(chrome_driver_versions)) %>%
  as.numeric_version() %>%
  max() %>%
  as.character()


if (length(chrome_driver_current) == 0) {
  
  # Query the Chrome for Testing API to find the chrome driver version that must be downloaded
  versionToDownload <- paste0("https://googlechromelabs.github.io/chrome-for-testing/LATEST_RELEASE_",
         chrome_browser_version %>% magrittr::extract(!is.na(.)) %>%
           str_remove("\\.$")) %>%
    readLines(warn = FALSE)
  
  
  # A new directory folder will be created in the chromedriver folder (in the user's AppData folder)
  # Specify that new path in a variable
  newDir <- paste0(app_dir("chromedriver", check = FALSE), "/win32/", versionToDownload)
  
  
  # Create a folder in the destination directory for the new chromedriver
  dir.create(newDir)
  
  
  # Download the ZIP file to the chromedriver directory
  # (Note: By default, download.file() will fail if the download requires more than 60 seconds)
  paste0("https://edgedl.me.gvt1.com/edgedl/chrome/chrome-for-testing/", 
         versionToDownload, 
         "/win32/chromedriver-win32.zip") %>%
    download.file(paste0(newDir, "/chromedriver-win32.zip"),
                  mode = "wb")
  
  
  # Unzip the new ZIP file 
  unzip(paste0(newDir, "/chromedriver-win32.zip"),
        exdir = newDir)
  
  
  
  # Finally, copy the chromedriver.exe file to the base of this new directory
  file.copy(paste0(newDir, "/chromedriver-win32/chromedriver.exe"),
            paste0(newDir, "/chromedriver.exe"), 
            overwrite = TRUE)
  
  
  # Finally, update 'chrome_driver_current' to this downloaded version
  chrome_driver_current <- versionToDownload
  
  
}

### Remove the LICENSE.chromedriver file (if it exists)----
chrome_driver_dir <- paste0(app_dir("chromedriver", FALSE),
                            '/win32/',
                            chrome_driver_current)
if ('LICENSE.chromedriver' %in% list.files(chrome_driver_dir)) {
  file.remove(
    paste0(chrome_driver_dir, '/', 'LICENSE.chromedriver')
  )
}

### Open a chrome browser session with RSelenium ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  check = TRUE,
  chromever = chrome_driver_current, #set to the version on your PC that most closely matches the chrome browser version
  port = free_port(),
  extraCapabilities = eCaps
)

Sys.sleep(1)
remDr <- rs_driver_object$client


#PRMS PRISM Precip Bulk Download----
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
StartMonth$sendKeysToElement(list(format(StartDate$date, "%B")))

StartYear <- remDr$findElement(using = "id", value = "tper_daily_start_year")
StartYear$sendKeysToElement(list(format(StartDate$date, "%Y")))

EndDay <-remDr$findElement(using = "id", value = "tper_daily_end_day")
EndDay$sendKeysToElement(list(format(EndDate$date, "%d")))

EndMonth <-remDr$findElement(using = "id", value = "tper_daily_end_month")
EndMonth$sendKeysToElement(list(format(EndDate$date, "%B")))

StartDay <- remDr$findElement(using = "id", value = "tper_daily_start_day")
StartDay$sendKeysToElement(list(format(StartDate$date, "%d")))

#Do not need to input EndYear (if in current year) Just breaks date range
# EndYear <- remDr$findElement(using = "id", value = "tper_daily_end_year")
# EndYear$sendKeysToElement(list(format(EndDate, "%Y")))

Sys.sleep(2)

#Prepare and Download Time Series
Download <- remDr$findElement(using = "id", value = "submitdown_button")
Download$clickElement()

Sys.sleep(2)

# PRMS PRISM Temp Bulk Download----
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

Sys.sleep(2)

#Rename PRMS PRISM raw files----
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

#SRP PRISM Scraper----
#Import Santa Rosa Plains Stations
file_path_srp <- here("InputData/prism_srp_stations.csv")
file_input <- remDr$findElement(using = "xpath", "//input[@type='file']")
file_input$sendKeysToElement(list(file_path_srp))

Sys.sleep(2)

#Check Precipitation checkbox
Precip_box <- remDr$findElement(using = "id", value = "cvar_ppt")
Precip_box$clickElement()

#English Units
Units_English <-remDr$findElement(using = "id", value = "units_eng")
Units_English$clickElement()

Sys.sleep(2)

#Prepare and Download Time Series
Download <- remDr$findElement(using = "id", value = "submitdown_button")
Download$clickElement()

Sys.sleep(4)
#End RSelenium Process
remDr$closeWindow()
system("taskkill /im java.exe /f")

# Get the list of file names in WebData
web_data <- here("Webdata")
file_list <- list.files(path = web_data)

#Rename SRP PRISM raw files----
for (file_name in file_list) {
  if (grepl("ppt_tmin_tmax", file_name)) {
    new_ppt_name <- "PRISM_SRP_Raw.csv"
    file.rename(file.path(web_data, file_name), file.path(web_data, new_ppt_name))
  }
}
#List all variables in the global environment
all_vars <- ls()
#Specify the variables you want to keep
vars_to_keep <- c("StartDate", "EndDate", "End_Date", "vars_to_keep")

#Remove all variables except for those listed in vars_to_keep
vars_to_remove <- setdiff(x = all_vars, y = vars_to_keep)
rm(list = vars_to_remove)

print("PRIMS_Scraper has finished running")