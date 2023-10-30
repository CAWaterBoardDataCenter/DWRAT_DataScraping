#Download the USGS Discharge data [CFS] from station EF Russian 11461500
#Change on Time Span to select the observed data range
#Select Data to graph--pick Discharge, cubic feet per second

#Click on Download data
#Under Select data to retrieve, pick Primary Time Series
#Click on Retrieve
#Import the CSV
#Calculate daily, monthly, and weekly running flow averages 
#Plot the averages


#SCRIPT LAST UPDATED:
#BY: Payman Alemi
#ON: 10/25/2023

# Load packages
library(tidyverse)
library(RSelenium)
library(netstat)
library(here)
library(binman)

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

#Navigate to USGS Station 11461500
URL = "https://waterdata.usgs.gov/monitoring-location/11461500"
remDr$navigate(URL)
Sys.sleep(3)


#Click on Change Time span button----
ChangeTimeSpan <- remDr$findElement(using = "xpath", "//button[contains(.,'Change') 
and contains(.,'time span')]")
ChangeTimeSpan$clickElement()

#Update start and end dates as needed 
PVP_StartDate = "10/01/2023" #site requires MM/DD/YYYY format
PVP_EndDate = "10/29/2023" #site requires MM/DD/YYYY format

PVP_Start = remDr$findElement(using = "id", value = "start-date")
PVP_Start$sendKeysToElement(list(PVP_StartDate))

PVP_End = remDr$findElement(using = "id", value = "end-date")
PVP_End$sendKeysToElement(list(PVP_EndDate))

#Select Discharge, cubic feet per second
# Locate the radio button with css selector
Discharge <- remDr$findElement(using = "css selector", value = "label[for='radio-00060']")
Discharge$clickElement()


                              

