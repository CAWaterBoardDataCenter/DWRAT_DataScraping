library(RSelenium)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)
library(binman)

# Find and remove previously downloaded CNRFC Data
matching_files <- list.files(path = here("WebData"), pattern = "temperaturePlot|cnrfc", full.names = TRUE)

if (length(matching_files) > 0) {
  # Remove the matching files
  for (i in 1:length(matching_files)) {
    if (file.exists(matching_files[i])) {
      file.remove(matching_files[i])
    } else {
      print("File does not exist.")
    }
  }
} else {
  print("No files found to remove.")
}

# Import CNRFC Temperature stations----
CNRFC_Stations <- read.csv(here("InputData/CNRFC_Stations.csv"))

#Set up RSelenium----
# StartDate <- as.Date("2023-04-01")
# EndDate <- as.Date("2023-05-21")

# Set up RSelenium
# Set Default download folder
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

# Set version of Chrome
# Get current version of chrome browser
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

# List the versions of chromedriver on this PC
chrome_driver_versions <- list_versions("chromedriver")

# Match drivers to version
chrome_driver_current <- chrome_browser_version %>%
  magrittr::extract(!is.na(.)) %>%
  str_replace_all(pattern = "\\.", replacement = "\\\\.") %>%
  paste0("^", .) %>%
  str_subset(string = last(chrome_driver_versions)) %>%
  as.numeric_version() %>%
  max() %>%
  as.character()

# Remove the LICENSE.chromedriver file (if it exists)
chrome_driver_dir <- paste0(app_dir("chromedriver", FALSE),
                            '/win32/',
                            chrome_driver_current)
if ('LICENSE.chromedriver' %in% list.files(chrome_driver_dir)) {
  file.remove(
    paste0(chrome_driver_dir, '/', 'LICENSE.chromedriver')
  )
}

##Open a chrome browser session with RSelenium ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever = chrome_driver_current, #set to the version on your PC that most closely matches the chrome browser version
  port = free_port(),
  extraCapabilities = eCaps
)

remDr <- rs_driver_object$client

##Navigate to CNRFC Temperature website----
for (i in 1:9){
CNRFC <- paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=", CNRFC_Stations$TempStation[i])
remDr$navigate(CNRFC)

#Select Chart Menu
ChartMenu <- remDr$findElement(using = "xpath", "//button[@aria-label = 'View chart menu']")
ChartMenu$clickElement()

##Download Temperature Data as CSVs----
CSVDownload <- remDr$findElement(using = "xpath", "//ul//li[contains(., 'CSV')]")
CSVDownload$clickElement()
}

##Navigate to CNRFC Precipitation website----
CNRFC <- paste0("https://www.cnrfc.noaa.gov/qpf.php")
remDr$navigate(CNRFC)

#Select 6-Day Basin QPF CSV
CSVDownload <- remDr$findElement(using = "link text", value  = "6-Day Basin QPF")
CSVDownload$clickElement()

#End RSelenium process
Sys.sleep(2)
remDr$closeWindow()
system("taskkill /im java.exe /f")
