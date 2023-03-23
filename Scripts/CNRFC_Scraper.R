#load packages ----
library(RSelenium)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)

#Find and remove previously downloaded CNRFC Data----
#Find all CSVS containing "temperaturePlot" in the filename
matching_files <- list.files(path = here("WebData"), pattern = "temperaturePlot.*\\.csv")

#remove the matching files
file.remove(here("WebData", matching_files))

#Find and remove the cnrfc_qpf.csv
file.remove(here("WebData/cnrfc_qpf.csv"))

# Import CNRFC Temperature stations----
CNRFC_Stations <- read.csv(here("InputData/CNRFC_Stations.csv"))

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
#   

## Find active versions of chrome on PC ----
binman::list_versions('chromedriver') 

## Open a chrome browser session with RSelenium ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever ='111.0.5563.64', #set to the version on your PC that most closely matches the chrome browser version
  port = free_port(),
  extraCapabilities = eCaps
)

remDr <- rs_driver_object$client

## Navigate to CNRFC Temperature website
for (i in 1:8) {
  CNRFC <- paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=", CNRFC_Stations$TempStation[i])
  remDr$navigate(CNRFC)
  
  # Select Chart Menu
  ChartMenu <- remDr$findElement(using = "xpath", "//button[@aria-label = 'View chart menu']")
  ChartMenu$clickElement()
  
  # Download Temperature Data as CSVs
  CSVDownload <- remDr$findElement(using = "xpath", "//ul//li[contains(., 'CSV')]")
  CSVDownload$clickElement()
}

##Navigate to CNRFC Precipitation website
CNRFC <- paste0("https://www.cnrfc.noaa.gov/qpf.php")
remDr$navigate(CNRFC)

#Select 6-Day Basin QPF CSV
CSVDownload <- remDr$findElement(using = "link text", value  = "6-Day Basin QPF")
CSVDownload$clickElement()

# ##Navigate to CNRFC Temperature website----
# for (i in 1:nrow(CNRFC_Stations)){
#   CNRFC <- paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=", CNRFC_Stations$TempStation[i])
#   remDr$navigate(CNRFC)
#   
#   #Select Chart Menu
#   ChartMenu <- remDr$findElement(using = "xpath", "//button[@aria-label = 'View chart menu']")
#   ChartMenu$clickElement()
#   
#   ##Download Temperature Data as CSVs----
#   CSVDownload <- remDr$findElement(using = "xpath", "//ul//li[contains(., 'CSV')]")
#   CSVDownload$clickElement()
# }
