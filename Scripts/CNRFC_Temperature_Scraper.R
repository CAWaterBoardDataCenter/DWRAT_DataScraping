# Set up RSelenium -------------------------------------------------------
## load packages ----
library(RSelenium)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)

# Import CNRFC Temperature stations----
CNRFC_Stations <- read.csv("InputData/CNRFC_Stations.csv")

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
## Open a chrome browser session with RSelenium ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever ='108.0.5359.71',
  port = free_port(),
  extraCapabilities = eCaps
)

remDr <- rs_driver_object$client

#Navigate to CNRFC website
for (i in 1:nrow(CNRFC_Stations)){
CNRFC <- paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=", CNRFC_Stations$TempStation[i])
remDr$navigate(CNRFC)

#Select Chart Menu
ChartMenu <- remDr$findElement(using = "xpath", "//button[@aria-label = 'View chart menu']")
ChartMenu$clickElement()

#Download as CSV
CSVDownload <- remDr$findElement(using = "xpath", "//ul//li[contains(., 'CSV')]")
CSVDownload$clickElement()
}