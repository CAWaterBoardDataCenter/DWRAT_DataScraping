#SCRIPT LAST UPDATED:
    #BY: Payman Alemi
    #ON: 5/22/2023

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
library(binman)

#Set up RSelenium----
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

###List the versions of chromedriver on this PC----
chrome_driver_versions <- list_versions("chromedriver")

###Match drivers to version----
chrome_driver_current <- chrome_browser_version %>%
  magrittr::extract(!is.na(.)) %>%
  str_replace_all(pattern = "\\.", replacement = "\\\\.") %>%
  paste0("^", .) %>%
  str_subset(string = last(chrome_driver_versions)) %>%
  as.numeric_version() %>%
  max() %>%
  as.character()

### Remove the LICENSE.chromedriver file (if it exists)----
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

Sys.sleep(1)
remDr <- rs_driver_object$client

#Input Data----
#Import RAWS stations
Stations = read.csv(here("InputData/Raws_Stations.csv"))

#Define Timeframe for which you're downloading observed data
# StartDate = data.frame("April", "01", "2023", as.Date("2023-04-01"))
# EndDate = data.frame("May", "21", "2023", as.Date("2023-05-21"))

# colnames(StartDate) = c("month", "day", "year", "date")
# colnames(EndDate) = c("month", "day", "year", "date")
ndays = seq(from = StartDate$date, to = EndDate$date, by = 'day')%>% length()
ndays

#Scrape RAWS Data----
#Create list to hold RAWS dataframes
DF_List <- list()

#Navigate to RAWS website----
for (i in 1:nrow(Stations)){
#i = 1
remDr$navigate(paste0("https://wrcc.dri.edu/cgi-bin/rawMAIN.pl?ca", Stations$Station[i]))

#Switch to Left Frame named "List"
ListFrame <- remDr$findElement(using = "name", value = "List")
remDr$switchToFrame(ListFrame)

#Select Daily Summary Time Series Link
Link1 <- remDr$findElement(using = "link text", value = "Daily Summary Time Series")
Link1$clickElement()

#Switch to Right Frame named "Graph"
remDr$switchToFrame(NA)
GraphFrame <- remDr$findElement(using = "name", value = "Graph")
remDr$switchToFrame(GraphFrame)

# #Set the Starting Date
StartMonth <- remDr$findElement(using = "name", value = "smon")
StartMonth$sendKeysToElement(list(month.name[StartDate$month]))

StartDay <- remDr$findElement(using = "name", value = "sday")
Formatted_StartDay <- sprintf("%02d", StartDate$day)
StartDay$sendKeysToElement(list(Formatted_StartDay))

StartYear <- remDr$findElement(using = "name", value = "syea")
StartYear$sendKeysToElement(list(as.character(StartDate$year)))

#Set the Ending Date
EndMonth <- remDr$findElement(using = "name", value = "emon")
EndMonth$sendKeysToElement(list(month.name[EndDate$month]))

EndDay <- remDr$findElement(using = "name", value = "eday")
Formatted_EndDay <- sprintf("%02d", EndDate$day)
EndDay$sendKeysToElement(list(Formatted_EndDay))

EndYear <- remDr$findElement(using = "name", value = "eyea")
EndYear$sendKeysToElement(list(as.character(EndDate$year)))

#Uncheck "Elements marked with *" box
Elements <-remDr$findElement(using = "name", value = "qBasic")
Elements$clickElement()

#Select Air Temperature and Precipitation
Temp <- remDr$findElement(using = "name", value = "qAT")
Temp$clickElement()

Precip <- remDr$findElement(using = "name", value = "qPR")
Precip$clickElement()

#Select Metric Output units
Units <- remDr$findElement(using = "xpath", "//input[@value = 'M']")
Units$clickElement()

#Select HTML Output
Format <-remDr$findElement(using = "xpath", "//input[@value = 'H']")
Format$clickElement()

#Select the Data Summarization requirements
Summarization <-remDr$findElement(using = "xpath", "//input[@value = 'C']")
Summarization$clickElement()

#Apply physical limits QC to data
Limits <-remDr$findElement(using = "xpath", "//input[@value = 'Y' and @name = 'qc']")
Limits$clickElement()

#Represent Missing data as -999
Missing<-remDr$findElement(using = "name", value = "miss")
Missing$sendKeysToElement(list("-999"))

#Don't include valid observations for each element
Validity <-remDr$findElement(using = "xpath", "//input[@value = 'N' and @name = 'obs']")
Validity$clickElement()

#Click on Submit info button
Submit <-remDr$findElement(using = "xpath", "//input[@value = 'Submit Info']")
Submit$clickElement()

#Switch to Graph frame (the right hand frame)
remDr$switchToFrame(NA)
GraphFrame <- remDr$findElement(using = "name", value = "Graph")
remDr$switchToFrame(GraphFrame)

#Scrape from Graph Frame using RSelenium
WeatherData <- remDr$findElement(using = "xpath", "//table/tbody")
WeatherDataText <- WeatherData$getElementText() %>% unlist() %>% data.frame()

#Extract Raw Data from WeatherDataText----
nchar(WeatherDataText)
Headers <-substring(WeatherDataText,1,161)
Headers
WeatherDataBody <-substring(WeatherDataText, 162,nchar(WeatherDataText))
WeatherDataBody <- gsub("\\\n", " ", WeatherDataBody)
WeatherDataBody <- strsplit( WeatherDataBody, " ") %>% unlist %>% data.frame()

#Force WeatherDataBody into dataframe with 8 columns
WeatherDataBody <- split(WeatherDataBody,rep(1:ndays,each=8)) %>% 
  data.frame %>%
  t() %>% 
  data.frame()
DF_List[[i]] <- WeatherDataBody 
}

#Name the individual RAWS dataframes in the list
names(DF_List) <- lapply(seq_along(DF_List),
                         function(i) names(DF_List)[[i]] = paste0("RAWS_", Stations$Station[i]))

#Set column names
RAWS_Names <- c("Date", "Year", "Day_Of_Year", "Day_Of_Run", "Tavg", "Tmax", "Tmin", "Precipitation")

#Apply column names to all dataframes
for (DF in seq_along(DF_List)){
  colnames(DF_List[[DF]]) <- RAWS_Names
}
#Finalize WeatherDataBody for Exportation----
#Extract dataframes from DF_List
lapply(names(DF_List),function(x) 
  assign(x,DF_List[[x]],.GlobalEnv))
ls()

#Drop Temp columns from Boonville RAWS (CBOO)
RAWS_CBOO <- select(RAWS_CBOO, -c(Tavg: Tmin)) 
#Drop Precipitation column from  Santa Rosa RAWS station (CSRS)
RAWS_CSRS$Precipitation <- NULL

#Add prefixes to the precipitation and temperature fields
names(RAWS_CBOO)[5] = "RAWS_PRECIP7"
names(RAWS_CLYO)[6] = "RAWS_TMAX7"
names(RAWS_CLYO)[7] = "RAWS_TMIN7"
names(RAWS_CLYO)[8] = "RAWS_PRECIP4"
names(RAWS_CHAW)[6] = "RAWS_TMAX5"
names(RAWS_CHAW)[7] = "RAWS_TMIN5"
names(RAWS_CHAW)[8] = "RAWS_PRECIP9"
names(RAWS_CSRS)[6] = "RAWS_TMAX8"
names(RAWS_CSRS)[7] = "RAWS_TMIN8"

#Consolidate all RAWS dataframes into 1 dataframe----
list_df = list(RAWS_CLYO, RAWS_CHAW, RAWS_CSRS, RAWS_CBOO)
RAWS_Processed = list_df %>% reduce(inner_join, by = "Date")

#Remove extra columns
RAWS_Processed = select(RAWS_Processed,c("Date", "RAWS_TMAX7", "RAWS_TMIN7", 
                                         "RAWS_PRECIP4", "RAWS_TMAX5", "RAWS_TMIN5", 
                                         "RAWS_PRECIP9", "RAWS_TMAX8", "RAWS_TMIN8", 
                                         "RAWS_PRECIP7"))
#Rearrange columns
col_order = c("Date", "RAWS_PRECIP4", "RAWS_PRECIP7", 
              "RAWS_PRECIP9", "RAWS_TMAX5", "RAWS_TMAX7", 
              "RAWS_TMAX8", "RAWS_TMIN5", "RAWS_TMIN7", "RAWS_TMIN8")
RAWS_Processed = RAWS_Processed[, col_order]

#End RSelenium process
Sys.sleep(2)
remDr$closeWindow()
system("taskkill /im java.exe /f")

#Replace missing values with PRISM data----
#Only run lines 217- if you already have the RAWS_Processed.csv downloaded; 
#if so, you don't need to run lines 1-215 again
#Import PRISM_Processed
Prism_Processed = read.csv(here("ProcessedData/Prism_Processed.csv"))
#Subset PP to just the RAWS columns
#Works only if columns are same in number and order; column names don't need to match
PRISM_cols <- Prism_Processed[c("Date", "PP_PRECIP4", "PP_PRECIP7", 
                                   "PP_PRECIP9", "PT_TMAX5", "PT_TMAX7", 
                                   "PT_TMAX8", "PT_TMIN5", "PT_TMIN7", "PT_TMIN8")]
RAWS_Processed[RAWS_Processed == -999] <- PRISM_cols[RAWS_Processed == -999]

#Combining RAWS data with CNRFC data----

RAWS_Processed$Date = as.Date(RAWS_Processed$Date, format = "%m/%d/%Y")
CNRFC_Processed <- read.csv(here("ProcessedData/CNRFC_Processed.csv"))

CNRFC_cols <- CNRFC_Processed[,c("Date","PRECIP4_HOPC1","PRECIP7_HOPC1", "PRECIP9_KCVC1",
                                 "TMAX5_BSCC1", "TMAX7_SKPC1", "TMAX8_SSAC1", "TMIN5_BSCC1",
                                 "TMIN7_SKPC1", "TMIN8_SSAC1")]

#Rename CNRFC Columns to match RAWS names to bind the datasets 
CNRFC_Names = c("Date", "RAWS_PRECIP4", "RAWS_PRECIP7", "RAWS_PRECIP9",
                "RAWS_TMAX5", "RAWS_TMAX7", "RAWS_TMAX8", "RAWS_TMIN5", "RAWS_TMIN7", "RAWS_TMIN8")
colnames(CNRFC_cols) = CNRFC_Names

#rbind() put scraped data first, CNRFC data second
RAWS_Processed <- rbind(RAWS_Processed,CNRFC_cols)

#Write RAWS_Final to RAWS_Processed.csv----
write.csv(RAWS_Processed, here("ProcessedData/RAWS_Processed.csv"), row.names = FALSE)
