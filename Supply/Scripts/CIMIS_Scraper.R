## load packages
library(RSelenium)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)
library(lubridate)
library(binman)

#InputData----
#Dates--adjust as needed; EndDate is always yesterday
Stations <- read.csv(here("InputData/CIMIS_Stations.csv"))
#StartDate = data.frame("April", "1", "2023", as.Date("2023-04-01"))
#EndDate = data.frame("May", "22", "2023", as.Date("2023-05-22"))

# colnames(StartDate) = c("month", "day", "year", "date")
# colnames(EndDate) = c("month", "day", "year", "date")
ndays <- as.numeric(EndDate$date)- as.numeric(StartDate$date) + 1
ndays

# Set up RSelenium ----
## Set Default download folder----
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

### List the versions of chromedriver on this PC----
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

### Remove the LICENSE.chromedriver file (if it exists)----
chrome_driver_dir <- paste0(app_dir("chromedriver", FALSE),
                            '/win32/',
                            chrome_driver_current)
if ('LICENSE.chromedriver' %in% list.files(chrome_driver_dir)) {
  file.remove(
    paste0(chrome_driver_dir, '/', 'LICENSE.chromedriver')
  )
}

## Open a chrome browser session with RSelenium ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever = chrome_driver_current, #set to the version on your PC that most closely matches the chrome browser version
  port = free_port(),
  extraCapabilities = eCaps
)

Sys.sleep(1)
remDr <- rs_driver_object$client

#Create a list to hold CIMIS dataframes
DF_List <- list()

#Navigate to CIMIS----
for (i in 1:nrow(Stations)){
#i=1
URL <- paste0("https://ipm.ucanr.edu/calludt.cgi/WXSTATIONDATA?MAP=&STN=", Stations$Alias[i])
URL <- toString(URL)
remDr$navigate(URL)

#Input Dates
StartMonth <- remDr$findElement(using = "name", value = "FROMMONTH")
StartMonth$sendKeysToElement(list(as.character(month.name[StartDate$month])))

StartDay <- remDr$findElement(using = "name", value = "FROMDAY")
Formatted_StartDay <- sprintf("%02d", StartDate$day)
StartDay$sendKeysToElement(list(as.character(Formatted_StartDay))) #ensure that the date is

StartYear <- remDr$findElement(using = "name", value = "FROMYEAR")
StartYear$sendKeysToElement(StartDate$year %>% as.character() %>% list()) #years are stored as characters by the webpage

# EndMonth <- remDr$findElement(using = "name", value = "THRUMONTH")
# EndMonth$sendKeysToElement(list(EndDate$month))
# EndDay <-remDr$findElement(using = "name", value = "THRUDAY")
# EndDay$sendKeysToElement(list(EndDate$day))
# EndYear <-remDr$findElement(using = "name", value = "THRUYEAR")
# EndYear$sendKeysToElement(list(EndDate$year))

#Use no backups
Backups <- remDr$findElement(using = "name", value = "NONE")
Backups$clickElement()

#Uncheck unnecessary checkboxes
Soil <- remDr$findElement(using = "name", value = "DT_SOIL")
Soil$clickElement()
Wind <- remDr$findElement(using = "name", value = "DT_WIND")
Wind$clickElement()
RH <- remDr$findElement(using = "name", value = "DT_RH")
RH$clickElement()
ET <- remDr$findElement(using = "name", value = "DT_ET")
ET$clickElement()
Solar <- remDr$findElement(using = "name", value = "DT_SOLAR")
Solar$clickElement()

#Metric Units
Metric <- remDr$findElement(using = "xpath", "//input[@value = 'M']")
Metric$clickElement()

#Comma delimited format
Comma <- remDr$findElement(using = "xpath", "//input[@value = 'T']")
Comma$clickElement()

#Retrieve Report
Report <-remDr$findElement(using = "xpath", "//input[@value = 'RETRIEVE DATA']")
Report$clickElement()

#Grab the Data
WeatherData <- remDr$findElement(using = "xpath", "//pre")
WeatherDataText <-WeatherData$getElementText() %>% unlist() %>% data.frame()
write.csv(x = WeatherDataText, file = "ProcessedData/CIMIS_WeatherDataText.csv")

#Manipulate CIMIS Data After Download----
  #Remove the first 2557 characters from WeatherDataText---this is just descriptive text that contains no data;
  #You should examine WeatherDataText in Notepad++ to dtermine how many characters to remove; 
  #the descriptive text has changed in size in the past

WeatherDataBody <- substring(WeatherDataText, 2558, nchar(WeatherDataText))
WeatherDataBody <-gsub("\\\n", " ", WeatherDataBody) #Remove \n from
WeatherDataBody <-gsub(" ", "", WeatherDataBody) #remove blank spaces
WeatherDataBody <- strsplit( WeatherDataBody, ",") %>% unlist %>% data.frame() #split by commas

#Force WeatherDataBody into a dataframe with 19 columns
WeatherDataBody <- split(WeatherDataBody,rep(1:(nrow(WeatherDataBody)/19),each=19)) %>% data.frame %>% t() %>% data.frame()

#Drop the last 12 columns which don't contain data
WeatherDataBody <-select(WeatherDataBody, -c(X8:X19))

#Add column headers to WeatherDataBody
Headers <- c("Station","Date","Time","Precip","type","Tmax","Tmin")
colnames(WeatherDataBody) = Headers

#Drop Time and type columns
WeatherDataBody <- select(WeatherDataBody, -c("Time", "type"))
DF_List[[i]] <- WeatherDataBody
}

#End RSelenium process
Sys.sleep(2)
remDr$closeWindow()
system("taskkill /im java.exe /f")

#Finalize CIMIS Data For Exportation To CSV----
#Name the individual RAWS dataframes in DF_List
names(DF_List) <- lapply(seq_along(DF_List),
                         function(i) names(DF_List)[[i]] = paste0("CIMIS_", Stations$Station[i]))

#Extract dataframes from DF_List
lapply(names(DF_List), function(i)
  assign(x = i, value = DF_List[[i]], .GlobalEnv))

#Finalize CIMIS Sanel Valley 106
CIMIS_Sanel_Valley_106 = `CIMIS_Sanel Valley 106`
rm(`CIMIS_Sanel Valley 106`)
CIMIS_Sanel_Valley_106$Precip = NULL

#Finalize CIMIS Santa Rosa 83
CIMIS_Santa_Rosa_83 = `CIMIS_Santa Rosa 83`
rm(`CIMIS_Santa Rosa 83`)
CIMIS_Santa_Rosa_83$Precip = NULL

#Finalize CIMIS Windsor 103
CIMIS_Windsor_103 = `CIMIS_Windsor 103`
rm(`CIMIS_Windsor 103`)
CIMIS_Windsor_103$Tmin = NULL
CIMIS_Windsor_103$Tmax = NULL

#Finalize CIMIS Hopland 85 (just consists of -999)
CIMIS_Hopland_85 = cbind.data.frame(seq(from = StartDate$date, to = EndDate$date, by = 'day'),
                                    rep ("Hopland_85", ndays), rep(-999,ndays))
colnames(CIMIS_Hopland_85) = c("Date", "Station", "Precipitation")
CIMIS_Hopland_85$Date = as.character(CIMIS_Hopland_85$Date) #convert dates to characters
CIMIS_Hopland_85$Date = gsub("-", "", CIMIS_Hopland_85$Date) # remove dashes from dates

##Consolidate the CIMIS datasets into a single dataframe----
list_df = list(CIMIS_Hopland_85, CIMIS_Sanel_Valley_106, CIMIS_Santa_Rosa_83, CIMIS_Windsor_103)
CIMIS_Processed = list_df %>% reduce(inner_join, by='Date')
# CIMIS_Names = c("Date", "Hopland", "Hopland_85_PRECIP6", "Sanel Valley",
#                 "Sanel_Valley_106_TMAX3", "Sanel_Valley_106_TMIN3", "Santa Rosa",
#                 "Santa_Rosa_83_TMAX4", "Santa_Rosa_83_TMIN4", "Windsor", "Windsor_103_PRECIP12")
CIMIS_Names = c("Date", "Hopland", "CIMIS_PRECIP6", "Sanel Valley", "CIMIS_TMAX3", "CIMIS_TMIN3", 
                "Santa Rosa", "CIMIS_TMAX4", "CIMIS_TMIN4", "Windsor", "CIMIS_PRECIP12")
colnames(CIMIS_Processed) = CIMIS_Names
colnames(CIMIS_Processed)
CIMIS_Processed = select(CIMIS_Processed, -c("Hopland", "Sanel Valley", "Santa Rosa", "Windsor"))
# col_order = c("Date", "Hopland_85_PRECIP6", "Windsor_103_PRECIP12", "Sanel_Valley_106_TMAX3",
#               "Sanel_Valley_106_TMIN3", "Santa_Rosa_83_TMAX4", "Santa_Rosa_83_TMIN4")
col_order = c("Date", "CIMIS_PRECIP6", "CIMIS_PRECIP12", "CIMIS_TMAX3",
              "CIMIS_TMAX4","CIMIS_TMIN3", "CIMIS_TMIN4")
CIMIS_Processed = CIMIS_Processed[,col_order]
CIMIS_Processed

#Replace all missing values with -999
CIMIS_Processed[CIMIS_Processed == ""] = -999


#BEFORE THIS STEP: Run PRISM_Processor.R, CNRFC_Scraper.R, & CNRFC_Processor.R----
#Replace missing values with PRISM data
#Works only if columns are same in number and order; column names don't need to match
Prism_Processed <- read.csv(here("ProcessedData/Prism_Processed.csv"))
PRISM_cols <- Prism_Processed[, c("Date", "PP_PRECIP6", "PP_PRECIP12", "PT_TMAX3", "PT_TMAX4", "PT_TMIN3", "PT_TMIN4")]

# Convert relevant columns in CIMIS_Processed to numeric
numeric_cols <- c("CIMIS_PRECIP6", "CIMIS_PRECIP12", "CIMIS_TMAX3", "CIMIS_TMAX4", "CIMIS_TMIN3", "CIMIS_TMIN4")
CIMIS_Processed[numeric_cols] <- lapply(CIMIS_Processed[numeric_cols], as.numeric)

# Assign values from PRISM_cols to CIMIS_Processed
CIMIS_Processed[CIMIS_Processed == -999] <- PRISM_cols[CIMIS_Processed == -999]

# Verify the changes
str(CIMIS_Processed)

#Change Date format to match DAT_Shell Date format
CIMIS_Processed$Date = as.character(CIMIS_Processed$Date)
CIMIS_Processed$Date = as.Date(CIMIS_Processed$Date, format = "%Y%m%d")

#Combining CIMIS data with CNRFC data
CNRFC_Processed <- read.csv(here("ProcessedData/CNRFC_Processed.csv"))
CNRFC_cols <- CNRFC_Processed[,c("Date","PRECIP6_HOPC1","PRECIP12_MWEC1",
                                 "TMAX3_CDLC1","TMIN3_CDLC1","TMAX4_LSEC1","TMIN4_LSEC1")]
#Rename CNRFC Columns to match CIMIS names to bind the datasets 
CNRFC_Names = c("Date", "CIMIS_PRECIP6", "CIMIS_PRECIP12", "CIMIS_TMAX3",
                "CIMIS_TMAX4","CIMIS_TMIN3", "CIMIS_TMIN4")
colnames(CNRFC_cols) = CNRFC_Names

#rbind() put scraped data first, CNRFC data second
CIMIS_Processed <- rbind(CIMIS_Processed,CNRFC_cols)

##Export Dataframes to CSVs----
write.csv(CIMIS_Processed, here("ProcessedData/CIMIS_Processed.csv"), row.names = FALSE)
