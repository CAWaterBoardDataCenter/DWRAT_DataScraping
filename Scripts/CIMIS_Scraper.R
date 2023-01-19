## load packages
library(RSelenium)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)
library(lubridate)

#InputData----
#Dates--adjust as needed
Stations <- read.csv(here("InputData/CIMIS_Stations.csv"))
StartDate = data.frame("December", "15", "2022", as.Date("2022-12-15"))
EndDate = data.frame("January", "10", "2023", as.Date("2023-01-10"))

colnames(StartDate) = c("month", "day", "year", "date")
colnames(EndDate) = c("month", "day", "year", "date")
ndays = seq(from = StartDate$date, to = EndDate$date, by = 'day')%>% length()
ndays

# Set up RSelenium ----
# Open a chrome browser session with RSelenium 
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever ='108.0.5359.71',
  port = free_port(),
)

eCaps <- list(
  chromeOptions =
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = gsub(pattern = '/', replacement = '\\\\', x = here("WebData")) # download.dir
    )
    )
)
remDr <- rs_driver_object$client

#Create a list to hold CIMIS dataframes
DF_List <- list()

#Navigate to CIMIS----
for (i in 1:nrow(Stations)){
#i=1
remDr$navigate(paste0("https://ipm.ucanr.edu/calludt.cgi/WXSTATIONDATA?MAP=&STN=", Stations$Alias[i]))

#Input Dates
StartMonth <- remDr$findElement(using = "name", value = "FROMMONTH")
StartMonth$sendKeysToElement(list(StartDate$month))
StartDay <- remDr$findElement(using = "name", value = "FROMDAY")
StartDay$sendKeysToElement(list(StartDate$day))
StartYear <- remDr$findElement(using = "name", value = "FROMYEAR")
StartYear$sendKeysToElement(list(StartDate$year))

EndMonth <- remDr$findElement(using = "name", value = "THRUMONTH")
EndMonth$sendKeysToElement(list(EndDate$month))
EndDay <-remDr$findElement(using = "name", value = "THRUDAY")
EndDay$sendKeysToElement(list(EndDate$day))
EndYear <-remDr$findElement(using = "name", value = "THRUYEAR")
EndYear$sendKeysToElement(list(EndDate$year))

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

#Manipulate CIMIS Data After Download----
WeatherDataBody <- substring(WeatherDataText, 2551, nchar(WeatherDataText))
WeatherDataBody <-gsub("\\\n", " ", WeatherDataBody) #Remove \n from
WeatherDataBody <-gsub(" ", "", WeatherDataBody) #remove blank spaces
WeatherDataBody <- strsplit( WeatherDataBody, ",") %>% unlist %>% data.frame() #split by commas

#Force WeatherDataBody into a dataframe with 19 columns
WeatherDataBody <- split(WeatherDataBody,rep(1:(nrow(WeatherDataBody)/19),each=19)) %>% data.frame %>% t() %>% data.frame()

#Drop the last 12 columns
WeatherDataBody <-select(WeatherDataBody, -c(X8:X19))

#Add column headers to WeatherDataBody
Headers <- c("Station","Date","Time","Precip","type","Tmax","Tmin")
colnames(WeatherDataBody) = Headers

#Drop Time and type columns
WeatherDataBody <- select(WeatherDataBody, -c("Time", "type"))
DF_List[[i]] <- WeatherDataBody
}

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
CIMIS_Hopland_85 <- cbind.data.frame(seq(from = StartDate$date, to = EndDate$date, by = 'day'), rep(-999,ndays))
colnames(CIMIS_Hopland_85) <- c("Date", "Precipitation")


##Export Dataframes to CSVs----
write.csv(CIMIS_Windsor_103, here("ProcessedData/CIMIS_PRECIP12.csv"), row.names = FALSE)
write.csv(CIMIS_Sanel_Valley_106, here("ProcessedData/CIMIS_TEMP3.csv"), row.names = FALSE)
write.csv(CIMIS_Santa_Rosa_83, here("ProcessedData/CIMIS_TEMP4.csv"), row.names = FALSE)
write.csv(CIMIS_Hopland_85, here("ProcessedData/CIMIS_PRECIP6.csv"), row.names = FALSE)