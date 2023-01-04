#Set up RSelenium ----
## load packages ----
library(RSelenium)
library(rvest)
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)

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
default_folder = here("WebData")
## Open browser ----
rs_driver_object <-rsDriver(
  browser = 'chrome',
  chromever ='108.0.5359.71',
  port = free_port(),
)

remDr <- rs_driver_object$client

#Input Data----

#Import RAWS stations
Stations = read.csv("InputData/Raws_Stations.csv")

#Define Dates
StartDate = data.frame("December", "01", "2022", as.Date("2022-12-01"))
EndDate = data.frame("January", "03", "2023", as.Date("2023-01-03"))

colnames(StartDate) = c("month", "day", "year", "date")
colnames(EndDate) = c("month", "day", "year", "date")
ndays = seq(from = StartDate$date, to = EndDate$date, by = 'day')%>% length()
ndays

#Scrape RAWS Data----

#Create list to hold RAWS dataframes
DF_List <- list()

#Navigate to RAWS website
for (i in 1:nrow(Stations)){
#i = 1
remDr$open()
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
StartMonth$sendKeysToElement(list(StartDate$month))
# 
StartDay <- remDr$findElement(using = "name", value = "sday")
StartDay$sendKeysToElement(list(StartDate$day))
# 
StartYear <- remDr$findElement(using = "name", value = "syea")
StartYear$sendKeysToElement(list(StartDate$year))

#Set the Ending Date
EndMonth <- remDr$findElement(using = "name", value = "emon")
EndMonth$sendKeysToElement(list(EndDate$month))
# 
EndDay <- remDr$findElement(using = "name", value = "eday")
EndDay$sendKeysToElement(list(EndDate$day))
# 
EndYear <- remDr$findElement(using = "name", value = "eyea")
EndYear$sendKeysToElement(list(EndDate$year))

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

#Finalize WeatherDataBody for Exportation----
#Force WeatherDataBody into dataframe with 8 columns
WeatherDataBody <- split(WeatherDataBody,rep(1:ndays,each=8)) %>% data.frame %>% t() %>% data.frame()

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

#Extract dataframes from DF_List
lapply(names(DF_List),function(x) 
  assign(x,DF_List[[x]],.GlobalEnv))
ls()

#Drop Temp columns from Boonville RAWS (CBOO)
RAWS_CBOO <- select(RAWS_CBOO, -c(Tavg: Tmin)) 
#Drop Precipitation column from  Santa Rosa RAWS station (CSRS)
RAWS_CSRS$Precipitation <- NULL

#Create RAWS Precipitation only dataframes for CHAW and CLYO
RAWS_CHAW_Precip <- select(RAWS_CHAW, -c(Tavg:Tmin))
RAWS_CLYO_Precip <- select(RAWS_CLYO, -c(Tavg:Tmin))

#Create RAWS Temperature only dataframes fro CHAW and CLYO
RAWS_CHAW_Temp <- select(RAWS_CHAW, -c(Precipitation))
RAWS_CLYO_Temp <- select(RAWS_CLYO, -c(Precipitation))

#Write all RAWS dataframes to CSVs----
write.csv(RAWS_CHAW_Precip, here("ProcessedData/RAWS_PRECIP9.csv"), row.names = FALSE)
write.csv(RAWS_CLYO_Precip, here("ProcessedData/RAWS_PRECIP4.csv"), row.names = FALSE)
write.csv(RAWS_CHAW_Temp, here("ProcessedData/RAWS_TEMP5.csv"), row.names = FALSE)
write.csv(RAWS_CLYO_Temp, here("ProcessedData/RAWS_TEMP7.csv"), row.names = FALSE)
write.csv(RAWS_CBOO, here("ProcessedData/RAWS_PRECIP7.csv"), row.names = FALSE)
write.csv(RAWS_CSRS, here("ProcessedData/RAWS_TEMP8.csv"), row.names = FALSE)

