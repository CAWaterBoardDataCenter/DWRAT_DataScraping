# This script replaces both Downsizer and "NOAA_Scraper.R"
# It collects climate data from the NOAA NCEI API 
# Specifically, it gets daily values for Precipitation and Temperature (Min/Max) in mm and degrees Celsius, respectively


library(tidyverse)
library(readxl)


cat("Starting 'NOAA_API_Scraper.R'...\n")


# Start by preparing the request URL


# A list of stations is needed for that
# Use "RR_PRMS_StationList (2023-09-05).xlsx"
stationList <- read_xlsx("InputData/RR_PRMS_StationList (2023-09-05).xlsx", sheet = "StationList") %>%
  filter(`Observed and PRISM Station Guide` == "DOWNSIZER") %>%
  select(...5) %>% unlist() %>% as.vector() %>%
  paste0(collapse = ",")



# Paste 'stationList' into the request URL and include other setting customizations as well
requestURL <- paste0("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries",
                     "&stations=", stationList,
                     "&startDate=", StartDate$date, "T00:00:00",
                     "&endDate=", EndDate$date, "T23:59:59", 
                     "&dataTypes=PRCP,TMAX,TMIN", "&format=csv",
                     "&options=includeAttributes:false,includeStationName:true,includeStationLocation:false",
                     "&units=metric")



# Download the file to the "WebData" folder
download.file(requestURL, paste0("WebData/NOAA_API_", EndDate$date, ".csv"), mode = "wb", quiet = TRUE)



# Now that the procedure is complete, remove the variables
remove(stationList, requestURL)


cat("'NOAA_API_Scraper.R' is done!\n")