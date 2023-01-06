#Load libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(here)
library(lubridate)

#Import Downsizer Data
Downsizer = read.csv(file = here("InputData/2023.1.05.csv"))
Headers = read.csv(file = here("InputData/Downsizer_Stations.csv"))

#Account for timeframe of interest
StartDate = data.frame("December", "01", "2022", as.Date("2022-12-01"))
EndDate = data.frame("January", "04", "2023", as.Date("2023-01-04"))
colnames(StartDate) = c("month", "day", "year", "date")
colnames(EndDate) = c("month", "day", "year", "date")
ndays = seq(from = StartDate$date, to = EndDate$date, by = 'day')%>% length()
ndays
TimeFrame = seq(from = StartDate$date, to = EndDate$date, by = 'day')


#Extract the weather data from Downsizer----
#Drop the first 42 rows of Downsizer
Downsizer = tail(Downsizer, nrow(Downsizer)-42)
colnames(Downsizer) = "Downsizer"

Downsizer[1,]
