#THIS SCRIPT IS EXPERIMENTAL AND DOES NOT FULLY WORK YET#
#LAST UPDATED BY: PAYMAN ALEMI ON 1/11/2023

## load packages
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)
library(lubridate)

#Import Data
Stations = read.csv(here("InputData/CNRFC_Stations.csv"))
CNRFC_precip = read.csv(here("WebData/cnrfc_qpf.csv"), header = FALSE)
CNRFC_precip$V28 = NULL
nchar(CNRFC_precip[2,4])

#Create data frames for start dates and end dates
StartDates = mdy_hm(CNRFC_precip[2,4:27]) %>% data.frame()
colnames(StartDates) = "Date"
StartDates = date(StartDates$Date) %>% data.frame()
EndDates = mdy_hm(CNRFC_precip[3,4:27]) %>% data.frame()
colnames(EndDates) = "Date"
EndDates = date(EndDates$Date) %>% data.frame()

#Filter CNRFC_precipitation to just the stations we care about
CNRFC_precip_data = inner_join(x = CNRFC_precip,
                               y = Stations,
                               by = c("V1" = "PrecipStation"))

#Drop unnecessary columns
CNRFC_precip_data$V2 = NULL
CNRFC_precip_data$V3 = NULL
CNRFC_precip_data$TempStation = NULL

#Convert precipitation from inches to mm
CNRFC_precip_data[,2:25]= sapply(CNRFC_precip_data[,2:25], as.numeric)
CNRFC_precip_data[,2:25] = CNRFC_precip_data[,2:25]*25.4

#Transpose CNRFC_precip_data
CNRFC_precip_data = t(CNRFC_precip_data) %>% data.frame()
#Replace column names
colnames(CNRFC_precip_data) = CNRFC_precip_data[1,]
#Delete 1st row of data
#Add dates in 1st column
#Sum precipitation by date for each station


#Write to CSV
write.csv(CNRFC_precip_data, here("ProcessedData/CNRFC_Precip_Data.csv"), row.names= FALSE)
