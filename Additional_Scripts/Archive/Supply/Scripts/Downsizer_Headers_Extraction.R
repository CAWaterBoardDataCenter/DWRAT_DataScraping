#---PURPOSE----
# Extracted the Downsizer station names from the Downzier_Headers.csv--now obsolete
# because Downsizer has been phased out since it was just a clunky front-end for accessing 
# NOAA data.

# Last Updated by: Payman Alemi on 5/22/2025

#Load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate)
        
#Extract the Downsizer Headers----
#Import Downsizer_Headers.csv (long names)
Downsizer_Headers = read.csv(here("InputData/Downsizer_Headers.csv"), header = FALSE)

#Remove all forward slashes
Downsizer_Headers = gsub(pattern = "/", replacement = "", Downsizer_Headers)
Downsizer_Headers

#Remove all substrings "\t"
Downsizer_Headers = gsub(pattern = "\t", replacement =  "", Downsizer_Headers)
Downsizer_Headers

#Remove all blank spaces
Downsizer_Headers = gsub(pattern = " ", replacement = "", Downsizer_Headers)
Downsizer_Headers %>% matrix()

#Add columns for "Year", "Month", "Day"
Date_Headers <- c("Year", "Month", "Day")
Date_Headers %>% matrix()

Downsizer_Headers <- c(Date_Headers, Downsizer_Headers)

#Save Downsizer_Headers as a single-column dataframe for later use
Downsizer_Rows <- data.frame(Downsizer_Headers)
Downsizer_Rows <- Downsizer_Rows[4:33,] %>% as.data.frame()
colnames(Downsizer_Rows) = "Downsizer"
Downsizer_Rows

#Transpose data into columns, 1 header per column
Downsizer_Headers <- t(Downsizer_Headers)
colnames(Downsizer_Headers) <- unlist(Downsizer_Headers[1,])

#Remove first row from Downsizer_Headers
Downsizer_Headers2 = data.frame(matrix(nrow = 0, ncol = length(Downsizer_Headers)))
Downsizer_Headers2

colnames(Downsizer_Headers2) = colnames(Downsizer_Headers)
Downsizer_Headers = Downsizer_Headers2
rm(Downsizer_Headers2)
Downsizer_Headers

#Add a row containing the prefixes for the Downsizer stations that the PRMS model uses
  #Not all the Downsizer stations are used; this row will help us whittle down the data to the useful stations
Downsizer_PRMS <- c('Year', 'Month', 'Day', 'DOWNSIZER_PRECIP1', 'DOWNSIZER_PRECIP2', 'DOWNSIZER_PRECIP3', 
                    'DOWNSIZER_PRECIP5', 'DOWNSIZER_PRECIP8', 'DOWNSIZER_PRECIP10', 
                    'DOWNSIZER_PRECIP11', 'DOWNSIZER_PRECIP13', 'DOWNSIZER_PRECIP14', 
                    'DOWNSIZER_PRECIP15', 'NA', 'DOWNSIZER_TMAX6', 'DOWNSIZER_TMAX2', 
                    'NA', 'NA', 'DOWNSIZER_TMAX1', 'NA', 'NA', 'NA', 'NA', 'NA', 'DOWNSIZER_TMIN6', 
                    'DOWNSIZER_TMIN2', 'NA', 'NA', 'DOWNSIZER_TMIN1', 'NA', 'NA', 'NA', 'NA')
Downsizer_PRMS = t(Downsizer_PRMS)
Downsizer_Headers[1,] =  Downsizer_PRMS #Check against StationList.xlsx for accuracy
colnames(Downsizer_Headers) = Downsizer_PRMS

#Remove 1st row of data from Downsizer_Headers
Downsizer_Headers2 = data.frame(matrix(nrow = 0, ncol = length(Downsizer_Headers)))
Downsizer_Headers2

colnames(Downsizer_Headers2) = colnames(Downsizer_Headers)
Downsizer_Headers = Downsizer_Headers2
rm(Downsizer_Headers2)
Downsizer_Headers
  
write.csv(Downsizer_Headers, here("InputData/Downsizer_Stations.csv"), row.names = FALSE)
write.csv(Downsizer_Rows, here("InputData/Downsizer_Rows.csv"), row.names = FALSE)
