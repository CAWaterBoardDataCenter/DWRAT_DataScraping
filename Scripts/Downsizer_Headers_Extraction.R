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
Downsizer_Headers

#Rename the column to Stations
colnames(Downsizer_Headers) = c("Stations")
Downsizer_Headers

#Write to CSV
write.csv(Downsizer_Headers, here("InputData/Downsizer_Stations.csv"), row.names = FALSE)
