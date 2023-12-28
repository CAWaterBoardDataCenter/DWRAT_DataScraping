#Load libraries----
library(here)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

#PRISM Precipitation Data Manipulation----
#Import PRISM_Precipitation.csv by skipping first 10 rows
ndays = 85
PP <- read.csv(here("WebData/PRISM_Precip_Raw.csv"), skip = 10, header = T)

#Rename columns as needed
names(PP)[1] = "Station"
names(PP)[6] = "ppt"

#Remove unnecessary columns
PP = select(PP, c("Station", "Date", "ppt"))
#Pivot PP so that each station becomes a separate column
PP <- pivot_wider(PP, id_cols = Date, names_from = Station, values_from = ppt)

#Create a vector consisting of each station's new name
PP_NewNames <- c("Date", "PP_PRECIP1", "PP_PRECIP2", "PP_PRECIP3", "PP_PRECIP4", "PP_PRECIP5", 
              "PP_PRECIP6", "PP_PRECIP7", "PP_PRECIP8", "PP_PRECIP9", "PP_PRECIP10", 
              "PP_PRECIP11", "PP_PRECIP12", "PP_PRECIP13", "PP_PRECIP14", "PP_PRECIP15")

PP_OldNames <- unique(PP) #vector of unique Prism station names
colnames(PP) = PP_NewNames

#PRISM Temperature Data Manipulation----
#Import Prism_Temp.csv by skipping first 10 rows
PT <- read.csv(here("WebData/PRISM_Temp_Raw.csv"), skip = 10, header = T)
names(PT)[c(1, 6, 7)] <- c("Station", "Tmin", "Tmax")

#Remove unnecessary columns
PT <- select(PT, c("Station", "Date", "Tmin", "Tmax"))
#Pivot PT so that each Station becomes a column
PT <- pivot_wider(PT, id_cols = Date, names_from = Station, values_from = c("Tmin", "Tmax"))

##Create separate dataframes for each Prism temperature station----
PT_NewNames <- c("Date", "PT_TMIN1", "PT_TMIN2", "PT_TMIN3", "PT_TMIN4", 
                 "PT_TMIN5", "PT_TMIN6", "PT_TMIN7", "PT_TMIN8", 
                 "PT_TMAX1", "PT_TMAX2", "PT_TMAX3", "PT_TMAX4", 
                 "PT_TMAX5", "PT_TMAX6", "PT_TMAX7", "PT_TMAX8")

PT_OldNames <-unique(PT)

#Replace Old Prism station names with new names
colnames(PT) = PT_NewNames

#Merge PT and PP dataframes and export to CSV----
Prism_Processed = merge(x= PT, y = PP, by = "Date")
write.csv(Prism_Processed, here("ProcessedData/Prism_Processed.csv"), row.names = FALSE)
print("Prism_Processed.R has finished running!")

