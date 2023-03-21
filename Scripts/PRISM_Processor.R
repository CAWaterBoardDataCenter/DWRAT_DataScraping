#Load libraries----
library(here)
library(dplyr)
library(tidyverse)
library(lubridate)

#PRISM Precipitation Data Manipulation----
#Import PRISM_Precipitation.csv by skipping first 10 rows
PP <- read.csv(here("WebData/PRISM_Precipitation.csv"), skip = 10, header = T)
names(PP)[6] = "ppt"

#Remove unnecessary columns
PP = select(PP, c("Name", "ppt"))

##create separate dataframes for each Prism precipitation station----
#Create a vector consisting of each station's new name
PP_NewNames <- c("PP_PRECIP1", "PP_PRECIP2", "PP_PRECIP3", "PP_PRECIP4", "PP_PRECIP5", 
              "PP_PRECIP6", "PP_PRECIP7", "PP_PRECIP8", "PP_PRECIP9", "PP_PRECIP10", 
              "PP_PRECIP11", "PP_PRECIP12", "PP_PRECIP13", "PP_PRECIP14", "PP_PRECIP15")

PP_OldNames <- unique(PP$Name) #vector of unique Prism station names

#Replace old Prism station names with new names
PP$Name <- PP_NewNames[match(PP$Name,PP_OldNames, nomatch = 0)]

#Extract each station as a separate dataframe
for (i in unique(PP$Name)) {
  assign(i, PP %>% filter (Name == i), envir = .GlobalEnv)
}

#PRISM Temperature Data Manipulation----
#Import Prism_Temp.csv by skipping first 10 rows
PT <- read.csv(here("WebData/PRISM_Temperature.csv"), skip = 10, header = T)
names(PT)[6:7] = c("Tmin", "Tmax")

#Remove unnecessary columns
PT <- select(PT, c("Name", "Tmin", "Tmax"))

##Create separate dataframes for each Prism temperature station----
PT_NewNames <- c("Temp1", "Temp2", "Temp3", "Temp4", "Temp5", "Temp6", 
                 "Temp7", "Temp8")

PT_OldNames <-unique(PT$Name)

#Replace Old Prism station names with new names
PT$Name <- PT_NewNames[match(PT$Name,PT_OldNames, nomatch = 0)]

#Extract each station as a separate dataframe
for (i in unique(PT$Name)) {
  assign(i, PT %>% filter (Name == i), envir = .GlobalEnv)
}

#Write CSVs----
write.csv(Temp1, here("ProcessedData/PRISM_TEMP1.csv"), row.names = FALSE)
write.csv(Temp2, here("ProcessedData/PRISM_TEMP2.csv"), row.names = FALSE)
write.csv(Temp3, here("ProcessedData/PRISM_TEMP3.csv"), row.names = FALSE)
write.csv(Temp4, here("ProcessedData/PRISM_TEMP4.csv"), row.names = FALSE)
write.csv(Temp5, here("ProcessedData/PRISM_TEMP5.csv"), row.names = FALSE)
write.csv(Temp6, here("ProcessedData/PRISM_TEMP6.csv"), row.names = FALSE)
write.csv(Temp7, here("ProcessedData/PRISM_TEMP7.csv"), row.names = FALSE)
write.csv(Temp8, here("ProcessedData/PRISM_TEMP8.csv"), row.names = FALSE)
