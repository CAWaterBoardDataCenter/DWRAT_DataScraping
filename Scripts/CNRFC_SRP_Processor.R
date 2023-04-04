#Last Updated By: Payman Alemi
#Last Updated On: 4/4/2023

#Load libraries
library(here)
library(dplyr)
library(tidyr)
library(stringr)

#Import CNRFC Data----
#Import CNRFC Temperature Data for LSEC1 and MWEC1
MWEC1 = read.csv(here("WebData/MWEC1_temperaturePlot.csv"))
LSEC1 = read.csv(here("WebData/LSEC1_temperaturePlot.csv"))

#Import CNRFC precipitation Data for LSEC1 and MWEC1
CNRFC_Precip_Raw <- readr::read_csv(here::here("WebData/cnrfc_qpf.csv"), skip =1)

#Reformat the CNRFC Precipitation Data----
#Remove leading and trailing whitespaces from CNRFC_Precip_Raw
CNRFC_Precip <- apply(CNRFC_Precip_Raw, 2, trimws) %>% data.frame()

#Remove rows 3 and 4 
CNRFC_Precip <- CNRFC_Precip[-c(3,4),]
CNRFC_Precip

#Rename columns 1-3
New_Names = c("Station","Forecast_Group", "Station_Name")
colnames(CNRFC_Precip)[1:3] = New_Names

#Remove rows 1 and 2
CNRFC_Precip <- CNRFC_Precip[-c(1,2),]
CNRFC_Precip

#Filter to just the LSEC1 and MWEC1 rows----
CNRFC_Precip_SRP <- filter(CNRFC_Precip, Station == "LSEC1" | Station == "MWEC1")

#Remove the Forecast Group and Station Name columns
CNRFC_Precip_SRP <- select(CNRFC_Precip_SRP, -c("Station_Name", "Forecast_Group"))

#Remove the final 3 columns so that we just have the 4/4/2023 - 4/9/2023 date range
CNRFC_Precip_SRP <- CNRFC_Precip_SRP[, 1:23]
CNRFC_Precip_SRP <- t(CNRFC_Precip_SRP) %>% data.frame()

#Set column names
colnames(CNRFC_Precip_SRP) <- CNRFC_Precip_SRP[1, ]
CNRFC_Precip_SRP <- CNRFC_Precip_SRP[-1, ] 
CNRFC_Precip_SRP$Date <-rownames(CNRFC_Precip_SRP) 
#Delete row names
row.names(CNRFC_Precip_SRP) <- NULL

#Keep only the date portion of the dates (positions 2-11)
CNRFC_Precip_SRP$Date <- substr(CNRFC_Precip_SRP$Date,2,11)
#Convert the Date column to Date format
CNRFC_Precip_SRP$Date <- as.Date(CNRFC_Precip_SRP$Date, format = "%m.%d.%Y")

#Move the Date column to the 1st position
CNRFC_Precip_SRP <- CNRFC_Precip_SRP[, c(3,1,2)]

#Convert station columns to numeric format
CNRFC_Precip_SRP[, -1] <- lapply(CNRFC_Precip_SRP[,-1], as.numeric)

#Sup precipitation datate by Station and Date
sum_SRP <- aggregate(CNRFC_Precip_SRP[,-1], by = list(Date = CNRFC_Precip_SRP$Date),FUN = sum)

#Reformat the CNRFC Temperature Data----
#Import the MWEC1 and LSEC1 CSVs
MWEC1 <- read.csv(here("WebData/MWEC1_temperaturePlot.csv"))
LSEC1 <- read.csv(here("WebData/LSEC1_temperaturePlot.csv"))

#Add Station column to each dataframe
MWEC1$Station = "MWEC1"
LSEC1$Station = "LSEC1"

#Combine the MWEC1 and LSEC1 dataframes
SRP_Temp <- rbind(MWEC1, LSEC1)

#Remove unnecessary columns from SRP_Temp
SRP_Temp <- SRP_Temp[, -c(4:9)]

#Update column names in CNRFC Temp CSVs
NewNames <- c ("Date", "TObserved", "TForecast", "Station")
colnames(SRP_Temp) <- NewNames

#Convert Date column into MM/dd/YYYY Date format
SRP_Temp$Date <- as.Date(SRP_Temp$Date)
SRP_Temp$Date <- format(SRP_Temp$Date, "%m/%d/%Y")

#Consolidate the Temperature columns
SRP_Temp <- SRP_Temp %>%
  mutate(Temp = coalesce(TObserved, TForecast)) %>%
  select(Date, Temp, Station)

#Aggregate Tmax and Tmin by Date and Station
SRP_Temp <- SRP_Temp %>%
  group_by(Date, Station) %>%
  summarise(Tmin = min(Temp), Tmax = max(Temp))

SRP_Temp

#Pivot SRP_Temp so that each station appears as a separate column
SRP_Temp <- pivot_wider(SRP_Temp, names_from = Station, values_from = c("Tmin", "Tmax"))
col_order_temp <- c("Date", "Tmax_LSEC1", "Tmax_MWEC1", "Tmin_LSEC1", "Tmin_MWEC1")
SRP_Temp <- SRP_Temp2[, col_order_temp]
FinalNames <- c("Date", "TMAX1_LSEC1", "TMAX_MWEC1", "TMIN_LSEC1", "TMIN_MWEC1")

colnames(SRP_Temp) = FinalNames

#Create Final CNRFC_SRP_Processed.csv----
#Match the date formats in SRP_Temp and sum_SRP
SRP_Temp$Date <- as.Date(SRP_Temp$Date, format ="%m/%d/%Y")
SRP_Processed <- merge(SRP_Temp, sum_SRP, by = "Date")

#Rename precipitation columns in SRP_Processed
colnames(SRP_Processed)[6:7] = c("PPT_LSEC1", "PPT_MWEC1")

#Rearrange columns in SRP_Processed
SRP_Processed <- SRP_Processed[,c(1,6,2,4,3,5,7)]

#Write CSV
write.csv(SRP_Processed, here("ProcessedData/CNRFC_SRP_Processed.csv"), row.names = FALSE)
