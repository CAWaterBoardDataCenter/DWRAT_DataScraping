#Load libraries----
library(tidyverse)
library(here)

#PRISM Precipitation Data Manipulation----
ndays = 85
PSRP <- read.csv(here("WebData/PRISM_SRP_Raw.csv"), skip = 10, header = T)

#Rename columns as needed
names(PSRP)[c(1, 6, 7, 8)] <- c("Station", "ppt", "Tmin", "Tmax")

#Remove unnecessary columns
PSRP = select(PSRP, c("Station", "Date", "ppt", "Tmin", "Tmax"))
#Pivot PP so that each station becomes a separate column
PSRP <- pivot_wider(PSRP, id_cols = Date, names_from = Station, 
                    values_from = c("ppt", "Tmin", "Tmax"))

#Rename the columns based on station
PSRP_NewNames <- c("Date","CIMIS_083_ppt","CIMIS_103_ppt","CIMIS_083_tmin",
                   "CIMIS_103_tmin","CIMIS_083_tmax","CIMIS_103_tmax")
colnames(PSRP) = PSRP_NewNames

#Reorder the columns for transfer to SRP update spreadsheet
PSRP <- PSRP[,c("Date", "CIMIS_083_ppt", "CIMIS_083_tmin", "CIMIS_083_tmax", 
                "CIMIS_103_ppt", "CIMIS_103_tmin", "CIMIS_103_tmax")]

#BEFORE THIS STEP run CNRFC_SRP_Processor
#Combine PRISM and CNRFC data for SRP model
SRP_Processed <- read.csv(here("ProcessedData/CNRFC_SRP_Processed.csv"))
PSRP$Date <- as.Date(PSRP$Date)
PSRP <- rbind(PSRP,SRP_Processed)

#Export to CSV----
write.csv(PSRP, here("ProcessedData/PRISM_SRP_Processed.csv"), row.names = FALSE)
