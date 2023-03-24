#Load Libraries
library(here)
library(dplyr)

#Import the necessary starter files for the DAT_File----
#Dat_Shell serves as the shell of the DAT_File, observed data (already prefilled with PRISM and forecast data) contained in these files:
    #Raws_Processed.csv, CIMIS_Processed.csv, Downsizer_Processed.csv 
  
Dat_Shell <- read.csv(here("InputData/Dat_Shell.csv"))
RAWS <- read.csv(here("ProcessedData/RAWS_Processed.csv"))
Downsizer <- read.csv(here("ProcessedData/Downsizer_Processed.csv"))
CIMIS <- read.csv(here("ProcessedData/CIMIS_Processed.csv"))

#Merge observed data sources into one dataframe
Observed <- merge(Downsizer,merge(RAWS, CIMIS, by = "Date"))

#Merge Dat_Shell with Observed data
Dat_Shell2 <- Dat_Shell[,-c(1:6, 39:60)]
Dat_Shell3 <- inner_join(Observed, select(Dat_Shell2, Date), by = "Date")
Dat_Shell4 <- cbind(Dat_Shell[,c(1:6)], Dat_Shell3)
Dat_Shell5 <-cbind(Dat_Shell4, cbind(Dat_Shell[,39:60]))
Dat_Shell5$Date = NULL
Dat_Final = Dat_Shell5
#Write to CSV----
write.csv(Dat_Final, here("ProcessedData/Dat_Final_2023-03-24.csv"), row.names = FALSE)