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
#Order columns to PRMS_Data_UPDATE convention
Dat_order = c("Year","month","day","h","m","s","DOWNSIZER_PRECIP1","DOWNSIZER_PRECIP2","DOWNSIZER_PRECIP3",
              "RAWS_PRECIP4","DOWNSIZER_PRECIP5","CIMIS_PRECIP6","RAWS_PRECIP7","DOWNSIZER_PRECIP8",
              "RAWS_PRECIP9","DOWNSIZER_PRECIP10","DOWNSIZER_PRECIP11","CIMIS_PRECIP12","DOWNSIZER_PRECIP13",
              "DOWNSIZER_PRECIP14","DOWNSIZER_PRECIP15","DOWNSIZER_TMAX1","DOWNSIZER_TMAX2","CIMIS_TMAX3",
              "CIMIS_TMAX4","RAWS_TMAX5","DOWNSIZER_TMAX6","RAWS_TMAX7","RAWS_TMAX8","DOWNSIZER_TMIN1",
              "DOWNSIZER_TMIN2","CIMIS_TMIN3","CIMIS_TMIN4","RAWS_TMIN5","DOWNSIZER_TMIN6","RAWS_TMIN7",
              "RAWS_TMIN8")
Dat_Final = Dat_Final[, Dat_order]

#Write to CSV----
write.csv(Dat_Final, here("ProcessedData/Dat_Final_2023-03-24.csv"), row.names = FALSE)
