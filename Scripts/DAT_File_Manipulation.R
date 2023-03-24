#Import the necessary starter files for the DAT_File----
#Dat_Shell serves as the shell of the DAT_File, observed data (already prefilled with PRISM) contained in these files:
    #Raws_Processed.csv, CIMIS_Processed.csv, Downsizer_Processed.csv 
#Forecast data is here: CNRFC_Processed.csv
    #CIMIS_Processed

DAT_Shell <- read.csv(here("InputData/Dat_Shell.csv"))
CNRFC <- read.csv(here("ProcessedData/CNRFC_Processed.csv"))
RAWS <- read.csv(here("ProcessedData/RAWS_Processed.csv"))
Downsizer <- read.csv(here("ProcessedData/Downsizer_Processed.csv"))
CIMIS <- read.csv(here("ProcessedData/CIMIS_Processed.csv"))