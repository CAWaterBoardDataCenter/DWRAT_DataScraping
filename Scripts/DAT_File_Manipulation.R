#Import the necessary starter files for the DAT_File----
#Dat_Shell serves as the shell of the DAT_File, observed data (already prefilled with PRISM and forecast data) contained in these files:
    #Raws_Processed.csv, CIMIS_Processed.csv, Downsizer_Processed.csv 
  

DAT_Shell <- read.csv(here("InputData/Dat_Shell.csv"))
RAWS <- read.csv(here("ProcessedData/RAWS_Processed.csv"))
Downsizer <- read.csv(here("ProcessedData/Downsizer_Processed.csv"))
CIMIS <- read.csv(here("ProcessedData/CIMIS_Processed.csv"))

#Merge observed data sources into one dataframe
Observed <- merge(Downsizer,merge(RAWS, CIMIS, by = "Date"))
#ADD CNRFC Data to observed data sources----


#Compare column names in DAT_Shell to Observed dataframes
# colnames_Observed = names(Observed) %>% data.frame()
# colnames_Shell = names(DAT_Shell) %>% data.frame()
# 
# diff_col_names <- setdiff(colnames_Observed, colnames_Shell) 
# diff_col_names

#Write 
