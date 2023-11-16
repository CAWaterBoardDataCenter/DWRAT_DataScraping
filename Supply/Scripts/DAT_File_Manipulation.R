#Load Libraries
library(here)
library(dplyr)

#Import the necessary starter files for the DAT_File----
#DAT_Shell serves as the shell of the DAT_File, observed data (already pre-filled with PRISM and forecast data) contained in these files:
    #RAWS_Processed.csv, CIMIS_Processed.csv, Downsizer_Processed.csv 
  
Dat_Shell_PRMS <- read.csv(here("InputData/Dat_Shell_PRMS.csv"))
RAWS <- read.csv(here("ProcessedData/RAWS_Processed.csv"))
Downsizer <- read.csv(here("ProcessedData/Downsizer_Processed.csv"))
CIMIS <- read.csv(here("ProcessedData/CIMIS_Processed.csv"))

#Merge observed data sources into one dataframe
Observed <- merge(Downsizer,merge(RAWS, CIMIS, by = "Date"))

#Merge Dat_Shell_PRMS with Observed data----
#Whittle down Dat_Shell_PRMS to the timeframe of interest
Dat_Shell_PRMS2 <-filter(Dat_Shell_PRMS, Date>= StartDate$date, Date <= End_Date)

#Remove columns 1-6 of DAT_Shell_PRMS (individual datetime fields: year, month, etc) along with columns 39-60 (runoff columns)
Dat_Shell_PRMS3 <- Dat_Shell_PRMS2[,-c(1:6, 39:60)]

#Inner Join the observed dataframe to Dat_Shell2 by the Date field
Dat_Shell_PRMS4 <- inner_join(Observed, select(Dat_Shell_PRMS3, Date), by = "Date")

#Import the individual datetime fields (year, month, etc) from Dat_Shell_PRMS_2 into Dat_Shell_PRMS_4 to create Dat_Shell_PRMS_5
Dat_Shell_PRMS5 <- cbind(Dat_Shell_PRMS2[,c(1:6)], Dat_Shell_PRMS4)

#Import the runoff columns from Dat_Shell_PRMS_5 into Dat_Shell_PRMS to create Dat_Shell_PRMS5
Dat_Shell_PRMS6 <-cbind(Dat_Shell_PRMS5, cbind(Dat_Shell_PRMS2[,39:60]))

#Remove the Date field from Dat_Shell_PRMS5 because it does not exist in the final DAT_PRMS file
Dat_Shell_PRMS6$Date = NULL
col_order <- colnames(Dat_Shell_PRMS[-7]) #grab the column names from Dat_Shell_PRMS except for Date
Dat_Final_PRMS = Dat_Shell_PRMS6[,col_order]

#Write to tab-delimited text file----
write.table(Dat_Final_PRMS, here(paste0("ProcessedData/Dat_Final_PRMS","_Forecast_End_Date_", End_Date, ".txt")), sep="\t", row.names=F, quote = F)
#write.table(Dat_Final, here("ProcessedData/Dat_Final_2023-07-17.txt"), sep="\t", row.names=F, quote = F)

#Write to CSV
# write.csv(Dat_Final, here("ProcessedData/Dat_Final_2023-03-24.csv"), row.names = FALSE)

#Clean up variables----
#Add Dat_Final to vars_to_keep
vars_to_keep = c(vars_to_keep, "Dat_Final_PRMS")

# List all variables in the global environment
all_vars <- ls()

# Identify which variables to remove
vars_to_remove <- setdiff(all_vars, vars_to_keep)

# Remove variables except those in vars_to_keep
rm(list = vars_to_remove)

print("Dat_File_Manipulation.R has finished running")
