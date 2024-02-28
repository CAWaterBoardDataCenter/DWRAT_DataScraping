# Load Libraries----
library(here)
library(dplyr)
library(data.table) #for fread function

# Import the necessary starter files for the  PRMS DAT File----
# DAT_Shell_PRMS serves as the shell of the PRMS DAT File, observed data (already pre-filled with PRISM and forecast data) contained in these files:
# RAWS_Processed.csv, CIMIS_Processed.csv, NOAA_Processed_[DATE].csv 

## Observed Data Sources ----
RAWS <- read.csv(here("ProcessedData/RAWS_Processed.csv"))

NOAA <- list.files("ProcessedData", pattern = "NOAA_API_Processed_", full.names = TRUE) %>%
  sort() %>% tail(1) %>% read.csv() # Formerly Downsizer

CIMIS <- read.csv(here("ProcessedData/CIMIS_Processed.csv"))


# Create Dat_Final_PRMS----

# Merge observed data sources into one dataframe
Observed <- full_join(NOAA, merge(RAWS, CIMIS, by = "Date"), by = "Date")
Observed$Date = as.Date(Observed$Date)

# Merge Dat_Shell_PRMS with Observed data----

# Remove columns 1-6 of DAT_Shell_PRMS (individual datetime fields: year, month, etc) along with columns 39-60 (runoff columns)
Dat_Shell_PRMS2 <- Dat_Shell_PRMS[,-c(1:6, 39:60)]
head(Dat_Shell_PRMS2)

#Inner Join the observed dataframe to Dat_Shell_PRMS2 by the Date field
Dat_Shell_PRMS3 <- inner_join(Observed, select(Dat_Shell_PRMS2, Date), by = "Date")
head(Dat_Shell_PRMS3)

# Import the individual datetime fields (year, month, etc) from Dat_Shell_PRMS
# And import the 22 runoff columns from Dat_Shell_PRMS
Dat_Shell_PRMS4 <- cbind(Dat_Shell_PRMS[(1:6)], Dat_Shell_PRMS3,Dat_Shell_PRMS[,39:60])
head(Dat_Shell_PRMS4)


#Remove the date column from Dat_Final_PRMS
Dat_Shell_PRMS4$Date = NULL

# 'anti_join' keeps all records from Dat_PRMS_Body except the ones that match in Dat_Shell_PRMS6
# 'bind_rows' appends the records from Dat_Shell_PRMS6 to the result

#Write to tab-delimited text file in the climate scenarios folder of RR PRMS----
  # Unless your RR PRMS folder is not saved to C:/RR_PRMS, this step eliminates the need 
  # to copy and paste the PRMS DAT file from GitHub to the local version of your RR PRMS model

Climate_Scenarios_Path = "C:\\RR_PRMS\\PRMS\\input\\climate_scenarios\\"
write.table(x = Dat_Final_PRMS, file = paste0(Climate_Scenarios_Path, "DAT_Final_PRMS_Forecast_End_Date_", End_Date, ".dat"),
            sep = "\t", row.names = F, quote = F, col.names = F)

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