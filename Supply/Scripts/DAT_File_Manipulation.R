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

## PRMS DAT Stuff----
Dat_Shell_PRMS <- read.csv(here("InputData/Dat_Shell_PRMS.csv"))
Dat_Fields_PRMS <- read.csv(here("InputData/Dat_Fields_PRMS.csv"))
Dat_File_PRMS_Path <- list.files("InputData", pattern = "\\.dat$", full.names = TRUE) %>% sort() %>% tail(1)
Dat_PRMS_Body = fread(input = Dat_File_PRMS_Path,

                      skip = 6,
                      header = FALSE)
Dat_PRMS_Header = read.csv(here("InputData/Dat_PRMS_header.csv"), header = FALSE)


# Create Dat_Final_PRMS----
# Add column names to DAT_PRMS_Body
colnames(Dat_PRMS_Body) = colnames(Dat_Fields_PRMS)
colnames(Dat_PRMS_Body)

# Add Date column to DAT_PRMS_Body
Dat_PRMS_Body$Date <- make_date(year = Dat_PRMS_Body$Year,
                                month = Dat_PRMS_Body$month,
                                day = Dat_PRMS_Body$day)

# Set Date as the 7th column in Dat_PRMS_Body
Dat_PRMS_Body <- Dat_PRMS_Body %>% relocate(Date, .after = s)

# Add 58 empty columns to DAT_PRMS_Header
empty_cols <- data.frame(matrix("", nrow = 6, ncol = 58))
Dat_PRMS_Header = cbind(Dat_PRMS_Header, empty_cols)
colnames(Dat_PRMS_Header) = colnames(Dat_Fields_PRMS)

# Merge observed data sources into one dataframe
Observed <- full_join(NOAA, merge(RAWS, CIMIS, by = "Date"), by = "Date")

# Merge Dat_Shell_PRMS with Observed data----
# Whittle down Dat_Shell_PRMS to the timeframe of interest
Dat_Shell_PRMS2 <- filter(Dat_Shell_PRMS, Date >= StartDate$date, Date <= End_Date)

# Remove columns 1-6 of DAT_Shell_PRMS (individual datetime fields: year, month, etc) along with columns 39-60 (runoff columns)
Dat_Shell_PRMS3 <- Dat_Shell_PRMS2[,-c(1:6, 39:60)]

#Inner Join the observed dataframe to Dat_Shell_PRMS2 by the Date field
Dat_Shell_PRMS4 <- inner_join(Observed, select(Dat_Shell_PRMS3, Date), by = "Date")

#Import the individual datetime fields (year, month, etc) from Dat_Shell_PRMS_2 into Dat_Shell_PRMS_4 to create Dat_Shell_PRMS_5
Dat_Shell_PRMS5 <- cbind(Dat_Shell_PRMS2[,c(1:6)], Dat_Shell_PRMS4)

#Import the runoff columns from Dat_Shell_PRMS_5 into Dat_Shell_PRMS to create Dat_Shell_PRMS5
Dat_Shell_PRMS6 <-cbind(Dat_Shell_PRMS5, cbind(Dat_Shell_PRMS2[,39:60]))

#Convert Date field in Dat_Shell_PRMS6 to date format
Dat_Shell_PRMS6$Date = as.Date(Dat_Shell_PRMS6$Date)

col_order <- colnames(Dat_Shell_PRMS) #grab the column names from Dat_Shell_PRMS except for Date
# Dat_Shell_PRMS6 = Dat_Shell_PRMS6 %>% # Note: Don't do this if you're going to anti_join by "Date" later
#   select(-Date)

#Convert s and runoff columns in Dat_Shell_PRMS6 to integers
Dat_Shell_PRMS6 = mutate_at(.tbl = Dat_Shell_PRMS6, 
                       .vars = c("s", paste0("Runoff", 1:22)), 
                       .funs = as.integer)

#Convert s column in Dat_Shell_PRMS6 to integer
Dat_PRMS_Body$s = as.integer(Dat_PRMS_Body$s)

#Replace corresponding portion of Dat_PRMS_Body with Dat_Shell_PRMS6
# Join Dat_PRMS_Body with the matching records in Dat_Shell_PRMS6
Dat_Final_PRMS <- Dat_PRMS_Body %>%
  anti_join(Dat_Shell_PRMS6, by = "Date") %>%
  bind_rows(Dat_Shell_PRMS6)

#Sort Dat_PRMS_Final by Date 
Dat_Final_PRMS  <- Dat_Final_PRMS[order(Dat_Final_PRMS$Date), ]

#Remove the date column from Dat_Final_PRMS
Dat_Final_PRMS$Date = NULL

#Merge Dat_Final_PRMS with the original header
Dat_Final_PRMS = rbind(Dat_PRMS_Header, Dat_Final_PRMS)

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