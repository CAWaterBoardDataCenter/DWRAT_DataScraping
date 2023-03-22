library(here)
library(dplyr)
library(tidyr)
#Bulk Import CNRFC Temp CSVS----
filenames <- list.files(path = here("WebData"), pattern = "temperaturePlot.*\\csv$")

setwd(here("WebData"))
for (i in filenames){
  # Read in CSV file
  df <- read.csv(i)
  
  # Extract partial filename
  partial_filename <- gsub("_temperaturePlot.csv", "", i)
  
  #Read in CSV file
  df <- read.csv(i)
  
  #Add partial filename as a new column
  df$filename <- partial_filename
  
  # Assign the modified dataframe to a variable with the same name as the file
  assign(partial_filename, df)
}

#Reformat CNRC Temp data to match DAT File format----
#Combine all the CNRFC Temp dataframes
CNRFC_Temp <- rbind(BSCC1, CDLC1, HEAC1, LAMC1, LSEC1, SKPC1, SSAC1, UKAC1)
rm(BSCC1, CDLC1, HEAC1, LAMC1, LSEC1, SKPC1, SSAC1, UKAC1)

#Update column names in CNRFC Temp cSVs
CNRFC_Temp2 <- CNRFC_Temp[, c(1:3,10)]
NewNames <- c("Date", "Tobserved", "TForecast", "Station")
colnames(CNRFC_Temp2) <- NewNames

#Convert Date column into MM/dd/YYYY Date format
CNRFC_Temp2$Date <- as.Date(CNRFC_Temp2$Date)
CNRFC_Temp2$Date <- format(CNRFC_Temp2$Date, "%m/%d/%Y")

#Consolidate the Temperature columns
CNRFC_Temp2 <- CNRFC_Temp2 %>%
  mutate(Temp = coalesce(Tobserved, TForecast)) %>%
  select(Date, Temp, Station)

#Aggregate Tmax and Tmin by date and station
CNRFC_Temp3 <- CNRFC_Temp2 %>%
  group_by(Date, Station) %>%
  summarise(Tmin = min(Temp), Tmax = max(Temp))

CNRFC_Temp3

#Pivot CNRFC_Temp3 so that each station appears as a separate column
CNRFC_Temp4 <- pivot_wider(CNRFC_Temp3, names_from = Station, values_from = c("Tmin", "Tmax"))
CNRFC_Temp4

#Rearrange CNRFC_Temp4 columns to match the order in the DAT_File
col_order_temp <- c("Date", "Tmax_HEAC1", "Tmax_UKAC1", "Tmax_CDLC1", 
                    "Tmax_LSEC1", "Tmax_BSCC1", "Tmax_LAMC1", "Tmax_SKPC1", 
                    "Tmax_SSAC1", "Tmin_HEAC1", "Tmin_UKAC1", "Tmin_CDLC1", 
                    "Tmin_LSEC1", "Tmin_BSCC1", "Tmin_LAMC1", "Tmin_SKPC1", "Tmin_SSAC1")

CNRFC_Temp4 <- CNRFC_Temp4[, col_order_temp]
FinalNames <- c("Date", "HEAC1_TMAX1", "UKAC1_TMAX2", "CDLC1_TMAX3", "LSEC1_TMAX4", 
                "BSCC1_TMAX5", "LAMC1_TMAX6", "SKPC1_TMAX7", "SSAC1_TMAX8", 
                "HEAC1_TMIN1", "UKAC1_TMIN2", "CDLC1_TMIN3", "LSEC1_TMIN4", 
                "BSCC1_TMIN5", "LAMC1_TMIN6", "SKPC1_TMIN7", "SSAC1_TMIN8")

colnames(CNRFC_Temp4) = FinalNames
CNRFC_Temp4

