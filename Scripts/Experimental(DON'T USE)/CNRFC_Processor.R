library(here)
library(dplyr)
library(tidyr)
library(stringr)
#Bulk Import CNRFC Temp CSVS----
filenames <- list.files(path = here("WebData"), pattern = "temperaturePlot.*\\csv$")

setwd(here("WebData"))
for (i in filenames){
  # Read in CSV file
  CNRFC_Precip <- read.csv(i)
  
  # Extract partial filename
  partial_filename <- gsub("_temperaturePlot.csv", "", i)
  
  #Read in CSV file
  CNRFC_Precip <- read.csv(i)
  
  #Add partial filename as a new column
  CNRFC_Precip$filename <- partial_filename
  
  # Assign the modified dataframe to a variable with the same name as the file
  assign(partial_filename, CNRFC_Precip)
}

#Reformat CNRC Temp data to match DAT File format----
#Combine all the CNRFC Temp dataframes
CNRFC_Temp <- rbind(BSCC1, CDLC1, HEAC1, LAMC1, LSEC1, SKPC1, SSAC1, UKAC1)
rm(BSCC1, CDLC1, HEAC1, LAMC1, LSEC1, SKPC1, SSAC1, UKAC1)

#Update column names in CNRFC Temp cSVs
CNRFC_Temp <- CNRFC_Temp[, c(1:3,10)]
NewNames <- c("Date", "Tobserved", "TForecast", "Station")
colnames(CNRFC_Temp) <- NewNames

#Convert Date column into MM/dd/YYYY Date format
CNRFC_Temp$Date <- as.Date(CNRFC_Temp$Date)
CNRFC_Temp$Date <- format(CNRFC_Temp$Date, "%m/%d/%Y")

#Consolidate the Temperature columns
CNRFC_Temp <- CNRFC_Temp %>%
  mutate(Temp = coalesce(Tobserved, TForecast)) %>%
  select(Date, Temp, Station)

#Aggregate Tmax and Tmin by date and station
CNRFC_Temp <- CNRFC_Temp %>%
  group_by(Date, Station) %>%
  summarise(Tmin = min(Temp), Tmax = max(Temp))

CNRFC_Temp
#Export CNRFC_Temp to CSV
#write.csv(CNRFC_Temp, here("ProcessedData/CNRFC_Temp_Processed.csv"), row.names = FALSE)

#Pivot CNRFC_Temp so that each station appears as a separate column
CNRFC_Temp <- pivot_wider(CNRFC_Temp, names_from = Station, values_from = c("Tmin", "Tmax"))
CNRFC_Temp

#Rearrange CNRFC_Temp columns to match the order in the DAT_File
col_order_temp <- c("Date", "Tmax_HEAC1", "Tmax_UKAC1", "Tmax_CDLC1", 
                    "Tmax_LSEC1", "Tmax_BSCC1", "Tmax_LAMC1", "Tmax_SKPC1", 
                    "Tmax_SSAC1", "Tmin_HEAC1", "Tmin_UKAC1", "Tmin_CDLC1", 
                    "Tmin_LSEC1", "Tmin_BSCC1", "Tmin_LAMC1", "Tmin_SKPC1", "Tmin_SSAC1")

CNRFC_Temp <- CNRFC_Temp[, col_order_temp]
FinalNames <- c("Date", "TMAX1_HEAC1", "TMAX2_UKAC1", "TMAX3_CDLC1", "TMAX4_LSEC1", 
                "TMAX5_BSCC1", "TMAX6_LAMC1", "TMAX7_SKPC1", "TMAX8_SSAC1",
                "TMIN1_HEAC1", "TMIN2_UKAC1", "TMIN3_CDLC1", "TMIN4_LSEC1", "TMIN5_BSCC1", 
                "TMIN6_LAMC1", "TMIN7_SKPC1", "TMIN8_SSAC1")


colnames(CNRFC_Temp) = FinalNames
CNRFC_Temp

#Restrict CNRFC_Temp to the 3/23/2023 - 3/28/2023 date range
CNRFC_Temp <- CNRFC_Temp[6:11,] #this line may have to be manually adjusted depending on the original dataset

#CNRFC Precipitation Data Formatting
##Import raw CNRFC precipitation data----
CNRFC_Precip_Raw <- readr::read_csv(here::here("WebData/cnrfc_qpf.csv"), skip =1)

#Remove leading and trailing whitespaces from CNRFC_Precip_Raw
CNRFC_Precip <- apply(CNRFC_Precip_Raw, 2, trimws) %>% data.frame()

#Reformat the CNRFC Precipitation data----
#Remove rows 3 and 4 
CNRFC_Precip <- CNRFC_Precip[-c(3,4),]
CNRFC_Precip

#Rename columns 1-3
New_Names = c("Station","Forecast_Group", "Station_Name")
colnames(CNRFC_Precip)[1:3] = New_Names

#Remove rows 1 and 2
CNRFC_Precip <- CNRFC_Precip[-c(1,2),]
CNRFC_Precip

#Rename Forecast Group for KCV1 station to Russian
# Modify the Forecast_Group value to "Russian" for any Station containing "KCV"
CNRFC_Precip <- CNRFC_Precip %>%
  mutate(Forecast_Group = ifelse(grepl("KCVC", Station), "Russian", Forecast_Group))

#Filter to just the rows containing Russian
CNRFC_Precip_Russian <- filter(CNRFC_Precip, grepl("Russian", Forecast_Group))

#Import Final CNRFC Precip Stations
CNRFC_Precip_Stations <- read.csv(here("InputData/CNRFC_Stations.csv"))
CNRFC_Precip_Stations <-CNRFC_Precip_Stations[,1] %>% data.frame()
colnames(CNRFC_Precip_Stations) = c("Station")

#Filter CNRFC_Precip_Russian to just the final CNRFC Precip Stations
CNRFC_Precip <- inner_join(x = CNRFC_Precip_Russian, y = CNRFC_Precip_Stations, by = "Station")

#Remove the Forecast_Group and Station_Name columns
CNRFC_Precip <- select(CNRFC_Precip, -c("Station_Name", "Forecast_Group"))

#Convert precipitation from inches to mm
CNRFC_Precip[, 2:26] <- apply(CNRFC_Precip[,2:26], 2, as.numeric)
CNRFC_Precip[, 2:26] <- CNRFC_Precip[, 2:26]*25.4

#Remove the final 3 columns so that we just have the 03/23/2023 - 03/28/2023 timeframe
CNRFC_Precip <- CNRFC_Precip[, 1:23]
CNRFC_Precip <- t(CNRFC_Precip) %>% data.frame()
colnames(CNRFC_Precip) <- CNRFC_Precip[1, ]
CNRFC_Precip <- CNRFC_Precip[-1, ]
CNRFC_Precip$Date <- rownames(CNRFC_Precip)
row.names(CNRFC_Precip) <- NULL
CNRFC_Precip <- cbind(CNRFC_Precip[, 11], CNRFC_Precip[, -11])
colnames(CNRFC_Precip)[1] = "Date"
CNRFC_Precip$Date = substr(CNRFC_Precip$Date, 2,11)

#Convert Date column to Date format
CNRFC_Precip$Date <- as.Date(CNRFC_Precip$Date, format = "%m.%d.%Y")

#Convert station columns to numeric format
CNRFC_Precip[, -1] <- lapply(CNRFC_Precip[,-1], as.numeric)

#Sum precipitation data by Station and Date
sum_CNRFC_Precip <- aggregate(CNRFC_Precip[, -1], by = list(Date = CNRFC_Precip$Date), FUN = sum)

#Create empty dataframe with CNRFC columns in right order
CNRFC_Empty <- matrix(data = "", nrow= nrow(sum_CNRFC_Precip), ncol = 16) %>% data.frame()
colnames(CNRFC_Empty) = c("Date","PRECIP1_UKAC1", "PRECIP2_LAMC1", "PRECIP3_UKAC1", "PRECIP4_HOPC1", 
                          "PRECIP5_UKAC1", "PRECIP6_HOPC1", "PRECIP7_HOPC1", "PRECIP8_CDLC1", 
                          "PRECIP9_KCVC1", "PRECIP10_HEAC1", "PRECIP11_RMKC1", "PRECIP12_MWEC1", 
                          "PRECIP13_GUEC1", "PRECIP14_LSEC1", "PRECIP15_GUEC1")

#Rename CNRFC_Empty and sum_CNRFC_Precip to df_empty and df for simplicity
df_empty <- CNRFC_Empty
df <- sum_CNRFC_Precip
df_empty$Date =df$Date

##Fill in df_empty with corresponding columns from df----
#Manual Approach
 # df_empty$PRECIP1_UKAC1 = df$UKAC1
 # df_empty$PRECIP2_LAMC1 = df$LAMC1
 # df_empty$PRECIP3_UKAC1 = df$UKAC1
 # df_empty$PRECIP4_HOPC1 = df$HOPC1
 # df_empty$PRECIP5_UKAC1 = df$UKAC1
 # df_empty$PRECIP6_HOPC1 = df$HOPC1
 # df_empty$PRECIP7_HOPC1 = df$HOPC1
 # df_empty$PRECIP8_CDLC1 = df$CDLC1
 # df_empty$PRECIP9_KCVC1 = df$KCVC1
 # df_empty$PRECIP10_HEAC1 = df$HEAC1
 # df_empty$PRECIP11_RMKC1 = df$RMKC1
 # df_empty$PRECIP12_MWEC1 = df$MWEC1
 # df_empty$PRECIP13_GUEC1 = df$GUEC1
 # df_empty$PRECIP14_LSEC1 = df$LSEC1
 # df_empty$PRECIP15_GUEC1 = df$GUEC1

#Loop Approach from ChatGPT
 for (i in 2:ncol(df_empty)) {
   col_name <- colnames(df_empty)[i]
   match_str <- sub(".*_", "", col_name)
   col_indices <- grep(match_str, colnames(df))
   if (length(col_indices) > 0) {
     df_empty[, i] <- df[, col_indices[1]]
   }
 }
 
#Create Final CNRFC_Processed.csv
CNRFC_Precip_Final = df_empty
CNRFC_Temp_Final = CNRFC_Temp
CNRFC_Temp_Final$Date <- as.Date(CNRFC_Temp_Final$Date, format="%m/%d/%Y")
CNRFC_Processed = merge(CNRFC_Precip_Final, CNRFC_Temp_Final, by = "Date")
str(CNRFC_Processed)
write.csv(CNRFC_Processed, here("ProcessedData/CNRFC_Processed.csv"), row.names = FALSE)