#Last Updated By: Payman Alemi
#Last Updated On: 2/6/2024

#Load libraries----
require(tidyverse) #required for %>% operator

#RR_PRMS_Processor----
#Process the output CSV of the Russian River PRMS model
##Import RR PRMS CSV----
PRMS_Output_Folder = "C:\\RR_PRMS\\PRMS\\output"
PRMS_Output_File_Path = list.files(PRMS_Output_Folder, pattern = "inq.csv$", full.names = TRUE) %>% sort() %>% tail(1)
RR <- read.csv(PRMS_Output_File_Path)

#Add RR Headers
RR_Headers <- c("Date", seq(1:22)) %>% as.character()
colnames(RR) <- RR_Headers

##Whittle to Timeframe of Interest----
#Convert Date column to date format
RR$Date <- as.Date(RR$Date)
RR_Subset <- subset(RR, Date>= StartDate$date & Date <= End_Date)

##Unit Conversions----
#Convert Cubic Feet/Second (CFS) to Acre-Feet/Day
AFD <- 3600*24/43560 #3600 seconds/hr, 24 hrs/day, 1 acre-ft/ 43560 ft^3
RR_Subset[, 2:23] <- RR_Subset[,2:23]*AFD 

##Aggregate values by month----
RR_Subset_Summed <- RR_Subset %>%
  pivot_longer(cols = 2:23, names_to = "basin", values_to = "value") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d") %>% format("%m")) %>% 
  group_by(basin, Date) %>%
  summarise(total = sum(value), .groups = "drop") %>% 
  pivot_wider(names_from = "basin", values_from = "total")

#Reset original column order
RR_Order <- c("Date", seq(1:22))
RR_Subset_Summed <- RR_Subset_Summed[, RR_Order]

# create a vector of month values
Date <- RR_Subset_Summed$Date
# convert the month values to date objects
RR_Subset_Summed$Date <- as.Date(paste0(Date, "/01/2024"), format = "%m/%d/%Y")

#Write a csv for SRP_Post_Processing.R to combine the 2 model outputs for DWRAT
write.csv(RR_Subset_Summed, here("ProcessedData/PRMS_2024-01.csv"), row.names = FALSE)

#SRP Processor----
#This code modifies the outputs of the Santa Rosa Plains model into the format required by the flows spreadsheet
#Export CSv for use in URR DWRAT model----
#URR DWRAT model will use Basins 1-13, all derived from PRMS
URR_monthly <- RR_Subset_Summed[,1:14]
write.csv(URR_monthly, here("ProcessedData/URR_2023-10.csv"), row.names = FALSE) #rename CSV as needed to reflect current month

#Export CSV for use in LRR model
#LRR DWRAT mode
#LRR model will use Basins 14-28; Basins 14-22 are derived from PRMS, 23-28 from SRP

