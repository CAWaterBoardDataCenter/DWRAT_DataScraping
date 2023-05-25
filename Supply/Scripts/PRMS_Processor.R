#Last Updated By: Payman Alemi
#Last Updated On: 3/28/2023

#Load libraries----
library(here)
library(dplyr) #required for %>% operator
library(tidyr)

#RR_PRMS_Processor----
#Process the output CSV of the Russian River PRMS model
##Import RR PRMS CSV----
RR <- read.csv(here("InputData/PRMS_MK_2023-04-06_inq.csv"))

#Add RR Headers
RR_Headers <- c("Date", seq(1:22)) %>% as.character()
colnames(RR) <- RR_Headers

##Whittle to Timeframe of Interest----
#Convert Date column to date format
RR$Date <- as.Date(RR$Date)
RR_Subset <- subset(RR, Date>= "2023-04-01" & Date <= "2023-04-30")

#Write RR_Subset to ProcessedData Folder
write.csv(RR_Subset, here("ProcessedData/RR_PRMS_2023-04.csv"), row.names = FALSE)

##Unit Conversions----
#Convert Cubic Feet/Second (CFS) to Acre-Feet/Day
AFD <- 3600*24/43560 #3600 seconds/hr, 24 hrs/day, 1 acre-ft/ 43560 ft^3
RR_Subset[, 2:23] <- RR_Subset[,2:23]*AFD 

##Aggregate values by month----
RR_Subset_Summed <- RR_Subset %>%
  pivot_longer(cols = 2:23, names_to = "basin", values_to = "value") %>% 
  mutate(month = as.Date(Date, format = "%Y-%m-%d") %>% format("%m")) %>% 
  group_by(basin, month) %>%
  summarise(total = sum(value)) %>% 
  pivot_wider(names_from = "basin", values_from = "total")

#Reset original column order
RR_Order <- c("month", seq(1:22))
RR_Subset_Summed <- RR_Subset_Summed[, RR_Order]

#Set first cell equal to ModelMonth
RR_Subset_Summed[1,1] ="04/01/2023"

#SRP Processor----
#This code modifies the outputs of the Santa Rosa Plains model into the format required by the flows spreadsheet
#Export CSv for use in URR DWRAT model----
#URR DWRAT model will use Basins 1-13, all derived from PRMS
URR_monthly <- RR_Subset_Summed[,1:14]
write.csv(URR_monthly, here("ProcessedData/URR_2023-04.csv"), row.names = FALSE) #rename CSV as needed to reflect current month

#Export CSV for use in LRR model
#LRR DWRAT mode
#LRR model will use Basins 14-28; Basins 14-22 are derived from PRMS, 23-28 from SRP

