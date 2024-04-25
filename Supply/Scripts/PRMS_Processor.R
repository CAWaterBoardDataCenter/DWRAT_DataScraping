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
RR$Date <- as.Date(x = RR$Date, format = "%Y-%m-%d")
RR_Subset <- subset(RR, Date>= Hydro_StartDate & Date <= Hydro_EndDate)

##Unit Conversions----
#Convert Cubic Feet/Second (CFS) to Acre-Feet/Day
AFD <- 3600*24/43560 #3600 seconds/hr, 24 hrs/day, 1 acre-ft/ 43560 ft^3
RR_Subset[, 2:23] <- RR_Subset[,2:23]*AFD 

#Add Year, Month, and Year-
RR_Subset$Year <- as.numeric(format(RR_Subset$Date, "%Y"))
RR_Subset$Month <- format(RR_Subset$Date, "%m")
RR_Subset$Year_Month <- format(RR_Subset$Date, "%Y-%m")

#Add Year, Month, and Year-
##Aggregate values by month----
RR_Subset_Summed <- RR_Subset %>%
  group_by(Year_Month) %>%
  summarise(across(-c(Date, Month, Year), sum))

#Reset original column order
colnames(RR_Subset_Summed) = RR_Headers

# convert the month values to date objects
RR_Subset_Summed$Date <- as.Date(paste0(RR_Subset_Summed$Date, "-01"), format = "%Y-%m-%d")

#Write a csv for SRP_Post_Processing.R to combine the 2 model outputs for DWRAT
PRMS_DataRange = paste0("Observed_Data_", Hydro_StartDate, "_", Hydro_EndDate)
write.csv(x = RR_Subset_Summed, 
          file = paste0("ProcessedData/PRMS_", PRMS_DataRange, ".csv"),
          row.names = FALSE)


