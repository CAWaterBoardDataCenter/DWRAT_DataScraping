#Load Libraries
library(here)
library(dplyr)

#Import the necessary starter files for the DAT_File----
#Dat_Shell serves as the shell of the DAT_File, observed data (already prefilled with PRISM and forecast data) contained in these files:
    #Raws_Processed.csv, CIMIS_Processed.csv, Downsizer_Processed.csv 
  
Dat_Shell <- read.csv(here("InputData/Dat_Shell.csv"))
RAWS <- read.csv(here("ProcessedData/RAWS_Processed.csv"))
Downsizer <- read.csv(here("ProcessedData/Downsizer_Processed.csv"))
CIMIS <- read.csv(here("ProcessedData/CIMIS_Processed.csv"))

#Merge observed data sources into one dataframe
Observed <- merge(Downsizer,merge(RAWS, CIMIS, by = "Date"))

#Merge Dat_Shell with Observed data----
#Whittle down Dat_Shell to the timeframe of interest
Dat_Shell2 <-filter(Dat_Shell, Date>= StartDate$date, Date <= End_Date)

#Remove columns 1-6 of DAT_Shell (individual datetime fields: year, month, etc) along with columns 39-60 (runoff columns)
Dat_Shell3 <- Dat_Shell2[,-c(1:6, 39:60)]

#Inner Join the observed dataframe to Dat_Shell2 by the Date field
Dat_Shell4 <- inner_join(Observed, select(Dat_Shell3, Date), by = "Date")

#Import the individual datetime fields (year, month, etc) from Dat_Shell into Dat_Shell3 to creat Dat_shell4
Dat_Shell5 <- cbind(Dat_Shell2[,c(1:6)], Dat_Shell4)

#Import the runoff columns from Dat_Shell into Dat_Shell4 to create Dat_Shell5
Dat_Shell6 <-cbind(Dat_Shell5, cbind(Dat_Shell2[,39:60]))

#Remove the Date field from Dat_Shell5 because it does not exist in the final DAT file
Dat_Shell6$Date = NULL
col_order <- colnames(Dat_Shell[-7]) #grab the column names from Dat_Shell except for Date
Dat_Final = Dat_Shell6[,col_order]

#Write to tab-delimited text file----
write.table(Dat_Final, here(paste0("ProcessedData/Dat_Final_", End_Date, ".txt")), sep="\t", row.names=F, quote = F)
#write.table(Dat_Final, here("ProcessedData/Dat_Final_2023-07-17.txt"), sep="\t", row.names=F, quote = F)

#Write to CSV
# write.csv(Dat_Final, here("ProcessedData/Dat_Final_2023-03-24.csv"), row.names = FALSE)
