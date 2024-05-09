#Constructing a new SRP Dat File
#Run the first portion of Master_Script_PRMS.R where you define StartDate, EndDate, and you download the SRP raw data
#Import Pre-2023 WY SRP DAT file
#Import SPI WY 2023-2024 SRP DAT File
#Import SRP_Processed.csv

#Have some logic that overrides overlapping portion of SPI file with observed data if observed data exists. 
#Never use SPI data if you have observed data available.
#combine the 3 datasets (Pre-2023 WY SRP DAT File, SPI data, and observed data) to generate final SRP DAT file
# for a specific month
#Save the SRP_Dat file for a specific month to the ProcessedData folder with a timestamp. The timestamp is the EndDate;
#EndDate is the last day of the observed data range. 

# Import Processed SRP data
SRP_Processed = read.csv(file = "ProcessedData/SRP_Processed.csv")