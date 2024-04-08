library(lubridate)

#Import an older PRMS Dat file that has all have the 1/1/1990 - 9/30/2023 data intact
Dat_PRMS_Original <- read.delim(file = "data_update_to_2023_10_25_PA.dat",
                                sep = "\t", skip = 6, header = FALSE)

Dat_Fields_PRMS <- read.csv("C:/users/palemi/Documents/GitHub/DWRAT_DataScraping/Supply/InputData/Dat_Fields_PRMS.csv")

#Set Dat_File column names
colnames(Dat_PRMS_Original) <- colnames(Dat_Fields_PRMS)

#Add a Date Field
Dat_PRMS_Original$Date <- make_date(year = Dat_PRMS_Original$Year, 
                                    month = Dat_PRMS_Original$month,
                                    day = Dat_PRMS_Original$day)

#Whittle to 11/1/2023 - 9/30/2024
# October 2023 data consists of forecast CNRFC and observed data; SPI data begins on 11/1/2023

SPI_2023_24 = subset(Dat_PRMS_Original, 
                      Date > as.Date("2023-10-31", format = "%Y-%m-%d") &
                       Date < as.Date("2024-10-01", format = "%Y-%m-%d"))

#Export SPI_2023-2024 to InputData folder on GitHub
setwd("~/GitHub/DWRAT_DataScraping/Supply/InputData")
write.csv(x = SPI_2023_24, file = "SPI_2023_24.csv",row.names = FALSE)