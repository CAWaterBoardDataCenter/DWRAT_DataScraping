#Install libraries----
#Uncomment the lines below if you haven't installed these packages yet
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("here")
# install.packages("lubridate")

#Load libraries----
library(dplyr)
library(tidyverse)
library(tidyr)
library(here)
library(lubridate)

#Import Downsizer Data----
#Copy and paste the Downsizer CSV into the InputData folder;
  #rename the Downsizer file like so: Downsizer_2023.03.16.csv (suffix is today's date in YYYY.mm.dd format)
Downsizer_Original = read.csv(file = here("InputData/Downsizer_2023-03-23.csv"))
Headers = read.csv(file = here("InputData/Downsizer_Stations.csv"))

#Account for timeframe of interest----
StartDate = data.frame("January", "11", "2023", as.Date("2023-01-11"))
EndDate = data.frame("March", "23", "2023", as.Date("2023-03-23"))
colnames(StartDate) = c("month", "day", "year", "date")
colnames(EndDate) = c("month", "day", "year", "date")
ndays = seq(from = StartDate$date, to = EndDate$date, by = 'day') %>% length()
ndays
TimeFrame = seq(from = StartDate$date, to = EndDate$date, by = 'day')

#Extract the weather data from Downsizer_Original----
#Drop the first 42 rows of Downsizer
Downsizer = tail(Downsizer_Original, nrow(Downsizer_Original)-42) %>%data.frame()
colnames(Downsizer) = "Downsizer"

#Format the Downsizer dataframe to match the PRMS_Update DAT file----
##Calculate the number of columns after splitting the 1st column by the space delimiter----
ncols <- max(stringr::str_count(Downsizer$Downsizer))
ncols #creates 260 columns, but only the first 36 have content

colmn <- paste0("Col", 1:ncols)
colmn

##Split Downsizer into 260 columns by using spaces as delimiters----
Downsizer_Processed <-
  tidyr::separate(
    data = Downsizer,
    col = Downsizer,
    sep = " ",
    into = colmn,
    remove = FALSE
  )

##Delete extra columns and apply column names----
Downsizer_Processed[38:261] = NULL
Downsizer_Processed[5:7] = NULL
Downsizer_Processed[1] = NULL

#Fill in the column names of Downsizer_Processed
colnames(Downsizer_Processed) = colnames(Headers)
colnames(Downsizer_Processed)

#Remove all NA columns from Downsizer_Processed 
Downsizer_Processed <- select(Downsizer_Processed, -c(starts_with("NA")))
colnames(Downsizer_Processed)

##Rearrange Downsizer_Processed columns in proper order----
col_order <- c('Year', 'Month', 'Day', 'DOWNSIZER_PRECIP1', 'DOWNSIZER_PRECIP2',
               'DOWNSIZER_PRECIP3', 'DOWNSIZER_PRECIP5', 'DOWNSIZER_PRECIP8', 'DOWNSIZER_PRECIP10',
               'DOWNSIZER_PRECIP11', 'DOWNSIZER_PRECIP13','DOWNSIZER_PRECIP14', 'DOWNSIZER_PRECIP15',
               'DOWNSIZER_TMAX1', 'DOWNSIZER_TMAX2', 'DOWNSIZER_TMAX6', 'DOWNSIZER_TMIN1', 'DOWNSIZER_TMIN2', 
               'DOWNSIZER_TMIN6')
Downsizer_Processed <- Downsizer_Processed[,col_order]

#BEFORE THIS STEP: Run PRISM_Processor.R, CNRFC_Scraper.R, & CNRFC_Processor.R----
#Replace missing values with PRISM data
#Works only if columns are same in number and order; column names don't need to match
Prism_Processed = read.csv(here("ProcessedData/Prism_Processed.csv"))
#Change date format of Downsizer data to match PRISM
Downsizer_Replaced <- Downsizer_Processed
Downsizer_Replaced <- Downsizer_Replaced %>% 
  unite(col = "Date", Year, Month, Day, sep = "-") %>% 
  mutate(date = as.Date(Date))

#Create PRISM df to replace missing values
PRISM_cols <- Prism_Processed[,c('Date', 'PP_PRECIP1', 'PP_PRECIP2',
                                 'PP_PRECIP3', 'PP_PRECIP5', 'PP_PRECIP8', 'PP_PRECIP10',
                                 'PP_PRECIP11', 'PP_PRECIP13','PP_PRECIP14', 'PP_PRECIP15',
                                 'PT_TMAX1', 'PT_TMAX2', 'PT_TMAX6', 'PT_TMIN1', 'PT_TMIN2', 
                                 'PT_TMIN6')]
#Change -999.0 values to -999
for (i in 2:18) {
  Downsizer_Replaced[, i] <- gsub("-999.0", "-999", Downsizer_Replaced[, i])
}
#Replace -999 values with PRISM data
Downsizer_Replaced[Downsizer_Replaced == -999] <- PRISM_cols[Downsizer_Replaced == -999]

#Write CSV to ProcessedData Folder----
write.csv(Downsizer_Replaced, here("ProcessedData/Downsizer_Processed.csv"), row.names = FALSE)


