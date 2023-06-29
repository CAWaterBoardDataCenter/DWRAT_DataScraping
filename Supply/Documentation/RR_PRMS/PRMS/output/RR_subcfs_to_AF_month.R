library(ggplot2)
library(dplyr)
library(lubridate)
#install.packages("plotly")
library(plotly)
library(tidyverse)

# CONVERT sub_cfs to MONTHLY
s1 <- read.delim("Daily_CFS_RR_Independent__ZERO_PRECIP_Jun22_to_Oct31.csv", header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
s1$Date <- as.Date(s1$Date,format = '%m/%d/%Y')

#convert daily average cfs to acre feet per day
s1[,c(2:29)] <- s1[,c(2:29)]*1.98347

#create df of monthly flows
s1_monthly <- s1 %>% group_by(month(Date),year(Date)) %>%
  summarize_at(vars(c(2:29)),sum)

#create a date column with the iso dates. Note that the day will always be 1
s1_monthly$date <- as.Date(ISOdate(s1_monthly$`year(Date)`, s1_monthly$`month(Date)`,1))

#delete unnecessary year date and month date columns
s1_monthly <- s1_monthly[,-c(1,2)]

#arrage the data by date
s1_monthly <- s1_monthly %>% arrange(date)

#sort the data by date
s1_monthly <- s1_monthly %>%
  select(date, everything())

colnames(s1_monthly) <- c('Date',' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9',' 10',' 11',' 12',' 13',' 14',' 15',' 16',' 17',' 18',' 19',' 20',' 21',' 22',' 23',' 24',' 25',' 26',' 27',' 28')


write.csv(s1_monthly,"Monthly_AcFt_RR_Independent_ZERO_PRECIP_Jun22_to_Oct31.csv", row.names = FALSE)


