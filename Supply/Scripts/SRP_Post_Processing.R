## load packages
library(tidyverse)
library(lubridate)
library(here)

# import Gag Files----
# set the column widths for the .gag file
col_widths <- (c(20,16,16,16,16,16,16,16,16,16,16,16,16))

# import the .gag files - loop approach
gag_list <- list()  # create an empty list to store the imported datasets
for (i in 1:6) {
  filename <- paste0("InputData/SRP_inflow_", i, ".gag")
  gag <- read_fwf(here(filename), skip = 2, fwf_widths(col_widths))
  gag_list[[i]] <- gag  # add the imported dataset to the list
}
# remove all columns but date and flows
# define a function to modify the column names and subset the data
modify_data <- function(df, index) {
  colnames(df)[c(1, 3)] <- c("Date", paste0("gag", index))
  df <- subset(df, select = c("Date", paste0("gag", index)))
  return(df)
}
# use lapply to apply the function to each dataset in the list
gag_list <- lapply(seq_along(gag_list), function(i) modify_data(gag_list[[i]], i))

# combine the modified data frames into a single data frame
gag <- Reduce(function(...) merge(..., by = "Date", all = TRUE), gag_list)

# change timestep to date starting on 10/1/1974----
start_date <- as.Date("1974-10-01")
# end_date <- as.Date("2023-09-30")
date_seq <- seq(from = start_date, length.out = nrow(gag), by = "day")
gag$Date <- date_seq[1:nrow(gag)]

# create a subset for the timeframe of interest----
gag <- subset(gag, Date>= "2023-07-01" & Date <= "2023-10-31")
date_seq = gag$Date

# gag manipulation----
# read in percent reduction factors CSV
reduct <- read.csv(here("InputData/srp_percent_reduction.csv"))
#Accuracy of spreadsheet verified in PowerQuery by Payman Alemi on 4/18/2023; check out
  #SRP GW Reduction Factors.xlsx

# multiply gag columns by monthly reduction factor
gag$Month <- month(gag$Date)
gag <- merge(gag, reduct, by = "Month")

# loop over the column names and multiply the corresponding columns
for (i in 1:6) {
  # match the column names with a regular expression
  gag_col <- grep(paste0("^gag", i, "$"), names(gag), value = TRUE)
  X_col <- grep(paste0("^X", i, "$"), names(gag), value = TRUE)
  # multiply the corresponding columns and assign the result to a new column
  gag[[paste0("flows", i)]] <- gag[[gag_col]] * gag[[X_col]]
}

# configure the dataframe so it is just date and flows columns
gag_names <- colnames(gag[,2:8])
gag <- gag[,-c(1,3:14)]
colnames(gag) = gag_names

# creating the final subbasin values----
# create an empty dataframe with columns for date and subbasins 23-28
SRP <- data.frame(Date = date_seq)

# create the data frames with mutate() and select()
sub23 <- gag %>% select(Date, gag1) %>% 
  mutate(sub23 = gag1) %>% select(Date = Date, sub23)

sub24 <- gag %>% select(Date, gag1, gag6, gag5) %>% 
  mutate(sub24 = gag6 - gag1 - gag5) %>% select(Date = Date, sub24)

sub25 <- gag %>% select(Date, gag5, gag4, gag3) %>% 
  mutate(sub25 = gag5 - gag4 - gag3) %>% select(Date = Date, sub25)

sub26 <- gag %>% select(Date, gag4, gag2) %>% 
  mutate(sub26 = gag4 - gag2) %>% select(Date = Date, sub26)

sub27 <- gag %>% select(Date, gag2) %>% 
  mutate(sub27 = gag2) %>% select(Date = Date, sub27)

sub28 <- gag %>% select(Date, gag3) %>% 
  mutate(sub28 = gag3) %>% select(Date = Date, sub28)

# bind the columns to the empty data frame
for (i in 23:28) {
  sub_df <- get(paste0("sub", i))
  SRP <- merge(SRP, sub_df, by = "Date")
}
# remove intermediaries from environment
rm(sub23,sub24,sub25,sub26,sub27,sub28,sub_df)

# convert cubic feet/day (CFD) to acre-feet/day
AFD <- 1/43560 # 1 acre-ft/ 43560 ft^3
SRP[, 2:7] <- SRP[,2:7]*AFD

# aggregate the sub-columns by monthly totals
SRP$Month <- format(SRP$Date, "%m")
SRP_monthly <- aggregate(SRP[, 2:7], by = list(Month = SRP$Month), sum)
# remove month column from SRP to capture daily AcFt totals
SRP = SRP[,-c(8)]

# create a vector of month values
months <- SRP_monthly$Month
# convert the month values to date objects
SRP_monthly$Month <- as.Date(paste0(months, "/01/2023"), format = "%m/%d/%Y")

# rename columns to match DWRAT naming convention
colnames(SRP_monthly)[colnames(SRP_monthly) == "Month"] <- "Date"
colnames(SRP_monthly)[2:7] <- c(23:28)

# write subset data to CSV----
write.csv(SRP_monthly, here("ProcessedData/SRP_update_AF_2023-10.csv"), row.names = FALSE)

# merge data to include the rest of LRR subbasins
Raw_Flows <- merge(RR_Subset_Summed, SRP_monthly, by = "Date")

# write Raw Flows to cvs for DWRAT input
write.csv(Raw_Flows, here("ProcessedData/Raw_Flows_2023-10.csv"), row.names = FALSE)

# write daily values - if needed - to CSV
# write.csv(SRP, here("ProcessedData/SRP_daily_AcFt_2023.04.05.csv"), row.names = FALSE)
