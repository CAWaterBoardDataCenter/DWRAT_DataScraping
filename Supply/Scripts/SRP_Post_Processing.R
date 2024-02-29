## load packages
library(tidyverse)
library(lubridate)
library(here)

# Import gag Files----

# set the column widths for the .gag file
col_widths <- (c(20,16,16,16,16,16,16,16,16,16,16,16,16))

# Set the source folder path for importing .gag files
  # Change the source folder path to wherever you've stored your SRP model, e.g.
  # E:/SRPHSM_update_ag for example
source_folder <- "C:/SRPHM_update_ag"

# Now import the files directly from the source folder
gag_list <- list()  # create an empty list to store the imported datasets
col_widths <- c(20, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)  # replace with your actual column widths

# For Loop imports the gag files 1 at a time into a list and applies the column widths 
for (i in 1:6) {
  filename <- paste0(source_folder, "/SRP_inflow_", i, ".gag")
  gag <- read_fwf(here(filename), skip = 2, fwf_widths(col_widths))
  gag_list[[i]] <- gag  # add the imported dataset to the list
}

# Modify the gag files----
# remove all columns except date and flows
# define a function to modify the column names and subset the data
modify_data <- function(df, index) {
  colnames(df)[c(1, 3)] <- c("Date", paste0("gag", index))
  df <- subset(df, select = c("Date", paste0("gag", index)))
  return(df)
}
# use lapply to apply the function to each dataframe in the list
gag_list <- lapply(seq_along(gag_list), function(i) modify_data(gag_list[[i]], i))

# combine the 6 modified data frames into a single data frame by merging on the Date field
merge_gags <- function(df1, df2) {
  # the merge function merges two dataframes, all = TRUE ensures a full join, all = FALSE performs an inner join
  merge(df1, df2, by = "Date", all = TRUE) 
}

# The reduce function takes the result from the previous merger and runs the merge function again until it exhausts all 
# the dataframes in the gag_list. 1st merger is gag1 and gag2, 2nd merger is gag1_2 and gag3, 3rd merger is gag1_2_3
# gag 4 and so on.
gag <- Reduce(merge_gags,gag_list)


## change timestep to date starting on 10/1/1974----
start_date <- as.Date("1974-10-01")
# end_date <- as.Date("2023-09-30")
date_seq <- seq(from = start_date, length.out = nrow(gag), by = "day")
gag$Date <- date_seq[1:nrow(gag)]

## create a subset for the timeframe of interest----
#Define the SRP timeframe--changes with each run
SRP_StartDate = as.Date("2013-04-01")
SRP_EndDate = as.Date("2014-01-31")
SRP_Timeframe = data.frame(Date = seq(from = SRP_StartDate, to = SRP_EndDate, by = "days"))

gag <- subset(gag, Date>= SRP_StartDate & Date <= SRP_EndDate)

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

# configure the dataframe to consist of just date and flows columns
gag_names <- colnames(gag[,2:8])
gag <- gag[,-c(1,3:14)]
colnames(gag) = gag_names

# creating the final subbasin values----

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

# Merge dataframes SRP_Timeframe and sub23 - sub28
SRP <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), 
                    list(SRP_Timeframe, sub23, sub24, sub25, sub26, sub27, sub28))

# remove intermediaries from environment
rm(sub23,sub24,sub25,sub26,sub27,sub28)

# convert cubic feet/day (CFD) to acre-feet/day
AFD <- 1/43560 # 1 acre-ft/ 43560 ft^3
SRP[, 2:7] <- SRP[,2:7]*AFD

# aggregate the sub-columns by monthly totals and a year column
SRP$Year <- as.numeric(format(SRP$Date, "%Y"))
SRP$Month <- format(SRP$Date, "%m")
SRP$Year_Month <- format(SRP$Date, "%Y-%m")

SRP_monthly <- aggregate(SRP[, 2:7], by = list(Month = SRP$Year_Month), sum)

# create a vector of month values
months <- SRP_monthly$Month

# convert the month values to date objects
SRP_monthly$Month <- paste0(months, "-01")

# rename columns to match DWRAT naming convention
colnames(SRP_monthly)[colnames(SRP_monthly) == "Month"] <- "Date"
colnames(SRP_monthly)[2:7] <- c(23:28)

#Define your data range--this will vary with each run
DataRange = "Observed_Data_2023-04_to_2024-01"
# write subset data to CSV----
write.csv(SRP_monthly, file = paste0("ProcessedData/SRP_",DataRange, ".csv"), row.names = FALSE)

# merge data to include the rest of LRR subbasins
PRMS <- read.csv(here("ProcessedData/PRMS_2023-10.csv"))
PRMS$Date <- as.Date(PRMS$Date)
colnames(PRMS)[2:23] <- c(1:22)
Raw_Flows <- merge(PRMS, SRP_monthly, by = "Date")

# write Raw Flows to cvs for DWRAT input
write.csv(Raw_Flows, here("ProcessedData/Raw_Flows.csv"), row.names = FALSE)

# write daily values - if needed - to CSV
# write.csv(SRP, here("ProcessedData/SRP_daily_AcFt_2023.04.05.csv"), row.names = FALSE)
