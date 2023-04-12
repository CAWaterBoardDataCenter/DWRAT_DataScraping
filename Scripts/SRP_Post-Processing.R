## load packages
library(tidyverse)
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
end_date <- as.Date("2023-09-30")
date_seq <- seq(from = start_date, to = end_date, by = "day")
gag$Date <- date_seq[1:nrow(gag)]

# create an empty dataframe with columns for date and subbasins 23-28
basins <- data.frame(Date = date_seq)

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
  basins <- merge(basins, sub_df, by = "Date")
}
# remove dataframes from environment
rm(sub23,sub24,sub25,sub26,sub27,sub28,sub_df)

# create a subset for the timeframe of interest----
SRP <- subset(basins, Date>= "2022-11-01" & Date <= "2023-09-30")

# write subset data to CSV
write.csv(SRP, here("ProcessedData/SRP_update_2023.04.05.csv"), row.names = FALSE)
