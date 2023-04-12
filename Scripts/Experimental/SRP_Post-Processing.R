## load packages
library(tidyverse)
library(lubridate)
library(here)

#Import Gag Files----
# Set the column widths for the .gag file
col_widths <- (c(20,16,16,16,16,16,16,16,16,16,16,16,16))

# Import the .gag files - loop approach
gag_list <- list()  # create an empty list to store the imported datasets
for (i in 1:6) {
  filename <- paste0("InputData/SRP_inflow_", i, ".gag")
  gag <- read_fwf(here(filename), skip = 2, fwf_widths(col_widths))
  gag_list[[i]] <- gag  # add the imported dataset to the list
}
# remove all columns but date and flows
# Define a function to modify the column names and subset the data
modify_data <- function(df, index) {
  colnames(df)[c(1, 3)] <- c("Date", paste0("gag", index))
  df <- subset(df, select = c("Date", paste0("gag", index)))
  return(df)
}
# Use lapply to apply the function to each dataset in the list
gag_list <- lapply(seq_along(gag_list), function(i) modify_data(gag_list[[i]], i))

# Combine the modified data frames into a single data frame
gag <- Reduce(function(...) merge(..., by = "Date", all = TRUE), gag_list)

# change timestep to date starting on 10/1/1974
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2023-09-30")
date_seq <- seq(from = start_date, to = end_date, by = "day")
gag$Date <- date_seq[1:nrow(gag)]

# write to csv
write.csv(gag, here("ProcessedData/SRP_gag_Processed.csv"), row.names = FALSE)
