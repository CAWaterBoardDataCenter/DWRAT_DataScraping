#Last Updated By: Payman Alemi
#Last Updated On: 4/11/2023

#Load libraries
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)

#Import GAG Files----
input_dir = here("ProcessedData")
gag_files <- list.files(input_dir, pattern = "\\.gag$", full.names = TRUE)

# Convert each .gag file to a CSV in the same directory
for (gag_file in gag_files) {
  csv_file <- gsub("\\.gag$", ".csv", basename(gag_file))
  csv_path <- file.path(dirname(gag_file), csv_file)
  gag_data <- read.table(gag_file, header = TRUE, sep = "\t", na.strings = "NA", dec = ".", quote = "")
  write.csv(gag_data, csv_path, row.names = FALSE)
}

# Get a list of all .csv files in the input directory
csv_files <- list.files(input_dir, pattern = "SRP_inflow.*\\.csv$", full.names = TRUE)

# Define the  names and widths of each field in the fixed-width file format
widths <- c(19,16,16,16,16,16,16,16,16,16,16,16,16)
colnames = c("Time", "Stage", "Flow", "Depth", "Width", "Midpt-Flow", "Precip.", "ET", "SFR-Runoff",
              "UZF-Runoff", "Conductance", "HeadDiff", "Hyd.Grad.")

# Import each CSV file into a named list of data frames

# Loop through the CSV files
for (i in 1:length(csv_files)) {
  # Read CSV file
  csv_data <- read.fwf(csv_files[i], widths = widths, col.names = colnames, header = TRUE, skip = 1)
  
  # Add date column incremented daily beginning on 10/1/1974
  csv_data$Date <- seq(as.Date("1974-10-01"), by = "day", length.out = nrow(csv_data))
  
  # Select only "Date" and "flow" columns and remove all other columns
  csv_data <- select(csv_data, Date, Flow)
  
  # Assign data frame to a variable with a name based on the file name
  assign(paste0("SRP_inflow", i), csv_data)
}


