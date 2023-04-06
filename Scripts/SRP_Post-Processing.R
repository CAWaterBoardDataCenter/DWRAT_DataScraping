## load packages
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)
library(lubridate)

#Import Gag Files----
# Set the column widths for the .gag file
col_widths <- c(8, 12, 10, 10, 12, 12, 12, 12)

# Import the .gag file
data <- read.fwf(here("InputData/SRP_inflow_0.gag"), widths = col_widths, skip = 2)
