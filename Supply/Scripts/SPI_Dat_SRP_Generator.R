#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function
library(readxl) #for read_xlsx function


# Rely on the shared functions from the Demand scripts
source("../Supply/Scripts/Shared_Functions_Supply.R")

# Import Latest Dat SRP File

  # Divide the SRP File into a pre-WY 2021 portion
  # Rely on observed data to fill in 10/1/2020 - 9/30/2023
  # Save the observed portion from 10/1/2020 - 9/30/2023 and pre-WY 2021 into a
  # single SRP Dat pre-WY 2023 file, 1/1/1947 - 9/30/2023
  # Write code that downloads the observed data from 10/1/2023 - 3/31/2024
  # Generate an SPI portion corresponding to 4/1/2024 - 9/30/2024

