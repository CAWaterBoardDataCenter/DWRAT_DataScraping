#THIS SCRIPT IS EXPERIMENTAL AND DOES NOT FULLY WORK YET#
#LAST UPDATED BY: PAYMAN ALEMI ON 3/21/2023

## load packages
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)
library(lubridate)

#Import Data
Stations = read.csv(here("InputData/CNRFC_Stations.csv"))
