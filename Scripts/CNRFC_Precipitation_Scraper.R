## load packages
library(tidyverse)
library(netstat)
library(here)
library(dplyr)
library(readr)
library(lubridate)

#Import CNRFC precipitation CSV
CNRFC_precip = read.csv(here("WebData/cnrfc_qpf.csv"), header = FALSE)
CNRFC_precip$V28 = NULL
StartDate = as.Date(CNRFC_precip[2,4])