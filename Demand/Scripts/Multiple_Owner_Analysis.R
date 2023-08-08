# Load required libraries
library(dplyr)
library(lubridate)
library(readxl)
library(here)
library(writexl)

data<- read_excel(here("IntermediateData/WATER_RIGHTS_WITH_MULTIPLE_OWNERS.xlsx"))

# Convert date columns to Date type
data$EFFECTIVE_FROM_DATE <- as.Date(data$EFFECTIVE_FROM_DATE)
data$EFFECTIVE_TO_DATE <- as.Date(data$EFFECTIVE_TO_DATE)

# Define the reporting years
reporting_years <- seq(2018, 2023)

# Perform the analysis
RY_2018 <- data %>%
  mutate(Reporting_Year = 2018) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2019 <- data %>%
  mutate(Reporting_Year = 2019) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2020 <- data %>%
  mutate(Reporting_Year = 2020) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2021 <- data %>%
  mutate(Reporting_Year = 2021) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2022 <- data %>%
  mutate(Reporting_Year = 2022) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2023 <- data %>%
  mutate(Reporting_Year = 2023) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

# Create a list of your labeled data frames
RY <- list(
  RY_2018 = RY_2018,
  RY_2019 = RY_2019,
  RY_2020 = RY_2020,
  RY_2021 = RY_2021,
  RY_2022 = RY_2022,
  RY_2023 = RY_2023
)

# Export data frames to an Excel file with separate sheets
write_xlsx(RY, path = here("IntermediateData/ReportingYears_MultipleOwners.xlsx"))
