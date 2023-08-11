# Load required libraries----
library(dplyr)
library(lubridate)
library(readxl)
library(here)
library(writexl)
library(readr)
library(stringr)

# Define the reporting years
reporting_years <- seq(2017, 2023)

# Import Relevant Files----
#Import Water Rights with Multiple Owners in the RR watershed
Multiple_Owners<- read_excel(here("IntermediateData/WATER_RIGHTS_WITH_MULTIPLE_OWNERS.xlsx"))

# Import Application Numbers for Water Rights in the RR watershed
appYears <- read_csv("IntermediateData/Statistics_FINAL.csv", show_col_types = FALSE) %>%
  select(APPLICATION_NUMBER, YEAR) %>% unique() #Just select the unique application_number and year combos


#Import eWRIMS Water Use Report Flat File
#We need to find out if there's a fieed for reporting party ID--if we know who submitted the report in a given year,
#then perhaps we can bypass the manual review of the individual reports in eWRIMS
Reporting_Flat_File <- read_csv("RawData/water_use_report.csv") %>% 
  filter(APPL_ID %in% appYears$APPLICATION_NUMBER) %>% #Whittle Reporting_Flat_File to just the water rights in the RR watershed
  filter(YEAR %in% reporting_years) #Filter to just the reporting_years 


# Also read in 'ewrims_flat_file_party.csv'
  # Filter it down to only "Primary Owner" records and "APPLICATION_ID" values that
  # match "APPLICATION_NUMBER" in 'appYears'
  # Along with that, the "EFFECTIVE_TO_DATE" should be NA (currently active), 
  # or the ownership ended during the dataset's timeframe ("EFFECTIVE_TO_DATE" is between 2017-present)
partyDF <- readr::read_csv("RawData/ewrims_flat_file_party.csv", col_types = cols(.default = col_character())) %>%
  filter(RELATIONSHIP_TYPE == "Primary Owner" & APPLICATION_ID %in% appYears$APPLICATION_NUMBER) %>%
  mutate(EFFECTIVE_TO_YEAR = as.numeric(str_extract(EFFECTIVE_TO_DATE, "[0-9]{4}$"))) %>%
  filter(is.na(EFFECTIVE_TO_DATE) | EFFECTIVE_TO_YEAR >= min(appYears$YEAR)) %>%

#Whittle the partyDF to just a few relevant fields
partyDF_slim <-select(partyDF,PARTY_ID, PARTY_NAME, ENTITY_TYPE, EFFECTIVE_FROM_DATE, EFFECTIVE_TO_DATE)

# Analysis----
## Export owners for each water right by reporting year----
RY_2018 <- Multiple_Owners %>%
  mutate(Reporting_Year = 2018) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2019 <- Multiple_Owners %>%
  mutate(Reporting_Year = 2019) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2020 <- Multiple_Owners %>%
  mutate(Reporting_Year = 2020) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2021 <- Multiple_Owners %>%
  mutate(Reporting_Year = 2021) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2022 <- Multiple_Owners %>%
  mutate(Reporting_Year = 2022) %>%
  filter(START_YEAR <= Reporting_Year,
         is.na(END_YEAR) | END_YEAR >= Reporting_Year)

RY_2023 <- Multiple_Owners %>%
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


# Generate List of Primary Owners for Russian River from 2018-2023----



