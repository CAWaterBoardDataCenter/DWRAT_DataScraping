# Load required libraries----
library(dplyr)
library(janitor)
library(lubridate)
library(readxl)
library(here)
library(writexl)
library(readr)
library(stringr)
library(openxlsx)

# Define the reporting years
reporting_years <- seq(2017, 2023)

# Import Relevant Files----
# Import Water Rights with Multiple Owners in the RR watershed
Multiple_Owners<- read_excel(here("IntermediateData/WATER_RIGHTS_WITH_MULTIPLE_OWNERS.xlsx"))

# Import Application Numbers for Water Rights in the RR watershed
appYears <- read_csv("IntermediateData/Statistics_FINAL.csv", show_col_types = FALSE) %>%
  #Just select the unique application_number and year combos
  select(APPLICATION_NUMBER, YEAR) %>% unique()

## Import eWRIMS Annual Report Flat File-----
  # We need to find out if there's a field for reporting party ID--
  # if we know who submitted the report in a given year,
  # then perhaps we can bypass the manual review of the individual reports in eWRIMS
Reporting_Flat_File <- read_csv("RawData/ewrims_flat_file_annual_report.csv") %>% 
  #Filter Reporting_Flat_File to just the water rights in the RR watershed
  filter(APPLICATION_NUMBER %in% appYears$APPLICATION_NUMBER) %>% 
  #Filter to just the reporting years 2017 to 2023
  filter(REPORT_YEAR %in% reporting_years)

# Whittle Reporting_Flat_File to a few relevant fields
Report_Flat_File_Slim = Reporting_Flat_File %>% 
  select(APPLICATION_NUMBER, PRIMARY_OWNER, PRIMARY_CONTACT, 
         REPORT_YEAR, DATE_SUBMITTED, REVISION, REPORT_SYSTEM_UNIQUE_ID) %>%
  #Sort alphabetically by PRIMARY_OWNER
  arrange(PRIMARY_OWNER)

## Import 'ewrims_flat_file_party.csv'----
# Filter to only "Primary Owner" records and "APPLICATION_ID" values that
partyDF <- read_csv("RawData/ewrims_flat_file_party.csv", col_types = cols(.default = col_character())) %>%
  mutate(PARTY_NAME = str_replace_all(PARTY_NAME, "  ", " ")) %>%  # Eliminate extra spaces in the middle of the string
  filter(RELATIONSHIP_TYPE == "Primary Owner") %>%
  mutate(PARTY_NAME = trimws(PARTY_NAME)) %>%  # Remove leading and trailing spaces

# Add an EFFECTIVE_TO_YEAR field as a 4-digit year 
  mutate(EFFECTIVE_TO_YEAR = as.numeric(str_extract(EFFECTIVE_TO_DATE, "[0-9]{4}$")))
  #filter(is.na(EFFECTIVE_TO_DATE)) #generates 55,209 records
  # Filter to records that are currently ACTIVE OR that ceased to be active after 1/1/2017
  #filter(is.na(EFFECTIVE_TO_DATE) | EFFECTIVE_TO_YEAR >= min(appYears$YEAR)) #generates 34,827 records
  #filter(is.na(EFFECTIVE_TO_DATE) | EFFECTIVE_TO_YEAR >= 2016) #generates 35,475 records
  #filter(is.na(EFFECTIVE_TO_DATE) | EFFECTIVE_TO_YEAR >= 2015) #generates 36,028 records
       
#Whittle the partyDF to just a few relevant fields and unique party_IDs
partyDF_slim <-select(partyDF,PARTY_ID, PARTY_NAME, FIRST_NAME, MIDDLE_NAME,
                      LAST_NAME_OR_COMPANY_NAME,ENTITY_TYPE, EFFECTIVE_FROM_DATE, 
                      EFFECTIVE_TO_DATE,) %>%
  unique() %>%
  #arrange(PARTY_ID)
  arrange(EFFECTIVE_TO_DATE)

# Remove extra spaces from PARTY_NAME field
# Remove extra spaces from PARTY_NAME field
partyDF_slim$PARTY_NAME <- gsub("\\s{2,}", " ", partyDF_slim$PARTY_NAME)

#Unique Owners in the RR from 2017 - present----
RR_Owners <- unique(Report_Flat_File_Slim$PRIMARY_OWNER) %>%
  trimws() %>%  # Remove leading and trailing spaces
  str_replace_all("  ", " ") %>%  # Replace extra spaces in the middle of the string
  data.frame(PARTY_NAME = .) %>%  # Create a data frame with a single column
  mutate(PARTY_NAME = str_replace_all(PARTY_NAME, "  ", " "))  # Replace extra spaces again

print(RR_Owners)
 #Returns 1280 records 

#Left Join RR_Owners to partyDF_Slim by PARTY_NAME
RR_Owners2 = left_join(x = RR_Owners, 
          y = partyDF_slim, 
          by = "PARTY_NAME") %>% 
        select(PARTY_NAME, PARTY_ID, FIRST_NAME, MIDDLE_NAME, LAST_NAME_OR_COMPANY_NAME) %>%
    unique() %>%
  arrange(PARTY_NAME) 
#Returns 1310 records when just PARTY_NAME and PARTY_ID columns are selected
#Returns 1311 records when PARTY_NAME, PARTY_ID, FIRST_NAME, MIDDLE_NAME, LAST_NAME_OR_COMPANY_NAME are selected
#Returns 1319 records when we don't filter out any primary owners in partyDF

# Check for NA Party IDs in RR_Owners
RR_Owners2 %>% filter(is.na(PARTY_ID)) %>% nrow() 
#360 owners with no corresponding PartyID when we don't filter any primary owners in partyDF
#365 owners with no corresponding party ID

#Check for duplicate PARTY_NAMES
dupe_RR = get_dupes(RR_Owners2,PARTY_NAME)
#76 records with duplicate party_names; so multiple party IDs are tied to the same party_name


# Export RR_Owners2 to Excel to manually review duplicate primary owners and NA party IDs; goal is to 
  # generate a one to one list of party_name to party_ID for the Russian River

openxlsx::write.xlsx(RR_Owners2, "Documentation/RR_Owner_Manual_Review.xlsx")

# Import RR_Owner_Manual_Review spreadsheet After Manual Review----
  # After finishing the manual review of party IDs and party names in eWRIMS, import the review spreadsheet
    #goal is to eliminate the erroneous party IDs and finalize a one to one list of party_name to party_ID for RR

#In case you accidentally overwrite the fully reviewed version of the spreadsheet, a copy has been saved to our 
RR_Owner_Manual_Review =  read_excel("Documentation/RR_Owner_Manual_Review.xlsx")  # SDA sharepoint: https://cawaterboards.sharepoint.com/:x:/r/DWR/SDA/Shared%20Documents/SOPs%20and%20Documentation/1.%20Demand%20Data/SDA%20Methodology/RR_Owner_Manual_Review.xlsx?d=wd885472edd3946b28e06cacbf70b5b93&csf=1&web=1&e=lrxYXG

#Records to Delete 
RR_Delete = filter(RR_Owner_Manual_Review, Action_Taken == "Delete")
RR_Delete