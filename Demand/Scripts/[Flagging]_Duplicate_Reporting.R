
#Load libraries----
require(dplyr) #for numerous functions
require(tidyverse) #for read_csv
require(data.table) #for fread function
require(janitor) # for get_dupes function
require(writexl) # for write_xlsx function
require(readxl) # for read_xlsx function



flagDuplicateReports <- function () {
  
  # 
  
  
  source("Scripts/Watershed_Selection.R")
  source("Scripts/Dataset_Year_Range.R")
  
  
  
  sameYearDupReports(ws, yearRange)
  
  
  
  
  
  
}



sameYearDupReports <- function (ws, yearRange) {
  
  
  # Get the flag table
  # The water rights in this table will be a necessary filter
  flagDF <- paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv") %>%
    fread()
  
  
  # From the extended CSV, extract party information
  # Then apply some filters: 
  #   (1) Filter the dataset to years within the desired dataset range
  #   (2) The diversion type should be either "STORAGE" or "DIRECT" only
  #   (3) Consider only rights that appear within 'flagDF'
  partyDF <- fread("RawData/Snowflake_water_use_report_extended.csv",
                   select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", 
                              "DIVERSION_TYPE", "APPLICATION_PRIMARY_OWNER", "PARTY_ID")) %>%
    filter(YEAR >= yearRange[1] & YEAR <= yearRange[2],
           DIVERSION_TYPE %in% c("STORAGE", "DIRECT"),
           APPLICATION_NUMBER %in% flagDF$APPLICATION_NUMBER)
  
  
  
  # Group by "APPLICATION_NUMBER" and "YEAR"
  # Then, get the unique counts for each grouping
  # (Ideally, there should be one unique report per right/year)
  # The concern is whether multiple parties submitted a report for the same year
  # for a right (e.g., during the year when it changed ownership)
  partyCounts <- partyDF %>%
    select(APPLICATION_NUMBER, YEAR, APPLICATION_PRIMARY_OWNER, PARTY_ID) %>%
    unique() %>%
    group_by(APPLICATION_NUMBER, YEAR) %>%
    summarize(PARTY_COUNTS = n(), .groups = "drop")
  
  
  
  # Create a flagging column related to whether "PARTY_COUNTS" is greater than 1
  # Append this column to 'flagDF'
  flagDF <- flagDF %>%
    left_join(partyCounts %>%
                mutate(DIFFERENT_OWNERS_SUBMITTED_FOR_THE_SAME_RIGHT_AND_YEAR = 
                         if_else(PARTY_COUNTS > 1, TRUE, FALSE)) %>%
                select(-PARTY_COUNTS),
              by = c("APPLICATION_NUMBER", "YEAR"), 
              relationship = "many-to-one")
  
  
  
  
  
}





#Import Raw Data ----

#Import Statistics_FINAL.csv. This is one of the output files of the Priority_Date_Preprocessing.R 
#script; but we just want the unique APPLICATION_NUMBERS; these are just water rights
#in the Russian River.
appYears <- fread("RawData/Snowflake_water_use_report_extended.csv",
                  select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE")) %>%
  unique() %>%
  #Add a YEAR_ID column that concatenates the year and application number
  mutate(YEAR_ID = paste(YEAR, APPLICATION_NUMBER, sep = "_"))



##Import only 7 columns of the water_use_report_extended.csv----
#APPL_ID is the Water Right ID
#YEAR is the reporting year
#MONTH is the calendar month
#AMOUNT is the monthly amount diverted/used
#DIVERSION_TYPE categorizes the diversion type as direct diversion, storage, or use
#APPLICATION_PRIMARY_OWNER is the primary owner in the reporting year
#PARTY_ID is the Party ID tied to the primary owner in the reporting year

file_path <- "RawData/water_use_report_extended.csv"
selected_columns <- c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE",
                      "APPLICATION_PRIMARY_OWNER", "PARTY_ID")

#Import only the selected_columns of the water_use_report_extended.csv
RMS_parties <- fread(file = file_path, select = selected_columns)





#Prepare the RMS_parties dataset for manual review----

#Filter to 2017-present records
RMS_parties <- filter(RMS_parties, YEAR >= 2017)

#Filter to just direct diversion and  diversion to storage--usage is irrelevant because we only care about 
#what's diverted out of stream
RMS_parties <- filter(RMS_parties, DIVERSION_TYPE != "USE")

#Filter RMS_parties to just the Russian River
RMS_parties <- RMS_parties %>%
  filter(APPLICATION_NUMBER %in% appYears$APPLICATION_NUMBER)

##Look for duplicate party IDs ----
#Add a Year_Right column to RMS_parties
RMS_parties <- RMS_parties %>%
  mutate(ADJ_YEAR = if_else(YEAR < 2021, YEAR, if_else(MONTH > 9, YEAR + 1, YEAR))) %>%
  mutate(YEAR_ID = paste(ADJ_YEAR, APPLICATION_NUMBER, sep = "_"))

#Group RMS_parties by YEAR_ID
#the purpose is to catch any instances where more than 1 party ID was
#affiliated with an annual water right report--if so, that's a problem

#Remove diversion data columns--NDD = No Diversion Data
RMS_parties_NDD = select(RMS_parties, -c("AMOUNT", "MONTH", "DIVERSION_TYPE", "YEAR")) %>% unique()

RMS_parties_aggregate = RMS_parties_NDD %>% group_by(YEAR_ID) %>%
  summarise(PartyCount = n()) %>%
  arrange(desc(PartyCount)) #None of the PartyCounts exceed 1; so we have no duplicate party IDs!


stopifnot(max(RMS_parties_aggregate$PartyCount) == 1)


##Look for duplicate reporting----
#These are cases where the year, party ID, diversion type, and AnnualTotal are duplicated
#We will create a new field, PK, short for primary key, that concatenates these fields:
# YEAR, PARTY_ID, DIVERSION_TYPE, AnnualTotal; 
#we will separate the fields with an underscore(_) for legibility

RMS_parties2 <- RMS_parties %>%
  select(-c("APPLICATION_PRIMARY_OWNER", "YEAR_ID", "YEAR")) %>% #Remove extra columns
  filter(AMOUNT > 0) #Filter out all zero-diversion records

#Calculate annual direct diversion and annual diversion to storage amounts per right per year
RMS_parties3 = RMS_parties2 %>% 
  group_by(APPLICATION_NUMBER, ADJ_YEAR, DIVERSION_TYPE) %>%
  summarise(AnnualTotal = sum(AMOUNT), .groups = "keep")

#Inner Join RMS_parties3 to RMS_parties2 to bring back PARTY_ID field
#We need to inner join by multiple columns: #APPL_ID, YEAR, DIVERSION TYPE
RMS_parties4 = inner_join(x = RMS_parties2,
                          y = RMS_parties3, #Includes Annual Totals
                          by = c("APPLICATION_NUMBER", "ADJ_YEAR", "DIVERSION_TYPE")) %>%
  #Add PK_WR column to allow us to remove monthly data
  mutate(PK_WR = paste(ADJ_YEAR, APPLICATION_NUMBER, DIVERSION_TYPE, AnnualTotal, sep = "_")) %>%
  select(-c("MONTH", "AMOUNT")) %>% #Remove month and Amount columns
  unique() %>%
  
  #Add PK column to account for party IDs
  mutate(PK = paste(ADJ_YEAR, PARTY_ID, AnnualTotal, sep = "_"))

#Aggregate by PK
RMS_parties_PK_aggregate = RMS_parties4 %>% group_by(PK) %>%
  summarise(PKCount = n()) %>% #Count PKs 
  filter(PKCount >1) %>% #Look for PKCount > 1
  arrange(desc(PKCount)) #There are 211 duplicate PKs

#Extract duplicate PKs and export to Excel for manual review; the purpose of this
#manual review spreadsheet is to catch instances where the same owner reported the same
#diversions across 2 or more rights; hence "duplicate_reports"
Duplicate_Reports = get_dupes(RMS_parties4, PK)


# Before exporting the table, remove entries that were already manually reviewed by SDA in Fall 2022; duplicate
# primary keys will be removed and this saves SDA from reviewing the same records over and over again;
# primary key is a concatenation of
# Reporting Year ("YEAR"), 
# PARTY_ID (unique ID assigned to owners in eWRIMS)
# DIVERSION_TYPE (Direct Diversion or Diversion to Storage),
# AnnualTotal (sum of direct diversion and diversion to storage in a given reporting year for a given right)

# Newer reporting data will not be removed because even if all the other fields are identical, the YEAR will 
#be the new reporting year, 2023, 2024, and so on.

#A similar protection

if (!is.na(ws$QAQC_DUPLICATE_REPORTING_SPREADSHEET_PATH)) {
  
  reviewDF <- getXLSX(ws = ws, 
                      SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_QAQC_DUPLICATE_REPORTING_SPREADSHEET", 
                      FILEPATH = "QAQC_DUPLICATE_REPORTING_SPREADSHEET_PATH", 
                      WORKSHEET_NAME = "QAQC_DUPLICATE_REPORTING_WORKSHEET_NAME")
  
  
  Duplicate_Reports <- Duplicate_Reports %>%
    filter(!(PK %in% reviewDF$PK))
  
  
  remove(reviewDF)
  
}



# Add review columns as well
Duplicate_Reports <- Duplicate_Reports %>%
  mutate(QAQC_Action_Taken = NA_character_,
         QAQC_Reason = NA_character_)



# Sort the columns too
Duplicate_Reports <- Duplicate_Reports %>%
  arrange(PARTY_ID, ADJ_YEAR, AnnualTotal, APPLICATION_NUMBER, DIVERSION_TYPE)



writexl::write_xlsx(x= Duplicate_Reports, path = paste0("OutputData/", ws$ID, "_Duplicate_Reports_Manual_Review.xlsx"), col_names = TRUE)

print("The Multiple_Owner_Analysis.R script is done running!")



remove(appYears, Duplicate_Reports, RMS_parties, RMS_parties_aggregate,# conn,
       RMS_parties_NDD, RMS_parties_PK_aggregate, RMS_parties2, RMS_parties3, file_path,
       RMS_parties4, selected_columns)