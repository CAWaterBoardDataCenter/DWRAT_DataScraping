#Load libraries----
require(dplyr) #for numerous functions
require(tidyverse) #for read_csv
require(data.table) #for fread function
require(janitor) # for get_dupes function
require(writexl) # for write_xlsx function
require(RSQLite) # for SQLite queries
require(readxl) # for read_xlsx function

#Import Raw Data ----

#Import Statistics_FINAL.csv. This is one of the output files of the Priority_Date_Preprocessing.R 
#script; but we just want the unique APPLICATION_NUMBERS; these are just water rights
#in the Russian River.
appYears <- read_csv(paste0("IntermediateData/", ws$ID, "_Statistics_FINAL.csv"), show_col_types = FALSE) %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>% unique() %>%
  #Add a YEAR_ID column that concatenates the year and application number
  mutate(YEAR_ID = paste(YEAR, APPLICATION_NUMBER, sep = "_"))


##Water use extended report flat file structural analysis----

#Import the first 10 rows of the water use report extended flat file
# extended <- read_csv("RawData/water_use_report_extended.csv", nrows = 100000)
# extended2 = extended %>%filter(AMOUNT > 0) %>% nrow()
# 
# extended_column_names <- colnames(extended)
# print(extended_column_names)
# 
# write.csv(extended, file = "IntermediateData/water_use_report_extended_subset.csv", 
#           row.names = FALSE)
# 
# #Export extended_column_names to CSV
# write.csv(extended_column_names, file = "IntermediateData/extended_names.csv")


##Import only 7 columns of the water_use_report_extended.csv----
  #APPL_ID is the Water Right ID
  #YEAR is the reporting year
  #MONTH is the calendar month
  #AMOUNT is the monthly amount diverted/used
  #DIVERSION_TYPE categorizes the diversion type as direct diversion, storage, or use
  #APPLICATION_PRIMARY_OWNER is the primary owner in the reporting year
  #PARTY_ID is the Party ID tied to the primary owner in the reporting year

#file_path <- "RawData/water_use_report_extended.csv"
selected_columns <- c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE",
                      "APPLICATION_PRIMARY_OWNER", "PARTY_ID")

#Import only the selected_columns of the water_use_report_extended.csv
RMS_parties <- fread(file = file_path, select = selected_columns)
# conn <- dbConnect(dbDriver("SQLite"), "RawData/water_use_report_extended_subset.sqlite")
# RMS_parties <- dbGetQuery(conn, 
#                           paste0('SELECT DISTINCT ',
#                                  selected_columns %>% paste0('"', ., '"', collapse = ", "),
#                                  ' FROM "Table"',
#                                  ' WHERE "YEAR" > 2016')) 
# dbDisconnect(conn)

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
    mutate(YEAR_ID = paste(YEAR, APPLICATION_NUMBER, sep = "_"))

#Group RMS_parties by YEAR_ID
  #the purpose is to catch any instances where more than 1 party ID was
  #affiliated with an annual water right report--if so, that's a problem

#Remove diversion data columns--NDD = No Diversion Data
RMS_parties_NDD = select(RMS_parties, -c("AMOUNT", "MONTH", "DIVERSION_TYPE"))%>%unique()

RMS_parties_aggregate = RMS_parties_NDD %>% group_by(YEAR_ID) %>%
  summarise(PartyCount = n()) %>%
  arrange(desc(PartyCount)) #None of the PartyCounts exceed 1; so we have no duplicate party IDs!

##Look for duplicate reporting----
  #These are cases where the year, party ID, diversion type, and AnnualTotal are duplicated
  #We will create a new field, PK, short for primary key, that concatenates these fields:
    # YEAR, PARTY_ID, DIVERSION_TYPE, AnnualTotal; 
  #we will separate the fields with an underscore(_) for legibility

RMS_parties2 <- RMS_parties %>%
  select(-c("APPLICATION_PRIMARY_OWNER", "YEAR_ID")) %>% #Remove extra columns
  filter(AMOUNT > 0) #Filter out all zero-diversion records

#Calculate annual direct diversion and annual diversion to storage amounts per right per year
RMS_parties3 = RMS_parties2 %>% 
  group_by(APPLICATION_NUMBER, YEAR, DIVERSION_TYPE) %>%
  summarise(AnnualTotal = sum(AMOUNT), .groups = "keep")

#Inner Join RMS_parties3 to RMS_parties2 to bring back PARTY_ID field
  #We need to inner join by multiple columns: #APPL_ID, YEAR, DIVERSION TYPE
RMS_parties4 = inner_join(x = RMS_parties2,
                          y = RMS_parties3, #Includes Annual Totals
                          by = c("APPLICATION_NUMBER", "YEAR", "DIVERSION_TYPE")) %>%
  #Add PK_WR column to allow us to remove monthly data
  mutate(PK_WR = paste(YEAR, APPLICATION_NUMBER, DIVERSION_TYPE, AnnualTotal, sep = "_")) %>%
  select(-c("MONTH", "AMOUNT")) %>% #Remove month and Amount columns
  unique() %>%
  
  #Add PK column to account for party IDs
  mutate(PK = paste(YEAR, PARTY_ID, DIVERSION_TYPE, AnnualTotal, sep = "_"))

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

if (length(list.files("InputData", pattern = paste0(ws$ID, "_Duplicate_Reports"))) > 0) {
  
  reviewDF <- list.files("InputData", pattern = paste0(ws$ID, "_Duplicate_Reports"), full.names = TRUE) %>%
    tail(1) %>%
    read_xlsx()
  
  
  Duplicate_Reports <- Duplicate_Reports %>%
    filter(!(PK %in% reviewDF$Primary_Key))
  
}



writexl::write_xlsx(x= Duplicate_Reports, path = paste0("OutputData/", ws$ID, "_Duplicate_Reports_Manual_Review.xlsx"), col_names = TRUE)

print("The Multiple_Owner_Analysis.R script is done running!")



remove(appYears, conn, Duplicate_Reports, RMS_parties, RMS_parties_aggregate,
       RMS_parties_NDD, RMS_parties_PK_aggregate, RMS_parties2, RMS_parties3,
       RMS_parties4, selected_columns, reviewDF)