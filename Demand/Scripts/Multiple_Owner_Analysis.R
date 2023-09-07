#Load libraries----
library(dplyr) #for numerous functions
library(data.table) #for fread function
library(janitor) # for get_dupes function

#Import Raw Data ----

#Import Statistics_FINAL.csv. This is one of the output files of the Priority_Date_Preprocessing.R 
#script; but we just want the unique APPLICATION_NUMBERS; these are just water rights
#in the Russian River.
appYears <- read_csv("IntermediateData/Statistics_FINAL.csv", show_col_types = FALSE) %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>% unique() %>%

  #Add a YEAR_ID column that concatenates the year and application number
  mutate(YEAR_ID = paste(YEAR, APPLICATION_NUMBER, sep = "_"))


#Import the first 10 rows of the water use report extended flat file
extended <- read.csv("RawData/water_use_report_extended.csv", nrows = 100000)
extended2 = extended %>%filter(AMOUNT > 0) %>% nrow()

extended_column_names <- colnames(extended)
print(extended_column_names)

write.csv(extended, file = "IntermediateData/water_use_report_extended_subset.csv", 
          row.names = FALSE)

#Export extended_column_names to CSV
write.csv(extended_column_names, file = "IntermediateData/extended_names.csv")


#Import only 4 columns of the water_use_report_extended.csv----
  #Column 2 = APPL_ID, character
  #Column 3 = YEAR, integer
  #Column 27 = APPLICATION_PRIMARY_OWNER, character
  #Column 65 = PARTY_ID, integer

file_path <- "RawData/water_use_report_extended.csv"
selected_columns <- c("APPL_ID", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE",
                      "APPLICATION_PRIMARY_OWNER", "PARTY_ID")

#Import only the selected_columns of the water_use_report_extended.csv
RMS_parties <- fread(file = file_path, select = selected_columns)

#Filter to just 2017 
RMS_parties <- filter(RMS_parties, YEAR >= 2017)

#Remove duplicate YEAR + APPL_ID combos;we just want one record
#per water right per year
RMS_parties <- unique(RMS_parties) %>% data.frame()

#Filter RMS_parties to just the Russian River
RMS_parties <- RMS_parties %>%
  filter(APPL_ID %in% appYears$APPLICATION_NUMBER)

#Export RMS_parties to Excel for manual analysis----
#write.xlsx(x = RMS_parties, file = "IntermediateData/RMS_parties.xlsx")

#Look for duplicate party IDs ----
#Add a Year_Right column to RMS_parties
RMS_parties <- RMS_parties %>%
  mutate(YEAR_ID = paste(YEAR, APPL_ID, sep = "_"))

#Group RMS_parties by YEAR_ID
  #the purpose is to catch any instances where more than 1 party ID was
  #affiliated with an annual water right report--if so, that's a problem

#Remove diversion data columns--NDD = No Diversion Data
RMS_parties_NDD = select(RMS_parties, -c("AMOUNT", "MONTH", "DIVERSION_TYPE")) %>%unique()

RMS_parties_aggregate = RMS_parties_NDD %>% group_by(YEAR_ID) %>%
  summarise(PartyCount = n()) %>%
  arrange(desc(PartyCount)) #None of the PartyCounts exceed 1; so we have no
  #duplicate party IDs!

#Look for cases where the year, party ID, diversion type, and AnnualTotal are duplicated----
  #We will create a new field, PK, short for primary key, that concatenates these fields:
    # YEAR, PARTY_ID, DIVERSION_TYPE, AnnualTotal; 
  #we will separate the fields with an underscore(_) for legibility
    #

RMS_parties2 <- RMS_parties %>%
  select(-c("APPLICATION_PRIMARY_OWNER", "YEAR_ID")) %>% #Remove extra columns
  filter(AMOUNT > 0) #Filter out all zero-diversion records


RMS_parties3 = RMS_parties2 %>% 
  group_by(APPL_ID, YEAR, DIVERSION_TYPE) %>%
  summarise(AnnualTotal = sum(AMOUNT), .groups = "keep")

#Inner Join RMS_parties2 to RMS_parties by multiple columns
  #APPL_ID, YEAR, DIVERSION TYPE
RMS_parties4 = inner_join(x = RMS_parties2, 
                          y = RMS_parties3,
                          by = c("APPL_ID", "YEAR", "DIVERSION_TYPE")) %>%
  
              #Add PK_WR column to allow us to remove monthly data
              mutate(PK_WR = paste(YEAR, APPL_ID, DIVERSION_TYPE, AnnualTotal, sep = "_")) %>%
              select(-c("MONTH", "AMOUNT")) %>% #Remove month and Amount columns
              unique() %>%
              #Add PK column to account for party IDs
              mutate(PK = paste(YEAR, PARTY_ID, DIVERSION_TYPE, AnnualTotal, sep = "_"))

#write.xlsx(x = RMS_parties4, file = "IntermediateData/RMS_parties4.xlsx")

#Extract duplicate PKs and export to Excel for manual review
Duplicate_Reports = get_dupes(RMS_parties4, PK)
write.xlsx(x= Duplicate_Reports, file = "IntermediateData/Duplicate_Reports.xlsx")

#Aggregate by PK
RMS_parties_PK_aggregate = RMS_parties4 %>% group_by(PK) %>%
  summarise(PKCount = n()) %>%
  arrange(desc(PKCount))



