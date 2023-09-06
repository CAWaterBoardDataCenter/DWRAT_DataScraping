#Load libraries----
library(dplyr) #for numerous functions
library(data.table) #for fread function

#Import Raw Data ----

#Import Statistics_FINAL.csv. This is one of the output files of the Priority_Date_Preprocessing.R 
#script; but we just want the unique APPLICATION_NUMBERS; these are just water rights
#in the Russian River.
appYears <- read_csv("IntermediateData/Statistics_FINAL.csv", show_col_types = FALSE) %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>% unique() %>%

  #Add a YEAR_ID column that concatenates the year and application number
  mutate(YEAR_ID = paste(YEAR, APPLICATION_NUMBER, sep = "_"))


#Import the first 10 rows of the water use report extended flat file
extended <- read.csv("RawData/water_use_report_extended.csv", nrows = 10)
extended_column_names <- colnames(extended)
print(extended_column_names)

#Export extended_column_names to CSV
write.csv(extended_column_names, file = "IntermediateData/extended_names.csv")


#Import only 4 columns of the water_use_report_extended.csv----
  #Column 2 = APPL_ID, character
  #Column 3 = YEAR, integer
  #Column 27 = APPLICATION_PRIMARY_OWNER, character
  #Column 65 = PARTY_ID, integer

file_path <- "RawData/water_use_report_extended.csv"
selected_columns <- c("APPL_ID", "YEAR", "AMOUNT", "DIVERSION_TYPE",
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
RMS_parties_NDD = select(RMS_parties, -c("AMOUNT", "DIVERSION_TYPE")) %>%unique()

RMS_parties_aggregate = RMS_parties_NDD %>% group_by(YEAR_ID) %>%
  summarise(PartyCount = n()) %>%
  arrange(desc(PartyCount)) #None of the PartyCounts exceed 1; so we have no
  #duplicate party IDs!

#Look for cases where the year, party ID, diversion type, and amount are duplicated----
  #We will create a new field, PK, short for primary key, that concatenates these fields:
    # YEAR, PARTY_ID, DIVERSION_TYPE, AMOUNT; 
  #we will separate the fields with an underscore(_) for legibility
    #



