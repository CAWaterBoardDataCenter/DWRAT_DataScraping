#Load libraries----
library(dplyr) #for numerous functions
library(data.table) #for fread function

#Import Raw Data ----

#Import Statistics_FINAL.csv. This is one of the output files of the Priority_Date_Preprocessing.R 
#script; but we just want the unique APPLICATION_NUMBERS; these are just water rights
#in the Russian River.
appYears <- read_csv("IntermediateData/Statistics_FINAL.csv", show_col_types = FALSE) %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>% unique()


#Import ewrims_flat_file_party.csv--this contains party information for all 
#water rights in eWRIMS, not just the Russian River.

  # Filter it down to only "Primary Owner" records and "APPLICATION_ID" values that
  # match "APPLICATION_NUMBER" in 'appYears'
  # Along with that, the "EFFECTIVE_TO_DATE" should be NA (currently active), 
  # or the ownership ended during the dataset's timeframe ("EFFECTIVE_TO_DATE" is between 2017-present)
partyDF <- read_csv("RawData/ewrims_flat_file_party.csv", col_types = cols(.default = col_character())) %>%
  filter(RELATIONSHIP_TYPE == "Primary Owner" & APPLICATION_ID %in% appYears$APPLICATION_NUMBER) %>%
  mutate(EFFECTIVE_TO_YEAR = as.numeric(str_extract(EFFECTIVE_TO_DATE, "[0-9]{4}$"))) %>%
  filter(is.na(EFFECTIVE_TO_DATE) | EFFECTIVE_TO_YEAR >= min(appYears$YEAR))

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
selected_columns <- c("APPL_ID", "YEAR", "APPLICATION_PRIMARY_OWNER", "PARTY_ID")

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



