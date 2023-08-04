# For each reporting year, identify a water right's primary owner (by "PARTY_ID")

# This script relies on 'ewrims_flat_file_party.csv' and 'Statistics_FINAL.csv'


#### Dependencies ####

library(tidyverse)

#### Functions ####

mainProcedure <- function () {
  
  # Read in 'Statistics_FINAL.csv' and create a variable of unique application-year pairs
  appYears <- read_csv("IntermediateData/Statistics_FINAL.csv", show_col_types = FALSE) %>%
    select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>% unique()
  
  
  # Also read in 'ewrims_flat_file_party.csv'
  # Filter it down to only "Primary Owner" records and "APPLICATION_ID" values that
  # match "APPLICATION_NUMBER" in 'appYears'
  # Along with that, the "EFFECTIVE_TO_DATE" should be NA (currently active), 
  # or the ownership ended during the dataset's timeframe ("EFFECTIVE_TO_DATE" is between 2017-present)
  partyDF <- read_csv("RawData/ewrims_flat_file_party.csv", col_types = cols(.default = col_character())) %>%
    filter(RELATIONSHIP_TYPE == "Primary Owner" & APPLICATION_ID %in% appYears$APPLICATION_NUMBER) %>%
    mutate(EFFECTIVE_TO_YEAR = as.numeric(str_extract(EFFECTIVE_TO_DATE, "[0-9]{4}$"))) %>%
    filter(is.na(EFFECTIVE_TO_DATE) | EFFECTIVE_TO_YEAR >= min(appYears$YEAR))
  
  
  
  # Ideally, there should only be one row per application number in 'partyDF'
  # But that will not be the case
  
  # There are two issues that need to be addressed:
  # (1) Rights that changed owners over the years
  #     "EFFECTIVE_TO_DATE" will be not NA for all except one record
  #     The end year in "EFFECTIVE_TO_DATE" **MINUS ONE** will be the last year 
  #     where that owner is considered the primary owner (because the report
  #     for that end year will be filled by the new owner in the subsequent year)
  # 
  # (2) Simultaneously active water rights
  #     More than one owner may be listed as an active owner simultaneously
  #     Manual review would be needed
  
  
  # Extract a subset of the columns in 'partyDF'
  partyDF <- partyDF %>%
    select(APPLICATION_NUMBER, PARTY_ID, EFFECTIVE_FROM_DATE, EFFECTIVE_TO_DATE) %>%
    mutate(PARTY_ID = as.numeric(PARTY_ID)) %>%
    unique()
  
  
  # First check for Issue (2)
  # At the time of this script's creation (08/03/2023), this issue is not relevant
  # (A029789 and D032313 each have duplicates, but "PARTY_ID" is the same in both rows)
  if (partyDF %>% 
      group_by(APPLICATION_NUMBER) %>%
      summarize(NA_Count = sum(is.na(EFFECTIVE_TO_DATE))) %>%
      filter(NA_Count > 1) %>%
      nrow() > 0) {
    
    stop("Manual review is needed (please use 'QAQC_Manual_Review.xlsx'). Though, the code for this procedure has not been implemented yet.")
    
  }
  
  
  # Next, focus on Issue (1)
  # Check for applications with multiple rows in 'partyDF'
  freq <- table(partyDF$APPLICATION_NUMBER)
  
  extraApp <- names(freq)[freq > 1]
  
  
  
  # Separate the rows with and without this issue
  singleRowDF <- partyDF %>% filter(!(APPLICATION_NUMBER %in% extraApp))
  
  multiRowDF <- partyDF %>% filter(APPLICATION_NUMBER %in% extraApp)
  
  
  
  # Join the data in 'singleRowDF' to 'appYears'
  appYears <- appYears %>%
    left_join(singleRowDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  # Next, focus on 'multiRowDF'
  
  # Get preliminary values for "START_YEAR" and "END_YEAR" using
  # "EFFECTIVE_FROM_DATE" and "EFFECTIVE_TO_DATE"
  # (Because of the way "END_YEAR" is defined, it does not capture well rightholders
  #  that held a right for less than a year; therefore, an extra definition is used)
  multiRowDF <- multiRowDF %>%
    mutate(START_YEAR = as.numeric(str_extract(EFFECTIVE_FROM_DATE, "[0-9]{4}$")),
           END_YEAR = as.numeric(str_extract(EFFECTIVE_TO_DATE, "[0-9]{4}$")),
           END_YEAR = if_else(!is.na(END_YEAR) & END_YEAR < START_YEAR, START_YEAR, END_YEAR))
  
  
  # For each application number,
  # The final "END_YEAR" will be NA; change that to the maximum year in 'appYears'
  multiRowDF <- multiRowDF %>%
    mutate(#START_YEAR = if_else(START_YEAR < min(appYears$YEAR), min(appYears$YEAR), START_YEAR),
           END_YEAR = if_else(is.na(END_YEAR), max(appYears$YEAR), END_YEAR))
  
  
  
  # Write both 'appYears' and 'multiRowDF' to files
  appYears %>%
    write.xlsx("IntermediateData/WATER_RIGHT_OWNERSHIP_INCOMPLETE.xlsx", overwrite = TRUE)
  
  
  multiRowDF %>%
    write.xlsx("IntermediateData/WATER_RIGHTS_WITH_MULTIPLE_OWNERS.xlsx", overwrite = TRUE)
  
  
  #### STOP HERE ####
  # The next section of code is not ready for use
  # A proper procedure hasn't been developed yet
  
  stop("Procedure incomplete")
  
  
  
  # Create a new data frame from 'multiRowDF' 
  # For each of the years in 'appYears' collect corresponding rows from 'multiRowDF' 
  # that contain that iteration's year within their "START_YEAR" and "END_YEAR"
  years <- appYears$YEAR %>% unique() %>% sort()
  
  
  for (i in 1:length(years)) {
    
    tempDF <- multiRowDF %>%
      filter(START_YEAR <= years[i] & END_YEAR >= years[i]) %>%
      mutate(YEAR = years[i])
    
    
    stopifnot(sum(table(tempDF$APPLICATION_NUMBER) > 1) == 0)
    
    
    if (i == 1) {
      compiledDF <- tempDF
    } else {
      compiledDF <- rbind(compiledDF, tempDF)
    }
    
    
  }
  
  
  
    
  
}



#### Script Execution ####

mainProcedure()