# For relatively new appropriative water rights, if they have no amount reported for a year,
# replace their values such that the year's total is the face value amount


library(tidyverse)
library(readxl)
library(data.table)
library(openxlsx)



faceValSub <- function (inputDF, yearRange = 2021:2023) {
  
  
  # If 'yearRange' is outside of the range of 'inputDF', make no changes
  if (sum(yearRange %in% inputDF$YEAR) == 0) {
    return(inputDF)
  }
  
  
  
  # Read in data from "water_use_report_extended.csv"
  # Filter it to APPLICATION_NUMBER values in 'inputDF'
  extendedDF <- fread("RawData/water_use_report_extended.csv",
                      select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE",
                                 "FACE_VALUE_AMOUNT", "FACE_VALUE_UNITS", "EFFECTIVE_DATE",
                                 "WATER_RIGHT_TYPE", "DIRECT_DIV_SEASON_START",
                                 "STORAGE_SEASON_START", "DIRECT_DIV_SEASON_END", "STORAGE_SEASON_END")) %>%
    filter(APPLICATION_NUMBER %in% inputDF$APPLICATION_NUMBER)

  
  
  # Filter 'extendedDF' to appropriative rights that have an "EFFECTIVE_DATE" within 'yearRange'
  extendedDF <- extendedDF %>%
    filter(grepl("^Appro", WATER_RIGHT_TYPE)) %>%
    mutate(YEAR = as.numeric(str_extract(EFFECTIVE_DATE, "[0-9]{4}$"))) %>%
    filter(YEAR %in% yearRange) %>%
    filter(DIVERSION_TYPE != "USE")
  
  
  
  # Get a list of unique application numbers next
  appVec <- extendedDF$APPLICATION_NUMBER %>%
    unique() %>% sort()
  
  
  
  # If 'appVec' is empty, return 'inputDF' with no changes
  if (length(appVec) == 0) {
    return(inputDF)
  }
  
  
  
  # To help with processing the data, create some new columns in 'extendedDF'
  # Extract the starting and ending months for the DIRECT DIVERSION and STORAGE seasons
  # Then, create a column of lists that contain months within each right's diversion seasons
  extendedDF <- extendedDF %>%
    rowwise() %>%
    mutate(DD_START = DIRECT_DIV_SEASON_START %>%
             str_split("/") %>% unlist() %>% pluck(1) %>% 
             str_replace("^$", "0") %>% as.numeric(),
           DD_END = DIRECT_DIV_SEASON_END %>%
             str_split("/") %>% unlist() %>% pluck(1) %>% 
             str_replace("^$", "0") %>% as.numeric(),
           DD_RANGE = if_else(DD_START == 0,
                              list(""),
                              list(seq(from = DD_START,
                                         to = DD_END))),
           STOR_START = STORAGE_SEASON_START %>%
             str_split("/") %>% unlist() %>% pluck(1) %>% 
             str_replace("^$", "0") %>% as.numeric(),
           STOR_END = STORAGE_SEASON_END %>%
             str_split("/") %>% unlist() %>% pluck(1) %>% 
             str_replace("^$", "0") %>% as.numeric(),
           STOR_RANGE = if_else(STOR_START == 0,
                              list(""),
                              list(seq(from = STOR_START,
                                       to = STOR_END)))) %>%
    ungroup()
  
  
  
  # Otherwise, iterate through 'appVec'
  for (i in 1:length(appVec)) {
    
    
    # Next, iterate through each of the years in 'yearRange'
    for (j in 1:length(yearRange)) {
      
      
      # If this right doesn't have data for this iteration's year, skip it
      if (inputDF %>%
          filter(APPLICATION_NUMBER == appVec[i] & YEAR == yearRange[j]) %>%
          nrow() == 0) {
        next
      }
      
      
      
      # If this right already has non-zero data for this year, skip it
      if (inputDF %>%
          filter(APPLICATION_NUMBER == appVec[i] & YEAR == yearRange[j]) %>%
          summarize(TOTAL = sum(AMOUNT, na.rm = TRUE)) %>%
          select(TOTAL) %>% unlist() > 0) {
        next
      }
      
      
      
      # Otherwise, the right's face-value amount will be assigned to one month during their diversion season
      # The default choice is a month in their DIRECT_DIVERSION season, but if they have none,
      # a month from the STORAGE season will be used instead
      
      
      
      # First, get the face value
      faceValue <- extendedDF %>%
        filter(APPLICATION_NUMBER == appVec[i] & FACE_VALUE_UNITS == "Acre-feet per Year") %>%
        select(FACE_VALUE_AMOUNT) %>% unique() %>% unlist() %>% as.vector()
      
      
      
      # Error Check (exactly one value should have been found)
      stopifnot(length(faceValue) == 1)
      
      
      
      # Check whether they have a DIRECT DIVERSION season
      if (extendedDF %>%
          filter(APPLICATION_NUMBER == appVec[i] & YEAR == yearRange[j]) %>%
          select(DD_RANGE) %>% unique() %>% unlist() %>% length() > 1) {
        
        
        # Get a random month from the right's DIRECT_DIVERSION season
        validMonth <- extendedDF %>%
          filter(APPLICATION_NUMBER == appVec[i] & YEAR == yearRange[j]) %>%
          select(DD_RANGE) %>% unique() %>% unlist() %>% as.vector() %>% sample(1)
        
        
        
        # Assign the face value to this month
        inputDF[inputDF$APPLICATION_NUMBER == appVec[i] &
                  inputDF$YEAR == yearRange[j] &
                  inputDF$DIVERSION_TYPE == "DIRECT" & 
                  inputDF$MONTH == validMonth, ]$AMOUNT <- faceValue
        
        
        
      # Check their STORAGE season next
      } else if (extendedDF %>%
                 filter(APPLICATION_NUMBER == appVec[i] & YEAR == yearRange[j]) %>%
                 select(STOR_RANGE) %>% unique() %>% unlist() %>% length() > 1) {
        
        
        # Get a random month from the right's STORAGE season
        validMonth <- extendedDF %>%
          filter(APPLICATION_NUMBER == appVec[i] & YEAR == yearRange[j]) %>%
          select(STOR_RANGE) %>% unique() %>% unlist() %>% as.vector() %>% sample(1)
        
        
        
        # Assign the face value to this month
        inputDF[inputDF$APPLICATION_NUMBER == appVec[i] &
                  inputDF$YEAR == yearRange[j] &
                  inputDF$DIVERSION_TYPE == "STORAGE" & 
                  inputDF$MONTH == validMonth, ]$AMOUNT <- faceValue
        
        
      # Throw an error otherwise
      } else {
        stop(paste0("Right ", appVec[i], " has no DIRECT season and no STORAGE season"))
      }
      
      
    } # End of 'j' loop through 'yearRange'
    
  } # End of 'i' loop through 'appVec'
  
  
  
  # Return 'inputDF' after these changes
  return(inputDF)
  
}