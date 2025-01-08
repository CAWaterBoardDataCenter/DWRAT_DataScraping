# Looking for potential unit conversion errors based on ratios between each water
# right's annual total and their face-value amount or initial diversion amount
# Also, compare the annual total to the average and median annual total for each right



#### Dependencies ####


require(tidyverse)
require(openxlsx)
require(readxl)
require(data.table)


#### Script Procedure ####


flagUnitErrors <- function () {
  
  # The main body of the script
  
  
  
  source("Scripts/Watershed_Selection.R")
  source("Scripts/Dataset_Year_Range.R")
  
  
  
  # Read in the flag table
  flagDF <- paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv") %>%
    fread()
  
  
  
  # Extract columns related to the RMS report volumes
  reportDF <- flagDF %>%
    select(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE, AMOUNT, PARTY_ID) %>%
    unique()
  
  
  
  
  # Create a table of monthly "AMOUNT" values for each "DIVERSION_TYPE"
  # "APPLICATION_NUMBER" and "YEAR" will be specified in each row
  # There will be separate columns for each month and diversion type
  # Use a separate function for this
  monthlyDF <- monthlyUseValues(reportDF)
  
  
  
  # Get the annual direct diversion for each application and year
  # Add that column to 'monthlyDF' with the name "ANNUAL_DIRECT"
  monthlyDF <- monthlyDF %>%
    rowwise() %>%
    mutate(ANNUAL_DIRECT = valAdd(JAN_DIRECT_DIVERSION, FEB_DIRECT_DIVERSION, 
                                  MAR_DIRECT_DIVERSION, APR_DIRECT_DIVERSION, MAY_DIRECT_DIVERSION, 
                                  JUN_DIRECT_DIVERSION, JUL_DIRECT_DIVERSION, AUG_DIRECT_DIVERSION, 
                                  SEP_DIRECT_DIVERSION, OCT_DIRECT_DIVERSION, NOV_DIRECT_DIVERSION, 
                                  DEC_DIRECT_DIVERSION)) %>%
    ungroup()
  
  
  
  # Do the same for the storage values (call the column "ANNUAL_STORAGE")
  monthlyDF <- monthlyDF %>%
    rowwise() %>%
    mutate(ANNUAL_STORAGE = valAdd(JAN_STORAGE_DIVERSION, FEB_STORAGE_DIVERSION, 
                                   MAR_STORAGE_DIVERSION, APR_STORAGE_DIVERSION, MAY_STORAGE_DIVERSION, 
                                   JUN_STORAGE_DIVERSION, JUL_STORAGE_DIVERSION, AUG_STORAGE_DIVERSION, 
                                   SEP_STORAGE_DIVERSION, OCT_STORAGE_DIVERSION, NOV_STORAGE_DIVERSION, 
                                   DEC_STORAGE_DIVERSION)) %>%
    ungroup()
  
  
  
  # Next, add a calendar/water year sum of "ANNUAL_DIRECT" and "ANNUAL_STORAGE"
  monthlyDF <- monthlyDF %>%
    mutate(YEAR_TOTAL = if_else(is.na(ANNUAL_DIRECT),
                                ANNUAL_STORAGE,
                                if_else(is.na(ANNUAL_STORAGE), ANNUAL_DIRECT, ANNUAL_DIRECT + ANNUAL_STORAGE)))
  
  
  
  # The first set of flags will be based on the face value and initial diversion amounts
  # These unit conversion error flags will be added to 'flagDF' in a separate function
  flagDF <- flagDF %>%
    faceValAndIniDivFlags(monthlyDF)
  
  
  
  # The next set of unit conversion error flags will be based on the average and median
  # annual volume for each water right
  # There is a separate function for that procedure as well
  flagDF <- flagDF %>%
    avgAndMedFlags(monthlyDF)
  
  
  
  # Save the updated 'flagDF'
  write_csv(flagDF,
            paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv"))  
  
  
  
  # Save 'monthlyDF' to a file too for use in other scripts
  write_csv(monthlyDF,
            paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Intermediate_Flow_Volumes.csv"))
  
  
  
  # Output a message to the console
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



monthlyUseValues <- function (statDF) {
  
  # Create a data frame with a water volume specified for each use type and month
  
  
  # Define variables that contain the different months and use types
  # (Note: The use type "Combined (Direct + Storage)" is ignored in this module)
  monthNames <- month.abb %>% toupper()
  
  useTypes <- unique(statDF$DIVERSION_TYPE) %>%
    sort() %>%
    str_subset("Combined ", negate = TRUE) %>%
    str_replace("^USE$", "REPORTED_USE")
  
  
  
  # Iterate through the use types and months
  # Create a new data frame with this volume information
  for (i in 1:length(useTypes)) {
    
    for (j in 1:length(monthNames)) {
      
      # Filter 'statDF' to this iteration's month and use type
      # Then, sum the values in the "AMOUNT" column
      # Save that result to a temporary data frame
      tempDF <- statDF %>%
        filter(DIVERSION_TYPE == useTypes[i] & MONTH == j) %>%
        select(APPLICATION_NUMBER, YEAR, AMOUNT) %>%
        group_by(APPLICATION_NUMBER, YEAR) %>%
        summarize(!! paste0(monthNames[j], "_",
                            useTypes[i], "_DIVERSION") :=
                    sum(AMOUNT, na.rm = TRUE), .groups = "keep")
      
      
      # NOTE
      # The use of "!!" and ":=" inside summarize() allows a string to be used
      # as a column name
      
      
      # If this is the first iteration of the loops
      if (i == 1 && j == 1) {
        
        # Define the main table DF with 'tempDF'
        useDF <- tempDF
        
        
        # Otherwise, join 'tempDF' to 'useDF'
      } else {
        
        useDF <- useDF %>%
          full_join(tempDF, by = c("APPLICATION_NUMBER", "YEAR"),
                    relationship = "one-to-one")
        
      }
      
    } # End of loop j
    
  } # End of loop i
  
  
  
  # Return 'useDF'
  return(useDF)
  
}



valAdd <- function (...) {
  
  # Add together a variable number of values (denoted by "...")
  # This function is a wrapper of sum() that includes a special exception
  # If every value provided to 'valAdd' is NA, then NA will be returned
  # Otherwise, sum() will be used as normal
  
  
  
  # Place all values together in a single vector
  vec <- c(...)
  
  
  
  # If every value in 'vec' is NA, return NA
  if (sum(is.na(vec)) == length(vec)) {
    return(NA_real_)
  }
  
  
  
  # Error Check
  stopifnot(is.numeric(vec))
  
  
  
  # If there are non-NA values in 'vec', sum together the elements in 'vec'
  # (while removing NA elements)
  return(sum(vec, na.rm = TRUE))
  
}



faceValAndIniDivFlags <- function (flagDF, monthlyDF) {
  
  # Create unit conversion error flags based on the face value and initial diversion volumes
  # 'flagDF' will have new columns added for these flags
  
  
  
  # From the extended CSV, get the face value information
  # (And initial diversion amounts for statements)
  fvDF <- fread("RawData/Snowflake_water_use_report_extended.csv",
                select = c("APPLICATION_NUMBER", "INI_REPORTED_DIV_AMOUNT", "INI_REPORTED_DIV_UNIT",
                           "FACE_VALUE_AMOUNT", "FACE_VALUE_UNITS")) %>%
    unique()
  
  
  
  # After that, link the data in 'fvDF' to 'monthlyDF'
  # The join will be based on "APPLICATION_NUMBER" 
  # Each application number should appear only once in 'fvDF' 
  # (and 'monthlyDF' will have multiple rows per application number)
  monthlyDF <- monthlyDF %>%
    left_join(fvDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  
  # Check the values that appear in "INI_REPORTED_DIV_UNIT"
  stopifnot(length(unique(monthlyDF$INI_REPORTED_DIV_UNIT)) == 3)
  stopifnot("Gallons" %in% monthlyDF$INI_REPORTED_DIV_UNIT)
  stopifnot(monthlyDF %>%
              filter(is.na(INI_REPORTED_DIV_UNIT) & !is.na(INI_REPORTED_DIV_AMOUNT)) %>%
              nrow() == 0)
  
  
  
  # Create a column that converts "Initial_Reported_Diversion" to acre-feet per year
  # if its units are reported as "Gallons"
  # (There are 325,851 gallons in 1 AF)
  monthlyDF <- monthlyDF %>%
    mutate(IniDiv_Converted_to_AF = if_else(!is.na(INI_REPORTED_DIV_UNIT) & INI_REPORTED_DIV_UNIT == "Gallons",
                                            INI_REPORTED_DIV_AMOUNT / 325851,
                                            INI_REPORTED_DIV_AMOUNT))
  
  
  
  # Check the units that appear in "FACE_VALUE_UNITS"
  stopifnot(length(unique(monthlyDF$FACE_VALUE_UNITS)) == 1)
  stopifnot(unique(monthlyDF$FACE_VALUE_UNITS) == "Acre-feet per Year")
  stopifnot(!anyNA(monthlyDF$FACE_VALUE_AMOUNT))
  
  
  
  # Next calculate "Diversion_as_Percent_of_FV"
  # This is "YEAR_TOTAL" divided by "FACE_VALUE_AMOUNT"
  # (If "FACE_VALUE_AMOUNT" is NA, this calculation will produce NA too)
  monthlyDF <- monthlyDF %>%
    mutate(Diversion_as_Percent_of_FV = YEAR_TOTAL / FACE_VALUE_AMOUNT)
  
  
  
  # Repeat the above step with "IniDiv_Converted_to_AF" instead of "FACE_VALUE_AMOUNT"
  monthlyDF <- monthlyDF %>%
    mutate(Diversion_as_Percent_of_IniDiv = YEAR_TOTAL / IniDiv_Converted_to_AF,
           Amount_over_IniDiv = if_else(!is.na(IniDiv_Converted_to_AF) & IniDiv_Converted_to_AF > 0,
                                        if_else(Diversion_as_Percent_of_IniDiv > 1,
                                                YEAR_TOTAL - IniDiv_Converted_to_AF,
                                                0),
                                        NA_real_))
  
  
  
  # The next group of columns check "YEAR_TOTAL" for different units
  # That column's values are assumed to be different from AF/year
  # The options are "gallons", "gallons per minute", "gallons per day", 
  # and "cubic feet per second"
  # Attempt to convert the values into "AF/year" assuming one of the previous unit options
  # In 1 AF, there are 325,851 gal or 43,559.9 ft^3
  # In 1 yr, there are 365 days, 525,600 min, or 31,536,000 s
  monthlyDF <- monthlyDF %>%
    mutate(Annual_Diversion_if_reported_in_Gallons = YEAR_TOTAL / 325851,
           Annual_Diversion_if_reported_in_GPM = YEAR_TOTAL / 325851 * 525600,
           Annual_Diversion_if_reported_in_GPD = YEAR_TOTAL / 325851 * 365,
           Annual_Diversion_if_reported_in_CFS = YEAR_TOTAL / 43559.9 * 31536000)
  
  
  
  # Add columns related to reviewing records
  monthlyDF <- monthlyDF %>%
    mutate(QAQC_Action_Taken = NA_character_,
           QAQC_Reason = NA_character_)
  
  
  
  # Select columns from 'monthlyDF' and create a spreadsheet with diversion data
  # monthlyDF %>%
  #   select(APPLICATION_NUMBER, YEAR, JAN_DIRECT_DIVERSION,
  #          FEB_DIRECT_DIVERSION, MAR_DIRECT_DIVERSION,
  #          APR_DIRECT_DIVERSION, MAY_DIRECT_DIVERSION,
  #          JUN_DIRECT_DIVERSION, JUL_DIRECT_DIVERSION,
  #          AUG_DIRECT_DIVERSION, SEP_DIRECT_DIVERSION,
  #          OCT_DIRECT_DIVERSION, NOV_DIRECT_DIVERSION,
  #          DEC_DIRECT_DIVERSION, JAN_STORAGE_DIVERSION,
  #          FEB_STORAGE_DIVERSION, MAR_STORAGE_DIVERSION,
  #          APR_STORAGE_DIVERSION, MAY_STORAGE_DIVERSION,
  #          JUN_STORAGE_DIVERSION, JUL_STORAGE_DIVERSION,
  #          AUG_STORAGE_DIVERSION, SEP_STORAGE_DIVERSION,
  #          OCT_STORAGE_DIVERSION, NOV_STORAGE_DIVERSION,
  #          DEC_STORAGE_DIVERSION) %>%
  #   write.xlsx(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Monthly_Diversions.xlsx"),
  #              overwrite = TRUE)
  
  
  
  # Output information about the face value and initial diversion amount as well
  # monthlyDF %>%
  #   select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT, 
  #          FACE_VALUE_AMOUNT, FACE_VALUE_UNITS, IniDiv_Converted_to_AF) %>%
  #   unique() %>%
  #   write.xlsx(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_ExpectedDemand_FV.xlsx"), overwrite = TRUE)
  
  
  
  # Some rights lack both a face value and an initial diversion amount
  # Note this in 'flagDF'
  if (monthlyDF %>%
      select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, FACE_VALUE_AMOUNT) %>% 
      filter(is.na(INI_REPORTED_DIV_AMOUNT) & FACE_VALUE_AMOUNT == 0) %>% unique() %>%
      nrow() > 0) {
    
    
    
    # Get the water rights where this issue occurs
    issueRights <- monthlyDF %>%
      select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, FACE_VALUE_AMOUNT) %>% 
      filter(is.na(INI_REPORTED_DIV_AMOUNT) & FACE_VALUE_AMOUNT == 0) %>% unique() %>%
      select(APPLICATION_NUMBER) %>% unlist()
    
    
    
    # Update 'flagDF' accordingly
    flagDF <- flagDF %>%
      mutate(NO_INITIAL_DIVERSION_AMOUNT_AND_NO_FACE_VALUE_AMOUNT = APPLICATION_NUMBER %in% issueRights)
    
  }
  
  
  
  # Add flags related to extreme values next
  # Compare "YEAR_TOTAL" to the face-value amount and initial diversion amount
  # Make flags based on this (more than 100 times greater or smaller than the face value or initial diversion)
  diversionFlags <- monthlyDF %>%
    select(APPLICATION_NUMBER, YEAR, YEAR_TOTAL,
           FACE_VALUE_AMOUNT, IniDiv_Converted_to_AF, 
           Diversion_as_Percent_of_FV, Diversion_as_Percent_of_IniDiv) %>%
    unique() %>%
    mutate(YEAR_TOTAL_MORE_THAN_100_TIMES_GREATER_THAN_FACE_VALUE = Diversion_as_Percent_of_FV > 100 & FACE_VALUE_AMOUNT > 0,
           YEAR_TOTAL_MORE_THAN_100_TIMES_SMALLER_THAN_FACE_VALUE = Diversion_as_Percent_of_FV < 0.01 & Diversion_as_Percent_of_FV > 0 & FACE_VALUE_AMOUNT > 0,
           YEAR_TOTAL_MORE_THAN_100_TIMES_GREATER_THAN_INITIAL_DIVERSION = !is.na(IniDiv_Converted_to_AF) & Diversion_as_Percent_of_IniDiv > 100 & IniDiv_Converted_to_AF > 0,
           YEAR_TOTAL_MORE_THAN_100_TIMES_SMALLER_THAN_INITIAL_DIVERSION = !is.na(IniDiv_Converted_to_AF) & Diversion_as_Percent_of_IniDiv < 0.01 & Diversion_as_Percent_of_IniDiv > 0 & IniDiv_Converted_to_AF > 0) %>%
    select(-YEAR_TOTAL, -FACE_VALUE_AMOUNT, -IniDiv_Converted_to_AF, 
           -Diversion_as_Percent_of_FV, -Diversion_as_Percent_of_IniDiv)
  
  
  
  # Append these flags to 'flagDF'
  flagDF <- flagDF %>%
    left_join(diversionFlags, by = c("APPLICATION_NUMBER", "YEAR"), 
              relationship = "many-to-one")
  
  
  
  # Return 'flagDF'
  return(flagDF)
  
}



avgAndMedFlags <- function (flagDF, monthlyDF) {
  
  
  
  # Create a summary tibble with median and average values of "YEAR_TOTAL" 
  # for each unique "APPLICATION_NUMBER"
  summaryDF <- monthlyDF %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(MEDIAN_TOTAL_AF = median(YEAR_TOTAL, na.rm = TRUE),
              Q1_TOTAL_AF = quantile(YEAR_TOTAL, na.rm = TRUE)[2], # Not used currently for analysis
              IQR_TOTAL_AF = IQR(YEAR_TOTAL, na.rm = TRUE), # Not used currently for analysis
              Q3_TOTAL_AF = quantile(YEAR_TOTAL, na.rm = TRUE)[4], # Not used currently for analysis
              AVG_TOTAL_AF = mean(YEAR_TOTAL, na.rm = TRUE),
              SD_TOTAL_AF = sd(YEAR_TOTAL, na.rm = TRUE))
  
  
  
  # Append these values back to 'monthlyDF'
  monthlyDF <- monthlyDF %>%
    left_join(summaryDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  
  # Calculate ratios between "YEAR_TOTAL" and both "MEDIAN_TOTAL_AF" and "AVG_TOTAL_AF"
  # Keep records that are more than 100 times away from the median/average (in either direction)
  # Also, records with an absolute distance of more than 100 AF from their median/average will be flagged
  monthlyDF <- monthlyDF %>%
    mutate(YEAR_TOTAL_MORE_THAN_100_TIMES_GREATER_THAN_MEDIAN_TOTAL = MEDIAN_TOTAL_AF > 0 & YEAR_TOTAL / MEDIAN_TOTAL_AF > 100,
           YEAR_TOTAL_MORE_THAN_100_TIMES_SMALLER_THAN_MEDIAN_TOTAL = MEDIAN_TOTAL_AF > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / MEDIAN_TOTAL_AF < 1/100,
           YEAR_TOTAL_MORE_THAN_100_AF_DIFFERENCE_FROM_MEDIAN_TOTAL = YEAR_TOTAL > 0 & abs(YEAR_TOTAL - MEDIAN_TOTAL_AF) > 100,
           YEAR_TOTAL_MORE_THAN_100_TIMES_GREATER_THAN_AVERAGE_TOTAL = AVG_TOTAL_AF > 0 & YEAR_TOTAL / AVG_TOTAL_AF > 100,
           YEAR_TOTAL_MORE_THAN_100_TIMES_SMALLER_THAN_AVERAGE_TOTAL = AVG_TOTAL_AF > 0 & YEAR_TOTAL > 0 & YEAR_TOTAL / AVG_TOTAL_AF < 1/100,
           YEAR_TOTAL_MORE_THAN_100_AF_DIFFERENCE_FROM_AVERAGE_TOTAL = YEAR_TOTAL > 0 & abs(YEAR_TOTAL - AVG_TOTAL_AF) > 100)
  
  
  
  # Write 'monthlyDF' to a spreadsheet
  # write.xlsx(monthlyDF %>%
  #              mutate(QAQC_Action_Taken = NA,
  #                     QAQC_Reason = NA),
  #            paste0("OutputData/", ws$ID, "_Expected_Demand_Units_QAQC_Median_Based.xlsx"), overwrite = TRUE)
  
  
  
  # Take a subset of 'monthlyDF'
  monthlyDF <- monthlyDF %>%
    select(APPLICATION_NUMBER, YEAR, 
           YEAR_TOTAL_MORE_THAN_100_TIMES_GREATER_THAN_MEDIAN_TOTAL,
           YEAR_TOTAL_MORE_THAN_100_TIMES_SMALLER_THAN_MEDIAN_TOTAL,
           YEAR_TOTAL_MORE_THAN_100_AF_DIFFERENCE_FROM_MEDIAN_TOTAL,
           YEAR_TOTAL_MORE_THAN_100_TIMES_GREATER_THAN_AVERAGE_TOTAL,
           YEAR_TOTAL_MORE_THAN_100_TIMES_SMALLER_THAN_AVERAGE_TOTAL,
           YEAR_TOTAL_MORE_THAN_100_AF_DIFFERENCE_FROM_AVERAGE_TOTAL)
  
  
  
  # Join 'monthlyDF' to 'flagDF'
  flagDF <- flagDF %>%
    left_join(monthlyDF, by = c("APPLICATION_NUMBER", "YEAR"),
              relationship = "many-to-one")
  
  
  
  # Return 'flagDF'
  return(flagDF)
  
}





#### Script Execution ####

cat("Starting 'Expected_Demand.R'...\n")


flagUnitErrors()


print("The Expected_Demand.R script is done running!")


remove(flagUnitErrors, monthlyUseValues, valAdd)

