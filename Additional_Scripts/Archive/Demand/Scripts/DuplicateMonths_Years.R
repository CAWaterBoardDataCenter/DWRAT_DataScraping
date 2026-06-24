 # Identify water rights with duplicate values


# This script is a recreation of the "DuplicateMonths_Years (1)" XLSX file

# It will produce a spreadsheet called "DuplicateMonths_Years_(1)_Scripted.xlsx" that 
# mirrors the format of the original "DuplicateMonths_Years (1).xlsx" file


#### Dependencies ####


require(tidyverse)
require(openxlsx)


#### Script Procedure ####


mainProcedure <- function () {
  
  # The main body of the script
  
  
  # Read in the input CSV file ("Statistics_FINAL.csv")
  statFinal <- read.csv("IntermediateData/Statistics_FINAL.csv")
  
  
  # Add the following columns to 'statFinal'
  # Column G: TotalMonthlyDiverted
  # Column H: AnnualReportedTotalDirect
  # Column I: AnnualTotalStorage
  # Column J: AnnualTotalDiversion
  # Column K: NumberOfOccurencesWithinSingleReport
  # Column L: OccurencesAcrossReports
  
  
  # Start with the first column "TotalMonthlyDiverted"
  
  
  # Create another variable that summarizes the total diversion and storage
  # for each application number (separately for each month/year)
  # (The only use types considered are "DIRECT" and "STORAGE")
  tempDF <- statFinal %>%
    filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
    group_by(APPLICATION_NUMBER, YEAR, MONTH) %>%
    summarize(TotalMonthlyDiverted = sum(AMOUNT, na.rm = TRUE), .groups = "drop")
  
  
  # Join those results back to 'statFinal' using left_join()
  # (The relationship is "many-to-one" because multiple rows in 'tempDF' will
  #  use the same value from 'tempDF')
  # Also, replace any NA values in "TotalMonthlyDiverted" with 0
  statFinal <- statFinal %>% 
    left_join(tempDF, 
              by = c("APPLICATION_NUMBER", "YEAR", "MONTH"), 
              relationship = "many-to-one") %>%
    mutate(TotalMonthlyDiverted = replace_na(TotalMonthlyDiverted, 0))
  
  
  
  # Perform similar steps for the next column, "AnnualReportedTotalDirect"
  # Grouping will be done by "APPLICATION_NUMBER" and "YEAR"
  # (And the "DIVERSION_TYPE" filter will be "DIRECT" only)
  tempDF <- statFinal %>%
    filter(DIVERSION_TYPE == "DIRECT") %>%
    group_by(APPLICATION_NUMBER, YEAR) %>%
    summarize(AnnualReportedTotalDirect = sum(AMOUNT, na.rm = TRUE), .groups = "drop")
  
  
  # Join those results back to 'statFinal' using left_join()
  # (The relationship is "many-to-one" because multiple rows in 'tempDF' will
  #  use the same value from 'tempDF')
  statFinal <- statFinal %>% 
    left_join(tempDF, 
              by = c("APPLICATION_NUMBER", "YEAR"), 
              relationship = "many-to-one") %>%
    mutate(AnnualReportedTotalDirect = replace_na(AnnualReportedTotalDirect, 0))
  
  
  
  # The next column is "AnnualTotalStorage"
  # The steps are exactly the same as for "AnnualReportedTotalDirect" except
  # "DIVERSION_TYPE" is "STORAGE" only
  tempDF <- statFinal %>%
    filter(DIVERSION_TYPE == "STORAGE") %>%
    group_by(APPLICATION_NUMBER, YEAR) %>%
    summarize(AnnualTotalStorage = sum(AMOUNT, na.rm = TRUE), .groups = "drop")
  
  
  # Join those results back to 'statFinal' using left_join()
  # (The relationship is "many-to-one" because multiple rows in 'tempDF' will
  #  use the same value from 'tempDF')
  statFinal <- statFinal %>% 
    left_join(tempDF, 
              by = c("APPLICATION_NUMBER", "YEAR"), 
              relationship = "many-to-one") %>%
    mutate(AnnualTotalStorage = replace_na(AnnualTotalStorage, 0))
  
  
  
  # After that, calculate "AnnualTotalDiversion"
  # It is simply the sum of "AnnualReportedTotalDirect" and "AnnualTotalStorage"
  statFinal <- statFinal %>%
    mutate(AnnualTotalDiversion = AnnualReportedTotalDirect + AnnualTotalStorage)
  
  
  
  # Finally, only two columns remain
  # They will also require a temporary DF that is joined to 'statFina'
  
  
  
  # Start with "NumberOfOccurencesWithinSingleReport"
  # Filter 'statFinal to only "DIRECT" use records
  # Count the number of reports with the same "APPLICATION_NUMBER", "YEAR", and
  # "TotalMonthlyDiverted" values
  # (There is one important exception, however; if "TotalMonthlyDiverted" is 0,
  #  the value of this new column will also be 0)
  tempDF <- statFinal %>%
    filter(DIVERSION_TYPE == "DIRECT") %>%
    group_by(APPLICATION_NUMBER, YEAR, TotalMonthlyDiverted) %>%
    summarize(NumberOfOccurencesWithinSingleReport = n(), .groups = "drop") %>%
    mutate(NumberOfOccurencesWithinSingleReport = 
             if_else(TotalMonthlyDiverted == 0, 0L, NumberOfOccurencesWithinSingleReport))
  
  
  
  # Join this new column to 'statFinal'
  statFinal <- statFinal %>%
    left_join(tempDF, 
              by = c("APPLICATION_NUMBER", "YEAR", "TotalMonthlyDiverted"),
              relationship = "many-to-one")
  
  
  
  # The final column "OccurencesAcrossReports" is similar
  # It counts one-twelfth the number of entries with "DIRECT" usage
  # that have the same "APPLICATION_NUMBER" and "AnnualTotalDiversion" values
  # (And similar to before, if "AnnualTotalDiversion" is 0, this column will be 0 too)
  tempDF <- statFinal %>%
    filter(DIVERSION_TYPE == "DIRECT") %>%
    group_by(APPLICATION_NUMBER, AnnualTotalDiversion) %>%
    summarize(OccurencesAcrossReports = n(), .groups = "drop") %>%
    mutate(OccurencesAcrossReports = 
             if_else(AnnualTotalDiversion == 0, 0, OccurencesAcrossReports / 12))
  
  
  # Join these results to 'statFinal'
  statFinal <- statFinal %>%
    left_join(tempDF, 
              by = c("APPLICATION_NUMBER", "AnnualTotalDiversion"),
              relationship = "many-to-one")
  
  
  
  # After that, save 'statFinal' to a new XLSX file
  write.xlsx(statFinal, "OutputData/DuplicateMonths_Years_Scripted.xlsx", overwrite = TRUE)
  
  
  
  # Output a message to the console
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

cat("Starting 'DuplicateMonths_Years.R'...")

mainProcedure()



# At the end of the script, remove the function generated for this procedure
remove(mainProcedure)