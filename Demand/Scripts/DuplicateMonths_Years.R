# Identify water rights with duplicate values


# This script is a recreation of the "DuplicateMonths_Years (1)" XLSX file

# It will produce a spreadsheet called "DuplicateMonths_Years_(1)_Scripted.xlsx" that 
# mirrors the format of the original "DuplicateMonths_Years (1).xlsx" file


# NOTE
# This script takes around 45 minutes to run!!


#### Dependencies ####


require(tidyverse)
require(openxlsx)
require(doParallel)
require(data.table)


#### Script Procedure ####


mainProcedure <- function () {
  
  # The main body of the script
  
  
  # Read in the input CSV file ("Statistics_FINAL.csv")
  statFinal <- read.csv("InputData/Statistics_FINAL.csv")
  
  
  # Add the following columns to 'statFinal'
  # Column G: TotalMonthlyDiverted
  # Column H: AnnualReportedTotalDirect
  # Column I: AnnualTotalStorage
  # Column J: AnnualTotalDiversion
  # Column K: NumberOfOccurencesWithinSingleReport
  # Column L: OccurencesAcrossReports

  
  # Initialize the first three columns
  # They will be filled in via parallel processing
  statFinal <- statFinal %>%
    mutate(TotalMonthlyDiverted = NA_real_,
           AnnualReportedTotalDirect = NA_real_,
           AnnualTotalStorage = NA_real_)
  
  
  
  # Prepare for parallel processing 
  # The parallel loop will iterate through the rows of 'statFinal'
  
  
  # Create a cluster of parallel R sessions
  #(Use all except 1 of the system's cores)
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  
  # To make the process even faster, convert 'statFinal' into a data.table
  tableStat <- as.data.table(statFinal)
  
  
  # Also, prepare the output variable in advance
  # The values will be returned in a list format, with each entry containing
  # a vector of three numeric values (corresponding to each of the three columns)
  parRes <- vector(mode = "list", length = nrow(statFinal))
  
  # In a separate function, calculate the values for each of these three columns
  parRes <- foreach(i = 1:nrow(statFinal), .packages = "data.table") %dopar% calcGHI(tableStat, i)
  
  
  # NOTE
  # If 'statFinal' has 517,980 rows, with 7 workers and doParallel,
  # the parallel loop will take approximately 30 minutes!!!

  
  
  # Update 'statFinal' with these results
  
  
  # The first vector element of each list element will be "TotalMonthlyDiverted"
  statFinal$TotalMonthlyDiverted <- parRes %>% map_dbl(~ pluck(., 1))
  
  
  # The second vector element in each list entry will be "AnnualReportedTotalDirect"
  statFinal$AnnualReportedTotalDirect <- parRes %>% map_dbl(~ pluck(., 2))
  
  
  # The third vector element in each list element will be "AnnualTotalStorage"
  statFinal$AnnualTotalStorage <- parRes %>% map_dbl(~ pluck(., 3))
  
  
  
  # Next, calculate "AnnualTotalDiversion"
  # It is simply the sum of "AnnualReportedTotalDirect" and "AnnualTotalStorage"
  statFinal <- statFinal %>%
    mutate(AnnualTotalDiversion = AnnualReportedTotalDirect + AnnualTotalStorage)
  
  
  
  # Finally, only two columns remain
  # Initialize them now
  # They will be filled in via parallel processing
  statFinal <- statFinal %>%
    mutate(NumberOfOccurencesWithinSingleReport = NA_real_,
           OccurencesAcrossReports = NA_real_)
  
  
  # Update 'tableStat' with the latest data
  tableStat <- as.data.table(statFinal)
  
  
  # Prepare the output variable again
  # It will be similar to the previous version of 'parRes'
  # (except with 2 vector elements per list element instead of 3)
  parRes <- vector(mode = "list", length = nrow(statFinal))
  
  
  # Perform the parallel operations using iteration and another function
  parRes <- foreach(i = 1:nrow(statFinal), .packages = "data.table") %dopar% calcKL(tableStat, i)
  
  
  # NOTE
  # If 'statFinal' has 517,980 rows, with 7 workers and doParallel,
  # the parallel loop will take approximately 10 minutes!!!
  
  
  
  # Update 'statFinal' with this new data
  statFinal$NumberOfOccurencesWithinSingleReport <- parRes %>% map_dbl(~ pluck(., 1))
  statFinal$OccurencesAcrossReports <- parRes %>% map_dbl(~ pluck(., 2))
  
  
  
  # After that, save 'statFinal' to a new XLSX file
  write.xlsx(statFinal, "OutputData/DuplicateMonths_Years_Scripted.xlsx", overwrite = TRUE)
  
  
  # At the end of these operations, remove the extra R session workers
  stopCluster(cl)
  
  
  # Return nothing
  return(invisible(NULL))
}


calcGHI <- function (tableStat, i) {
  
  # Given the data.table 'tableStat' and row index 'i',
  # calculate that row's values for Columns G, H, and I
  # ("TotalMonthlyDiverted", "AnnualReportedTotalDirect", and "AnnualTotalStorage")
  
  
  # NOTE
  # The syntax for data.table will look a little different from the tidyverse format
  # In the brackets, the first expression will be the filter condition
  # The second expression (after the comma) will be the extracted column

  
  # First, calculate "TotalMonthlyDiverted"
  # It is the sum of values from the "AMOUNT" column with certain requirements:
  # "DIVERSION_TYPE" must be "DIRECT" or "STORAGE"
  # "APPLICATION_NUMBER" must match this iteration's value
  # Simiarly, "YEAR" and "MONTH" must also match this iteration's value
  TotalMonthlyDiverted <- sum(
    as.numeric(
      tableStat[DIVERSION_TYPE %in% c("DIRECT", "STORAGE") & 
                  APPLICATION_NUMBER == APPLICATION_NUMBER[i] &
                  YEAR == YEAR[i] &
                  MONTH == MONTH[i], AMOUNT]
    )
  )
    
  
  # Next, calculate "AnnualReportedTotalDirect"
  # It is also a sum of values in "AMOUNT"
  # "APPLICATION_NUMBER" must match this iteration's value
  # "YEAR" must also match this iteration's value
  # "DIVERSION_TYPE" should be "DIRECT"
  AnnualReportedTotalDirect <- sum(
    as.numeric(
      tableStat[APPLICATION_NUMBER == statFinal$APPLICATION_NUMBER[i] &
                  YEAR == statFinal$YEAR[i] & 
                  DIVERSION_TYPE == "DIRECT", AMOUNT]
    )
  )

  
  # Finally, calculate "AnnualTotalStorage"
  # It is a sum of the "AMOUNT" column
  # "APPLICATION_NUMBER" must match this iteration's value
  # "YEAR" must also match this iteration's value
  # "DIVERSION_TYPE" should be "STORAGE"
  AnnualTotalStorage <- sum(
    as.numeric(
      tableStat[APPLICATION_NUMBER == statFinal$APPLICATION_NUMBER[i] &
                  YEAR == statFinal$YEAR[i] & 
                  DIVERSION_TYPE == "STORAGE", AMOUNT]
    )
  )

  
  # Return those values in a single vector
  return(c(TotalMonthlyDiverted, AnnualReportedTotalDirect, AnnualTotalStorage))
  
}


calcKL <- function (tableStat, i) {
  
  # Given the data.table 'tableStat' and row index 'i',
  # calculate that row's values for Columns K and L
  # ("NumberOfOccurencesWithinSingleReport" and "OccurencesAcrossReports")
  
  
  
  # First, calculate "NumberOfOccurencesWithinSingleReport"
  # If "TotalMonthlyDiverted" is 0, this variable will be 0 too
  if (tableStat$TotalMonthlyDiverted[i] == 0) {
    
    NumberOfOccurencesWithinSingleReport <- 0
    
  # Otherwise,
  } else {
    
    # This variable is a count of rows when the following filters are active:
    # "APPLICATION_NUMBER" must match this iteration's value
    # "YEAR" must also match this iteration's value
    # "TotalMonthlyDiverted" must match this iteration's value too
    # "DIVERSION_TYPE" should be "DIRECT"
    NumberOfOccurencesWithinSingleReport <- length(
      tableStat[APPLICATION_NUMBER == tableStat$APPLICATION_NUMBER[i] &
                  YEAR == tableStat$YEAR[i] &
                  TotalMonthlyDiverted == tableStat$TotalMonthlyDiverted[i] &
                  DIVERSION_TYPE == "DIRECT", AMOUNT]
    )
    
  }
  
  
  # After that, calculate "OccurencesAcrossReports"
  # If "AnnualTotalDiversion" is 0, this variable will be 0 too
  if (tableStat$AnnualTotalDiversion[i] == 0) {
    
    OccurencesAcrossReports <- 0
    
    # Otherwise,
  } else {
    
    # This variable is 1/12 times a count of rows with these filters:
    # "APPLICATION_NUMBER" must match this iteration's value
    # "AnnualTotalDiversion" must match this iteration's value too
    # "DIVERSION_TYPE" should be "DIRECT"
    OccurencesAcrossReports <- length(
      tableStat[APPLICATION_NUMBER == tableStat$APPLICATION_NUMBER[i] &
                  AnnualTotalDiversion == tableStat$AnnualTotalDiversion[i] &
                  DIVERSION_TYPE == "DIRECT", AMOUNT]
    ) / 12
    
  }
  
  
  
  # Return these values in a single vector
  return(c(NumberOfOccurencesWithinSingleReport, OccurencesAcrossReports))
  
}


#### Script Execution ####

mainProcedure()