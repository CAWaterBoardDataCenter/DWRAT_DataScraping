# Perform calculations related to out-of-season diversions


# This script is a recreation of the Excel modules 
# "Diversion out of Season_Part_A.xlsx" and "Diversion out of Season_Part_B.xlsx"


# This script also contains a unique procedure where the direct diversion and
# storage season are combined into a single "super" season; the analysis is repeated
# with this super season instead of the separate direct and storage seasons


#### Dependencies ####


require(tidyverse)
require(openxlsx)


#### Script Procedure ####


mainProcedure <- function () {
  
  # The main body of the script
  
  
  # Read data from the input file
  diverDF <- read.csv("IntermediateData/Diversion_out_of_Season_Part_A_FINAL.csv")
  
  
  
  # First, add some simple logic columns to 'diverDF'
  # In another function, create columns with diversion information
  diverDF <- diverDF %>%
    seasonInfo(inputColNames = c("DIRECT_SEASON_START_MONTH_1", "STORAGE_SEASON_START_MONTH_1",
                                 "DIRECT_DIV_SEASON_END_MONTH_1", "STORAGE_SEASON_END_MONTH_1"),
               newColNames = c("NO_DD_SEASON_INFO", "NO_STOR_SEASON_INFO", "NO_SEASON_INFO",
                               "WRAP_AROUND_SEASON_DD", "WRAP_AROUND_SEASON_STOR"))
  
  
  
  # The next set of columns is an in-season flag for non-wraparound and wraparound cases
  # For each month, if that month is within the right holder's DD/Storage authorized time range,
  # its column will be "IN-SEASON"; in all other cases, it is empty ("")
  diverDF <- diverDF %>%
    authorizedUses(inputColNames = c("NO_DD_SEASON_INFO", "WRAP_AROUND_SEASON_DD",
                                     "DIRECT_SEASON_START_MONTH_1", "DIRECT_DIV_SEASON_END_MONTH_1"),
                   newColNames = c(month.abb %>% toupper() %>% paste0("_DD_AUTH_NOTWRAPAROUND"),
                                   month.abb %>% toupper() %>% paste0("_DD_AUTH_WRAPAROUND")))
  
  
  
  # Next, all previous columns will now be generated for the second season
  diverDF <- diverDF %>%
    seasonInfo(inputColNames = c("DIRECT_SEASON_START_MONTH_2", "STORAGE_SEASON_START_MONTH_2",
                                 "DIRECT_DIV_SEASON_END_MONTH_2", "STORAGE_SEASON_END_MONTH_2"),
               newColNames = c("NO_DD_SEASON2_INFO", "NO_STOR_SEASON2_INFO", "NO_SEASON2_INFO",
                               "WRAP_AROUND_SEASON2_DD", "WRAP_AROUND_SEASON2_STOR"))
  
  
  diverDF <- diverDF %>%
    authorizedUses(inputColNames = c("NO_DD_SEASON2_INFO", "WRAP_AROUND_SEASON2_DD",
                                     "DIRECT_SEASON_START_MONTH_2", "DIRECT_DIV_SEASON_END_MONTH_2"),
                   newColNames = c(month.abb %>% toupper() %>% paste0("_DD_AUTH_NOTWRAPAROUND2"),
                                   month.abb %>% toupper() %>% paste0("_DD_AUTH_WRAPAROUND2")))
  
  
  
  # For the next step, create a column for each month 
  # Check if at least one of the previous diversion columns for that month has "IN_SEASON"
  # This column will be "IN_SEASON" if that is the case (otherwise it is empty)
  # Use a generic function for these steps
  diverDF <- diverDF %>%
    finalAuthCol(inputColStrs = c("_DD_AUTH_NOTWRAPAROUND", "_DD_AUTH_WRAPAROUND",
                                  "_DD_AUTH_NOTWRAPAROUND2", "_DD_AUTH_WRAPAROUND2"),
                 newColStr = "_DD_AUTH")
  
  
  
  # After that, create corresponding columns for storage variables instead of direct diversions
  diverDF <- diverDF %>%
    authorizedUses(inputColNames = c("NO_STOR_SEASON_INFO", "WRAP_AROUND_SEASON_STOR",
                                     "STORAGE_SEASON_START_MONTH_1", "STORAGE_SEASON_END_MONTH_1"),
                   newColNames = c(month.abb %>% toupper() %>% paste0("_STOR_AUTH_NOTWRAPAROUND"),
                                   month.abb %>% toupper() %>% paste0("_STOR_AUTH_WRAPAROUND")))
  
  
  diverDF <- diverDF %>%
    authorizedUses(inputColNames = c("NO_STOR_SEASON2_INFO", "WRAP_AROUND_SEASON2_STOR",
                                     "STORAGE_SEASON_START_MONTH_2", "STORAGE_SEASON_END_MONTH_2"),
                   newColNames = c(month.abb %>% toupper() %>% paste0("_STOR_AUTH_NOTWRAPAROUND2"),
                                   month.abb %>% toupper() %>% paste0("_STOR_AUTH_WRAPAROUND2")))
  
  
  diverDF <- diverDF %>%
    finalAuthCol(inputColStrs = c("_STOR_AUTH_NOTWRAPAROUND", "_STOR_AUTH_WRAPAROUND",
                                  "_STOR_AUTH_NOTWRAPAROUND2", "_STOR_AUTH_WRAPAROUND2"),
                 newColStr = "_STOR_AUTH")
  
  
  
  # Create new columns that are adjusted versions of the final auth columns
  # If "USE_STATUS" is "Deleted by order", "Deleted when permitted", or "Removed by request",
  # the column will be empty (""); otherwise, the values from the final auth columns are reused
  diverDF <- diverDF %>%
    useStatusAdjustment()
  
  
  
  # For the final step of Part A, create a new tibble with one row per unique application number
  # Separately for "DD" and "STORAGE", if at least one of an applicant's rows is "IN_SEASON" for a given month,
  # this column will also be "IN_SEASON"; otherwise, it is empty ("")
  finalDF <- diverDF %>%
    applicantSeasonSummary()
  
  
  
  # Super season check
  # Create columns for each month that will be "IN_SEASON" if either "STORAGE" or "DIRECT" is "IN_SEASON" for that month
  finalDF <- finalDF %>%
    mutate(JAN_SUPER_DIVERSION = if_else(JAN_DIRECT == "IN_SEASON" | JAN_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           FEB_SUPER_DIVERSION = if_else(FEB_DIRECT == "IN_SEASON" | FEB_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           MAR_SUPER_DIVERSION = if_else(MAR_DIRECT == "IN_SEASON" | MAR_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           APR_SUPER_DIVERSION = if_else(APR_DIRECT == "IN_SEASON" | APR_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           MAY_SUPER_DIVERSION = if_else(MAY_DIRECT == "IN_SEASON" | MAY_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           JUN_SUPER_DIVERSION = if_else(JUN_DIRECT == "IN_SEASON" | JUN_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           JUL_SUPER_DIVERSION = if_else(JUL_DIRECT == "IN_SEASON" | JUL_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           AUG_SUPER_DIVERSION = if_else(AUG_DIRECT == "IN_SEASON" | AUG_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           SEP_SUPER_DIVERSION = if_else(SEP_DIRECT == "IN_SEASON" | SEP_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           OCT_SUPER_DIVERSION = if_else(OCT_DIRECT == "IN_SEASON" | OCT_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           NOV_SUPER_DIVERSION = if_else(NOV_DIRECT == "IN_SEASON" | NOV_STORAGE == "IN_SEASON", "IN_SEASON", ""),
           DEC_SUPER_DIVERSION = if_else(DEC_DIRECT == "IN_SEASON" | DEC_STORAGE == "IN_SEASON", "IN_SEASON", ""))
  
  
  
  # 'finalDF' serves as an input for Part B of the module
  
  
  # Another input file is "Diversion_out_of_Season_Part_B_FINAL.csv"
  # Read that file into R
  oosDF <- read.csv("IntermediateData/Diversion_out_of_Season_Part_B_FINAL.csv") %>%
    filter(AMOUNT > 1)
  
  
  
  # Join 'finalDF' to 'oosDF'
  # The new columns in 'oosDF' will be defined using this data
  
  # Unlike in 'finalDF', there are multiple rows per application number in 'oosDF'
  # Therefore, the join will be "many-to-one"
  oosDF <- oosDF %>%
    left_join(finalDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")
  
  
  
  # The first column considers each row's value for "MONTH"
  # If the corresponding month's DIRECT column is "IN_SEASON", this column is also "IN_SEASON"
  # Otherwise, it is empty ("")
  oosDF <- oosDF %>%
    mutate(`month in DIRECT season?` = 
             if_else(MONTH == 1,
                     if_else(JAN_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                     if_else(MONTH == 2,
                             if_else(FEB_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                             if_else(MONTH == 3,
                                     if_else(MAR_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                     if_else(MONTH == 4,
                                             if_else(APR_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                             if_else(MONTH == 5,
                                                     if_else(MAY_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                     if_else(MONTH == 6,
                                                             if_else(JUN_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                             if_else(MONTH == 7,
                                                                     if_else(JUL_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                                     if_else(MONTH == 8,
                                                                             if_else(AUG_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                                             if_else(MONTH == 9,
                                                                                     if_else(SEP_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                                                     if_else(MONTH == 10,
                                                                                             if_else(OCT_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                                                             if_else(MONTH == 11,
                                                                                                     if_else(NOV_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                                                                     if_else(MONTH == 12,
                                                                                                             if_else(DEC_DIRECT == "IN_SEASON", "IN_SEASON", ""),
                                                                                                             "")))))))))))))
  
  
  
  # Create a similar variable for storage
  oosDF <- oosDF %>%
    mutate(`month in STOR season?` = 
             if_else(MONTH == 1,
                     if_else(JAN_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                     if_else(MONTH == 2,
                             if_else(FEB_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                             if_else(MONTH == 3,
                                     if_else(MAR_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                     if_else(MONTH == 4,
                                             if_else(APR_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                             if_else(MONTH == 5,
                                                     if_else(MAY_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                     if_else(MONTH == 6,
                                                             if_else(JUN_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                             if_else(MONTH == 7,
                                                                     if_else(JUL_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                                     if_else(MONTH == 8,
                                                                             if_else(AUG_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                                             if_else(MONTH == 9,
                                                                                     if_else(SEP_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                                                     if_else(MONTH == 10,
                                                                                             if_else(OCT_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                                                             if_else(MONTH == 11,
                                                                                                     if_else(NOV_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                                                                     if_else(MONTH == 12,
                                                                                                             if_else(DEC_STORAGE == "IN_SEASON", "IN_SEASON", ""),
                                                                                                             "")))))))))))))
  
  
  
  
  
  
  
  oosDF <- oosDF %>%
    mutate(`month in SUPER season?` = 
             if_else(MONTH == 1,
                     if_else(JAN_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                     if_else(MONTH == 2,
                             if_else(FEB_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                             if_else(MONTH == 3,
                                     if_else(MAR_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                     if_else(MONTH == 4,
                                             if_else(APR_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                             if_else(MONTH == 5,
                                                     if_else(MAY_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                     if_else(MONTH == 6,
                                                             if_else(JUN_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                             if_else(MONTH == 7,
                                                                     if_else(JUL_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                                     if_else(MONTH == 8,
                                                                             if_else(AUG_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                                             if_else(MONTH == 9,
                                                                                     if_else(SEP_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                                                     if_else(MONTH == 10,
                                                                                             if_else(OCT_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                                                             if_else(MONTH == 11,
                                                                                                     if_else(NOV_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                                                                     if_else(MONTH == 12,
                                                                                                             if_else(DEC_SUPER_DIVERSION == "IN_SEASON", "IN_SEASON", ""),
                                                                                                             "")))))))))))))
  
  
  # The next column checks if the reported DD usage is out-of-season
  # If the row is for "DIRECT" use with a non-zero "AMOUNT" specified while
  # "month in DIRECT season?" is empty (""), an out-of-season diversion occurred
  # and this column will have the value "OUT_OF_SEASON_DIRECT"; otherwise it is empty
  oosDF <- oosDF %>%
    mutate(OUT_OF_SEASON_DIRECT = 
             if_else(DIVERSION_TYPE == "DIRECT" & `month in DIRECT season?` == "" & AMOUNT > 0,
                     "OUT_OF_SEASON_DIRECT",
                     ""))
  
  
  
  # Create a similar column for out-of-season storage
  oosDF <- oosDF %>%
    mutate(OUT_OF_SEASON_STOR = 
             if_else(DIVERSION_TYPE == "STORAGE" & `month in STOR season?` == "" & AMOUNT > 0,
                     "OUT_OF_SEASON_STOR",
                     ""))
  
  
  # Create a similar column for out-of-super-season
  oosDF <- oosDF %>%
    mutate(OUT_OF_SEASON_SUPER = 
             if_else(`month in SUPER season?` == "" & AMOUNT > 0,
                     "OUT_OF_SEASON_SUPER",
                     ""))
  
  
  
  # The next column is a binary counter column
  # If the row is an out-of-season use (either "DIRECT" or "STOR"), the value is 1
  # Otherwise, its value is 0
  oosDF <- oosDF %>%
    mutate(DIVERSION_OUT_OF_SEASON_RECORD = 
             if_else(OUT_OF_SEASON_DIRECT == "OUT_OF_SEASON_DIRECT" | 
                       OUT_OF_SEASON_STOR == "OUT_OF_SEASON_STOR",
                     1,
                     0)) %>%
    filter(DIVERSION_OUT_OF_SEASON_RECORD > 0) %>%
    arrange(desc(AMOUNT))
  
  
  oosDF_Super <- oosDF %>%
    mutate(DIVERSION_OUT_OF_SEASON_RECORD = 
             if_else(OUT_OF_SEASON_SUPER == "OUT_OF_SEASON_SUPER",
                     1,
                     0)) %>%
    filter(DIVERSION_OUT_OF_SEASON_RECORD > 0) %>%
    arrange(desc(AMOUNT))
  
  
  # The final new column will be a summary of counts based on "DIVERSION_OUT_OF_SEASON_RECORD"
  # For each unique application number, a count of records for out-of-season use will be calculated
  oosCounts <- oosDF %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(`NUMBER OF OUT OF SEASON RECORDS` = sum(DIVERSION_OUT_OF_SEASON_RECORD)) %>%
    filter(`NUMBER OF OUT OF SEASON RECORDS` > 0)
  
  
  oosCounts_Super <- oosDF_Super %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(`NUMBER OF OUT OF SEASON RECORDS` = sum(DIVERSION_OUT_OF_SEASON_RECORD)) %>%
    filter(`NUMBER OF OUT OF SEASON RECORDS` > 0)
  
  
  
  # After that, the last step is to export the data into two Excel files
  # Complete those steps in a separate function
  excelExport(diverDF, finalDF, oosDF, oosDF_Super, oosCounts, oosCounts_Super)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


seasonInfo <- function (inputDF, inputColNames, newColNames) {
  
  # Create several string-based logical columns based on season information
  # This function is written generically due to its eventual reuse in the module
  
  # Here are example specifications for the first group of columns that uses this function:
  
  # "NO_DD_SEASON_INFO": If "DIRECT_SEASON_START_MONTH_1" is blank, 
  #   "NO_DD_SEASON", otherwise "DD_SEASON"
  # "NO_STOR_SEASON_INFO": If "STORAGE_SEASON_START_MONTH_1" is blank, 
  #   "NO_STOR_SEASON", otherwise "STOR_SEASON"
  # "NO_SEASON_INFO": If both of the previous columns are "NO_[X]_SEASON",
  #   "NO_SEASON_INFO", otherwise "SEASON_INFO_AVAIL"
  # "WRAP_AROUND_SEASON_DD": If "NO_DD_SEASON_INFO" is not "DD_SEASON", "NO_DD_SEASON",
  #   otherwise if "DIRECT_SEASON_START_MONTH_1" is greater than 
  #   "DIRECT_DIV_SEASON_END_MONTH_1", "YES", otherwise "NO"
  # "WRAP_AROUND_SEASON_STOR": If "NO_STOR_SEASON_INFO" is not "STOR_SEASON", 
  #   "NO_STOR_SEASON", otherwise if "STORAGE_SEASON_START_MONTH_1" is greater than 
  #   "STORAGE_SEASON_END_MONTH_1", "YES", otherwise "NO"
  
  # In this generic function, the names for the new columns are specified in 'newColNames'
  # The columns used in these computations are listed in 'inputColNames' in order of appearance
  
  inputDF <- inputDF %>%
    mutate(!! newColNames[1] := if_else(is.na(!! as.name(inputColNames[1])),
                                       "NO_DD_SEASON", "DD_SEASON"),
           !! newColNames[2] := if_else(is.na(!! as.name(inputColNames[2])),
                                        "NO_STOR_SEASON", "STOR_SEASON"),
           !! newColNames[3] := if_else(!! as.name(newColNames[1]) == "NO_DD_SEASON" & !! as.name(newColNames[2]) == "NO_STOR_SEASON",
                                    "NO_SEASON_INFO", "SEASON_INFO_AVAIL"),
           !! newColNames[4] := if_else(!! as.name(newColNames[1]) == "DD_SEASON",
                                        if_else(!! as.name(inputColNames[1]) > !! as.name(inputColNames[3]), "YES", "NO"),
                                        "NO_DD_SEASON"),
           !! newColNames[5] := if_else(!! as.name(newColNames[2]) == "STOR_SEASON",
                                        if_else(!! as.name(inputColNames[2]) > !! as.name(inputColNames[4]), "YES", "NO"),
                                        "NO_STOR_SEASON"))
  
  
  # Return 'inputDF' after these changes
  return(inputDF)
  
  
  
  # Here's the original (non-generic) code:
  
  # diverDF <- diverDF %>%
  #   mutate(NO_DD_SEASON_INFO = if_else(is.na(DIRECT_SEASON_START_MONTH_1),
  #                                      "NO_DD_SEASON", "DD_SEASON"),
  #          NO_STOR_SEASON_INFO = if_else(is.na(STORAGE_SEASON_START_MONTH_1),
  #                                        "NO_STOR_SEASON", "STOR_SEASON"),
  #          NO_SEASON_INFO = if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" & NO_STOR_SEASON_INFO == "NO_STOR_SEASON",
  #                                   "NO_SEASON_INFO", "SEASON_INFO_AVAIL"))
  # 
  # 
  # diverDF <- diverDF %>%
  #   mutate(WRAP_AROUND_SEASON_DD = if_else(NO_DD_SEASON_INFO == "DD_SEASON",
  #                                          if_else(DIRECT_SEASON_START_MONTH_1 > DIRECT_DIV_SEASON_END_MONTH_1,
  #                                                  "YES", "NO"),
  #                                          "NO_DD_SEASON"),
  #          WRAP_AROUND_SEASON_STOR = if_else(NO_STOR_SEASON_INFO == "STOR_SEASON",
  #                                            if_else(STORAGE_SEASON_START_MONTH_1 > STORAGE_SEASON_END_MONTH_1,
  #                                                    "YES", "NO"),
  #                                            "NO_STOR_SEASON"))
  
}


authorizedUses <- function (inputDF, inputColNames, newColNames) {
  
  # 24 new columns will be created in this generic function
  
  # The first set of 12 are for non-wraparound cases
  # The second set of 12 are for wraparound cases
  
  # Each column corresponds to one of the twelve months
  
  # It is an in-season flag
  # For each month, if that month is within the right holder's specified range,
  # its column value will be "IN-SEASON"; in all other cases, it is empty ("")
  
  
  # This is a generic function, so the column names may not be readily understandable
  # 'inputColNames' will have the names of the variables used in these computations
  # 'newColNames' has the names of the new columns that will be generated
  
  
  
  # Error Check
  stopifnot(length(newColNames) == 24)
  
  
  
  # Iterate through the months
  for (i in 1:12) {
    
    # First focus on the non-wraparound case
    inputDF <- inputDF %>%
      mutate(!! newColNames[i] :=
               if_else(!! as.name(inputColNames[1]) %in% c("NO_DD_SEASON", "NO_STOR_SEASON") | !! as.name(inputColNames[2]) == "YES",
                       "",
                       if_else(!! as.name(inputColNames[3]) <= i & i <= !! as.name(inputColNames[4]), "IN_SEASON", "")))
    
    
    # Define a similar column for the second group (the wraparound case)
    inputDF <- inputDF %>%
      mutate(!! newColNames[i + 12] :=
               if_else(!! as.name(inputColNames[1]) %in% c("NO_DD_SEASON", "NO_STOR_SEASON") | !! as.name(inputColNames[2]) == "NO",
                       "",
                       if_else(!! as.name(inputColNames[3]) >= i & i <= !! as.name(inputColNames[4]), "IN_SEASON", "")))
    
  }
  
  
  
  # Return 'inputDF'
  return(inputDF)
  
  
  # Here's the original (non-generic) code
  
  # diverDF <- diverDF %>%
  #   mutate(JAN_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 1 & 1 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          FEB_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 2 & 2 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          MAR_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 3 & 3 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          APR_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 4 & 4 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          MAY_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 5 & 5 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          JUN_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 6 & 6 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          JUL_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 7 & 7 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          AUG_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 8 & 8 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          SEP_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 9 & 9 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          OCT_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 10 & 10 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          NOV_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 11 & 11 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          DEC_DD_AUTH_NOTWRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "YES",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 <= 12 & 12 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")))
  # 
  # 
  # 
  # # The next columns are a repeat of the previous code with one exception
  # # Now the wraparound cases are considered
  # diverDF <- diverDF %>%
  #   mutate(JAN_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 1 & 1 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          FEB_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 2 & 2 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          MAR_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 3 & 3 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          APR_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 4 & 4 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          MAY_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 5 & 5 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          JUN_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 6 & 6 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          JUL_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 7 & 7 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          AUG_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 8 & 8 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          SEP_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 9 & 9 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          OCT_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 10 & 10 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          NOV_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 11 & 11 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")),
  #          DEC_DD_AUTH_WRAPAROUND = 
  #            if_else(NO_DD_SEASON_INFO == "NO_DD_SEASON" | WRAP_AROUND_SEASON_DD == "NO",
  #                    "",
  #                    if_else(DIRECT_SEASON_START_MONTH_1 >= 12 & 12 <= DIRECT_DIV_SEASON_END_MONTH_1,
  #                            "IN_SEASON", "")))
  
}


finalAuthCol <- function (inputDF, inputColStrs, newColStr) {
  
  # In this generic function, a new column is created for each of the twelve months
  # Previous columns related to that month will be checked (using 'inputColStrs' to get the full column names)
  # If at least one of those columns has "IN_SEASON", this new column will also be "IN_SEASON"
  # Otherwise, it is empty ("")
  
  
  
  # Iterate through all twelve months
  for (i in 1:12) {
    
    # To simplify the syntax, get this iteration's month string ready now
    mon <- month.abb[i] %>% toupper()
    
    
    
    # Also, get the names of the columns to check using 'mon' and 'inputColStrs'
    inputColNames <- paste0(mon, inputColStrs)
    
    
    
    # Use if_else() to define the new column
    inputDF <- inputDF %>%
      mutate(!! paste0(mon, newColStr) := 
               if_else(!! as.name(inputColNames[1]) == "IN_SEASON" |
                         !! as.name(inputColNames[2]) == "IN_SEASON" |
                         !! as.name(inputColNames[3]) == "IN_SEASON" |
                         !! as.name(inputColNames[4]) == "IN_SEASON",
                       "IN_SEASON", ""))
    
  }
  
  
  
  # Return 'inputDF'
  return(inputDF)
  
}


useStatusAdjustment <- function (inputDF) {
  
  # For each month, create a new column that is an adjusted version of the final auth columns
  # If "USE_STATUS" is "Deleted by order", "Deleted when permitted", or "Removed by request",
  # the column will be empty (""); otherwise, the values from the auth columns are reused
  
  
  # There will be separate columns for "DIRECT" and "STORAGE" in each month
  # For direct diversions, "XXX_DD_AUTH" will be checked
  # For storage, "XXX_STOR_AUTH" will be checked
  
  
  # Iterate through each month
  for (i in 1:12) {
    
    # Define "DIRECT" and "STOR" columns for this month
    inputDF <- inputDF %>%
      mutate(!! paste0(i, "_DIRECT") := 
               if_else(USE_STATUS %in% c("Deleted by order", "Deleted when permitted", "Removed by request"),
                       "",
                       !! as.name(paste0(month.abb[i] %>% toupper(), "_DD_AUTH"))),
             !! paste0(i, "_STORAGE") := 
               if_else(USE_STATUS %in% c("Deleted by order", "Deleted when permitted", "Removed by request"),
                       "",
                       !! as.name(paste0(month.abb[i] %>% toupper(), "_STOR_AUTH"))))
    
  }
  
  
  
  # Return 'inputDF' afterwards
  return(inputDF)
  
}


applicantSeasonSummary <- function (inputDF) {
  
  # Create a new tibble with one row per application number
  # For both "DD" and "STORAGE", if at least one of an applicant's rows is "IN_SEASON" for a given month,
  # this column will also be "IN_SEASON"; otherwise, it is empty ("")
  
  
  
  # Iterate through the twelve months
  for (i in 1:12) {
    
    
    # For this iteration's month, create a summarized table
    # Grouped by application number, two new columns are defined
    
    # If "IN_SEASON" appears in at least one of the group members' corresponding month column,
    # the new columns will also be "IN_SEASON"
    # Otherwise, they will be empty ("")
    
    # Examples:
    # For January/DD, "JAN_DIRECT" is created with "1_DIRECT" checked in each group
    # For March/STOR, "MAR_STORAGE" is created with "3_STORAGE" checked in each group
    
    # Also, save the new table to a temporary data frame
    tempDF <- inputDF %>%
      group_by(APPLICATION_NUMBER) %>%
      summarize(!! paste0(month.abb[i] %>% toupper(), "_DIRECT") :=
                  if_else("IN_SEASON" %in% !! as.name(paste0(i, "_DIRECT")),
                          "IN_SEASON", ""),
                !! paste0(month.abb[i] %>% toupper(), "_STORAGE") :=
                  if_else("IN_SEASON" %in% !! as.name(paste0(i, "_STORAGE")),
                          "IN_SEASON", ""),
                .groups = "drop")
    
    
    
    # If this is the first iteration, use 'tempDF' to define a new compiled data frame
    # If this is a later iteration, join the results of 'tempDF' to 'compiledDF'
    if (i == 1) {
      
      compiledDF <- tempDF
      
    } else {
      
      compiledDF <- compiledDF %>%
        full_join(tempDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")
      
    }
    
  }
  
  
  
  # After these operations, return 'compiledDF'
  return(compiledDF)
  
}


excelExport <- function (diverDF, finalDF, oosDF, oosDF_Super, oosCounts, oosCounts_Super) {
  
  # Create two Excel files using the previously defined variables
  
  
  # Start with the Part A workbook
  wbA <- createWorkbook()
  
  
  # Create a worksheet called "USE_SEASON_FLATFILE"
  addWorksheet(wbA, "USE_SEASON_FLATFILE")
  
  
  
  # Write data from 'diverDF' to this sheet
  writeData(wbA, "USE_SEASON_FLATFILE", startRow = 3, startCol = 1,
            diverDF %>%
              select(APPLICATION_NUMBER, USE_STATUS, DIRECT_SEASON_START_MONTH_1, DIRECT_DIV_SEASON_END_MONTH_1, DIRECT_SEASON_START_MONTH_2, DIRECT_DIV_SEASON_END_MONTH_2, 
                     STORAGE_SEASON_START_MONTH_1, STORAGE_SEASON_END_MONTH_1, STORAGE_SEASON_START_MONTH_2, STORAGE_SEASON_END_MONTH_2, NO_DD_SEASON_INFO, NO_STOR_SEASON_INFO, NO_SEASON_INFO, 
                     WRAP_AROUND_SEASON_DD, WRAP_AROUND_SEASON_STOR, JAN_DD_AUTH_NOTWRAPAROUND, FEB_DD_AUTH_NOTWRAPAROUND, MAR_DD_AUTH_NOTWRAPAROUND, APR_DD_AUTH_NOTWRAPAROUND, MAY_DD_AUTH_NOTWRAPAROUND,
                     JUN_DD_AUTH_NOTWRAPAROUND, JUL_DD_AUTH_NOTWRAPAROUND, AUG_DD_AUTH_NOTWRAPAROUND, SEP_DD_AUTH_NOTWRAPAROUND, OCT_DD_AUTH_NOTWRAPAROUND, NOV_DD_AUTH_NOTWRAPAROUND, DEC_DD_AUTH_NOTWRAPAROUND, 
                     JAN_DD_AUTH_WRAPAROUND, FEB_DD_AUTH_WRAPAROUND, MAR_DD_AUTH_WRAPAROUND, APR_DD_AUTH_WRAPAROUND, MAY_DD_AUTH_WRAPAROUND, JUN_DD_AUTH_WRAPAROUND, JUL_DD_AUTH_WRAPAROUND, AUG_DD_AUTH_WRAPAROUND,
                     SEP_DD_AUTH_WRAPAROUND, OCT_DD_AUTH_WRAPAROUND, NOV_DD_AUTH_WRAPAROUND, DEC_DD_AUTH_WRAPAROUND, NO_DD_SEASON2_INFO, NO_STOR_SEASON2_INFO, NO_SEASON2_INFO, WRAP_AROUND_SEASON2_DD, 
                     WRAP_AROUND_SEASON2_STOR, JAN_DD_AUTH_NOTWRAPAROUND2, FEB_DD_AUTH_NOTWRAPAROUND2, MAR_DD_AUTH_NOTWRAPAROUND2, APR_DD_AUTH_NOTWRAPAROUND2, MAY_DD_AUTH_NOTWRAPAROUND2, JUN_DD_AUTH_NOTWRAPAROUND2, 
                     JUL_DD_AUTH_NOTWRAPAROUND2, AUG_DD_AUTH_NOTWRAPAROUND2, SEP_DD_AUTH_NOTWRAPAROUND2, OCT_DD_AUTH_NOTWRAPAROUND2, NOV_DD_AUTH_NOTWRAPAROUND2, DEC_DD_AUTH_NOTWRAPAROUND2, JAN_DD_AUTH_WRAPAROUND2, 
                     FEB_DD_AUTH_WRAPAROUND2, MAR_DD_AUTH_WRAPAROUND2, APR_DD_AUTH_WRAPAROUND2, MAY_DD_AUTH_WRAPAROUND2, JUN_DD_AUTH_WRAPAROUND2, JUL_DD_AUTH_WRAPAROUND2, AUG_DD_AUTH_WRAPAROUND2, 
                     SEP_DD_AUTH_WRAPAROUND2, OCT_DD_AUTH_WRAPAROUND2, NOV_DD_AUTH_WRAPAROUND2, DEC_DD_AUTH_WRAPAROUND2, JAN_DD_AUTH, FEB_DD_AUTH, MAR_DD_AUTH, APR_DD_AUTH, MAY_DD_AUTH, JUN_DD_AUTH, JUL_DD_AUTH, 
                     AUG_DD_AUTH, SEP_DD_AUTH, OCT_DD_AUTH, NOV_DD_AUTH, DEC_DD_AUTH, JAN_STOR_AUTH_NOTWRAPAROUND2, FEB_STOR_AUTH_NOTWRAPAROUND2, MAR_STOR_AUTH_NOTWRAPAROUND2, APR_STOR_AUTH_NOTWRAPAROUND2, 
                     MAY_STOR_AUTH_NOTWRAPAROUND2, JUN_STOR_AUTH_NOTWRAPAROUND2, JUL_STOR_AUTH_NOTWRAPAROUND2, AUG_STOR_AUTH_NOTWRAPAROUND2, SEP_STOR_AUTH_NOTWRAPAROUND2, OCT_STOR_AUTH_NOTWRAPAROUND2, 
                     NOV_STOR_AUTH_NOTWRAPAROUND2, DEC_STOR_AUTH_NOTWRAPAROUND2, JAN_STOR_AUTH_WRAPAROUND2, FEB_STOR_AUTH_WRAPAROUND2, MAR_STOR_AUTH_WRAPAROUND2, APR_STOR_AUTH_WRAPAROUND2, MAY_STOR_AUTH_WRAPAROUND2, 
                     JUN_STOR_AUTH_WRAPAROUND2, JUL_STOR_AUTH_WRAPAROUND2, AUG_STOR_AUTH_WRAPAROUND2, SEP_STOR_AUTH_WRAPAROUND2, OCT_STOR_AUTH_WRAPAROUND2, NOV_STOR_AUTH_WRAPAROUND2, DEC_STOR_AUTH_WRAPAROUND2, 
                     JAN_STOR_AUTH_NOTWRAPAROUND, FEB_STOR_AUTH_NOTWRAPAROUND, MAR_STOR_AUTH_NOTWRAPAROUND, APR_STOR_AUTH_NOTWRAPAROUND, MAY_STOR_AUTH_NOTWRAPAROUND, JUN_STOR_AUTH_NOTWRAPAROUND, JUL_STOR_AUTH_NOTWRAPAROUND,
                     AUG_STOR_AUTH_NOTWRAPAROUND, SEP_STOR_AUTH_NOTWRAPAROUND, OCT_STOR_AUTH_NOTWRAPAROUND, NOV_STOR_AUTH_NOTWRAPAROUND, DEC_STOR_AUTH_NOTWRAPAROUND, JAN_STOR_AUTH_WRAPAROUND, FEB_STOR_AUTH_WRAPAROUND, 
                     MAR_STOR_AUTH_WRAPAROUND, APR_STOR_AUTH_WRAPAROUND, MAY_STOR_AUTH_WRAPAROUND, JUN_STOR_AUTH_WRAPAROUND, JUL_STOR_AUTH_WRAPAROUND, AUG_STOR_AUTH_WRAPAROUND, SEP_STOR_AUTH_WRAPAROUND, OCT_STOR_AUTH_WRAPAROUND, 
                     NOV_STOR_AUTH_WRAPAROUND, DEC_STOR_AUTH_WRAPAROUND, JAN_STOR_AUTH, FEB_STOR_AUTH, MAR_STOR_AUTH, APR_STOR_AUTH, MAY_STOR_AUTH, JUN_STOR_AUTH, JUL_STOR_AUTH, AUG_STOR_AUTH, SEP_STOR_AUTH, OCT_STOR_AUTH, 
                     NOV_STOR_AUTH, DEC_STOR_AUTH, `1_DIRECT`, `2_DIRECT`, `3_DIRECT`, `4_DIRECT`, `5_DIRECT`, `6_DIRECT`, `7_DIRECT`, `8_DIRECT`, `9_DIRECT`, `10_DIRECT`, `11_DIRECT`, `12_DIRECT`, `1_STORAGE`, `2_STORAGE`, 
                     `3_STORAGE`, `4_STORAGE`, `5_STORAGE`, `6_STORAGE`, `7_STORAGE`, `8_STORAGE`, `9_STORAGE`, `10_STORAGE`, `11_STORAGE`, `12_STORAGE`))
  
  
  
  # Add in the data from the second data frame next
  writeData(wbA, "USE_SEASON_FLATFILE", startRow = 3, startCol = 166,
            finalDF %>%
              select(APPLICATION_NUMBER, JAN_DIRECT, FEB_DIRECT, MAR_DIRECT, APR_DIRECT, MAY_DIRECT, JUN_DIRECT, JUL_DIRECT, AUG_DIRECT, SEP_DIRECT, OCT_DIRECT, NOV_DIRECT, DEC_DIRECT, JAN_STORAGE, FEB_STORAGE, MAR_STORAGE, 
                     APR_STORAGE, MAY_STORAGE, JUN_STORAGE, JUL_STORAGE, AUG_STORAGE, SEP_STORAGE, OCT_STORAGE, NOV_STORAGE, DEC_STORAGE))
  
  
  
  # The final step for this spreadsheet is to add the text written throughout the sheet
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 2, startRow = 1, "INFO:")
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 2, startRow = 2, "ACTION:")
  
  
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 3, startRow = 1, 
            "INPUT DATA FOR SPREADSHEET - FROM FLAT FILES")
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 3, startRow = 2, 
            "PASTE NEW INPUT DATA FROM [NAME OF SCRIPT] SCRIPT HERE - DELETE SAMPLE DATA BELOW")
  
  
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 11, startRow = 1, 
            "FORMULA - INTERMEDIATE CALCULATION")
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 11, startRow = 2, 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 141, startRow = 1, 
            "FORMULA - INTERMEDIATE CALCULATION")
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 141, startRow = 2, 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 153, startRow = 1, 
            "FORMULA - INTERMEDIATE CALCULATION")
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 153, startRow = 2, 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 166, startRow = 1, 
            "FINAL RESULTS - DIRECT DIVERSION SEASON")
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 166, startRow = 2, 
            "Hit Enter  in cell FJ3; FILL DOWN FORMULA  for Cells FK - FV- DO NOT MODIFY FORMULA")
  
  
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 179, startRow = 1, 
            "FINAL RESULTS - STORAGE DIVERSION SEASON")
  writeData(wbA, "USE_SEASON_FLATFILE", startCol = 179, startRow = 2, 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  
  # Save "wbA" to a workbook
  saveWorkbook(wbA, "OutputData/Diversion_Out_of_Season_Part_A_Scripted.xlsx", overwrite = TRUE)
  
  
  
  # Next, begin working on the workbook for Part B
  wbB <- createWorkbook()
  
  
  # This file will have two worksheets
  addWorksheet(wbB, "DATA_FROM_PART_A")
  addWorksheet(wbB, "DIVERSION_OUT_OF_SEASON")
  addWorksheet(wbB, "DIVERSION_OUT_OF_SUPER_SEASON")
  
  
  
  # The first worksheet mainly contains data in 'finalDF'
  writeData(wbB, "DATA_FROM_PART_A", startRow = 3, startCol = 1,
            finalDF %>%
              select(APPLICATION_NUMBER, JAN_DIRECT, FEB_DIRECT, MAR_DIRECT, APR_DIRECT, MAY_DIRECT, JUN_DIRECT, JUL_DIRECT, AUG_DIRECT, SEP_DIRECT, OCT_DIRECT, NOV_DIRECT, DEC_DIRECT, JAN_STORAGE, FEB_STORAGE, MAR_STORAGE, 
                     APR_STORAGE, MAY_STORAGE, JUN_STORAGE, JUL_STORAGE, AUG_STORAGE, SEP_STORAGE, OCT_STORAGE, NOV_STORAGE, DEC_STORAGE))
  
  
  # The "DIRECT" and "STORAGE" columns are then repeated in the worksheet
  writeData(wbB, "DATA_FROM_PART_A", startRow = 3, startCol = 26,
            finalDF %>%
              select(JAN_DIRECT, FEB_DIRECT, MAR_DIRECT, APR_DIRECT, MAY_DIRECT, JUN_DIRECT, JUL_DIRECT, AUG_DIRECT, SEP_DIRECT, OCT_DIRECT, NOV_DIRECT, DEC_DIRECT, JAN_STORAGE, FEB_STORAGE, MAR_STORAGE, 
                     APR_STORAGE, MAY_STORAGE, JUN_STORAGE, JUL_STORAGE, AUG_STORAGE, SEP_STORAGE, OCT_STORAGE, NOV_STORAGE, DEC_STORAGE))
  
  
  # The column names should be different for the repeats from 'finalDF'
  writeData(wbB, "DATA_FROM_PART_A", startRow = 3, startCol = 26,
            c(1:12, 1:12) %>% matrix(nrow = 1) %>% data.frame(), colNames = FALSE)
  
  
  
  # After that, add the descriptive text to the sheet
  writeData(wbB, "DATA_FROM_PART_A", startRow = 1, startCol = 1, "INFO:")
  writeData(wbB, "DATA_FROM_PART_A", startRow = 2, startCol = 1, "ACTION:")
  
  
  writeData(wbB, "DATA_FROM_PART_A", startRow = 1, startCol = 2, 
            "INPUT DATA FOR SPREADSHEET - FROM PART_A")
  writeData(wbB, "DATA_FROM_PART_A", startRow = 2, startCol = 2, 
            "PASTE AS DATA NEW INPUT DATA FROM PART_A DIRECT SEASON HERE - DELETE SAMPLE DATA BELOW")
  
  
  writeData(wbB, "DATA_FROM_PART_A", startRow = 1, startCol = 14, 
            "INPUT DATA FOR SPREADSHEET - FROM PART_A")
  writeData(wbB, "DATA_FROM_PART_A", startRow = 2, startCol = 14, 
            "PASTE NEW INPUT DATA FROM PART_A STORAGE SEASON HERE - DELETE SAMPLE DATA BELOW")
  
  
  writeData(wbB, "DATA_FROM_PART_A", startRow = 1, startCol = 26, 
            "FORMULA - INTERMEDIATE CALCULATION")
  writeData(wbB, "DATA_FROM_PART_A", startRow = 2, startCol = 26, 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  
  # Next, begin working on the second sheet
  # Add 'oosDF' to this sheet
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 1, startRow = 3,
            oosDF %>%
              select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE, 
                     `month in DIRECT season?`, `month in STOR season?`, 
                     OUT_OF_SEASON_DIRECT, OUT_OF_SEASON_STOR, DIVERSION_OUT_OF_SEASON_RECORD))
  
  
  
  # After a gap column, add 'oosCounts' too
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 12, startRow = 3,
            oosCounts)
  
  
  
  # Then, fill in the explanatory text cells
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 1, startRow = 1, "INFO:")
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 1, startRow = 2, "ACTION:")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 2, startRow = 1, 
            "INPUT DATA FOR SPREADSHEET - FROM WATER_USE_REPORT FLAT FILES")
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 2, startRow = 2, 
            "PASTE NEW INPUT DATA FROM [NAME OF SCRIPT] SCRIPT HERE - DELETE SAMPLE DATA BELOW")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 6, startRow = 1, 
            "FORMULA - INTERMEDIATE CALCULATION")
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 6, startRow = 2, 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 10, startRow = 1, 
            "RESULT - OUT OF SEASON DIVERSION")
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 10, startRow = 2, 
            "FILL DOWN FORMULAS")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 12, startRow = 1, 
            "RESULT - NO OF OUT OF SEASON DIVERSIONS BY APPLICATION ID")
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 12, startRow = 2, 
            "ARRAY - DO NOT FILL")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SEASON", startCol = 13, startRow = 2, "FILL DOWN")
  
  
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 1, startRow = 3,
            oosDF_Super %>%
              select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE, 
                     `month in SUPER season?`, 
                     OUT_OF_SEASON_SUPER, DIVERSION_OUT_OF_SEASON_RECORD))
  
  
  
  # After a gap column, add 'oosCounts' too
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 12, startRow = 3,
            oosCounts_Super)
  
  
  
  # Then, fill in the explanatory text cells
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 1, startRow = 1, "INFO:")
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 1, startRow = 2, "ACTION:")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 2, startRow = 1, 
            "INPUT DATA FOR SPREADSHEET - FROM WATER_USE_REPORT FLAT FILES")
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 2, startRow = 2, 
            "PASTE NEW INPUT DATA FROM [NAME OF SCRIPT] SCRIPT HERE - DELETE SAMPLE DATA BELOW")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 6, startRow = 1, 
            "FORMULA - INTERMEDIATE CALCULATION")
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 6, startRow = 2, 
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 10, startRow = 1, 
            "RESULT - OUT OF SEASON DIVERSION")
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 10, startRow = 2, 
            "FILL DOWN FORMULAS")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 12, startRow = 1, 
            "RESULT - NO OF OUT OF SEASON DIVERSIONS BY APPLICATION ID")
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 12, startRow = 2, 
            "ARRAY - DO NOT FILL")
  
  
  writeData(wbB, "DIVERSION_OUT_OF_SUPER_SEASON", startCol = 13, startRow = 2, "FILL DOWN")
  
  
  
  
  # Finally, save 'wbB' to a file
  saveWorkbook(wbB, "OutputData/Diversion_Out_of_Season_Part_B_Scripted.xlsx", overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()