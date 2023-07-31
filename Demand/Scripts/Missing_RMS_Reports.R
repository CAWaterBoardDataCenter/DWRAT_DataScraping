# Check for missing RMS report submissions


# This script is a recreation of the Excel module "Missing_RMS_Reports.xlsx"


#### Dependencies ####


require(tidyverse)
require(openxlsx)


#### Script Procedure ####


mainProcedure <- function () {
  
  # The main body of the script
  
  
  
  # Read in the input CSV file "Missing_RMS_Reports_FINAL.csv"
  reportDF <- read.csv("InputData/Missing_RMS_Reports_FINAL.csv")
  
  
  
  # Define the default starting year for reports
  # (It will be 2014)
  reportStartYear <- 2014
  
  
  
  # Define the end year for reports next
  # (It will be last year)
  reportEndYear <- year(Sys.Date()) - 1
  
  
  
  # Next, columns will be added to 'reportDF'
  
  # The first one is an intermediate column not in the module
  # The years will be extracted from "ASSIGNED_PRIORITY_DATE" (they're the first four digits of each number)
  reportDF <- reportDF %>%
    mutate(PRIORITY_YEAR = str_extract(ASSIGNED_PRIORITY_DATE, "^[0-9]{4}") %>% as.integer())
  
  
  # The next column is "EXPECTED_REPORTS_BY_WR"
  # This is an estimate of the number of expected reports 
  # It uses 'reportStarYear', 'reportEndYear', and the column "PRIORITY_YEAR"
  reportDF <- reportDF %>%
    rowwise() %>%
    mutate(EXPECTED_REPORTS_BY_WR = reportEndYear - max(reportStartYear, PRIORITY_YEAR) + 1) %>%
    ungroup()
  
  
  
  # After that, create a new data frame that has one row per application number
  # It will only have two columns; the second column will be "EXPECTED_REPORTS_BY_WR"
  expectedReports <- reportDF %>%
    select(APPLICATION_NUMBER, EXPECTED_REPORTS_BY_WR) %>% 
    unique()
  
  
  
  # Error Check
  # Each application number should only appear once
  stopifnot(nrow(expectedReports) == length(unique(expectedReports$APPLICATION_NUMBER)))
  
  
  
  # Create another two-column data frame that also has only one row per application number
  # The other column will be a count of the number of annual submissions made
  reportCounts <- reportDF %>%
    filter(YEAR >= 2014) %>%
    select(APPLICATION_NUMBER, YEAR) %>%
    unique() %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(ANNUAL_REPORTS_SUBMITTED = n())
  
  
  
  # Merge these two data frames together
  reportCounts <- reportCounts %>%
    left_join(expectedReports, by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  # Add another column to 'reportCounts'
  # This is the number of annual reports missing for each right holder
  reportCounts <- reportCounts %>%
    mutate(NO_OF_MISSING_ANNUAL_REPORTS = EXPECTED_REPORTS_BY_WR - ANNUAL_REPORTS_SUBMITTED)
  
  
  
  # The final column is for the opposite problem 
  # (A surplus of report submissions instead of a deficit)
  # This can be an indicator of issues with the priority dates
  reportCounts <- reportCounts %>%
    mutate(MORE_REPORTS_THAN_EXPECTED = 
             if_else(ANNUAL_REPORTS_SUBMITTED > EXPECTED_REPORTS_BY_WR,
                     "PRIORITY_DATE_ERROR", ""))
  
  
  
  # After that, export the results as a spreadsheet
  # Use a separate function for that
  writeExcel(reportDF, reportCounts, reportStartYear, reportEndYear)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


writeExcel <- function (reportDF, reportCounts, reportStartYear, reportEndYear) {
  
  # Create an Excel file that matches the format of the module "Missing_RMS_Reports.xlsx"
  
  
  
  # Initialize the workbook
  wb <- createWorkbook()
  
  
  
  # Add a worksheet to that file
  addWorksheet(wb, "Missing_RMS_Reports")
  
  
  
  # Write most of 'reportDF' to the beginning of the spreadsheet
  writeData(wb, "Missing_RMS_Reports", startCol = 1, startRow = 3, 
            reportDF %>%
              select(APPLICATION_NUMBER, WATER_RIGHT_ID, 
                     YEAR, MONTH, AMOUNT, DIVERSION_TYPE, ASSIGNED_PRIORITY_DATE))
  
  
  
  # Add the last column of 'reportDF' to Column J (10)
  writeData(wb, "Missing_RMS_Reports", startCol = 10, startRow = 3,
            reportDF %>% select(EXPECTED_REPORTS_BY_WR))
  
  
  
  # Add data about the starting and ending years as well
  writeData(wb, "Missing_RMS_Reports", startCol = 8, startRow = 3,
            "DEFAULT_START_REPORTING_YEAR")
  
  writeData(wb, "Missing_RMS_Reports", startCol = 8, startRow = 4,
            reportStartYear)
  
  writeData(wb, "Missing_RMS_Reports", startCol = 9, startRow = 3,
            "MANUAL_START_YEAR")
  
  
  writeData(wb, "Missing_RMS_Reports", startCol = 8, startRow = 5,
            "DEFAULT_END_REPORTING_YEAR")
  
  writeData(wb, "Missing_RMS_Reports", startCol = 8, startRow = 6,
            reportEndYear)
  
  writeData(wb, "Missing_RMS_Reports", startCol = 9, startRow = 5,
            "MANUAL_END_YEAR")
  
  
  
  # Also fill in some of the surrounding text on this part of the spreadsheet
  writeData(wb, "Missing_RMS_Reports", startCol = 1, startRow = 1,
            "INFO:")
  
  writeData(wb, "Missing_RMS_Reports", startCol = 1, startRow = 2,
            "ACTION:")
  
  
  writeData(wb, "Missing_RMS_Reports", startCol = 2, startRow = 1,
            "INPUT DATA FOR SPREADSHEET - FROM FLAT FILES")
  
  writeData(wb, "Missing_RMS_Reports", startCol = 2, startRow = 2,
            "PASTE NEW INPUT DATA FROM [NAME OF SCRIPT] SCRIPT HERE - DELETE SAMPLE DATA BELOW")
  
  
  writeData(wb, "Missing_RMS_Reports", startCol = 10, startRow = 1,
            "FORMULA - INTERMEDIATE CALCULATION")
  
  writeData(wb, "Missing_RMS_Reports", startCol = 10, startRow = 2,
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA")
  
  
  
  # Add the data from 'reportCounts' next
  writeData(wb, "Missing_RMS_Reports", startCol = 12, startRow = 3,
            reportCounts %>%
              select(APPLICATION_NUMBER, ANNUAL_REPORTS_SUBMITTED,
                     EXPECTED_REPORTS_BY_WR, NO_OF_MISSING_ANNUAL_REPORTS,
                     MORE_REPORTS_THAN_EXPECTED))
  
  
  
  # After that, add in more descriptive text
  writeData(wb, "Missing_RMS_Reports", startCol = 12, startRow = 1,
            "ARRAY FORMULA - INDEX")
  
  writeData(wb, "Missing_RMS_Reports", startCol = 12, startRow = 2,
            "DO NOT MODIFY FORMULA - DO NOT FILL DOWN")
  
  
  writeData(wb, "Missing_RMS_Reports", startCol = 13, startRow = 1,
            "FINAL RESULTS - MISSING RMS REPORTS")
  
  writeData(wb, "Missing_RMS_Reports", startCol = 13, startRow = 2,
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA - USE THESE RESULTS FOR QAQC - REFER TO DESCRIPTION TAB FOR DETAILS")
  
  
  
  # The final step is to save the workbook to a file
  saveWorkbook(wb, "OutputData/Missing_RMS_Reports_Scripted.xlsx", overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()