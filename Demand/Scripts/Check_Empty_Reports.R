# Some RMS reports are empty (NA for every month and diversion type)
# Later scripts will eventually remove these records from consideration
# However, some reports should be treated as 0s instead of NA
# (meaning they should impact the final monthly averages used by DWRAT)
# This script will try to find them by checking if the all-NA report actually exists on eWRIMS
# (Some NA-only years are introduced by the procedure for handling both calendar and water years)

require(tidyverse)
require(readxl)
require(writexl)


#### Functions ####


mainProcedure <- function (ws) {
  
  
  cat("Starting 'Check_NA_Reports.R'...\n")
  
  
  # Read in th expected demand dataset
  flowDF <- paste0("OutputData/", ws$ID, 
                   "_ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx") %>%
    read_xlsx()
  
  
  
  # Make sure that "APPLICATION_NUMBER" and "YEAR" contain no NA records
  stopifnot(!anyNA(flowDF$APPLICATION_NUMBER))
  stopifnot(!anyNA(flowDF$YEAR))
  
  
  
  # Find the records with a value of "NA"
  # Perform sums along each row (this gives the number of NA values per row)
  naSums <- is.na(flowDF) %>% rowSums()
  
  
  
  # "APPLICATION_NUMBER" and "YEAR" are confirmed to NOT contain any NA values
  # Therefore, if a row has all NA flow volumes, 
  # the total NA count for the row should equal "ncol(flowDF) - 2"
  naRecords <- flowDF[which(naSums == ncol(flowDF) - 2), ]
  
  
  
  # If 'naRecords' contains no rows, nothing needs to be done
  if (nrow(naRecords) == 0) {
    
    cat("Done!\n")
    
    return(invisible(NULL))
    
  }
  
  
  
  # Check if a manual review was already performed for this issue
  if (!is.na(ws$NA_REPORTS_SPREADSHEET_PATH)) {
    
    
    
  }
  
  
  
  # 
  
  
  # Otherwise, output a message about the presence of NA reports
  cat("There are reports with empty values for every month and diversion type (DIRECT/STORAGE) in the calendar year/water year\n")
  
  
  
  # Prepare a data frame that will hold information about the NA records
  naDF <- data.frame(APPLICATION_NUMBER = naRecords$APPLICATION_NUMBER,
                     YEAR = naRecords$YEAR,
                     REPORT_LIST_TABLE_LINK = NA_character_,
                     REPORT_LINK = NA_character_,
                     REPLACE_NA_VALUES_WITH_ZEROS = NA,
                     ALT_VALUE_REPLACEMENT = NA_real_)
  
  
  
  # Then, send a query to eWRIMS about these water rights
  # Check if an RMS report exists for the flagged year
  
  
  
  # First read in the eWRIMS flat file
  # (It has the water right IDs needed to access the RMS Reports table)
  ewrimsDF <- list.files("IntermediateData/", pattern = "^Flat_File_e", full.names = TRUE) %>%
    sort() %>% tail(1) %>%
    read_csv(show_col_types = FALSE)
  
  
  
  cat("Checking reports on eWRIMS...\n")
  
  
  
  # Iterate through the values in 'naRecords'
  for (i in 1:nrow(naRecords)) {
    
    # First, get the water right ID that corresponds to this value of "APPLICATION_NUMBER"
    wrID <- ewrimsDF$WR_WATER_RIGHT_ID[which(ewrimsDF$APPLICATION_NUMBER == 
                                               naRecords$APPLICATION_NUMBER[i])] %>%
      unique()
    
    
    
    # Make sure 'wrID' was successfully identified
    stopifnot(length(wrID) == 1)
    stopifnot(!is.na(wrID))
    
    
    
    # Construct the URL that will lead to the table of RMS reports for this right
    # (That URL will be saved to 'naDF')
    naDF$REPORT_LIST_TABLE_LINK[i] <- paste0("https://ciwqs.waterboards.ca.gov/ciwqs/",
                                             "ewrims/listReportsForWaterRight.do?waterRightId=", wrID)
    
    
    
    # Request a list of RMS reports for the right from eWRIMS
    htmlPage <- naDF$REPORT_LIST_TABLE_LINK[i] %>%
      read_lines()
    
    
    
    # Wait a bit
    Sys.sleep(runif(1, min = 1.1, max = 1.3))
    
    
    
    # Extract the report table from the HTML
    reportDF <- extractTable(htmlPage)
    
    
    
    # Check if the reporting year mentioned in 'naRecords' appears in 'reportDF'
    # If it is, get the link to the report and add it to 'naDF'
    if (naRecords$YEAR[i] %in% reportDF$Year) {
      
      # Extract the URL from the "Action" column of 'reportDF'
      # Save it to 'naDF'
      naDF$REPORT_LINK[i] <- reportDF$Action[reportDF$Year == naRecords$YEAR[i]] %>%
        str_extract("href=.+\"") %>%
        str_split("\"") %>% unlist() %>%
        str_subset("/") %>%
        str_replace("^\\.+", "https://ciwqs.waterboards.ca.gov/ciwqs")
      
    } else {
      
      naDF$REPLACE_NA_VALUES_WITH_ZEROS[i] <- FALSE
      
    }
    
  }
  
  
  
  # Output a message that a manual review is required
  # Then write 'naDF' to a file
  cat("\nA manual review is required to inspect the reports that contain only NA (empty values)\n")
  cat("Reports that truly do not exist should be left as empty. Reports that have data (even if 0) should not be NA\n")
  cat(paste0("\nPlease see ", "OutputData/", ws$ID, "_Empty_Reports_Manual_Review.xlsx", "\n"))
  
  
  
  naDF %>%
    write_xlsx(paste0("OutputData/", ws$ID, "_Empty_Reports_Manual_Review.xlsx"))
  
  
  
  # Output a completion message
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



extractTable <- function (htmlPage) {
  
  # From the eWRIMS report page, extract the table of report submissions
  
  
  
  # Combine the HTML vector into one string
  # Then split it at the table row (<tr>) elements
  # Also split it at the closing </table> tags
  htmlPage <- htmlPage %>%
    paste0(collapse = "") %>%
    str_split("<tr>?") %>% unlist() %>%
    str_split("</table>?") %>% unlist()
  
  
  
  # Remove rows from before the table header tags
  htmlPage <- htmlPage[grep("<th", htmlPage):length(htmlPage)]
  
  
  
  # These are the report table's headers
  tableHeaders <- htmlPage %>%
    str_subset("<th") %>%
    str_split("</?t[hr]>?") %>% unlist() %>%
    trimws() %>% str_subset("^$", negate = TRUE)
  
  
  
  # Convert the report table into a data frame
  reportDF <- htmlPage %>%
    str_subset("View</a") %>%
    str_split("</?t[dr]>?") %>% unlist() %>%
    trimws() %>% str_subset("^$", negate = TRUE) %>%
    matrix(ncol = length(tableHeaders), byrow = TRUE) %>%
    data.frame() %>%
    set_names(tableHeaders)
  
  
  
  # Return 'reportDF'
  return(reportDF)
  
}


#### Script Execution ####


mainProcedure(ws)


#### Cleanup ####

remove(mainProcedure, extractTable)