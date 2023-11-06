# This script is an alternative to 'RAWS_Scraper.R' that does not use RSelenium


#### Dependencies ####


require(tidyverse)
require(httr)
require(rvest)


#### Functions ####

mainProcedure <- function (StartDate, EndDate) {
  
  # Get a list of RAWS stations
  stationDF <- read_csv("InputData/Raws_Stations.csv", show_col_types = FALSE)
  
  
  
  # Iterate through the list of stations and collect data from the RAWS website
  for (i in 1:nrow(stationDF)) {
    
    # Make a POST request to the API and collect a data frame from the returned content
    resTable <- requestTable(stationDF$Station[i], StartDate, EndDate)
    
    
    
    # For some stations, remove several columns of data
    resTable <- columnRemoval(stationDF$Station[i], resTable)
    
    
    
    # For different stations, modify the column names to be slightly unique
    resTable <- columnRename(stationDF$Station[i], resTable)
    
    
    
    # All stations' tables will be combined into one data frame
    # If this is the first iteration, initialize the combined table variable
    if (i == 1) {
      
      combinedTable <- resTable
      
    # Otherwise, perform an inner join between 'combinedTable' and 'resTable' using the "Date" column
    } else {
      
      combinedTable <- combinedTable %>%
        inner_join(resTable, by = "Date")
      
    }
    
    
    
    # Wait before submitting another request
    Sys.sleep(runif(1, min = 1.25, max = 3))
    
  }
  
  
  
  # Select a subset of 'combinedTable' (while also rearranging the columns)
  combinedTable <- combinedTable %>%
    select(Date, 
           RAWS_PRECIP4, RAWS_PRECIP7, RAWS_PRECIP9,
           RAWS_TMAX5, RAWS_TMAX7, RAWS_TMAX8, 
           RAWS_TMIN5, RAWS_TMIN7, RAWS_TMIN8)
  
  
  
  # The next step is to use "Prism_Processed.csv" to help fill in missing data
  # Use a separate function for that
  combinedTable <- combinedTable %>%
    prismSub()
  
  
  
  # Next, combine the RAWS data in 'combinedTable' with CNRFC data
  # For these next operations, make sure that the "Date" column in 'combinedTable' is recognized as a Date
  combinedTable <- combinedTable %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  
  
  # Read in the CNRFC data and take a subset of its columns
  # This data will be appended to 'combinedTable', so the column names need to match
  CNRFC_Processed <- read_csv("ProcessedData/CNRFC_Processed.csv", show_col_types = FALSE) %>%
    select(Date,
           PRECIP4_HOPC1, PRECIP7_HOPC1, PRECIP9_KCVC1,
           TMAX5_BSCC1, TMAX7_SKPC1, TMAX8_SSAC1, 
           TMIN5_BSCC1, TMIN7_SKPC1, TMIN8_SSAC1) %>%
    rename(RAWS_PRECIP4 = PRECIP4_HOPC1, RAWS_PRECIP7 = PRECIP7_HOPC1, RAWS_PRECIP9 = PRECIP9_KCVC1,
           RAWS_TMAX5 = TMAX5_BSCC1, RAWS_TMAX7 = TMAX7_SKPC1, RAWS_TMAX8 = TMAX8_SSAC1,
           RAWS_TMIN5 = TMIN5_BSCC1, RAWS_TMIN7 = TMIN7_SKPC1, RAWS_TMIN8 = TMIN8_SSAC1)
  
  
  
  # Bind the two tables together
  # Then, write the result to a new CSV file
  rbind(combinedTable, CNRFC_Processed) %>%
    write_csv("ProcessedData/RAWS_Processed.csv")
  
  
  
  # Output a completion message
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



requestTable <- function (stationName, StartDate, EndDate) {
  
  # Get a table of data for the specified station within the date range
  # delineated by 'StartDate' and 'EndDate'
  
  
  
  # Prepare a POST request to the WRCC server
  dataReq <- POST(url = "https://wrcc.dri.edu/cgi-bin/wea_dysimts2.pl",
                  body = list("stn" = stationName,
                              # Set the Start Date
                              "smon" = paste0(if_else(StartDate$month < 10, "0", ""), StartDate$month),
                              "sday" = paste0(if_else(StartDate$day < 10, "0", ""), StartDate$day),
                              "syea" = StartDate$year %>% str_extract("[0-9]{2}$"),
                              # Set the End Date
                              "emon" = paste0(if_else(EndDate$month < 10, "0", ""), EndDate$month),
                              "eday" = paste0(if_else(EndDate$day < 10, "0", ""), EndDate$day),
                              "eyea" = EndDate$year %>% str_extract("[0-9]{2}$"),
                              # Select "Air Temperature" and "Precipitation" data
                              "qAT" = "ON",
                              "qPR" = "ON",
                              # Metric units
                              "unit" = "M",
                              # HTML output
                              "Ofor" = "H",
                              # Only Complete data
                              "Datareq" = "C",
                              # Apply physical limits QC to the data
                              "qc" = "Y",
                              # Missing values are "-999"
                              "miss" = "07",
                              # Don't include number of valid observations for each element
                              "obs" = "N",
                              # Subinterval start and end dates
                              "WsMon" = "01",
                              "WsDay" = "01",
                              "WeMon" = "12",
                              "WeDay" = "31"))
  
  
  
  # Wait for the response and check if it is a valid
  stopifnot(dataReq$status_code == 200)
  
  
  
  # Extract the table from the HTML content of 'dataReq'
  htmlTable <- content(dataReq) %>% as.character() %>%
    read_html() %>%
    html_node("table") %>%
    html_table(header = TRUE)
  
  
  
  # Rename most columns within the data frame
  # Then, remove unnecessary columns
  htmlTable <- htmlTable %>%
    rename(Day_Of_Year = `Day of Year`, 
           Day_Of_Run = `Day of Run`,
           Tavg = `Ave.  Average Air Temperature   Deg C`,
           Tmax = `Max.  Average Air Temperature   Deg C`,
           Tmin = `Min.  Average Air Temperature   Deg C`,
           Precipitation = `Total  Precipitation    mm`) %>%
    select(-Year, -Day_Of_Year, -Day_Of_Run)
  
  
  
  # Return 'htmlTable'
  return(htmlTable)
  
}



columnRemoval <- function (stationID, dataTable) {
  
  # For certain stations, remove some columns of data
  
  
  
  if (stationID == "CBOO") {
    
    # Remove temperature columns for the "Boonville" station
    dataTable <- dataTable %>%
      select(-Tavg, -Tmax, -Tmin)
    
  } else if (stationID == "CSRS") {
    
    # Remove the precipitation column for the "Santa Rosa" station
    dataTable <- dataTable %>%
      select(-Precipitation)
    
  }
  
  
  
  # Return 'dataTable'
  return(dataTable)
  
}



columnRename <- function (stationID, dataTable) {
  
  # To differentiate different stations' data, adjust their columns names
  
  
  
  # "Boonville" station
  if (stationID == "CBOO") {
    
    dataTable <- dataTable %>%
      rename(RAWS_PRECIP7 = Precipitation)
    
  # "Lyons Valley" station
  } else if (stationID == "CLYO") {
    
    dataTable <- dataTable %>%
      rename(RAWS_TMAX7 = Tmax,
             RAWS_TMIN7 = Tmin,
             RAWS_PRECIP4 = Precipitation)
    
  # "Hawkeye" station
  } else if (stationID == "CHAW") {
    
    dataTable <- dataTable %>%
      rename(RAWS_TMAX5 = Tmax,
             RAWS_TMIN5 = Tmin,
             RAWS_PRECIP9 = Precipitation)
    
  # "Santa Rosa" station
  } else if (stationID == "CSRS") {
    
    dataTable <- dataTable %>%
      rename(RAWS_TMAX8 = Tmax,
             RAWS_TMIN8 = Tmin)
    
  }
  
  
  
  # Return 'dataTable' after these changes
  return(dataTable)
  
}



prismSub <- function (rawsTable) {
  
  # In 'rawsTable', missing data is written as "-999"
  # Substitute those entries with data from "Prism_Processed.csv"
  
  
  
  # Read in the PRISM data file
  Prism_Processed <- read_csv("ProcessedData/Prism_Processed.csv", show_col_types = FALSE)
  
  
  
  # Subset 'Prism_Processed' to just the columns in 'rawsTable'
  # Then, substitute that data into wherever 'rawsTable' has a value of -999
  # (This works only if both variables have the same number and order of columns; column names don't need to match)
  # (Also, both 'rawsTable' and 'Prism_Processed' cannot be a tibble for this to work)
  Prism_Processed <- Prism_Processed %>%
    select(Date, 
           PP_PRECIP4, PP_PRECIP7, PP_PRECIP9, 
           PT_TMAX5, PT_TMAX7, PT_TMAX8, 
           PT_TMIN5, PT_TMIN7, PT_TMIN8) %>%
    as.data.frame()
  
  
  
  rawsTable <- as.data.frame(rawsTable)
  
  
  
  rawsTable[rawsTable == -999] <- Prism_Processed[rawsTable == -999]
  
  
  
  # Return 'rawsTable' afterwards
  return(rawsTable)
  
}




#### Script Execution ####

cat("Starting 'RAWS_API_Scraper.R'...\n")


mainProcedure(StartDate, EndDate)


remove(mainProcedure, requestTable, columnRemoval, columnRename, prismSub)