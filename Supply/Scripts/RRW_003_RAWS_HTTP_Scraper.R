# Download precipitation and temperature data from RAWS at various stations  
# in the Russian River watershed


# The required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be the four-character IDs on RAWS (e.g., "CHAW" or "CBOO") 


# The raw output will be stored in the "WebData" folder as 
# "RAWS_HTTP_Data_[startDate]_[endDate].csv"

# Note: SI units are used for the output (mm and Celsius)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")

#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_003_RAWS_HTTP_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Read in the list of stations 
  stationDF <- getFromControl_RR("RAWS_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "RAWS_STATIONS_CSV", "RAWS")
  
  
  # Iteratively submit requests to RAWS in a for loop
  for (i in 1:nrow(stationDF)) {
    
    cat(paste0("\n[", i, "/", nrow(stationDF), "]\tGetting temperature and ",
               "precipitation data for ", stationDF$STATION_ID[i], "...\n"))
    
    
    # In another function, submit a POST request to RAWS to acquire data
    downloadDF <- requestRAWS(stationDF$STATION_ID[i], startDate, endDate)
    
    
    # If this is the first iteration, define a variable to hold all stations' data
    # For subsequent iterations, simply append new data to that data frame
    if (i == 1) {
      
      rawsDF <- downloadDF
      
    } else {
      
      rawsDF <- bind_rows(rawsDF, downloadDF)
      
    }
    
    
    cat("\tDone!\n\n")
    
  }
  
  
  # Define the output file name as well
  outFile <- paste0("WebData/RAWS_HTTP_Data_", startDate, "_",
                    endDate, ".csv")
  
  
  # Write the file to the "WebData" folder
  writeOutput(rawsDF, outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_003_RAWS_HTTP_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



requestRAWS <- function (stationID, startDate, endDate) {
  
  # Prepare a POST request and submit it to RAWS
  
  # Obtain a table of climate data for the specified station 
  # within the date range delineated by 'startDate' and 'endDate'
  
  
  # However, before the request can be submitted, adjustments to 'startDate' 
  # and 'endDate' may be required
  adjDates <- adjustScrapingBounds(stationID, startDate, endDate)
  
  
  # The next step is to submit a POST request to the WRCC server
  req <- POST(url = "https://wrcc.dri.edu/cgi-bin/wea_dysimts2.pl",
              body = list("stn" = stationID,
                          # Set the Start Date
                          "smon" = twoDigitText(month(adjDates[1])),
                          "sday" = twoDigitText(day(adjDates[1])),
                          "syea" = format(adjDates[1], "%y"), # Last two digits of the year
                          # Set the End Date
                          "emon" = twoDigitText(month(adjDates[2])),
                          "eday" = twoDigitText(day(adjDates[2])), 
                          "eyea" = format(adjDates[2], "%y"),
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
                          "WeDay" = "31"),
              add_headers(`User-Agent` = sessionInfo()[["R.version"]][["version.string"]],
                          `X-User-Contact` = "DWR-SDA@Waterboards.ca.gov"))
  
  
  # Wait a bit after receiving the response
  Sys.sleep(runif(1, min = 1.1, max = 1.4))
  
  
  # Check if the response is valid
  if (req$status_code != 200) {
    
    stop(paste0("RAWS HTTP Request Failed\n\n",
                "A request sent to RAWS's server returned an error code of ", 
                req$status_code, "\n\n",
                "This could be a problem with the request and/or RAWS's server\n\n",
                "Please investigate this issue for station \"", stationID, "\"") |>
           errWrap())
    
  }
  
  
  # After that, extract the table from the HTML content of 'req'
  htmlTable <- content(req) %>% as.character() |>
    read_html() |>
    html_node("table")
  
  
  # If 'htmlTable' is NA, a <table> element could not be extracted from the response
  if (is.na(htmlTable)) {
    
    stop(paste0("Could Not Parse RAWS Output\n\n",
                "The data returned by RAWS could not be interpreted correctly\n\n",
                "No <table> element was found in the response text\n\n", 
                "This could be a problem with the request and/or RAWS's server\n\n",
                "Please investigate this issue for station \"", stationID, "\"") |>
           errWrap())
    
  }
  
  
  # If a table was successfully found, read it in as a data frame
  htmlTable <- htmlTable |>
    html_table(header = TRUE)
  
  
  # After that, make sure the expected columns are in 'htmlTable'
  expectedCols <- c("DAY_OF_YEAR" = "Day of Year", 
                    "DAY_OF_RUN" = "Day of Run",
                    "TAVG" = "Ave.  Average Air Temperature   Deg C",
                    "TMAX" = "Max.  Average Air Temperature   Deg C",
                    "TMIN" = "Min.  Average Air Temperature   Deg C",
                    "PRECIPITATION" = "Total  Precipitation    mm",
                    "DATE" = "Date",
                    "YEAR" = "Year")
  
  # Note: The element names are the eventual column renames, while the 
  #       elements themselves are the expected names in 'htmlTable'
  
  
  # Check if any of the expected columns are missing in 'htmlTable'
  if (anyFalse(c(expectedCols) %in% names(htmlTable))) {
    
    stop(paste0("Could Not Parse RAWS Output\n\n",
                "The data returned by RAWS could not be interpreted correctly (",
                "not all of the expected columns were found)\n\n", 
                "This could be a problem with the request and/or RAWS's server\n\n",
                "Please investigate this issue for station \"", stationID, "\"\n\n",
                length(which(!(expectedCols %in% names(htmlTable)))), " Missing Column(s):\n\n",
                paste0("(*) ", expectedCols[which(!(expectedCols %in% names(htmlTable)))],
                       collapse = "\n\n")) |>
           errWrap())
    
  }
  
  
  # Update the column names using 'expectedCols'
  htmlTable <- htmlTable |>
    rename(all_of(expectedCols))
  
  
  # As a penultimate step, make sure the date column is formatted correctly
  htmlTable <- htmlTable |>
    mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))
  
  
  # Finally, append the station ID as a column and return the data
  return(htmlTable |>
           mutate(STATION_ID = stationID))
  
}



adjustScrapingBounds <- function (stationID, startDate, endDate) {
  
  # Requests to RAWS can fail if the dataset bounds are improper
  
  # This procedure checks the station's page on RAWS to get its start and 
  # end dates
  # 'startDate' cannot exceed these bounds; otherwise an error is returned
  # 'endDate' can exceed the limits without any issue
  # ('endDate' can even be a date later than today!)
  
  # The final returned result is a vector containing adjusted date bounds 
  # to use in the request
  
  
  # First, there is a glitch in RAWS's system
  # The "Total Precipitation" will always be returned as 
  # "missing" on the first day of the requested range
  # The workaround for this issue is to set 'startDate' to one day earlier
  adjStart <- startDate - 1
  
  
  # However, 'adjStart' cannot exceed the start of the dataset
  # To double-check this, extract the start and end dates from 
  # the station's "Daily Time Series" page
  dateBounds <- getDatasetBounds(stationID)
  
  
  # Compare 'startDate' to the station's data start date
  # It should be the more recent of the two dates
  if (adjStart < dateBounds[1]) {
    
    adjStart <- dateBounds[1]
    
    # The glitch with "Total Precipitation" still applies in this case,
    # but the request will fail if we try to set the date to one day earlier
    
  }
  
  
  # Make sure 'endDate' is more recent than 'adjStart'
  # If not, set it to one day greater than 'adjStart'
  if (endDate <= adjStart) {
    
    adjEnd <- adjStart + 1
    
  } else {
    
    adjEnd <- endDate
    
  }
  
  
  # Return 'adjStart' and 'adjEnd' in a vector
  return(c(adjStart, adjEnd))
  
}



getDatasetBounds <- function (stationID) {
  
  # For the RAWS station denoted by 'stationID', extract its start date 
  # and end date
  
  # This is noted on the "Daily Time Series" webpage
  
  # Towards the beginning of the page, 
  # there is a line that says "Earliest available data: [MONTH] [YEAR]"
  
  # Use that to determine the start date
  
  
  # Start by scraping the contents of the page
  pageURL <- paste0("https://wrcc.dri.edu/cgi-bin/wea_dysimts.pl?ca", 
                    stationID)
  
  
  pageContent <- pageURL |>
    read_lines()
  
  
  # Wait a bit before continuing
  Sys.sleep(runif(1, min = 1.0, max = 1.3))
  
  
  # Find the text that says "Earliest available data"
  # Extract the month and year from that name
  # Then, convert it into a date variable 
  # (with the day set to the first of the month)
  startDateString <- grep("Earliest available data:", pageContent, 
                          ignore.case = TRUE, value = TRUE) |>
    str_extract(" [A-Za-z]+ [0-9]+\\.?$") |>
    trimws() |>
    str_remove("\\.$") |>
    paste0(" 01") |>
    as.Date(format = "%B %Y %d")
  
  
  # If 'startDateString" is not a single string, the extraction failed
  if (is.na(startDateString)) {
    
    stop(paste0("RAWS Station - Could Not Extract Start Date\n\n",
                "The script attempted to find the start date for station \"",
                stationID,"\"; however, this information could not be ",
                "extracted from its \"Daily Time Series\" page on RAWS\n\n",
                "This could be due to a network error or a change to the ",
                "website (you can check that by viewing this URL: \"",
                pageURL, "\"); there should be a text element that starts with ",
                "\"Earliest available data\"") |>
           errWrap() |>
           str_replace("(not)", col_red("\\1")) |>
           str_replace("(network)", col_red("\\1")) |>
           str_replace("(error)", col_red("\\1")) |>
           str_replace("(change)", col_blue("\\1")))
    
  } else if (length(startDateString) != 1) {
    
    stop(paste0("RAWS Station - Could Not Extract Start Date\n\n",
                "The script attempted to find the start date for station \"",
                stationID,"\"; however, more than one match was found in its ",
                "\"Daily Time Series\" page on RAWS\n\n",
                "There should have been only one match on the page for ",
                "\"Earliest available data\" (you can investigate that by ",
                "viewing this URL: \"", pageURL, "\")") |>
           errWrap() |>
           str_replace("(more)", col_red("\\1")) |>
           str_replace("(than)", col_red("\\1")) |>
           str_replace("(one)", col_red("\\1")) |>
           str_replace("(match)", col_red("\\1")))
    
  }
  
  
  # After that, get the end date of the dataset
  # Find the text that says "Latest available data"
  # Extract the month and year from that name
  # Then, convert it into a date variable 
  # (with the day set to the first of the month)
  endDateString <- grep("Latest available data:", pageContent, 
                          ignore.case = TRUE, value = TRUE) |>
    str_extract(" [A-Za-z]+ [0-9]+\\.?$") |>
    trimws() |>
    str_remove("\\.$") |>
    paste0(" 01") |>
    as.Date(format = "%B %Y %d")
  
  
  # If 'endDateString" is not a single string, the extraction failed
  if (is.na(endDateString)) {
    
    stop(paste0("RAWS Station - Could Not Extract End Date\n\n",
                "The script attempted to find the end date for station \"",
                stationID,"\"; however, this information could not be ",
                "extracted from its \"Daily Time Series\" page on RAWS\n\n",
                "This could be due to a network error or a change to the ",
                "website (you can check that by viewing this URL: \"",
                pageURL, "\"); there should be a text element that starts with ",
                "\"Latest available data\"") |>
           errWrap() |>
           str_replace("(not)", col_red("\\1")) |>
           str_replace("(network)", col_red("\\1")) |>
           str_replace("(error)", col_red("\\1")) |>
           str_replace("(change)", col_blue("\\1")))
    
  } else if (length(endDateString) != 1) {
    
    stop(paste0("RAWS Station - Could Not Extract End Date\n\n",
                "The script attempted to find the end date for station \"",
                stationID,"\"; however, more than one match was found in its ",
                "\"Daily Time Series\" page on RAWS\n\n",
                "There should have been only one match on the page for ",
                "\"Latest available data\" (you can investigate that by ",
                "viewing this URL: \"", pageURL, "\")") |>
           errWrap() |>
           str_replace("(more)", col_red("\\1")) |>
           str_replace("(than)", col_red("\\1")) |>
           str_replace("(one)", col_red("\\1")) |>
           str_replace("(match)", col_red("\\1")))
    
  }
  
  
  
  # Return a vector containing 'startDateString' and 'endDateString'
  return(c(startDateString, endDateString))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
