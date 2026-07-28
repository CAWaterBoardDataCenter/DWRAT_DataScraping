# Download precipitation and temperature data from RAWS at various stations  
# in the Russian River watershed


# The required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be the four-character IDs on RAWS (e.g., "CHAW" or "CBOO") 


# The raw output will be stored in the "Intermediate" folder as 
# "RAWS_HTTP_Data_[startDate]_[endDate].csv"

# Note: SI units are used for the output (mm and Celsius)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")

#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_003_RAWS_HTTP_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
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
  outFile <- paste0("W2_Russian_River/Intermediate/RAWS_HTTP_Data_", 
                    startDate, "_", endDate, ".csv")
  
  
  # Write the file to the "Intermediate" folder
  writeOutput(rawsDF, outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_003_RAWS_HTTP_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



requestRAWS <- function (stationID, startDate, endDate, counter = 1, maxTries = 15) {
  
  # Prepare a POST request and submit it to RAWS
  
  # Obtain a table of climate data for the specified station 
  # within the date range delineated by 'startDate' and 'endDate'
  
  
  # However, before the request can be submitted, adjustments to 'startDate' 
  # and 'endDate' may be required
  adjDates <- adjustScrapingBounds(stationID, startDate, endDate)
  
  
  # One self-imposed restriction is request splitting
  # To avoid overwhelming RAWS's server, no more than three years of data 
  # should be requested at a time
  if (difftime(endDate, startDate, units = "days") > 365 * 3) {
    return(splitRequest(stationID, startDate, endDate, maxDays = 365 * 3))
  }
  
  
  # The next step is to submit a POST request to the WRCC server
  req <- try(POST(url = "https://wrcc.dri.edu/cgi-bin/wea_dysimts2.pl",
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
                              `X-User-Contact` = "DWR-SDA@Waterboards.ca.gov",
                              `X-User-Name` = Sys.info()[["user"]])))
  
  
  # Wait a bit after receiving the response
  Sys.sleep(runif(1, min = 1.4, max = 1.9))
  
  
  # Check for errors
  if ("try-error" %in% class(req)) {
    
    # If the error is "Failure when receiving data from the peer [wrcc.dri.edu]"
    # "schannel: server closed abruptly (missing close_notify)", 
    # consider retrying the request if 'counter' is less than 'maxTries'
    if (grepl("server closed abruptly", req[1]) && counter < maxTries) {
      
      # Determine the number of seconds to wait before retrying
      # (This value is related to the value of 'counter')
      waitTime <- counter * runif(1, min = 10, max = 25)
      
      
      # Notify the user about this
      cat("\n\n")
      paste0("The request failed! This was attempt ", counter, " of ",
             maxTries, "! Retrying in ", round(waitTime), 
             " seconds...") |>
        errWrap() |>
        message()
      cat("\n\n")
      
      
      # Wait for a bit
      Sys.sleep(waitTime)
      
      
      # Submit the request again
      return(requestRAWS(stationID, startDate, endDate, counter = counter + 1))
      
    }
    
    # If a different error occurred, or if too many failed requests were received,
    # output the error message and stop the script
    
    cat("\n\n")
    print(req)
    cat("\n\n")
    
    stop(paste0("RAWS HTTP Request Failed\n\n",
                "A request sent to RAWS's server failed to resolve successfully. ", 
                "This could be a problem with the request and/or RAWS's server ",
                "(the error message is posted above).\n\n",
                "Please investigate this issue for station \"", stationID, "\"") |>
           errWrap())
    
  }
  
  
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
  if (!all(expectedCols %in% names(htmlTable))) {
    
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



splitRequest <- function (stationID, startDate, endDate, maxDays) {
  
  # For data requests that cover a large date range, 
  # split the range into chunks and perform several requests to RAWS
  
  # Combine the response tibbles into one and return that
  
  
  # First, get intermediate dates between 'startDate' and 'endDate' 
  # that satisfy the limitation set by 'maxDays'
  
  # Borrow a function from CIMIS API to do that task
  functionStealer("W2_Russian_River/Scripts/RRW_004_CIMIS_API_Scraper.R", "splitDays")
  
  
  dateVec <- splitDays(startDate, endDate, dayGap = maxDays)
  
  
  # Output a message to the user to inform them of the split
  cat(paste0("\n\tSplitting into ", length(dateVec) - 1, " API calls...\n"))
  
  
  # Iterate through 'dateVec' and submit requests to RAWS
  for (i in 2:length(dateVec)) {
    
    # Start with a status message
    cat(paste0("\n\t[", i - 1, "/", length(dateVec) - 1, "]\tRequesting...\n"))
    
    
    # Take two consecutive dates from 'dateVec' 
    # and request the data between them
    
    
    # However, RAWS's glitches must be considered too
    # The server returns an error if the requested day is before the station's
    # actual start date
    
    # In the very first iteration (i = 2), 'combinedDF' will be defined with 
    # the earliest available data
    
    # If the end date in this iteration's split request is EARLIER than the 
    # first date in 'combinedDF', skip it
    if (i > 2 && dateVec[i] < min(combinedDF$DATE)) {
      
      cat("\n\t\tSkipping...\n")
      next
      
    }
    
    
    # If there are no issues (or if this is the first run), get data
    # for a subset of the full date range
    iterRes <- requestRAWS(stationID, dateVec[i - 1], dateVec[i])
    
    
    # Combine 'iterRes' after each request
    if (i == 2) {
      
      combinedDF <- iterRes
      
    } else {
      
      combinedDF <- bind_rows(combinedDF, 
                              iterRes |> filter(!(DATE %in% combinedDF$DATE))) |>
        unique()
      
      
      # Because of the issues with RAWS, data from the day prior to 'dateVec[i - 1]'
      # is present in 'iterRes' too 
      # (That day's values are all -999 because of the bug)
      # Exclude that data from 'combinedDF'
      
    }
    
    
    # Output another message to the user at the end of the loop
    cat("\n\t\tDone!\n")
    
    
    # Wait a bit before proceeding to the next request
    Sys.sleep(runif(1, min = 2.2, max = 3.8))
    
  }
  
  
  # Finally, return 'combinedDF'
  return(combinedDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
