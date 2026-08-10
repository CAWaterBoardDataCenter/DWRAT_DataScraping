# These functions help with obtaining and formatting data received from web sources

##### CDEC #####

#' @title Obtain data from CDEC
#' 
#' @description
#' This function can send a `GET` request to the [California Data Exchange 
#' Center](https://cdec.water.ca.gov/index.html) (CDEC) to procure data from 
#' one or more stations. 
#' 
#' @details
#' Given a set of arguments, this function builds the required web request. 
#' It has built-in error-handling capabilities and will process the result into 
#' a [tibble::tibble()]. 
#' 
#' The columns of this result will match the data obtained from CDEC ("STATION_ID", 
#' "DATE TIME", "VALUE", "UNITS", and "DATA_FLAG"). 
#' 
#' Typically, missing values are represented as "---" in the default data 
#' returned by CDEC. However, these entries are detected and replaced with "-999". 
#' This allows the column to be treated as numeric. 
#' 
#' @param stationVec A character vector containing one or more "Station IDs" 
#' (exactly as they appear on CDEC's website).
#' 
#' @param startDate A [Date()] object representing the first date in the data request.
#' 
#' @param endDate A [Date()] object representing the last date in the data request.
#' 
#' @param sensorNum The type of variable to obtain data for. Each Sensor Number 
#' corresponds to a different parameter. See a station's 
#' [metadata page](https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=COY) 
#' to determine which number should be specified. The default value is 23, which
#' corresponds to "RESERVOIR OUTFLOW, CFS".
#' 
#' @param durCode The timescale of the obtained data. "D" (the default) means   
#' "Daily" data, "H" is for "Hourly", and "E" is for "Event". 
#' 
#' @returns A [tibble::tibble()] containing the downloaded data.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Request data for Lake Sonoma ("WRS")
#' #
#' # According to its station metadata page, 
#' # it has "RESERVOIR INFLOW, CFS" data available on a daily scale 
#' # (https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=WRS)
#' #
#' request_CDEC("WRS", as.Date("2026-01-01"), Sys.Date(), sensorNum = 76, durCode = "D")
#' }
#' 
#' #' @examples
#' \dontrun{
#' # Request precipitation data from multiple stations
#' request_CDEC(c("COY", "WRS"), as.Date("2023-10-01"), as.Date("2024-09-30"), sensorNum = 45, durCode = "D")
#' }
request_CDEC <- function (stationVec, startDate, endDate, 
                          sensorNum = 23, durCode = "D") {
  
  # Prepare a GET request and submit it to CDEC
  
  # Obtain a table of data for the specified stations  
  # within the date range delineated by 'startDate' and 'endDate'
  
  # Sensor 23 is "Reservoir Outflow" (cfs)
  # Sensor 45 is "Incremental Precipitation" (in)
  # Sensor 76 is "Reservoir Inflow" (cfs) 
  
  # "Dur Code" set to "D" means "daily" data
  
  
  # Start by preparing the request URL
  requestURL <- paste0("https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?",
                       "Stations=", paste0(stationVec, collapse = ","),
                       "&SensorNums=", sensorNum, "&dur_code=", durCode,
                       # The dates use YYYY-MM-DD format
                       "&Start=", format(startDate, "%Y-%m-%d"),
                       "&End=", format(endDate, "%Y-%m-%d"))
  
  
  # Try to submit the GET request
  # (Also, ask for a CSV-formatted response)
  req <- try(httr::GET(requestURL), silent = TRUE)
  
  
  # Wait a bit after receiving the response
  Sys.sleep(runif(1, min = 1.1, max = 1.4))
  
  
  # Check if an error was received
  if ("try-error" %in% class(req)) {
    
    cat("\n\n")
    print(req[[1]])
    cat("\n\n")
    
    paste0("CDEC Call Failed\n\n",
           "A request failed to reach CDEC's server. Please examine the ",
           "error message above and investigate the cause.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Also check if the response is valid
  if (req$status_code != 200) {
    
    paste0("CDEC Call Failed\n\n",
           "A request sent to CDEC's server returned an error code of ", 
           req$status_code, ". This could be a problem with the request ",
           "and/or CDEC's server.\n\n",
           "Please double-check the request URL: ",
           requestURL) |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check the content of the response and change it into a data frame format
  # Then, return that result
  return(content(req) |>
           formatResponse_CDEC(sensorNum))
  
}



formatResponse_CDEC <- function (res, sensorNum) {
  
  # Given the raw CSV output from CDEC, process the data into a data frame
  
  
  # Parse 'res' as a CSV file
  cdecDF <- getDelim(I(res), delim = ",", guess_max = 10^4)
  
  
  # Make sure "STATION_ID", "DATE TIME", "VALUE", "UNITS", and "DATA_FLAG" 
  # appear in the results
  # Otherwise, output an error message
  expectedCols <- c("STATION_ID", "DATE TIME", "VALUE", "UNITS", "DATA_FLAG")
  
  
  if (!all(expectedCols %in% names(cdecDF))) {
    
    paste0("Could Not Parse CDEC Data\n\n",
           "The script failed to locate key columns in the table returned by ",
           "CDEC. Perhaps the formatting of the data has changed? Please ",
           "investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Confirm the units next
  if (sensorNum == 23) {
    
    # If 'sensorNum' is 23 (streamflow), the units should all be "CFS"
    expectedUnits <- "CFS"
    
  } else if (sensorNum == 45) {
    
    # If 'sensorNum' is 45 (incremental precip), the units should all be "INCHES"
    expectedUnits <- "INCHES"
    
  } else {
    
    # For all other sensors, take the first units that appear in the dataset
    # and assume that all records use that same value
    expectedUnits <- cdecDF$UNITS[1]
    
  }
  
  
  # Check if any entries in 'cdecDF' have different units
  if (!all(cdecDF$UNITS %in% expectedUnits)) {
    
    cat("\n\n")
    cat(paste0("Units for Sensor Number ", sensorNum, ":\n"))
    print(unique(cdecDF$UNITS))
    cat("\n\n")
    
    
    paste0("Unknown Units in CDEC Data\n\n",
           "The script expected all data for Sensor Number ", sensorNum, " to ",
           "have units of ", vec2QuotedStr(expectedUnits), ". However, something ",
           "else was detected. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, missing data in CDEC is represented by "---"
  # If these dashes appear in the "VALUE" column, the column type may be 
  # "character" instead of "numeric"
  if ("---" %in% cdecDF$VALUE) {
    
    # Replace "---" with "-999"
    cdecDF$VALUE[cdecDF$VALUE == "---"] <- -999
    
    
    # Then, convert the "VALUE" column into numeric
    cdecDF$VALUE <- as.numeric(cdecDF$VALUE)
    
  }
  
  
  # Return 'cdecDF'
  return(cdecDF)
  
}
