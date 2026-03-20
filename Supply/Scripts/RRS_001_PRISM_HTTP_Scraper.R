# Download precipitation and temperature data from PRISM at various locations 
# in the Russian River watershed

# These locations correspond to NOAA, RAWS, and CIMIS weather stations


# The required input is two CSV files that correspond to: 
#   (1) PRMS-related precipitation and temperature stations
#   (2) SRP-related precipitation and temperature stations 

# Each of these files must contain three columns:
#  (1) LATITUDE
#  (2) LONGITUDE
#  (3) STATION_ID


# Two corresponding output CSV files are produced and stored in the "WebData" folder
#  (1) "PRISM_PRMS_Data_[startDate]_[endDate].csv"
#  (2) "PRISM_SRP_Data_[startDate]_[endDate].csv"


# Note: The PRMS-related output file uses SI units (mm and Celsius), 
#       while the SRP-related output file has US customary units (in and Fahrenheit)


#### Setup ####

remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRS_001_PRISM_HTTP_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # PRISM does not have data earlier than 1981-01-01
  # If 'startDate' is earlier than this date, output an error message
  if (startDate < "1981-01-01") {
    
    stop(paste0("Requested Date Range - Start Date Issue\n\n",
                "The earliest date for which PRISM has data available is ",
                "1981-01-01. The input start date (\"", startDate, "\") is ",
                "too early. Please revise this input.") |>
           errWrap())
    
  }
  
  
  cat("\n[1/2]\tGetting precipitation and temperature data for PRMS-related stations...\n")
  
  
  # Read in the list of stations 
  stationDF <- getFromSupplyControl_RR("PRISM_PRMS_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateInput(stationDF, "PRISM_PRMS_STATIONS_CSV")
  
  
  # Prepare the request content
  bodyList <- list(call = "pp/daily_timeseries_mp",
                   proc = "gridserv",
                   lons = stationDF$LONGITUDE |> paste0(collapse = "|"),   # Latitude
                   lats = stationDF$LATITUDE |> paste0(collapse = "|"),    # Longitude
                   names = stationDF$STATION_ID |> paste0(collapse = "|"), # Station Names
                   spares = "800m",            # Resolution (4km or 800m)
                   interp = "idw",             # Interpolate grid cell values ("0" if no)
                   stats = "ppt tmin tmax",    # Precipitation + Minimum & Maximum Temperature
                   units = "si",               # Metric units
                   range = "daily",            # Daily values
                   start = paste0(year(startDate), twoDigitText(month(startDate)), twoDigitText(day(startDate))),
                   end = paste0(year(endDate), twoDigitText(month(endDate)), twoDigitText(day(endDate))),
                   stability = "provisional")
  
  
  # Submit the request for precipitation data
  getPRISM(bodyList, paste0("WebData/PRISM_PRMS_Data_", startDate, "_", endDate, ".csv"))
  
  
  # Add to the message
  cat("\tDone!\n\n")
  
  
  # Wait a bit before proceeding
  Sys.sleep(1)
  
  
  # The final step is to get both precipitation and temperature data for the SRP stations
  
  
  cat("[2/2]\tGetting precipitation and temperature data for SRP-related stations...\n")
  
  
  # Read in a list of SRP stations
  stationDF <- getFromSupplyControl_RR("PRISM_SRP_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateInput(stationDF, "PRISM_SRP_STATIONS_CSV")
  
  
  # Prepare the POST request content
  # The SRP stations require English units (inches and Fahrenheit)
  bodyList <- list(call = "pp/daily_timeseries_mp",
                   proc = "gridserv",
                   lons = stationDF$LONGITUDE |> paste0(collapse = "|"),  # Latitude
                   lats = stationDF$LATITUDE |> paste0(collapse = "|"), # Longitude
                   names = stationDF$STATION_ID |> paste0(collapse = "|"),       # Station Names
                   spares = "800m",
                   interp = "idw",
                   stats = "ppt tmin tmax", # Precipitation + Minimum and maximum temperatures
                   units = "eng", # US Customary units
                   range = "daily",
                   start = paste0(year(startDate), twoDigitText(month(startDate)), twoDigitText(day(startDate))),
                   end = paste0(year(endDate), twoDigitText(month(endDate)), twoDigitText(day(endDate))),
                   stability = "provisional")
  
  
  # Submit the request for precipitation and temperature data
  getPRISM(bodyList, paste0("WebData/PRISM_SRP_Data_", startDate, "_", endDate, ".csv"))
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  cat(col_green("\n'RRS_001_PRISM_HTTP_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateInput <- function (stationDF, sourceField) {
  
  # Make sure that 'stationDF' is formatted correctly
  # If there are any issues, notify the user
  
  
  # 'stationDF' should contain at least three columns: "LATITUDE", "LONGITUDE", and "STATION_ID"
  if (anyFalse(c("LATITUDE", "LONGITUDE", "STATION_ID") %in% names(stationDF))) {
    
    stop(paste0("Station Input File - Column Issue\n\n",
                "The input file containing PRISM target coordinates does not have ",
                "the three required columns (\"LATITUDE\", \"LONGITUDE\", and ",
                "\"STATION_ID\"). Please correct this file and try again.\n\n",
                "The input file must contain the WGS84 coordinates and unique ",
                "identifiers for each location\n\n",
                "Also, the names of these columns must match exactly\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), "')") |>
           errWrap() |>
           str_replace("(does not)", col_red("\\1")) |>
           str_replace("(exactly)", col_red("\\1")))
    
  }
  
  
  # Make sure there are no missing entries in these three columns
  if (anyNA(stationDF$LATITUDE) || anyNA(stationDF$LONGITUDE) || anyNA(stationDF$STATION_ID)) {
    
    stop(paste0("Station Input File - Missing Data Issue\n\n",
                "The input file containing PRISM target coordinates has one or more ",
                "missing elements in its required columns (\"LATITUDE\", ",
                "\"LONGITUDE\", and \"STATION_ID\")\n\n", 
                "Please fill in any empty entries in these three columns\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), "')") |>
           errWrap() |>
           str_replace("(missing)", col_red("\\1")))
    
  }
  
  
  # Ensure that no IDs are duplicated in 'stationDF'
  if (length(stationDF$STATION_ID) != length(unique(stationDF$STATION_ID))) {
    
    stop(paste0("Station Input File - Duplicate ID Issue\n\n",
                "The input file containing PRISM target coordinates has one or more ",
                "values in its \"STATION_ID\" column that are duplicated\n\n", 
                "Please ensure that each row of the input file has a unique value for ",
                "this column\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), "')") |>
           errWrap() |>
           str_replace("(duplicated)", col_red("\\1")))
    
  }
  
  
  # Finally, check the types of "LATITUDE" and "LONGITUDE"
  if (is.character(stationDF$LATITUDE) || is.character(stationDF$LONGITUDE)) {
    
    stop(paste0("Station Input File - Coordinates Type Issue\n\n",
                "The \"LATITUDE\" and/or \"LONGITUDE\" columns of the input file ",
                "are being read in as character columns instead of numeric columns\n\n", 
                "Since types are assigned automatically, this indicates that the columns ",
                "cannot be parsed as numeric columns due to the presence of non-number-related ",
                "characters (or the absence of any values at all)\n\n",
                "Please correct these columns and ensure that they are numeric values\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), "')") |>
           errWrap() |>
           str_replace("(character)", col_red("\\1")))
    
  } else if (!is.numeric(stationDF$LATITUDE) || !is.numeric(stationDF$LONGITUDE)) {
    
    stop(paste0("Station Input File - Coordinates Type Issue\n\n",
                "The \"LATITUDE\" and/or \"LONGITUDE\" columns of the input file ",
                "are being read in as a different type of column instead of numeric\n\n", 
                "Since types are assigned automatically, this indicates that the columns ",
                "cannot be parsed as numeric columns for some reason, such as being empty\n\n",
                "Please correct these columns and ensure that they are numeric values\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), "')") |>
           errWrap() |>
           str_replace("(empty)", col_red("\\1")))
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



getPRISM <- function (bodyList, writePath) {
  
  # The process of getting data from PRISM involves making two POST requests
  # This function contains a generic process for that 
  # 'bodyList' is the body content of the initial request
  # 'writePath' is the filepath where the output CSV file will be stored
  
  
  # Both requests will use the same headers
  # Define that here
  reqHeaders <- add_headers(Accept = "application/json, text/javascript, */*; q=0.01",
                            `Accept-Language` = "en-US,en;q=0.9",
                            `Accept-Encoding` = "gzip, deflate, br",
                            `Sec-Ch-Ua-Platform` = "Windows",
                            `User-Agent` = sessionInfo()[["R.version"]][["version.string"]],
                            `X-User-Name` = Sys.info()[["user"]],
                            `X-User-Contact` = "DWR-SDA@Waterboards.ca.gov",
                            `X-Requested-With` = "XMLHttpRequest",
                            `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8")
  
  
  # The first request obtains a "gricket value", which is a unique ID that works like a ticket
  # The final output can be found using this ID once the first request has been processed
  
  # Start with the first request here
  firstReq <- POST(url = "https://prism.oregonstate.edu/explorer/dataexplorer/rpc.php", 
                   body = bodyList,
                   encode = "form",
                   reqHeaders)
  
  
  # Check that the request was successful
  # If there are errors with the request
  # Stop the script and output this information
  validateReqResults(firstReq)
  
  
  # Extract the 'gricket' code from the response
  gricketVal <- content(firstReq) |> as.character() |>
    str_extract("gricket.: .+.errors.:") |>
    str_remove("gricket.: .") |>
    str_remove('", .error.+$')
  
  
  # The next step will be to send 'gricketVal' and request the CSV file path
  # If the request is very large, PRISM will need extra time to process the data
  
  
  # The output path will be stored in 'csvStr'
  csvStr <- NULL
  
  
  # Use a counter to prevent infinite loops
  attemptCounter <- 0
  
  
  # While 'csvStr' is NULL or NA, try to request data from PRISM 
  # However, to prevent infinite retries, only do this while 'attemptCounter' is less than 5
  while ((is.null(csvStr) || is.na(csvStr)) && attemptCounter < 5) {
    
    # Wait before sending the next request
    # (This gives PRISM's server time to process the request and prepare the output)
    
    # If 'csvStr' is NULL, this is the very first attempt, so wait only 2 seconds that time
    # For subsequent requests, 'csvStr' would be NA, and in those cases, wait more than 2 seconds
    
    if (is.null(csvStr)) {
      
      Sys.sleep(2)
      
    } else {
      
      # For repeated requests, wait at least 5 seconds before retrying
      # As the number of tries increases, increase the wait-time 
      cat("\n\n")
      message(paste0("PRISM needs more time to process the request! Retrying in ",
                     5 * attemptCounter, " seconds!\n\n"))
      
      Sys.sleep(5 * attemptCounter)
      
    }
    
    
    # Prepare the next request with 'gricketVal'
    nextReq <- POST(url = "https://prism.oregonstate.edu/explorer/dataexplorer/rpc.php", 
                    body = list(call = "pp/checkup",
                                proc = "gridserv",
                                gricket = gricketVal),
                    encode = "form",
                    reqHeaders)
    
    
    # Verify that the request was successful
    # (Skip the check for content errors, however)
    # (Those are the cases when 'csvStr' will become NA, which is needed for this procedure)
    validateReqResults(nextReq, checkForContentErrors = FALSE)
    
    
    # Get a string containing the filename of the CSV output on PRISM's server
    # If it fails, 'csvStr' will be NA
    csvStr <- content(nextReq) |> as.character() |>
      str_extract("csv.: .+\\.csv.,") |>
      str_remove(".,$") |>
      str_remove("^csv.: .")
    
    
    # Increment 'attemptCounter'
    attemptCounter <- attemptCounter + 1
    
  }
  
  
  # Wait a little before proceeding to the final step
  Sys.sleep(1.2)
  
  
  # Access that file and save it to a file
  paste0("https://prism.oregonstate.edu/explorer/tmp/", csvStr) |>
    read_lines() |>
    writeOutput(writePath, "write_lines")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateReqResults <- function (req, checkForContentErrors = TRUE) {
  
  # For a HTTP request sent to PRISM, verify that it was successful
  # If the status code is not 200, or if the response body contains 
  # an error message, notify the user
  
  # ('checkForContentErrors' can be set to FALSE to skip the second check)
  
  
  if (req$status_code != 200) {
    
    stop(paste0("PRISM HTTP Request Failed\n\n",
                "A request sent to PRISM's server returned an error code of ", 
                req$status_code, "\n\n",
                "This could be a problem with the request and/or PRISM's server\n\n",
                "Please investigate this issue") |>
           errWrap())
    
  } else if (checkForContentErrors && grepl("errors\": \\[\\[", as.character(content(req)))) {
    
    cat("\n\n")
    cat(as.character(content(req)))
    
    stop(paste0("PRISM HTTP Request Failed\n\n",
                "A request sent to PRISM's server returned the error message ",
                "shown above\n\n",
                "This could be a problem with the format of the request\n\n",
                "Please investigate this issue") |>
           errWrap() |>
           str_replace("(format)", col_red("\\1")))
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()


remove(list = ls())