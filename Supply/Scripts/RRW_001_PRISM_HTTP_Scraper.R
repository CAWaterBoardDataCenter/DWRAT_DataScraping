# Download precipitation and temperature data from PRISM at various locations 
# in the Russian River watershed

# These locations correspond to NOAA, RAWS, and CIMIS weather stations


# The required input is four CSV files that correspond to: 
#   (1) PRMS-related precipitation and temperature stations
#   (2) SRP-related precipitation and temperature stations 
#   (3) PRMS model domain PRISM grid cells
#   (4) SRP model domain PRISM grid cells

# Each of these files must contain three columns:
#  (1) LATITUDE
#  (2) LONGITUDE
#  (3) STATION_ID


# Four corresponding output CSV files are produced and stored in the "WebData" folder
#  (1) "PRISM_PRMS_Data_[startDate]_[endDate].csv"
#  (2) "PRISM_SRP_Data_[startDate]_[endDate].csv"
#  (3) "PRISM_PRMS_Domain_Data_[startDate]_[endDate].csv"
#  (4) "PRISM_SRP_Domain_Data_[startDate]_[endDate].csv"


# Note: The PRMS-related output file uses SI units (mm and Celsius), 
#       while the SRP-related output file has US customary units (in and Fahrenheit)

#       (The model domain outputs use SI units as well)


#### Setup ####

base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


# Allow greater time to download data from PRISM
# (This is only relevant for large data downloads)
options(timeout = 500) # 500 seconds



#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_001_PRISM_HTTP_Scraper.R'!\n")
  
  
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
  
  
  cat("\n[1/4]\tGetting precipitation and temperature data for PRMS-related stations...\n")
  
  
  # Read in the list of stations 
  stationDF <- getFromControl_RR("PRISM_PRMS_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "PRISM_PRMS_STATIONS_CSV", "PRISM")
  
  
  # Prepare and submit a request for meteorological data
  getPRISM(stationDF, startDate, endDate, 
           paste0("WebData/PRISM_PRMS_Data_", startDate, "_", endDate, ".csv"),
           useHighRes = TRUE, interpCells = TRUE,
           getPrecip = TRUE, getTemp = TRUE, useMetric = TRUE)
  
  
  # Add to the message
  cat("\tDone!\n\n")
  
  
  # Wait a bit before proceeding
  Sys.sleep(1)
  
  
  # The next step is to get both precipitation and temperature data for the SRP stations
  
  
  cat("[2/4]\tGetting precipitation and temperature data for SRP-related stations...\n")
  
  
  # Read in a list of SRP stations
  stationDF <- getFromControl_RR("PRISM_SRP_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "PRISM_SRP_STATIONS_CSV", "PRISM")
  
  
  # Prepare and submit a POST request for data
  # The SRP stations require English units (inches and Fahrenheit)
  getPRISM(stationDF, startDate, endDate, 
           paste0("WebData/PRISM_SRP_Data_", startDate, "_", endDate, ".csv"),
           useHighRes = TRUE, interpCells = TRUE,
           getPrecip = TRUE, getTemp = TRUE, useMetric = FALSE)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # After that, get precipitation data for the PRMS domain PRISM grid cells
  cat(paste0("[3/4]\tGetting precipitation data for PRISM grid cells ",
             "in the PRMS model domain...\n"))
  
  
  # For the next two data downloads, get data from the start of the 
  # current water year to 'endDate'
  wyStart <- getModeledWY(endDate)[1]
  
  
  # Read in a list of grid cells for the PRMS model domain
  stationDF <- getFromControl_RR("PRISM_PRMS_GRID_CELLS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "PRISM_PRMS_GRID_CELLS_CSV", "PRISM")
  
  
  # Prepare the POST request for precipitation data
  # No grid cell interpolation will be performed for this request
  getPRISM(stationDF, wyStart, endDate, 
           paste0("WebData/PRISM_PRMS_Domain_Data_", wyStart, "_", 
                  endDate, ".csv"),
           useHighRes = TRUE, interpCells = FALSE,
           getPrecip = TRUE, getTemp = FALSE, useMetric = TRUE)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # Finally, download precipitation data for the SRP domain PRISM grid cells
  
  
  cat(paste0("[4/4]\tGetting precipitation data for PRISM grid cells ",
             "in the SRP model domain...\n"))
  
  
  # Read in a list of grid cells for the SRP model domain
  stationDF <- getFromControl_RR("PRISM_SRP_GRID_CELLS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "PRISM_SRP_GRID_CELLS_CSV", "PRISM")
  
  
  # Prepare and submit POST requests
  # No grid cell interpolation will be performed for this request
  getPRISM(stationDF, wyStart, endDate, 
           paste0("WebData/PRISM_SRP_Domain_Data_", wyStart, "_", 
                  endDate, ".csv"),
           useHighRes = TRUE, interpCells = FALSE,
           getPrecip = TRUE, getTemp = FALSE, useMetric = TRUE)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'RRW_001_PRISM_HTTP_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getPRISM <- function (stationDF, startDate, endDate, writePath,
                      useHighRes = TRUE, interpCells = TRUE, 
                      getPrecip = TRUE, getTemp = TRUE, useMetric = TRUE,
                      quietly = FALSE, maxRetries = 15) {
  
  # The process of getting daily data from PRISM involves 
  # making two POST requests
  
  # This function contains a generic process for that 
  
  # 'stationDF' contains the coordinates and IDs of locations to get data
  
  # 'startDate' and 'endDate' define the range for which data will be obtained
  
  # 'writePath' is the filepath where the output CSV file will be stored
  
  # 'quietly' determines whether an output message is provided 
  # once the file is written
  
  # The remaining options customize the request
  
  
  # To start, check if the request is too large 
  if (nrow(stationDF) > 400) {
    
    # If data for more than 500 locations is requested, split up the request
    # (The actual limit is 500, but let's not bother PRISM too much)
    return(splitRequest(stationDF = stationDF, 
                        startDate = startDate, endDate = endDate, 
                        writePath = writePath, useHighRes = useHighRes,
                        interpCells = interpCells, getPrecip = getPrecip, 
                        getTemp = getTemp, useMetric = useMetric,
                        quietly = quietly, maxVal = 400))
    
  }
  
  
  # Prepare the body of the initial request
  bodyList <- list(call = "pp/daily_timeseries_mp",
                   proc = "gridserv",
                   # Latitude
                   lons = stationDF$LONGITUDE |> paste0(collapse = "|"),
                   # Longitude
                   lats = stationDF$LATITUDE |> paste0(collapse = "|"),    
                   # Station Names
                   names = stationDF$STATION_ID |> paste0(collapse = "|"), 
                   # Resolution (4km or 800m)
                   spares = if_else(useHighRes, "800m", "4km"),            
                   # Interpolate grid cell values
                   interp = if_else(interpCells, "idw", "0"),
                   # Precipitation + Minimum & Maximum Temperature
                   stats = paste(if_else(getPrecip, "ppt", ""),
                                 if_else(getTemp, "tmin tmax", ""),
                                 sep = " ") |>
                     trimws(),    
                   # Metric or US Customary units
                   units = if_else(useMetric, "si", "eng"),
                   range = "daily",
                   # Start and end dates in YYMMDD format
                   start = paste0(year(startDate), 
                                  twoDigitText(month(startDate)), 
                                  twoDigitText(day(startDate))),
                   end = paste0(year(endDate), 
                                twoDigitText(month(endDate)), 
                                twoDigitText(day(endDate))),
                   stability = "provisional")
  
  
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
  # However, to prevent infinite retries, 
  # only do this while 'attemptCounter' is less than 'maxRetries'
  while ((is.null(csvStr) || is.na(csvStr)) && attemptCounter < maxRetries) {
    
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
                     5 * attemptCounter, " seconds! [Attempt ",
                     attemptCounter + 1, "/", maxRetries, "]\n\n"))
      
      Sys.sleep(5 * attemptCounter + runif(1, min = 0, max = 1))
      
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
  
  
  # Check if 'csvStr' could not be extracted successfully
  if (is.na(csvStr)) {
    
    paste0("PRISM HTTP Request Failed\n\n",
           "The request sent to PRISM's server was unsuccessful. ",
           "Please investigate this issue and try again later.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Wait a little before proceeding to the final step
  Sys.sleep(1.2)
  
  
  # Save the result to a file
  paste0("https://prism.oregonstate.edu/explorer/tmp/", csvStr) |>
    read_lines() |>
    write_lines(writePath)
  
  # Note: The superior method using `download.file` does not work on our network :/
  # `read_lines` is able to bypass the SSL issues that occur with `download.file`
  
  # Otherwise, this code is preferred because it doesn't involve storing the data
  # temporarily in RAM:
  # paste0("https://prism.oregonstate.edu/explorer/tmp/", csvStr) |>
  #   download.file(writePath, mode = "w", quiet = TRUE, method = "libcurl")
  
  
  if (!file.exists(writePath)) {
    
    paste0("PRISM Request Failed\n\n",
           "The output file was not detected in the expected directory\n\n",
           "The POST request may have failed, please investigate this issue\n\n") |>
      errWrap() |>
      str_replace("(not)", col_red("\\1")) |>
      str_replace("(investigate)", col_green("\\1")) |>
      stop()
    
  }
  
  
  # If the file was written successfully, output a message
  # (only if 'quietly' is FALSE)
  if (!quietly) {
    
    cat(paste0("\nWrote data to \"", normalizePath(writePath), "\"!\n\n") |>
          col_cyan())
    
  }
  
  
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
    
  } else if (checkForContentErrors && grepl("errors\": [\\[\\{]", as.character(content(req))[1])) {
    
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



splitRequest <- function (stationDF, startDate, endDate, writePath, useHighRes,
                          interpCells, getPrecip, getTemp, useMetric,
                          quietly, maxVal = 500) {
  
  # If a PRISM request contains too many requested locations, it must be split
  
  
  # Determine the total number of requests required
  numRequests <- ceiling(nrow(stationDF) / maxVal)
  
  
  # Notify the user about this
  cat(paste0("\n\tSplitting his step into ", numRequests, 
             " smaller requests!\n\n"))
  
  
  # Modify 'writePath' to have a value for each intermediate file that
  # will be downloaded
  nameVec <- 1:numRequests |>
    map_chr(~ writePath |> str_replace("(\\.[A-Za-z]+)$",
                                       paste0("_", ., "\\1")))
  
  
  # Determine the cutoffs for each request
  rowRanges <- seq(from = 1, to = nrow(stationDF), by = maxVal)
  
  
  # Beginning making partial requests
  for (i in 1:numRequests) {
    
    # Start with a status message
    cat(paste0("\n\t[", i, "/", numRequests, "]\tRequesting...\n"))
    
    
    # Extract a subset of 'stationDF'
    subsetDF <- stationDF[rowRanges[i]:min(c(rowRanges[i] + maxVal - 1,
                                             nrow(stationDF))), ]
    
    
    # Submit a request for PRISM data
    getPRISM(stationDF = subsetDF, 
             startDate = startDate, endDate = endDate, 
             writePath = nameVec[i], useHighRes = useHighRes,
             interpCells = interpCells, getPrecip = getPrecip, 
             getTemp = getTemp, useMetric = useMetric,
             quietly = quietly)
    
    
    # Wait a little before continuing to the next iteration
    Sys.sleep(runif(1, min = 1.2, max = 2.1))
    
    
    cat(paste0("\n\t\tDone!\n"))
    
  }
  
  
  # The final step is to combine the downloaded CSV files into one output file
  cat("\n\tCombining downloaded files...\n\n")
  
  
  combineRawOutputs(nameVec, writePath)
  
  
  # Return nothing
  return(invisible())
  
}



combineRawOutputs <- function (nameVec, writePath) {
  
  # Combine the split CSV files downloaded from PRISM into one CSV
  
  # The metadata should only appear once at the beginning
  
  # After that, append data from each file into one long CSV file
  
  
  # The first downloaded CSV file will be the initial part of this combined file
  mainFile <- getFile(nameVec[1], fileType = "OTHER")
  
  
  # Record the number of locations stated in 'mainFile'
  # (This metadata must be updated as more rows are appended)
  numLocations <- mainFile |>
    str_subset("^Locations: [0-9]+$") |>
    str_extract("[0-9]+") |> as.numeric()
  
  
  # Iterate through the remaining files in 'nameVec'
  for (i in 2:length(nameVec)) {
    
    # Read in that other file
    tempFile <- getFile(nameVec[i], fileType = "OTHER")
    
    
    # Extract the number of locations in 'tempFile'
    # Add that number to 'numLocations'
    tempNum <- tempFile |>
      str_subset("^Locations: [0-9]+$") |>
      str_extract("[0-9]+") |> as.numeric()
    
    
    numLocations <- numLocations + tempNum
    
    
    # Remove everything up to and including the column headers in 'tempFile'
    tempFile <- tempFile[(grep("Name,Longitude", tempFile) + 1):length(tempFile)]
    
    
    # Append 'tempFile' to 'mainFile' 
    # (with a blank row before the start of 'tempFile')
    mainFile <- c(mainFile,
                  "",
                  tempFile)
    
  }
  
  
  # Update the number of locations at the start of the metadata in 'mainFile'
  mainFile[grep("^Locations: [0-9]+$", mainFile)[1]] <- paste0("Locations: ", 
                                                               numLocations)
  
  
  # Save 'mainFile' to 'writePath'
  writeOutput(mainFile, writePath, writeFunction = "write_lines")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


base::remove(list = ls())
