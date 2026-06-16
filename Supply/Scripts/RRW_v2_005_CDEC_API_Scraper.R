# Download streamflow data from CDEC at various locations 
# in the Russian River watershed


# The first required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be character strings that correspond to the (generally 
# three-character) IDs used by CDEC
# (https://cdec.water.ca.gov/webgis/?appid=cdecstation)


# The raw output will be stored in the "WebData" folder as 
# "CDEC_API_Data_[startDate]_[endDate].csv"

# Note: US Customary units are used for the output (cfs)


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
  cat("Starting 'RRW_v2_005_CDEC_API_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Read in the list of stations 
  stationDF <- getFromControl_RR("CDEC_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "CDEC_STATIONS_CSV", "CDEC")
  
  
  # Output a message
  cat(paste0("\nGetting streamflow data for ", nrow(stationDF), " CDEC station",
             if_else(nrow(stationDF) > 1, "s", ""),
             "...\n"))
  
  
  # Get data for all CDEC stations at once
  cdecDF <- requestCDEC(stationDF$STATION_ID, startDate, endDate)
  
  
  # Add another message
  cat("\tDone!\n\n")
  
  
  # Define the output file name as well
  outFile <- paste0("WebData/CDEC_API_Data_", startDate, "_",
                    endDate, ".csv")
  
  
  # Write the file to the "WebData" folder
  writeOutput(cdecDF, outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_005_CDEC_API_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



requestCDEC <- function (stationVec, startDate, endDate, 
                         sensorNum = 23, durCode = "D") {
  
  # Prepare a GET request and submit it to CDEC
  
  # Obtain a table of streamflow data for the specified stations  
  # within the date range delineated by 'startDate' and 'endDate'
  
  
  # Start by preparing the request URL
  requestURL <- paste0("https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?",
                       "Stations=", paste0(stationVec, collapse = ","),
                       # Sensor 23 is "Reservoir Outflow"
                       # "Dur Code" set to "D" means "daily" data
                       "&SensorNums=", sensorNum, "&dur_code=", durCode,
                       # The dates use YYYY-MM-DD format
                       "&Start=", format(startDate, "%Y-%m-%d"),
                       "&End=", format(endDate, "%Y-%m-%d"))
  
  
  # Try to submit the GET request
  # (Also, ask for a CSV-formatted response)
  req <- try(GET(requestURL), silent = TRUE)
  
  
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
           formatResponse(startDate, endDate))
  
}



formatResponse <- function (res, startDate, endDate) {
  
  # Given the raw CSV output from CDEC, process the data into a data frame
  
  
  # Use `read_csv` to parse 'res'
  cdecDF <- read_csv(I(res), show_col_types = FALSE)
  
  
  # Make sure "STATION_ID", "DATE TIME", "VALUE", and "UNITS" appear in the results
  # Otherwise, output an error message
  expectedCols <- c("STATION_ID", "DATE TIME", "VALUE", "UNITS")
  
  
  if (anyFalse(expectedCols %in% names(cdecDF))) {
    
    paste0("Could Not Parse CDEC Data\n\n",
           "The script failed to locate key columns in the table returned by ",
           "CDEC. Perhaps the formatting of the data has changed? Please ",
           "investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, missing data in CDEC is represented by "---"
  # If these dashes appear in the "VALUE" column, the column may be "character"
  # instead of "numeric"
  if ("---" %in% cdecDF$VALUE) {
    
    # Replace "---" with "-999"
    cdecDF$VALUE[cdecDF$VALUE == "---"] <- -999
    
    
    # Then, convert the "VALUE" column into numeric
    cdecDF$VALUE <- as.numeric(cdecDF$VALUE)
    
  }
  
  
  # Return 'cdecDF'
  return(cdecDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
