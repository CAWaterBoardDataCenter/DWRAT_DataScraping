# Download precipitation and temperature data from CIMIS at various stations  
# in the Russian River watershed


# The first required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be numeric values that correspond to the IDs used on 
# CIMIS's webpage to distinguish between different stations 
# (https://cimis.water.ca.gov/Stations.aspx)


# An API key is required as well--this should be specified in a text file and 
# referenced in "RR_Supply_Control_File.xlsx"

# Note 1: The first line of the text file should be just the API key
# Note 2: To get a key, create an account on https://www.cimis.water.ca.gov/
# Note 3: Don't use Microsoft Edge on this website (some buttons don't function properly)


# The raw output will be stored in the "WebData" folder as 
# "CIMIS_API_Data_[startDate]_[endDate].csv"

# Note: SI units are used for the output (mm and Celsius)


#### Setup ####

# Clear the environment
remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Supply_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRS_004_CIMIS_API_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # CIMIS does not have data earlier than 1982-06-07
  # If 'startDate' is earlier than this date, output an error message
  if (startDate < "1982-06-07") {
    
    stop(paste0("Requested Date Range - Start Date Issue\n\n",
                "The earliest date for which CIMIS has data available is ",
                "1982-06-07. The input start date (\"", startDate, "\") is ",
                "too early. Please revise this input.") |>
           errWrap())
    
  }
  
  
  # Read in the list of stations 
  stationDF <- getFromSupplyControl_RR("CIMIS_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "CIMIS_STATIONS_CSV", "CIMIS")
  
  
  # Output a message
  cat(paste0("\nGetting precipitation and temperature data for ",
             nrow(stationDF), " CIMIS station",
             if_else(nrow(stationDF) > 1, "s", ""),
             "...\n"))
  
  
  # Get data for all CIMIS stations at once
  cimisDF <- requestCIMIS(stationDF$STATION_ID, startDate, endDate)
  
  
  # Add another message
  cat("\tDone!\n\n")
  
  
  # Define the output file name as well
  outFile <- paste0("WebData/CIMIS_API_Data_", startDate, "_",
                    endDate, ".csv")
  
  
  # Write the file to the "WebData" folder
  writeOutput(cimisDF, outFile, "write_csv")
  
  
  # Output a completion message
  cat(col_green("\n'RRS_004_CIMIS_API_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



requestCIMIS <- function (stationVec, startDate, endDate) {
  
  # Prepare a GET request and submit it to CIMIS
  
  # Obtain a table of climate data for the specified stations  
  # within the date range delineated by 'startDate' and 'endDate'
  
  
  # First, obtain the user's API key
  # It should be specified in a file linked via the RR Supply Control File
  apiKey <- getFromSupplyControl_RR("CIMIS_API_KEY") |>
    getFile()
  
  
  # Validate the input
  validateAPI(apiKey, "CIMIS_API_KEY")
  
  
  # Keep only the first element of 'apiKey' 
  # (just in case additional input was included in the file)
  apiKey <- apiKey[1]
  
  
  # Before continuing, verify that 'startDate' and 'endDate' are within 
  # 400 days of each other
  # If the gap is wider, the request will need to be split into chunks
  if (difftime(endDate, startDate, units = "days") > 400) {
    
    # (CIMIS has a request limit of 1,750 records)
    # (Records, not days)
    return(splitRequest(stationVec, startDate, endDate, maxGap = 400))
    
  }
  
  
  # If there are no issues, prepare the request URL
  requestURL <- paste0("https://et.water.ca.gov/api/data?",
                       # State the API Key (a CIMIS account is required to get this)
                       "appKey=", apiKey,
                       # Station IDs (comma-separated)
                       "&targets=", stationVec |> paste0(collapse = ","),
                       # Dataset Start Date
                       "&startDate=", startDate,
                       # Dataset End Date
                       "&endDate=", endDate,
                       # Requesting Daily TMIN, TMAX, and PRECIP
                       "&dataItems=day-air-tmp-min,day-air-tmp-max,day-precip",
                       # Metric units (mm and Celsius)
                       "&unitOfMeasure=M")
  
  
  # Try to submit the GET request
  # (Also, ask for a JSON-formatted response)
  req <- try(GET(requestURL, add_headers("Accept" = "application/json")), 
             silent = TRUE)
  
  
  # Wait a bit after receiving the response
  Sys.sleep(runif(1, min = 1.1, max = 1.4))
  
  
  # Check if an error was received
  if ("try-error" %in% class(req)) {
    
    stop(paste0("CIMIS API Call Failed\n\n",
                "A request failed to reach CIMIS's server\n\n",
                "The most likely cause is a network firewall issue, but please ",
                "examine the error message to double-check this:\n\n",
                req[[1]][1]) |>
           errWrap())
    
  }
  
  
  # Also check if the response is valid
  if (req$status_code != 200) {
    
    stop(paste0("CIMIS API Call Failed\n\n",
                "A request sent to CIMIS's server returned an error code of ", 
                req$status_code, "\n\n",
                "This could be a problem with the request and/or CIMIS's server\n\n",
                "Please double-check the request URL: ",
                requestURL, "\n\n",
                "Alternatively, there may be a problem with CIMIS's server, ",
                "so please consider contacting them for assistance") |>
           errWrap())
    
  }
  
  
  # Check the content of the response and mold it into a data frame format
  # Then, return that result
  return(content(req) |>
           formatResponse(startDate, endDate))
  
}



validateAPI <- function (apiKey, sourceField) {
  
  # Confirm that the API key was provided correctly by the user 
  
  
  # Confirm that 'apiKey' is not empty or blank
  if (length(apiKey) == 0 || is.null(apiKey)) {
    
    stop(paste0("API Key Input File Issue\n\n",
                "The input file containing the CIMIS API key does not have ",
                "a value. There may be an issue with this file. ",
                "Please correct it and try again.\n\n",
                "If the input file is a .txt file, it must contain the API key ",
                "on the first line (with nothing else on that line)\n\n",
                "If the input file is something else (like a CSV, TSV, or XLSX file) ",
                "the API key should be alone on the first line after the column ",
                "header\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), 
                "')") |>
           errWrap() |>
           str_replace("(does not)", col_red("\\1")) |>
           str_replace("(.txt)", col_green("\\1")) |>
           str_replace("(something else)", col_green("\\1")) |>
           str_replace("(after)", col_blue("\\1")))
    
  }
  
  
  # The first element of 'apiKey' should be "character" type  
  if (!is.character(apiKey[1])) {
    
    stop(paste0("API Key Input File Issue\n\n",
                "The API key could not be parsed as a string. There may be an ",
                "issue with this file. Please correct it and try again.\n\n",
                "If the input file is a .txt file, it must contain the API key ",
                "on the first line (with nothing else on that line)\n\n",
                "If the input file is something else (like a CSV, TSV, or XLSX file) ",
                "the API key should be alone on the first line after the column ",
                "header\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), 
                "')") |>
           errWrap() |>
           str_replace("(does not)", col_red("\\1")) |>
           str_replace("(.txt)", col_green("\\1")) |>
           str_replace("(something else)", col_green("\\1")) |>
           str_replace("(after)", col_blue("\\1")))
    
  }
  
  
  # Confirm that the first line of 'apiKey' is not 'NA'
  if (is.na(apiKey[1])) {
    
    stop(paste0("API Key Input File - Missing Key Issue\n\n",
                "The first line of the input file is missing a value. ",
                "Please correct it and try again.\n\n",
                "If the input file is a .txt file, it must contain the API key ",
                "on the first line (with nothing else on that line)\n\n",
                "If the input file is something else (like a CSV, TSV, or XLSX file) ",
                "the API key should be alone on the first line after the column ",
                "header\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), 
                "')") |>
           errWrap() |>
           str_replace("(missing)", col_red("\\1")) |>
           str_replace("(.txt)", col_green("\\1")) |>
           str_replace("(something else)", col_green("\\1")) |>
           str_replace("(after)", col_blue("\\1")))
    
  }
  
  
  # It seems that CIMIS API keys are just numbers and letters separated by hyphens
  # If the API key is read in as something different, output a warning
  # (Not an error message)
  if (grepl("[^a-zA-Z0-9\\-]", apiKey[1])) {
    
    message(paste0("API Key Input File - Potential Key Issue\n\n",
                   "CIMIS API keys are typically a mix of letters and digits, ",
                   "separated by hyphens\n\n",
                   "The provided API key does not match this format. There may be ",
                   "issues encountered later on when submitting the API call.\n\n",
                   "(This flag occurred for '", getFromSupplyControl_RR(sourceField), 
                   "')") |>
              errWrap())
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



formatResponse <- function (res, startDate, endDate) {
  
  # After a successful request to CIMIS, reformat the returned data
  # Return a tibble with that information
  
  
  # First check that the request was actually successful
  # Sometimes, the response seems valid, but its contents are an error message
  if ("node" %in% names(res) && 
      grepl("requested URL was rejected", as.character(res)[1])) {
    
    cat("\n\n")
    cat(as.character(res))
    
    
    stop(paste0("CIMIS API Server Issue\n\n",
                "CIMIS's server may have been overloaded with requests. As a ",
                "result, the request URL was rejected. Please contact CIMIS ",
                "for assistance (or try again later).") |>
           errWrap())
    
  }
  
  
  # If no issue was found, proceed with reformatting the response
  
  
  # 'res' should have a JSON structure
  
  # Under 'Data' and 'Providers', there will be four elements
  # ("Name", "Type", "Owner", and "Records")
  
  # The first three elements are all individual strings
  
  # The fourth element is a list, with a separate entry for each day 
  # in the requested date range
  
  # Within each entry of "Records", the requested variables will be present
  # as separate, named sub-elements
  varNames <- c("TMIN" = "DayAirTmpMin", 
                "TMAX" = "DayAirTmpMax", 
                "PRECIP" = "DayPrecip")
  
  # Those variables' sub-elements are lists themselves
  # They are further divided into "Value", "Qc", and "Unit"
  
  
  # If the expected format was NOT received, output an error message
  if (# [1] The content should be stored under 
    #     "Data" > "Providers" > "Records"
    is.null(res[["Data"]][["Providers"]]) ||
    !("Records" %in% names(res[["Data"]][["Providers"]][[1]])) ||
    length(res[["Data"]][["Providers"]][[1]][["Records"]]) == 0 ||
    # [2] Every entry in "Records" should contain elements for "Date",  
    #     "Station", and the parameters listed in 'varNames'
    anyFalse(c("Date", "Station", varNames) %in% 
             names(res[["Data"]][["Providers"]][[1]][["Records"]][[1]])) ||
    # [3] The parameters in 'varNames' should be lists too
    #     They should each have an element called "Value"
    !("Value" %in% names(res[["Data"]][["Providers"]][[1]][["Records"]][[1]][[varNames[1]]])) ||
    !("Value" %in% names(res[["Data"]][["Providers"]][[1]][["Records"]][[1]][[varNames[2]]])) ||
    !("Value" %in% names(res[["Data"]][["Providers"]][[1]][["Records"]][[1]][[varNames[3]]]))) {
    
    # Output the returned content
    print(res)
    
    
    # One of the above conditions will have a unique message
    # In this instance, no data is available for the requested date range
    if (length(res[["Data"]][["Providers"]][[1]][["Records"]]) == 0) {
      
      stop(paste0("Empty CIMIS Response\n\n",
                  "CIMIS returned zero records for the requested date ",
                  "range (\"", startDate, "\" to \"", endDate, "\"). Please ",
                  "revise the input date range.") |>
             errWrap())
      
    }
    
    
    stop(paste0("Could Not Parse CIMIS Response\n\n",
                "The information returned by CIMIS could not be interpreted ",
                " correctly. The response text was not in the expected format.\n\n", 
                "Please investigate this issue further. Either this script ",
                "requires revisions, or CIMIS must be contacted about a ",
                "server issue.\n\n") |>
           errWrap())
    
  }
  
  
  # Extract data from different columns within the records in 'res'
  # Store that information in a tibble
  cimisDF <- tibble(
    
    # Dates are stored under "Date" for each element in "Records"
    DATE = res$Data$Providers[[1]]$Records |>
      map_chr(~ .[["Date"]]) |> 
      as.Date(format = "%Y-%m-%d"),
    
    # Station IDs are stored under "Station"
    STATION_ID = res$Data$Providers[[1]]$Records |>
      map_chr(~ .[["Station"]]) |> as.numeric(),
    
    # Minimum temperature
    !! names(varNames)[1] := res$Data$Providers[[1]]$Records |>
      map_chr(~ .[[varNames[1]]][["Value"]] |> 
                replace_null()) |> # (Missing entries become "NA")
      as.numeric(),
    
    # Maximum temperature
    !! names(varNames)[2] := res$Data$Providers[[1]]$Records |> 
      map_chr(~ .[[varNames[2]]][["Value"]] |> 
                replace_null()) |>  
      as.numeric(),
    
    # Precipitation
    !! names(varNames)[3] := res$Data$Providers[[1]]$Records |>
      map_chr(~ .[[varNames[3]]][["Value"]] |> 
                replace_null()) |>
      as.numeric(),
    
    # QC information for TMIN
    !! paste0(names(varNames)[1], "_QC") := res$Data$Providers[[1]]$Records |>
      map_chr(~ .[[varNames[1]]][["Qc"]] |> 
                replace_null()) |>
      trimws(),
    
    # QC information for TMAX
    !! paste0(names(varNames)[2], "_QC") := res$Data$Providers[[1]]$Records |>
      map_chr(~ .[[varNames[2]]][["Qc"]] |> 
                replace_null()) |> 
      trimws(),
    
    # QC information for PRECIP
    !! paste0(names(varNames)[3], "_QC") := res$Data$Providers[[1]]$Records |>
      map_chr(~ .[[varNames[3]]][["Qc"]] |> 
                replace_null()) |> 
      trimws())
  
  
  # Return the formatted tibble
  return(cimisDF)
  
}



replace_null <- function (x, replacement = NA_character_) {
  
  # If a value 'x' is NULL, replace it with 
  # the value listed in 'replacement'
  
  if (is.null(x)) {
    
    return(replacement)
    
  } else {
    
    return(x)
    
  }
  
}



splitRequest <- function (stationVec, startDate, endDate, maxGap) {
  
  # For data requests that cover a large date range, 
  # split the range into chunks and perform several requests to CIMIS
  
  # Combine the response tibbles into one and return that
  
  
  # First, get intermediate dates between 'startDate' and 'endDate' 
  # that satisfy the limitation set by 'maxGap'
  # (The number of days in each request will at most be ~90% 
  #  of the limit set by 'maxGap')
  dateVec <- seq(from = startDate, to = endDate,
                 by = paste0(round(0.90 * maxGap), " day"))
  
  
  # If 'endDate' does not appear in 'dateVec', add it in
  if (!(endDate %in% dateVec)) {
    
    dateVec <- c(dateVec, endDate)
    
  }
  
  
  # Output a message to the user to inform them of the split
  cat(paste0("\n\tSplitting into ", length(dateVec) - 1, " API calls...\n"))
  
  
  # Iterate through 'dateVec' and submit requests to CIMIS
  for (i in 2:length(dateVec)) {
    
    # Start with a status message
    cat(paste0("\n\t[", i - 1, "/", length(dateVec) - 1, "]\tRequesting...\n"))
    
    
    # Take two consecutive dates from 'dateVec' 
    # and request the date between them
    iterRes <- requestCIMIS(stationVec, dateVec[i - 1], dateVec[i])
    
    
    # Combine 'iterRes' after each request
    if (i == 2) {
      
      combinedDF <- iterRes
      
    } else {
      
      combinedDF <- bind_rows(combinedDF, iterRes) |>
        unique()
      
    }
    
    
    # Output another message to the user at the end of the loop
    cat("\n\t\tDone!\n")
    
  }
  
  
  # Finally, return 'combinedDF'
  return(combinedDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
remove(list = ls())
