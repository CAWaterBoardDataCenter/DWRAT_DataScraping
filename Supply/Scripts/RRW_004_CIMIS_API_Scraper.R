# Download precipitation and temperature data from CIMIS at various stations  
# in the Russian River watershed


# The first required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be numeric values that correspond to the IDs used on 
# CIMIS's webpage to distinguish between different stations 
# (https://cimis-uat.water.ca.gov/stations/station-list)


# An API key is required as well--this should be specified in a text file and 
# referenced in "RR_Supply_Control_File.xlsx"

# Note 1: The first line of the text file should be just the API key
# Note 2: To get a key, create an account on https://cimis-uat.water.ca.gov/


# The raw output will be stored in the "WebData" folder as 
# "CIMIS_API_Data_[startDate]_[endDate].csv"

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
  cat("Starting 'RRW_004_CIMIS_API_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # CIMIS does not have data earlier than 1982-06-07 for many stations
  # If 'startDate' is earlier than this date, output an error message
  if (startDate < "1982-06-07") {
    
    stop(paste0("Requested Date Range - Start Date Issue\n\n",
                "The earliest date for which CIMIS has data available is ",
                "1982-06-07. The input start date (\"", startDate, "\") is ",
                "too early. Please revise this input.") |>
           errWrap())
    
  }
  
  
  # Read in the list of stations 
  stationDF <- getFromControl_RR("CIMIS_STATIONS_CSV") |>
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
  writeOutput(cimisDF, outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_004_CIMIS_API_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



requestCIMIS <- function (stationVec, startDate, endDate, isSplit = FALSE) {
  
  # Prepare a GET request and submit it to CIMIS
  
  # Obtain a table of climate data for the specified stations  
  # within the date range delineated by 'startDate' and 'endDate'
  
  # 'isSplit' identifies whether `requestCIMIS` is being called normally
  # or through the function `splitRequest`
  
  
  # First, obtain the user's API key
  # It should be specified in a file linked via the RR Supply Control File
  apiKey <- getFromControl_RR("CIMIS_API_KEY") |>
    getFile()
  
  
  # Validate the input
  validateAPI(apiKey, "CIMIS_API_KEY")
  
  
  # Keep only the first element of 'apiKey' 
  # (just in case additional input was included in the file)
  apiKey <- apiKey |> unlist(use.names = FALSE) |> head(1)
  
  
  # Before continuing, check if 'startDate' and 'endDate' have a large date gap
  # CIMIS has a request limit of 1,750 records, so an excessively large gap
  # can cause issues
  
  # Calculate the maximum allowable request size
  # Max Number of Days Per Request = (1750 / # of Stations / # of Parameters)
  maxRequest <- floor(1750 / length(stationVec) / 3)
  
  
  # And because we don't want to be using the actual maximum limit,
  # Reduce 'maxRequest' by 15%
  maxRequest <- floor(maxRequest * .85)
  
  
  # Even then, make sure 'maxRequest' is not too large
  # Arbitrarily limit it to 300 days per request
  maxRequest <- min(maxRequest, 300)
  
  
  # If the gap is wider than 'maxRequest', the request will need to be split
  if (difftime(endDate, startDate, units = "days") > maxRequest) {
    
    # (CIMIS's limit is 1,750 records, not days)
    return(splitRequest(stationVec, startDate, endDate, maxGap = maxRequest))
    
  }
  
  
  # If there are no issues, prepare the request URL
  requestURL <- paste0("https://et-uat.water.ca.gov/StationWeb/GetDataByStationNumber?",
                       # Station IDs (comma-separated)
                       "&stationNbrs=", stationVec |> paste0(collapse = ","),
                       # Dataset Start Date
                       "&startDate=", format(startDate, "%Y-%m-%d"),
                       # Dataset End Date
                       "&endDate=", format(endDate, "%Y-%m-%d"),
                       # Daily, not Hourly data
                       "&isHourly=false",
                       # Requesting Daily TMIN, TMAX, and PRECIP
                       "&dataItems=day-air-tmp-min,day-air-tmp-max,day-precip",
                       # Metric units (mm and Celsius)
                       "&unitOfMeasure=M")
  
  
  # Try to submit the GET request
  # (Also, ask for a JSON-formatted response)
  req <- try(GET(requestURL, add_headers("Ocp-Apim-Subscription-Key" = apiKey,
                                         "Accept" = "application/json")), 
             silent = TRUE)
  
  
  # Wait a bit after receiving the response
  Sys.sleep(runif(1, min = 1.1, max = 1.4))
  
  
  # Check if an error was received
  if ("try-error" %in% class(req)) {
    
    # Print out the error message
    cat("\n\n")
    print(req[[1]])
    cat("\n\n")
    
    
    # Prepare a message about the failure
    # Whether it is used as an error message or a regular message depends on 
    # whether the follow-up dynamic scraping procedure will be used
    return(paste0("CIMIS API Call Failed\n\n",
                  "A request failed to reach CIMIS's server. The most ",
                  "likely cause is a CIMIS network issue, but please ",
                  "examine the error message above to double-check this.") |>
             errWrap() |>
             considerSelenium(stationVec, startDate, endDate, isSplit))
    
  }
  
  
  # Also check if the response is valid
  if (req$status_code != 200) {
    
    return(paste0("CIMIS API Call Failed\n\n",
                  "A request sent to CIMIS's server returned an error code of ", 
                  req$status_code, "\n\n",
                  "This could be a problem with the request and/or CIMIS's ",
                  "server\n\n", 
                  "Please double-check the request URL: ",
                  requestURL, "\n\n",
                  "Alternatively, there may be a problem with CIMIS's server, ",
                  "so please consider contacting them for assistance") |>
             errWrap() |>
             considerSelenium(stationVec, startDate, endDate))
    
  }
  
  
  # Check the content of the response and mold it into a data frame format
  # Then, return that result
  return(content(req) |>
           formatResponse(startDate, endDate, stationVec, isSplit))
  
}



validateAPI <- function (apiKey, sourceField, provider = "CIMIS") {
  
  # Confirm that the API key was provided correctly by the user 
  
  
  # Confirm that 'apiKey' is not empty or blank
  if (length(apiKey) == 0 || is.null(apiKey)) {
    
    stop(paste0("API Key Input File Issue\n\n",
                "The input file containing the ", provider, " API key does ",
                "not have a value. There may be an issue with this file. ",
                "Please correct it and try again.\n\n",
                "If the input file is a .txt file, it must contain the API key ",
                "on the first line (with nothing else on that line)\n\n",
                "If the input file is something else (like a CSV, TSV, or XLSX file) ",
                "the API key should be alone on the first line after the column ",
                "header\n\n",
                "(This error occurred for '", getFromControl_RR(sourceField), 
                "')") |>
           errWrap() |>
           str_replace("(does not)", col_red("\\1")) |>
           str_replace("(.txt)", col_green("\\1")) |>
           str_replace("(something else)", col_green("\\1")) |>
           str_replace("(after)", col_blue("\\1")))
    
  }
  
  
  # If 'apiKey' is a data frame or similar object, convert it into a vector
  if (!is.null(nrow(apiKey))) {
    
    apiKey <- apiKey[[1]]
    
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
                "(This error occurred for '", getFromControl_RR(sourceField), 
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
                "(This error occurred for '", getFromControl_RR(sourceField), 
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
  if (provider == "CIMIS" && grepl("[^a-zA-Z0-9\\-]", apiKey[1])) {
    
    message(paste0("API Key Input File - Potential Key Issue\n\n",
                   "CIMIS API keys are typically a mix of letters and digits, ",
                   "separated by hyphens\n\n",
                   "The provided API key does not match this format. There may be ",
                   "issues encountered later on when submitting the API call.\n\n",
                   "(This flag occurred for '", getFromControl_RR(sourceField), 
                   "')") |>
              errWrap())
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



formatResponse <- function (res, startDate, endDate, stationVec, isSplit) {
  
  # After a successful request to CIMIS, reformat the returned data
  # Return a tibble with that information
  
  
  # First check that the request was actually successful
  # Sometimes, the response seems valid, but its contents are an error message
  if ("node" %in% names(res) && 
      grepl("requested URL was rejected", as.character(res)[1])) {
    
    cat("\n\n")
    cat(as.character(res))
    
    
    return(paste0("CIMIS API Server Issue\n\n",
                  "CIMIS's server may have been overloaded with requests. As a ",
                  "result, the request URL was rejected. Please contact CIMIS ",
                  "for assistance (or try again later).") |>
             errWrap() |>
             considerSelenium(stationVec, startDate, endDate, isSplit))
    
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
    # ('isSplit' clarifies that this CIMIS request is a split request)
    iterRes <- requestCIMIS(stationVec, dateVec[i - 1], dateVec[i], 
                            isSplit = TRUE)
    
    
    # Check if 'iterRes' contains a list instead of a data frame
    # (This is an indication to abandon the procedure and use dynamic web 
    #  scraping via RSelenium instead)
    if (!is.data.frame(iterRes) && is.list(iterRes) &&
        length(iterRes) == 2 && all(c("LOGIN", "DRIVER") %in% names(iterRes))) {
      
      message("Switching from split requests to using RSelenium instead!")
      cat("\n\n")
      
      
      # If CIMIS's API is having issues, download data for the entire data range
      # using Selenium and CIMIS's Station Reports Form instead
      return(scrapeCIMIS(stationVec, startDate, endDate, 
                         iterRes$LOGIN, iterRes$DRIVER))
      
    }
    
    
    # If the above conditional statement is not applicable, continue with the
    # regular procedure of gathering CIMIS data from the API in chunks
    
    
    # Combine 'iterRes' after each request
    if (i == 2) {
      
      combinedDF <- iterRes
      
    } else {
      
      combinedDF <- bind_rows(combinedDF, iterRes) |>
        unique()
      
    }
    
    
    # Output another message to the user at the end of the loop
    cat("\n\t\tDone!\n")
    
    
    # Wait a bit before proceeding to the next request
    Sys.sleep(runif(1, min = 1.3, max = 2.4))
    
  }
  
  
  # Finally, return 'combinedDF'
  return(combinedDF)
  
}



considerSelenium <- function (issueStr, stationVec, startDate, endDate,
                              isSplit) {
  
  # Consider using RSelenium to gather CIMIS data
  # This would be an alternative pathway in case the API has issues
  
  # If the user has provided a value for "CIMIS_LOGIN_CREDENTIALS", the script
  # will make this attempt
  
  # If not, the script procedure will stop here with 'issueStr' 
  # as an error message
  
  
  # If no value was given for "CIMIS_LOGIN_CREDENTIALS"
  if (is.na(getFromControl_RR("CIMIS_LOGIN_CREDENTIALS"))) {
    
    # Use 'issueStr' as an error message and end the procedure
    stop(issueStr)
    
  }
  
  
  # Otherwise, use 'issueStr' as a message
  message(issueStr)
  
  cat("\n\n")
  
  
  # Then, try to get CIMIS data through an alternative method
  message("Attempting to collect CIMIS's web data via Selenium...")
  
  cat("\n\n")
  
  
  # Read in the user's credentials for their CIMIS account
  # Validate that input as well
  userLogin <- getFromControl_RR("CIMIS_LOGIN_CREDENTIALS") |>
    getFile() |>
    validateLogin("CIMIS_LOGIN_CREDENTIALS")
  
  
  # Check for an installation of Google Chrome on the user's computer
  # A proper chromedriver for this version must be present as well
  driverVersion <- checkChrome()
  
  
  # At this point, dynamic web scraping is a valid option
  # However, there is one sticking point: split requests
  
  # Requests are split automatically, but for this dynamic method, 
  # it would be better to get data for the entire range, all at once
  
  # However, if 'isSplit' is TRUE, the 'startDate' and 'endDate' given to 
  # this function are not the true date bounds
  
  # A notification needs to be sent to `splitRequest` to perform dynamic 
  # web scraping via Selenium for the entire data range
  
  # Use a special list for that
  if (isSplit) {
    
    # Returning this list to `splitRequest` will trigger `scrapeCIMIS` from 
    # that function with the full date range in tow
    return(list("LOGIN" = userLogin, "DRIVER" = driverVersion))
    
  }
  
  
  # For requests that are not split, `scrapeCIMIS` can be called normally here
  return(scrapeCIMIS(stationVec, startDate, endDate, userLogin, driverVersion))
  
}



validateLogin <- function (loginFile, sourceField) {
  
  # Confirm that the login information was provided correctly by the user 
  
  # There should be a username on the first line and a password on the second
  
  
  # Confirm that 'loginFile' is not empty or blank
  if (length(loginFile) == 0 || is.null(loginFile)) {
    
    paste0("Login File - Source Issue\n\n",
           "The input file containing the CIMIS login information does ",
           "not have a value. There may be an issue with this file. ",
           "Please correct it and try again.\n\n",
           "If the input file is a .txt file, it must contain the username ",
           "on the first line (with nothing else on that line) and the ",
           "password on the second line (also with nothing else beside it).\n\n",
           "If the input file is something else (like a CSV, TSV, or XLSX ",
           "file), the username and password should be alone on the first ",
           "and second lines after the column header's line.\n\n",
           "(This error occurred for '", getFromControl_RR(sourceField), 
           "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If 'loginFile' is not a vector, convert it into one, 
  # keeping its first column only
  if (!is.null(nrow(loginFile))) {
    
    loginFile <- loginFile[[1]]
    
  }
  
  
  # The first and second elements of 'loginFile' should be "character" type  
  if (!is.character(loginFile[1:2])) {
    
    paste0("Login File - Type Issue\n\n",
           "The login information could not be parsed as a string. There may ",
           "be an issue with this file. Please correct it and try again.\n\n",
           "If the input file is a .txt file, it must contain the username ",
           "on the first line (with nothing else on that line) and the ",
           "password on the second line (also with nothing else beside it).\n\n",
           "If the input file is something else (like a CSV, TSV, or XLSX ",
           "file), the username and password should be alone on the first ",
           "and second lines after the column header's line.\n\n",
           "(This error occurred for '", getFromControl_RR(sourceField), 
           "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Confirm that the first and second lines of 'loginFile' are not 'NA'
  if (anyNA(loginFile[1:2])) {
    
    paste0("Login File - Missing Value Issue\n\n",
           "Either the username or password is missing from the input ",
           "file containing login information. Please correct it and try ",
           "again.\n\n",
           "If the input file is a .txt file, it must contain the username ",
           "on the first line (with nothing else on that line) and the ",
           "password on the second line (also with nothing else beside it).\n\n",
           "If the input file is something else (like a CSV, TSV, or XLSX ",
           "file), the username and password should be alone on the first ",
           "and second lines after the column header's line.\n\n",
           "(This error occurred for '", getFromControl_RR(sourceField), 
           "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return the file if no errors are detected
  return(loginFile)
  
}



checkChrome <- function () {
  
  # Look for Google Chrome on the user's computer
  
  # Then, ensure that the required "chromedriver" for this version of Chrome is
  # present in the user's "AppData" folder (under "binman")
  
  
  # First look for Chrome
  # Either a 32-bit or a 64-bit installation will work
  chromeLoc <- tibble("win32" =
                        "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
                      "win64" =
                        "C:/Program Files/Google/Chrome/Application/chrome.exe")
  
  
  # If neither installation exists, output an error
  if (!any(file.exists(chromeLoc |> unlist(use.names = TRUE)))) {
    
    # Neither a 32-bit nor a 64-bit installation was detected 
    paste0("Chrome Installation Not Found\n\n",
           "The Selenium-based scraping procedure for CIMIS requires Google ",
           "Chrome to be installed on the user's device. However, it was ",
           "not found. Please download this web browser and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, check which version of Chrome is available
  # If both options exist, prioritize the 64-bit version
  if (file.exists(chromeLoc$win64)) {
    
    chromeLoc <- chromeLoc$win64
    
  } else {
    
    chromeLoc <- chromeLoc$win32
    
  }
  
  
  # After that, get the current version of Google Chrome
  # The "wmic" Windows command can do that
  chromeVersion <- system(paste0("wmic datafile where name=\"",
                                 chromeLoc |> 
                                   normalizePath() |>
                                   str_replace_all("\\\\", "\\\\\\\\"),
                                 "\" get Version /value"),
                          intern = TRUE)
  
  # The command will look like this:
  # wmic datafile where name = "chrome-path" get Version /value
  
  # The "chrome-path" component requires two sets of backslashes
  # (e.g., "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe")
  
  # The `str_replace_all` call looks weird, but it's taking every single 
  # instance of a backslash ("\") and doubling it ("\\")
  # Because backslashes need to be escaped twice each time, it ends up 
  # looking like that
  
  # Even when printed to the console, it'll look like: 
  # "C:\\\\Program Files\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe"
  # But that is just two backslashes (each with its own escape backslash)
  
  
  # From the results of the "wmic" call, try to extract the version string
  chromeVersion <- chromeVersion |>
    str_subset("Version") |>
    str_extract("(?<=Version=)[0-9]+(\\.[0-9]+)+")
  
  
  # If the version string could not be isolated, return an error message
  if (length(chromeVersion) != 1) {
    
    paste0("Could Not Determine Chrome Version\n\n",
           "The Selenium-based scraping procedure for CIMIS requires Google ",
           "Chrome. Based on the browser version, a certain version of ",
           "\"chromedriver\" also has to be installed.\n\n",
           "The script attempted to check the version of Chrome using the ",
           "\"wmic\" command, but the process failed for an unknown reason (",
           if_else(length(chromeVersion) > 1, 
                   "multiple version-related strings were detected",
                   "version-related information could not be found"),
           "). Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Once the current version of Chrome has been found, 
  # a corresponding driver is required
  
  # `checkChromeDriver` will look and see if the required version is already
  # installed on the user's device
  # If not, it will be installed automatically
  driverVersion <- checkChromeDriver(chromeVersion)
  
  
  # After the required driver has been identified, Google Chrome is ready 
  # for scraping via RSelenium
  
  
  # Return 'driverVersion'
  return(driverVersion)
  
}



checkChromeDriver <- function (chromeVersion) {
  
  # Given the user's current version of Chrome, check for a "chromedriver"
  # that is compatible with their browser
  
  # Note: The driver will be a 32-bit chromedriver, regardless of their
  #       browser type
  
  
  # Check the available versions of chromedriver
  installedDrivers <- list_versions("chromedriver")$win32
  
  
  # Look for a version that has the same version milestone as 'chromeVersion'
  # (The "milestone" is the first part of the version number)
  # (e.g., in "147.0.7727.138", it's "147")
  
  # Also, the version number should be less than or equal to 'chromeVersion'
  
  
  # Start with the latter filter
  if (anyNA(as.numeric_version(installedDrivers))) {
    
    paste0("Chromedriver Version Issue\n\n",
           "The directory containing downloaded chromedrivers should have ",
           "folders named after their respective Chrome versions. However, ",
           "one or more driver folders in the \"win32\" folder returned \"NA\" ",
           "when trying to parse them as version numbers.\n\n",
           "Please investigate the directory '", app_dir("chromedriver"), 
           "'.") |>
      errWrap() |>
      stop()
    
  } else if (is.na(as.numeric_version(chromeVersion))) {
    
    paste0("Chromedriver Version Issue\n\n",
           "The installed version of Google Chrome was determined to be \"",
           chromeVersion, "\". However, it could not be interpreted as a ",
           "version number string. Please investigate. There could be a ",
           "problem with the procedure.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Exclude versions of chromedriver that are newer than 'chromeVersion'
  installedDrivers <- 
    installedDrivers[installedDrivers <= as.numeric_version(chromeVersion)]
  
  
  # Then, try to find drivers with the same milestone as 'chromeVersin'
  
  # Extract the milestone from 'chromeVersion'
  milestone <- chromeVersion |>
    str_extract("^[0-9]+")
  
  
  # Filter 'installedDrivers' to the same version milestone
  # Keep the latest option
  installedDrivers <- installedDrivers |>
    str_subset(paste0("^", milestone, "\\.")) |>
    sort() |> tail(1)
  
  
  # If 'installedDrivers' has a driver version, return it
  if (length(installedDrivers) == 1) {
    return(installedDrivers)
  }
  
  
  # This error should never occur, but have it just in case
  if (length(installedDrivers) > 1) {
    
    paste0("Chromedriver Version Issue\n\n",
           "The directory containing downloaded chromedrivers should have ",
           "folders named after their respective versions. While searching ",
           "for a version similar to \"", chromeVersion, "\", the script ",
           "encountered a strange error. Please investigate the procedure.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The last possible case is that 'installedDrivers' is empty
  # That means that a new chromedriver is required
  message("A new \"chromedriver\" must be downloaded!")
  cat("\n\n")
  
  
  # Get a list of all available "chromedriver" versions from Google
  driverVersions <- paste0("https://googlechromelabs.github.io/",
                           "chrome-for-testing/",
                           "known-good-versions-with-downloads.json") |>
    read_json()
  
  
  # Wait a bit before continuing
  Sys.sleep(1.2)
  
  
  # Make sure 'driverVersions' arrived in the expected format
  if (length(driverVersions) != 2 || 
      anyFalse(c("timestamp", "versions") %in% names(driverVersions)) ||
      length(driverVersions[["versions"]][[1]][["downloads"]]) == 0) {
    
    paste0("Could Not Get Driver Information\n\n",
           "A JSON containing available drivers is located on Google's ",
           "Chrome Labs GitHub page. However, the returned data is not in ",
           "the expected format. Has something changed on Google's side?\n\n",
           "Please check \"https://googlechromelabs.github.io/",
           "chrome-for-testing/\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Not all downloadable versions of Chrome in 'driverVersions' have 
  # a chromedriver available
  # Check for "chromedriver" in each of the "downloads" entries
  hasDriver <- map_lgl(driverVersions$versions, 
                       ~ "chromedriver" %in% names(.[["downloads"]]))
  
  
  # If zero entries have "chromedriver" as an option, there is an issue
  # The names might've been modified
  if (sum(hasDriver) == 0) {
    
    paste0("Could Not Get Driver Information\n\n",
           "A JSON containing available drivers is located on Google's ",
           "Chrome Labs GitHub page. However, the returned data is not in ",
           "the expected format. \"chromedriver\" could not be located among ",
           "the \"downloads\" options. Has something changed on Google's ",
           "side?\n\n", 
           "Please check \"https://googlechromelabs.github.io/",
           "chrome-for-testing/\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Otherwise, filter 'driverVersions' to all versions 
  # with "chromedriver" available
  driverVersions <- driverVersions$versions[hasDriver]
  
  
  # Next, filter 'driverVersions' to options with the same milestone
  # as 'chromeVersion'
  sameMilestone <- map_lgl(driverVersions, 
                           ~ grepl(paste0("^", milestone, "\\."),
                                   .[["version"]]))
  
  
  # If no entries have this milestone, that is a sign of an error
  if (sum(sameMilestone) == 0) {
    
    paste0("Could Not Get Driver Information\n\n",
           "A JSON containing available drivers is located on Google's ",
           "Chrome Labs GitHub page. However, versions of Chrome with the ",
           "same milestone as \"", chromeVersion, "\" (i.e., \"", milestone, 
           "\") could not be found. Has something changed about the formatting ",
           "on Google's side?\n\n", 
           "Please check \"https://googlechromelabs.github.io/",
           "chrome-for-testing/\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Otherwise, keep only entries with the same milestone as 'chromeVersion'
  driverVersions <- driverVersions[sameMilestone]
  
  
  # The next step is to compare versions
  # Extract the version numbers from 'driverVersions'
  availableVersions <- driverVersions |>
    map_chr(~ .[["version"]]) |>
    as.numeric_version()
  
  
  # Make sure none of the extracted versions are NA
  if (anyNA(availableVersions)) {
    
    paste0("Could Not Get Driver Information\n\n",
           "A JSON containing available drivers is located on Google's ",
           "Chrome Labs GitHub page. However, the Chrome versions listed ",
           "in the \"version\" sub-element could not be extracted and parsed. ",
           "Has something changed about the formatting on Google's side?\n\n", 
           "Please check \"https://googlechromelabs.github.io/",
           "chrome-for-testing/\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Identify the driver version to download
  # It should be the latest version that is NOT newer than 'chromeVersion'
  chosenVersion <- availableVersions[availableVersions <= chromeVersion] |> 
    max()
  
  
  # Make sure 'chosenVersion' was identified successfully
  if (length(chosenVersion) == 0 || is.na(chosenVersion[1])) {
    
    paste0("Could Not Get Driver Information\n\n",
           "A JSON containing available drivers is located on Google's ",
           "Chrome Labs GitHub page. However, a driver for \"", chromeVersion,
           "\" could not be identified. The script was searching for a driver ",
           "that is from the same milestone and not newer than the browser ",
           "version. However, the procedure failed for an unknown reason. ",
           "Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Filter 'driverVersions' to the chosen chromedriver
  chosenDriver <- driverVersions[availableVersions == chosenVersion][[1]]
  
  
  # Get the 32-bit download link for 'chosenDriver'
  dlLink <- chosenDriver$downloads$chromedriver |>
    map_chr(~ if_else(.[["platform"]] == "win32", .[["url"]], NA_character_))
  
  
  # Remove the NA entries in dlLink
  dlLink <- dlLink[!is.na(dlLink)]
  
  
  # Make sure 'dlLink' is not empty now
  if (length(dlLink) == 0) {
    
    paste0("Could Not Get Driver Download URL\n\n",
           "A JSON containing available drivers is located on Google's ",
           "Chrome Labs GitHub page. However, when trying to extract the URL ",
           "for a 32-bit chromedriver (Version ", chosenVersion, "), the ",
           "procedure failed. Has something changed about the formatting ",
           "on Google's side?\n\n", 
           "Please check \"https://googlechromelabs.github.io/",
           "chrome-for-testing/\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, download the ZIP folder containing the chromedriver
  driverReq <- GET(dlLink)
  
  
  # Make sure the request was successful
  if (driverReq$status_code != 200) {
    
    paste0("Failed to Download New Chromedriver\n\n",
           "A GET request sent to \"", dlLink, "\" has failed. The HTTP status ",
           "code was ", driverReq$status_code, ". Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Make a directory in "binman_chromedriver" for this new driver
  newDir <- paste0(app_dir("chromedriver"), "/win32/", chosenVersion)
  
  
  # Make sure the directory does not already exist
  # Delete it if that's the case
  if (dir.exists(newDir)) {
    dir_delete(newDir)
  }
  
  
  # Create the directory
  dir.create(newDir)
  
  
  # Download the content in 'driverReq' as a ZIP file in the new folder
  writeBin(content(driverReq),
           paste0(newDir, "/chromedriver-win32.zip"))
  
  
  # Extract the contents of the ZIP file to 'newDir'
  unzip(paste0(newDir, "/chromedriver-win32.zip"),
        exdir = newDir, junkpaths = TRUE, overwrite = TRUE)
  
  # 'junkpaths' is set to TRUE so that an extra "chromedriver-win32" sub-folder
  # is not created when unzipping the file
  
  
  # Finally, return the chosen driver version
  return(chosenDriver$version)
  
}



scrapeCIMIS <- function (stationVec, startDate, endDate, 
                         userLogin, driverVersion) {
  
  # Try to scrape data from CIMIS using Selenium
  
  # The main webpage for CIMIS has a section with FTP data downloads
  
  # A CSV file with station data can be downloaded from there
  
  # That file will be read in and formatted the same way as data obtained
  # from CIMIS's API
  
  # That final data frame will be returned by this function
  
  
  # If the final intended output file "daily_report.csv" is already present in the 
  # "WebData" folder, remove it
  outFile <- "WebData/daily_report.csv"
  
  
  if (file.exists(outFile)) {
    unlink(outFile)
  }
  
  
  # First, setup the server and remote driver for RSelenium
  
  
  # Get an open network port
  openPort <- free_port()
  
  
  # Prepare extra capabilities for the Selenium instance
  
  # Set the default download folder location to be the "WebData" folder
  # The specified settings are as follows:
  #   - By default, don't allow popups
  #   - Don't open a prompt to ask where to download files
  #   - The default download directory is the "WebData" folder
  # NOTE: The download directory must be specified using backslashes
  exCap <- list(chromeOptions = 
                  list(prefs = 
                         list("profile.default_content_settings.popups" = 0L,
                              "download.prompt_for_download" = FALSE,
                              "download.default_directory" = 
                                paste0(getwd(), "/WebData") |> normalizePath())))
  
  
  # Open the server
  server <- chrome(port = openPort, 
                   version = driverVersion, 
                   verbose = FALSE)
  
  Sys.sleep(0.6)
  
  
  # Prepare the Chrome instance and wait a bit
  rd <- remoteDriver(browserName = "chrome", port = openPort,
                     extraCapabilities = exCap)
  
  Sys.sleep(1.2)
  
  
  # Open the bot window
  rd$open(silent = TRUE)
  
  Sys.sleep(0.8)
  
  
  # Initiate the login procedures
  seleniumLogin(rd, server, userLogin)
  
  
  # Fill out the form on CIMIS's data download webpage
  seleniumFormFill(rd, server, startDate, endDate, stationVec)
  
  
  # If "daily_report.csv" has not yet finished downloading, wait a little longer
  while ("daily_report.csv.crdownload" %in% list.files("WebData")) {
    
    message("Waiting for \"daily_report.csv\" to finish downloading!")
    cat("\n\n")
    
    Sys.sleep(runif(1, min = 1.2, max = 3.6))
    
  }
  
  
  # After that, log out of CIMIS 
  clickButton(rd, server, '//*[@id="top-of-page"]/div/div/nav/div/div[2]/div/a[3]')
  
  loopWait(rd, server, "log out?")
  
  
  clickButton(rd, server, '//*[@id="logoutModal"]/div/div/div[3]/button[2]')
  
  
  # Then, close the remote driver and turn off the server
  try(rd$quit(), silent = TRUE)
  try(server$stop(), silent = TRUE)
  
  
  # Read in "daily_report.csv"
  cimisDF <- getFile(outFile)
  
  
  # Process the file and return it
  return(processSeleniumCIMIS(cimisDF))
  
}



seleniumLogin <- function (rd, server, userLogin) {
  
  # Given a user's login credentials (username and password in 'userLogin'),
  # attempt to log into CIMIS
  
  
  # First, navigate to the login page
  
  
  # Visit the homepage of the new CIMIS website
  rd$navigate("https://cimis-uat.water.ca.gov/")
  
  loopWait(rd, server, "((Login</a>)|(Go to New CIMIS))")
  
  
  # The site may show an intermediate screen, noting that there is a new CIMIS page
  # If so, click on the button to visit the new website
  if (grepl("Go to New CIMIS", rd$getPageSource())) {
    
    clickButton(rd, server, '//*[@id="app"]/div/div[2]/div/div/div/div[1]/button/div[1]')
    
    loopWait(rd, server, "Login</a>")
    
  }
  
  
  # Click on the button to access the login page
  clickButton(rd, server, '//*[@id="top-of-page"]/div/div/nav/div/div[2]/div/a[2]')

  loopWait(rd, server, "Password")
  
  
  # Fill in the username field with the first element of 'userLogin'
  fillInput(rd, server, 
            '//*[@id="email"]',
            userLogin[1])
  
  
  # Add a password next
  # (This is the second element in 'userLogin')
  fillInput(rd, server, 
            '//*[@id="password"]',
            userLogin[2])
  
  
  # Click the "Login" button
  clickButton(rd, server, '//*[@id="next"]')
  
  loopWait(rd, server, "My Account")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



seleniumFormFill <- function (rd, server, startDate, endDate, stationVec) {
  
  # Fill out the "Station Reports" page on CIMIS's website
  
  # This will allow the user to generate a report with CIMIS data
  
  
  # Navigate to CIMIS's data download webpage
  rd$navigate("https://cimis-uat.water.ca.gov/data/station-reports")
  
  loopWait(rd, server, "Station Reports")
  loopWait(rd, server, "Report Type:")
  
  
  # Set the Report Style to "Daily"
  fillInput(rd, server,
            '//*[@id="reportTypeId"]',
            "d")
  
  
  # The output format will be "CSV Report"
  fillInput(rd, server,
            '//*[@id="reportFormatId"]',
            "c")
  
  
  # Set the unit of measure to "Metric Units"
  fillInput(rd, server, 
            '//*[@id="unitId"]',
            "m")
  
  
  # Set the "From Date" to 'startDate'
  
  # Clear out pre-filled text first
  clearText(rd, server, '//*[@id="pv_id_1"]')
  
  
  # Fill in the desired start date
  fillInput(rd, server,
            '//*[@id="pv_id_1"]',
            startDate |> format("%m/%d/%Y"))
  
  
  # Set the "To Date" to 'endDate'
  
  # Clear out pre-filled text first
  clearText(rd, server, '//*[@id="pv_id_4"]')
  
  
  # Fill in the desired end date
  fillInput(rd, server,
            '//*[@id="pv_id_4"]',
            endDate |> format("%m/%d/%Y"))
  
  
  # Select stations next
  
  # In case some of the stations are now inactive, check the box to show
  # inactive stations in the list
  stationButton <- rd$findElement(using = "id", "showAllStations")
  
  
  # Scroll to that element (and simultaneously click it)
  stationButton$sendKeysToElement(list(key = "space"))
  
  Sys.sleep(0.2)
  
  
  # Scroll down further slightly
  rd$executeScript("window.scrollBy(0, 50);")
  
  Sys.sleep(0.5)
  
  
  # The CIMIS stations are contained within rows of a table
  # The IDs of these elements do not match the station IDs, unfortunately
  
  
  # Still, there is a search bar that can help search for and click on stations
  for (i in 1:length(stationVec)) {
    
    # Clear out the search bar
    clearText(rd, server,
              '//*[@id="main-content"]/div/div/div/div/section/div/form/div[5]/div/div[1]/div/input',
              useDelete = TRUE)
    
    
    # Input the station ID into the search bar
    fillInput(rd, server,
              '//*[@id="main-content"]/div/div/div/div/section/div/form/div[5]/div/div[1]/div/input',
              stationVec[i] |> as.character())
    
    
    # Click on the checkbox for the station
    # (It should always be the first result in the table after filling in the search bar)
    clickButton(rd, server,
                '//*[@id="main-content"]/div/div/div/div/section/div/form/div[5]/div/div[2]/div/table/tbody[1]/tr/td[1]/div/input')
    
  }
  
  
  # Wait a little bit before proceeding
  Sys.sleep(0.6)
  
  
  # Finally, click the "Run Report" button to generate the CSV file
  clickButton(rd, server, '//*[@id="main-content"]/div/div/div/div/section/div/form/div[4]/button')
  
  
  # Wait a bit so that the file can be downloaded
  Sys.sleep(1)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



clickButton <- function (rd, server, val, searchType = "xpath") {
  
  # Use an element's attribute/value/xpath to locate it 
  # ('xpath' is the default method)
  
  # Then click on it
  
  
  # Find the element
  foundElement <- try(rd$findElement(using = searchType, value = val))
  
  
  # Error Check
  # Stop if no element is found or if more than one element is found
  if (length(foundElement) != 1 || "try-error" %in% class(foundElement)) {
    
    # Stop the remote driver and server
    try(rd$quit(), silent = TRUE)
    try(server$stop(), silent = TRUE)
    
    
    # Then output an error message
    paste0("Could Not Find Specified Element\n\n",
           "The element whose ", searchType, " is \"", val, 
           "\" was not found.",
           if_else(length(foundElement) != 1,
                   paste0(" The input returned ", length(foundElement), " ",
                          "matches."),
                   "")) |>
      errWrap() |>
      stop()
    
  }
  
  
  # Click on the element
  tryRes <- try(foundElement$clickElement())
  
  
  if (!is.null(tryRes) && "try-error" %in% class(tryRes)) {
    
    # Stop the remote driver and server
    try(rd$quit(), silent = TRUE)
    try(server$stop(), silent = TRUE)
    
    
    # Then output an error message
    paste0("Could Not Click the Element\n\n",
           "The element whose ", searchType, " is \"", val, 
           "\" could not be interacted with. It may be hidden.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Wait around a second before continuing
  Sys.sleep(runif(1, min = 1.4, max = 1.9))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



fillInput <- function (rd, server, val, input, searchType = "xpath") {
  
  # Use an element's attribute/value/xpath to locate it 
  # ('xpath' is the default method)
  
  # Then type 'input' into it
  
  
  # Find the element
  foundElement <- try(rd$findElement(using = searchType, value = val))
  
  
  # Error Check
  # Stop if no element is found or if more than one element is found
  if (length(foundElement) != 1 || "try-error" %in% class(foundElement)) {
    
    # Stop the remote driver and server
    try(rd$quit(), silent = TRUE)
    try(server$stop(), silent = TRUE)
    
    
    # Then output an error message
    paste0("Could Not Find Specified Element\n\n",
           "The element whose ", searchType, " is \"", val, 
           "\" was not found. The input returned ", length(foundElement),
           "matches.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Input the text into the element
  foundElement$sendKeysToElement(sendKeys = list(input))
  
  
  # Wait up to a second before continuing
  Sys.sleep(runif(1, min = 1.0, max = 1.8))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



loopWait <- function (rd, server, breakStr, sleepTime = 3, maxCount = 15) {
  
  # Wait in an infinite `while` loop 
  # Stop when 'breakStr' is detected in the page's HTML
  
  # The maximum number of loops is defined in 'maxCount'
  
  # 'sleepTime' sets the waiting period between loops
  
  
  # This counter tracks the loop iteration number
  counter <- 0
  
  
  # Continue while 'counter' is less than 'maxCount'
  while (counter < maxCount) {
    
    # Suspend operations for some time (default is 3 seconds)
    Sys.sleep(sleepTime)
    
    
    # If 'breakStr' is detected in the page's HTML, break the loop
    if (grepl(breakStr, rd$getPageSource())) {
      break
    }
    
    
    counter <- counter + 1
    
  }
  
  
  # If 'counter' reaches 'maxCount'
  # That means that the page never loaded in time
  # Output an error message in that case
  if (counter == maxCount) {
    
    # Stop the remote driver and server
    try(rd$quit(), silent = TRUE)
    try(server$stop(), silent = TRUE)
    
    
    # Then output an error message
    paste0("HTML Page Did Not Load As Expected\n\n",
           "The script was waiting for \"", breakStr, "\" to appear in the ",
           "page's HTML. However, the maximum number of iterations was ",
           "reached. Please investigate the cause.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



clearText <- function (rd, server, val, searchType = "xpath", 
                       useBackspace = TRUE, useDelete = FALSE, numIter = 15) {
  
  # Clear out typed input using the "Backspace" and/or "Delete" keys
  
  
  # Find an element and click it
  clickButton(rd, server, val, searchType = searchType)
  
  
  # Input "Backspace" and/or "Delete" to clear text in this element
  for (i in 1:numIter) {
    
    if (useBackspace) {
      
      rd$sendKeysToActiveElement(list(key = "backspace"))
      
    }
    
    
    if (useDelete) {
      
      rd$sendKeysToActiveElement(list(key = "delete"))
      
    }
    
    
    # Wait a bit before proceeding
    Sys.sleep(0.1)
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



processSeleniumCIMIS <- function (cimisDF) {
  
  # Given the Selenium-based output for CIMIS data, 
  # process it to match the formatting for CIMIS API data
  # that is set in `formatResponse`
  
  # Identify the locations of several key columns:
  # "Date", "Precip (mm)", "Max Air Temp (C)", "Min Air Temp (C)"
  
  # The weather data fields' "QC" columns will be gathered as well
  
  
  # Define a vector that contains the target columns and their planned renames
  renameVec <- c("DATE" = "Date",
                 "STATION_ID" = "Station Number",
                 "TMIN" = "Min Air Temp (C)",
                 "TMAX" = "Max Air Temp (C)",
                 "PRECIP" = "Precip (mm)",
                 "TMIN_QC" = "Min Air Temp QC",
                 "TMAX_QC" = "Max Air Temp QC",
                 "PRECIP_QC" = "Precip QC")
  
  
  # The next step is to extract the desired columns from 'cimisDF' 
  # and rename them
  cimisDF <- cimisDF |>
    select(all_of(renameVec))
  
  
  # Return 'cimisDF'
  return(cimisDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
