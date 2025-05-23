#----PURPOSE:----

# Perform the operations of "PRISM_Selenium Scraper.R" by using the POST method to fetch data.
# This script downloads precipitation data for 15 locations in the Russian River watershed
# and minimum and maximum temperature data for 8 locations in the Russian River watershed.

# Last Updated by: Aakash Prashar


#### Dependencies ####


require(tidyverse)
require(httr)


#### Functions ####

mainProcedure <- function (StartDate, EndDate) {
  
  
  # Start with gathering precipitation data
  
  
  
  # Read in station data
  statDF <- read_csv("InputData/prism_rr_precip_stations.csv", show_col_types = FALSE, col_names = FALSE)
  
  
  
  # Prepare the request content
  bodyList <- list(call = "pp/daily_timeseries_mp",
                   proc = "gridserv",
                   lons = statDF$X2 %>% paste0(collapse = "|"),  # Latitude
                   lats = statDF$X1 %>% paste0(collapse = "|"),  # Longitude
                   names = statDF$X3 %>% paste0(collapse = "|"), # Station Names
                   spares = "4km",   # Resolution
                   interp = "idw",   # Interpolate grid cell values ("0" if no)
                   stats = "ppt",    # Precipitation
                   units = "si",     # Metric units
                   range = "daily",  # Daily values
                   start = paste0(StartDate$year, twoDigitText(StartDate$month), twoDigitText(StartDate$day)),
                   end = paste0(EndDate$year, twoDigitText(EndDate$month), twoDigitText(EndDate$day)),
                   stability = "provisional")
  
  
  
  # Submit the request for precipitation data
  getPRISM(bodyList, "WebData/PRISM_Precip_Raw.csv")
  
  
  
  # Wait a bit before proceeding to the next step
  Sys.sleep(1)
  
  
  
  # Next, prepare to request temperature data
  
  
  # Read in a list of temperature stations
  statDF <- read_csv("InputData/temp_fill_stations.csv", show_col_types = FALSE, col_names = FALSE)
  
  
  
  # Prepare the POST request content
  bodyList <- list(call = "pp/daily_timeseries_mp",
                   proc = "gridserv",
                   lons = statDF$X2 %>% paste0(collapse = "|"),
                   lats = statDF$X1 %>% paste0(collapse = "|"),
                   names = statDF$X3 %>% paste0(collapse = "|"),
                   spares = "4km",
                   interp = "idw",
                   stats = "tmin tmax", # Minimum and maximum temperatures
                   units = "si",
                   range = "daily",
                   start = paste0(StartDate$year, twoDigitText(StartDate$month), twoDigitText(StartDate$day)),
                   end = paste0(EndDate$year, twoDigitText(EndDate$month), twoDigitText(EndDate$day)),
                   stability = "provisional")
  
  
  
  # Submit the request for temperature data
  getPRISM(bodyList, "WebData/PRISM_Temp_Raw.csv")
  
  
  
  # Wait a bit before proceeding to the next step
  Sys.sleep(1)
  
  
  
  # The final step is to get both precipitation and temperature data for the SRP stations
  
  
  
  # Read in a list of SRP stations
  statDF <- read_csv("InputData/prism_srp_stations.csv", show_col_types = FALSE, col_names = FALSE)
  
  
  
  # Prepare the POST request content
  # The SRP stations require English units (inches and Fahrenheit)
  bodyList <- list(call = "pp/daily_timeseries_mp",
                   proc = "gridserv",
                   lons = statDF$X2 %>% paste0(collapse = "|"),
                   lats = statDF$X1 %>% paste0(collapse = "|"),
                   names = statDF$X3 %>% paste0(collapse = "|"),
                   spares = "4km",
                   interp = "idw",
                   stats = "ppt tmin tmax",
                   units = "eng",   # US Customary units
                   range = "daily",
                   start = paste0(StartDate$year, twoDigitText(StartDate$month), twoDigitText(StartDate$day)),
                   end = paste0(EndDate$year, twoDigitText(EndDate$month), twoDigitText(EndDate$day)),
                   stability = "provisional")
  
  
  
  # Submit the request for precipitation and temperature data
  getPRISM(bodyList, "WebData/PRISM_SRP_Raw.csv")
  
  
  
  # Output a completion message
  cat("Done!\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getPRISM <- function (bodyList, writePath) {
  
  # The process of getting data from PRISM involves making two POST requests
  # Write a generic process for that here
  # 'bodyList' is the body content of the initial request
  # 'writePath' is the filepath where the output file will be stored
  
  
  
  # Make the first request
  firstReq <- POST(url = "https://prism.oregonstate.edu/explorer/dataexplorer/rpc.php", 
                   body = bodyList,
                   encode = "form",
                   add_headers(Accept = "application/json, text/javascript, */*; q=0.01",
                               `Accept-Language` = "en-US,en;q=0.9",
                               `Accept-Encoding` = "gzip, deflate, br",
                               `Sec-Ch-Ua-Platform` = "Windows",
                               `User-Agent` = "R-Programming-Script",
                               `User-Contact` = "DWR-SDA@Waterboards.ca.gov",
                               `X-Requested-With` = "XMLHttpRequest",
                               `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8"))
  
  
  # Check that the request was successful
  stopifnot(firstReq$status_code == 200)
  
  
  # Extract the 'gricket' code from the response
  gricketVal <- content(firstReq) %>% as.character() %>%
    str_extract("gricket.: .+.errors.:") %>%
    str_remove("gricket.: .") %>%
    str_remove('", .error.+$')
  
  
  # Wait before sending the next request
  Sys.sleep(2)
  
  
  
  # The next step will be to send 'gricketVal' and request the CSV file path
  # If the request is very large, PRISM will need extra time to process the data
  
  
  
  # Try to send the next request with 'gricketVal'
  nextReq <- POST(url = "https://prism.oregonstate.edu/explorer/dataexplorer/rpc.php", 
                  body = list(call = "pp/checkup",
                              proc = "gridserv",
                              gricket = gricketVal),
                  encode = "form",
                  add_headers(Accept = "application/json, text/javascript, */*; q=0.01",
                              `Accept-Language` = "en-US,en;q=0.9",
                              `Accept-Encoding` = "gzip, deflate, br",
                              `Sec-Ch-Ua-Platform` = "Windows",
                              `User-Agent` = "R-Programming-Script",
                              `User-Contact` = "DWR-SDA@Waterboards.ca.gov",
                              `X-Requested-With` = "XMLHttpRequest",
                              `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8"))
  
  
  
  # Verify that the request was successful 
  stopifnot(nextReq$status_code == 200)
  
  
  
  # Get a string containing the filename of the CSV output on PRISM's server
  csvStr <- content(nextReq) %>% as.character() %>%
    str_extract("csv.: .+\\.csv.,") %>%
    str_remove(".,$") %>%
    str_remove("^csv.: .")
  
  
  
  # If PRISM needs more time, 'csvStr' will be NA
  while (is.na(csvStr)) {
    
    # In that case, wait a little bit
    # Then, resend the request
    
    
    cat("PRISM needs more time to process the request!\n")
    Sys.sleep(5)
    
    
    
    # Submit the request with 'gricketVal' again
    nextReq <- POST(url = "https://prism.oregonstate.edu/explorer/dataexplorer/rpc.php", 
                    body = list(call = "pp/checkup",
                                proc = "gridserv",
                                gricket = gricketVal),
                    encode = "form",
                    add_headers(Accept = "application/json, text/javascript, */*; q=0.01",
                                `Accept-Language` = "en-US,en;q=0.9",
                                `Accept-Encoding` = "gzip, deflate, br",
                                `Sec-Ch-Ua-Platform` = "Windows",
                                `User-Agent` = "R-Programming-Script",
                                `User-Contact` = "DWR-SDA@Waterboards.ca.gov",
                                `X-Requested-With` = "XMLHttpRequest",
                                `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8"))
    
    
    
    # Verify that the request was successful 
    stopifnot(nextReq$status_code == 200)
    
    
    
    # Get a string containing the filename of the CSV output on PRISM's server
    csvStr <- content(nextReq) %>% as.character() %>%
      str_extract("csv.: .+\\.csv.,") %>%
      str_remove(".,$") %>%
      str_remove("^csv.: .")
    
  }
  
  
  
  # Wait
  Sys.sleep(1.2)
  
  
  
  # Access that file and save it to a file
  paste0("https://prism.oregonstate.edu/explorer/tmp/", csvStr) %>%
    read_lines() %>%
    writeLines(writePath)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



twoDigitText <- function (num) {
  
  # This function is called when a number is being written to a string
  # If it has only one digit, a zero will be added to the beginning
  
  
  if (str_count(num, "[0-9]") == 2) {
    
    return(num)
    
  } else if (str_count(num, "[0-9]") == 1) {
    
    return(paste0("0", num))
    
  } else {
    
    stop("This function only works with numbers that have up to two digits")
    
  }
  
}



#### Script Execution ####


print("Starting 'PRISM_HTTP_Scraper.R'...")


mainProcedure(StartDate, EndDate)


remove(mainProcedure, getPRISM, twoDigitText)