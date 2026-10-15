# Download precipitation and temperature data from NOAA at various stations  
# in the Russian River watershed


# The required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be the GHCND IDs (e.g., "USC00043875") 
# ("GHCND" stands for Global Historical Climatology Network Daily)


# The raw output will be stored in the "Intermediate" folder as 
# "NOAA_API_Data_[startDate]_[endDate].csv"

# Note: PRMS requires SI units (mm and Celsius)
# 
#       However, this data will be downloaded with standard units (inches 
#       and Fahrenheit)
#       A later script will convert this data into the proper units
#
#       The reason for this decision is because the SI data from the API is 
#       rounded to one decimal place, despite having one extra digit in the raw
#       measurements
#       More of this precision can be recovered when customary units are 
#       obtained and converted


#### Setup ####

base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


# Allow greater time to download data from NOAA
# (This is only relevant for large data downloads)
options(timeout = 5000) # 5000 seconds (~83 minutes)



#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_002_NOAA_API_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/1]\tGetting climate data for GHCND stations on NOAA...\n")
  
  
  # Read in the list of stations 
  stationDF <- getFromControl_RR("NOAA_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "NOAA_STATIONS_CSV", "NOAA")
  
  
  # Download the data using another function
  request_noaa(stationDF, startDate, endDate)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  cat(col_green("\n'RRW_002_NOAA_API_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



request_noaa <- function (stationDF, startDate, endDate, splitName = "") {
  
  # Download data for GHCND stations from NOAA
  
  # This URL obtains precipitation, maximum temperature, and minimum temperature
  # data from one or more stations in US customary units
  
  
  # Start by defining the output file name
  # (This is done early because it may be needed in the next check)
  outFile <- paste0("W2_Russian_River/Intermediate/NOAA_API_Data_", startDate, "_",
                    endDate, splitName, ".csv")
  
  
  # Before proceeding, check for excessively large requests
  # If the requested date range exceeds 5,000 days, split the request
  if (difftime(endDate, startDate, units = "days") > 5000) {
    
    return(stationDF |>
             split_request_noaa(startDate, endDate, outFile, maxGap = 5000))
    
  }
  
  
  # Prepare the request URL for NOAA
  requestURL <- paste0("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries",
                       "&stations=", stationDF$STATION_ID |> unique() |> paste0(collapse = ","),
                       "&startDate=", startDate, "T00:00:00",
                       "&endDate=", endDate, "T23:59:59", 
                       "&dataTypes=PRCP,TMAX,TMIN", "&format=csv",
                       "&options=includeAttributes:true,includeStationName:true",
                       ",includeStationLocation:false",
                       "&units=standard")
  
  
  # Download the file to the "Intermediate" folder
  download.file(requestURL, outFile, mode = "w", quiet = TRUE)
  
  
  # To Do: Retry failed NOAA calls
  
  
  # Confirm that 'outFile' exists
  # If not, output an error message
  if (!file.exists(outFile)) {
    
    stop(paste0("NOAA API Call Failed\n\n",
                "The output file was not detected in the expected directory\n\n",
                "The API call may have failed, please investigate this issue\n\n") |>
           errWrap() |>
           str_replace("(not)", col_red("\\1")) |>
           str_replace("(investigate)", col_green("\\1")))
    
  }
  
  
  # Return the output filename
  return(outFile)
  
}



split_request_noaa <- function (stationDF, startDate, endDate, finalPath, maxGap = 5000) {
  
  # Divide a request to NOAA into smaller chunks
  
  
  # The first step is to determine the start and end bounds for these requests
  functionStealer("W2_Russian_River/Scripts/RRW_004_CIMIS_API_Scraper.R", "splitDays")
  
  
  # Obtain new start and end dates to use in these requests
  dateVec <- splitDays(startDate, endDate, maxGap)
  
  
  # Output a message to the user to inform them of the split
  cat(paste0("\n\tSplitting into ", length(dateVec) - 1, " requests...\n"))
  
  
  # Iterate through 'dateVec' and submit requests to NOAA
  for (i in 2:length(dateVec)) {
    
    # Start with a status message
    cat(paste0("\n\t[", i - 1, "/", length(dateVec) - 1, "]\tRequesting...\n"))
    
    
    # Take two consecutive dates from 'dateVec' 
    # and request the data for all dates between them
    iterRes <- stationDF |>
      request_noaa(dateVec[i - 1], dateVec[i], splitName = paste0("_", i - 1))
    
    # 'splitName' will contain a value that can help distinguish between 
    # different iterations' intermediate results
    
    # It will appear as part of the output filename (like "_1" or "_2") 
    # at the end of the name (before the extension)
    
    
    # At this point, 'iterRes' contains a path to the downloaded file
    # Redefine the variable and replace its path with the actual data
    iterRes <- iterRes |>
      getFile()
    
    
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
    Sys.sleep(runif(1, min = 1.1, max = 2.4))
    
  }
  
  
  # End the function by writing 'combinedDF' to 'finalPath'
  combinedDF |>
    writeOutput(finalPath)
  
  
  # Finally, return 'finalPath'
  return(finalPath)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
