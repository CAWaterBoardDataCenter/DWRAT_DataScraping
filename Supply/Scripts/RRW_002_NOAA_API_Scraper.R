# Download precipitation and temperature data from NOAA at various stations  
# in the Russian River watershed


# The required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be the GHCND IDs (e.g., "USC00043875") 
# ("GHCND" stands for Global Historical Climatology Network Daily)


# The raw output will be stored in the "WebData" folder as 
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
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


# Allow greater time to download data from NOAA
# (This is only relevant for large data downloads)
options(timeout = 500) # 500 seconds



#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_002_NOAA_API_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/1]\tGetting climate data for GHCND stations on NOAA...\n")
  
  
  # Read in the list of stations 
  stationDF <- getFromControl_RR("NOAA_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "NOAA_STATIONS_CSV", "NOAA")
  
  
  # Prepare the request URL for NOAA
  requestURL <- paste0("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries",
                       "&stations=", stationDF$STATION_ID |> unique() |> paste0(collapse = ","),
                       "&startDate=", startDate, "T00:00:00",
                       "&endDate=", endDate, "T23:59:59", 
                       "&dataTypes=PRCP,TMAX,TMIN", "&format=csv",
                       "&options=includeAttributes:true,includeStationName:true",
                       ",includeStationLocation:false",
                       "&units=standard")
  
  
  # Define the output file name as well
  outFile <- paste0("WebData/NOAA_API_Data_", startDate, "_",
                    endDate, ".csv")
  
  
  # Download the file to the "WebData" folder
  download.file(requestURL, outFile, mode = "w", quiet = TRUE)
  
  
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
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  cat(col_green("\n'RRW_002_NOAA_API_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
