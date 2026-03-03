# Download precipitation and temperature data from NOAA at various stations  
# in the Russian River watershed


# The required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be the GHCND IDs (e.g., "USC00043875") 
# ("GHCND" stands for Global Historical Climatology Network Daily)


# The raw output will be stored in the "WebData" folder as 
# "NOAA_API_Data_[startDate]_[endDate].csv"

# Note: SI units are used for the output (mm and Celsius)


#### Setup ####

remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


# 
options(timeout = 500)


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRS_002_NOAA_API_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/1]\tGetting climate data for GHCND stations on NOAA...\n")
  
  
  # Read in the list of stations 
  stationDF <- getFromSupplyControl_RR("NOAA_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateInput(stationDF, "NOAA_STATIONS_CSV")
  
  
  # Prepare the request URL for NOAA
  requestURL <- paste0("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries",
                       "&stations=", stationDF$STATION_ID |> unique() |> paste0(collapse = ","),
                       "&startDate=", startDate, "T00:00:00",
                       "&endDate=", endDate, "T23:59:59", 
                       "&dataTypes=PRCP,TMAX,TMIN", "&format=csv",
                       "&options=includeAttributes:false,includeStationName:true",
                       ",includeStationLocation:false",
                       "&units=standard")
  
  
  # Define the output file name as well
  outFile <- paste0("WebData/NOAA_API_Data_", startDate, "_",
                    endDate, ".csv")
  
  
  # Download the file to the "WebData" folder
  download.file(requestURL, outFile, mode = "wb", quiet = TRUE)
  
  
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
  
  cat(col_green("\n'RRS_002_NOAA_API_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateInput <- function (stationDF, sourceField) {
  
  # Make sure that 'stationDF' is formatted correctly
  # If there are any issues, notify the user
  
  
  # 'stationDF' should contain at least one column: "STATION_ID"
  if (!("STATION_ID" %in% names(stationDF))) {
    
    stop(paste0("Station Input File - Column Issue\n\n",
                "The input file containing GHCND stations does not have ",
                "the required column (\"STATION_ID\"). ",
                "Please correct this file and try again.\n\n",
                "The input file must contain the GHCND IDs (e.g., 'USC00043875') ",
                "for each target location\n\n",
                "Also, the name of this column must match exactly\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), "')") |>
           errWrap() |>
           str_replace("(does not)", col_red("\\1")) |>
           str_replace("(exactly)", col_red("\\1")))
    
  }
  
  
  # Make sure there are no missing entries in the "STATION_ID" column
  if (anyNA(stationDF$STATION_ID)) {
    
    stop(paste0("Station Input File - Missing Data Issue\n\n",
                "The input file containing target GHCND stations has one or more ",
                "missing elements in its required column (\"STATION_ID\")\n\n", 
                "Please fill in any empty entries in this column\n\n",
                "(This error occurred for '", getFromSupplyControl_RR(sourceField), "')") |>
           errWrap() |>
           str_replace("(missing)", col_red("\\1")))
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()


# Clean up
remove(list = ls())
