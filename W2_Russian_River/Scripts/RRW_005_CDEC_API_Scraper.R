# Download precipitation data from CDEC at various locations 
# in the Russian River watershed

# The first required input is a CSV file with one column:
#  (1) STATION_ID

# These IDs should be character strings that correspond to the (generally 
# three-character) IDs used by CDEC
# (https://cdec.water.ca.gov/webgis/?appid=cdecstation)


# The raw output will be stored in the "Intermediate" folder as 
# "CDEC_API_Precip_Data_[startDate]_[endDate].csv"

# Note: US Customary units are used for the output (inches)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_005_CDEC_API_Scraper.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Read in the list of stations 
  stationDF <- getFromControl_RR("CDEC_PRECIPITATION_STATIONS_CSV") |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, "CDEC_PRECIPITAITON_CSV", "CDEC")
  
  
  # Output a message
  cat(paste0("\nGetting precipitation data for ", nrow(stationDF), " CDEC station",
             if_else(nrow(stationDF) > 1, "s", ""),
             "...\n"))
  
  
  # Get data for all CDEC stations at once
  cdecDF <- requestCDEC(stationDF$STATION_ID, startDate, endDate, 
                        sensorNum = 45)
  
  
  # Add another message
  cat("\tDone!\n\n")
  
  
  # Define the output file name as well
  outFile <- paste0("W2_Russian_River/Intermediate/CDEC_API_Precip_Data_", 
                    startDate, "_", endDate, ".csv")
  
  
  # Write the file to the "Intermediate" folder
  writeOutput(cdecDF, outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_005_CDEC_API_Scraper.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
