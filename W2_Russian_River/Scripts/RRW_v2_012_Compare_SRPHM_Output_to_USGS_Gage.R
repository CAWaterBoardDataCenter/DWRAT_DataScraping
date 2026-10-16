# Compare data in SRPHM gags file to USGS gage data at the same location

# Streamflow comparison charts will be produced, alongside statistical metrics

# Precipitation data is included in some versions of these comparison plots too

# There are two sources of precipitation data: PRISM grid cell averages and
# the average precipitation among gages in the SRP DAT file


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (gageID = "11466800") {
  
  cat("\n\n")
  cat("Starting 'RRW_v2_012_Compare_SRPHM_Output_to_USGS_Gage.R'!\n")
  
  
  # Notify the user which USGS gage is being assessed
  cat(paste0("\n\nRunning comparison for USGS Gage ", gageID, "!\n\n"))
  
  
  # Rely heavily on the functions from 
  # "W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R"
  c("gatherPrecipPRISM", "validateAndSummarizePRISM", "gatherPrecipDAT",
    "checkForValidKey", "requestUSGS", "validateUSGS", "compareGageAndModel",
    "prepNewDirectory", "generatePlotsAndTable", "generateStreamflowPlot",
    "getNiceAxisBreaks", "setPrecipColumnWidths", "generateComparisonScatterplot",
    "calcMetrics") |>
    map(~ functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/3]\tGetting gag file and precipitation data...\n")
  
  
  # Confirm that the model hydrology folder exists and get its directory path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Confirm that the gage's corresponding gag file is present in the model "output" folder
  gagPath <- paste0(dirPath, "/SRPHM/Output/", gageID, ".gag") |>
    checkForPreviousOutput()
  
  
  # Read in the gag file
  gagDF <- read_gag(gagPath)
  
  
  # Validate the contents of 'gagDF'
  # To do this, borrow the "validateGag" function from the Raw Flows script
  c("validateGag", "getColsFromMetadata") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_017_Generate_Raw_Flows.R", .))
  
  
  gagDF <- gagDF |>
    validateGag(gagPath, dirPath)
  
  # NOTE: `validateGag` will also add a "DATE" column to the file
  
  
  # Gather precipitation data next
  
  
  # Use the average precipitation among PRISM grid cells in the SRP model domain
  # However, this may be split between one to three files
  
  # Because of its complexity, use a separate function to gather (and archive)
  # this dataset
  prismDF <- gatherPrecipPRISM(dirPath, endDate, "SRPHM")
  
  
  # Read in precipitation data from the SRP DAT file too
  datDF <- gatherPrecipDAT(dirPath, startDate, endDate, "SRPHM")
  
  
  # For consistency, have both 'prismDF' and 'datDF' use "PRECIP" as the
  # name for their precipitation columns (and "Date" for dates)
  
  # Also, their units should be inches
  
  # 'datDF' is already setup to meet these requirements
  # 'prismDF' must be adjusted though
  # (mm = 1/25.4 in)
  prismDF <- prismDF |>
    mutate(PRECIP = `ppt (mm)` / 25.4)
  
  
  cat("\tDone!\n\n")
  
  
  # Next, request data from USGS 
  cat("\n[2/3]\tGetting data from USGS...\n")
  
  
  # Check if the user provided an API key in the control spreadsheet
  # If so, import and validate it
  apiKey <- checkForValidKey()
  
  
  # Send a HTTP GET request
  usgsDF <- requestUSGS(stationID = gageID, 
                        startDate = min(gagDF$DATE), endDate = max(gagDF$DATE),
                        apiKey = apiKey)
  
  
  # Validate and process the returned dataset
  usgsDF <- usgsDF |>
    validateUSGS()
  
  
  cat("\tDone!\n\n")
  
  
  # Finally, compare the two datasets
  # Produce plots and calculate parameters such as Nash-Sutcliffe Efficiency
  cat("\n[3/3]\tComparing gage data and model results...\n")
  
  
  compareGageAndModel(usgsDF, gagDF, dirPath, gageID, gagPath, prismDF, datDF, "SRPHM")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  "'RRW_v2_012_Compare_SRPHM_Output_to_USGS_Gage.R' is complete!\n\n" |>
    col_green() |>
    cat()
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

# Call the procedure for multiple USGS gages
mainProcedure(gageID = "11466800")
mainProcedure(gageID = "11465500")
mainProcedure(gageID = "11465700")


# Clean up
base::remove(list = ls())
