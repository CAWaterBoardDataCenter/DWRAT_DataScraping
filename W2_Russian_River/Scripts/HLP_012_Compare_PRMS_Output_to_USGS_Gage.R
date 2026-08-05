# Compare streamflow data in a PRMS output file to USGS gage data at the same location
# This script is specifically designed for comparing values in the "sub_cfs" file to 
# USGS gage 11461500 (Calpella) and 11464000 (Healdsburg)
# Those comparisons would be with sub-basins 2 and 13, respectively


# Precipitation data is included in some versions of these comparison plots too

# There are two sources of precipitation data: PRISM grid cell averages and
# the average precipitation among gages in the PRMS DAT file


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (gageID = "11464000", subbasin = 13) {
  
  cat("\n\n")
  cat("Starting 'HLP_012_Compare_PRMS_Output_to_USGS_Gage.R'!\n")
  
  
  # Notify the user which USGS gage is being assessed
  cat(paste0("\n\nComparing streamflow data from Sub-Basin ", subbasin, " ", 
             "to USGS Gage ", gageID, "!\n\n"))
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/3]\tGetting \"sub_cfs\" file and precipitation data...\n")
  
  
  # Confirm that the model hydrology folder exists and get its directory path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Confirm that the "sub_cfs" file is present in the PRMS "output" folder
  # (This is the cumulative counterpart of the "sub_inq" output file)
  subPath <- paste0(dirPath, "/PRMS/Output/RR_PRMS_Output_", startDate, 
                    "_", endDate, "_sub_cfs.csv") |>
    checkForPreviousOutput()
  
  
  # Read in the CSV file
  subDF <- getFile(subPath, trim_ws = TRUE)
  
  
  # Validate the contents of 'subDF'
  # To do this, borrow the "validateSubCSV" function from the Raw Flows script
  c("validateSubCSV", "getColsFromMetadata") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_017_Generate_Raw_Flows.R", .))
  
  
  subDF <- subDF |>
    validateSubCSV(subPath, dirPath, type = "sub")
  
  
  # Rename "Date" to "DATE" in 'subDF' to help with later steps
  # In addition, rename the key sub-basin column to "Flow" so that SRP-related
  # functions can be reused ("Flow" is the parameter name in the SRP gag files)
  subDF <- subDF |>
    rename(DATE = Date,
           Flow = as.character(subbasin))
  
  
  # Gather precipitation data next
  
  
  # All of the required columns are defined in this script's counterpart,
  # "HLP_011_Compare_SRP_Output_to_USGS_Gage.R"
  
  # Import those functions and apply them
  c("gatherPrecipPRISM", "validateAndSummarizePRISM",
    "gatherPrecipDAT") |>
    map(~ functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
  # Use the average precipitation among PRISM grid cells in the PRMS model domain
  # However, this may be split between one to three files
  
  # Because of its complexity, use a separate function to gather (and archive)
  # this dataset
  prismDF <- gatherPrecipPRISM(dirPath, endDate, model = "PRMS")
  
  
  # Read in precipitation data from the PRMS DAT file too
  datDF <- gatherPrecipDAT(dirPath, startDate, endDate, model = "PRMS")
  
  
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
  
  
  # Once again, import functions from the "SRP" script
  c("checkForValidKey", "requestUSGS", "validateUSGS") |>
    map(~ functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
  # Check if the user provided an API key in the control spreadsheet
  # If so, import and validate it
  apiKey <- checkForValidKey()
  
  
  # Send a HTTP GET request
  usgsDF <- requestUSGS(stationID = gageID, 
                        startDate = min(subDF$DATE), endDate = max(subDF$DATE),
                        apiKey = apiKey)
  
  
  # Validate and process the returned dataset
  usgsDF <- usgsDF |>
    validateUSGS()
  
  
  cat("\tDone!\n\n")
  
  
  # Finally, compare the two datasets
  # Produce plots and calculate parameters such as Nash-Sutcliffe Efficiency
  cat("\n[3/3]\tComparing gage data and model results...\n")
  
  
  # Import the required functions from "HLP_011_Compare_SRP_Output_to_USGS_Gage.R"
  c("compareGageAndModel", "prepNewDirectory", "generatePlotsAndTable",
    "generateStreamflowPlot", "getNiceAxisBreaks", "setPrecipColumnWidths",
    "generateComparisonScatterplot", "calcMetrics") |>
    map(~ functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  compareGageAndModel(usgsDF, subDF, dirPath, gageID, subPath, prismDF, datDF,
                      model = "PRMS")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  "'HLP_012_Compare_PRMS_Output_to_USGS_Gage.R' is complete!\n\n" |>
    col_green() |>
    cat()
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

# Call the procedure for two sub-basins and USGS gages
mainProcedure(gageID = "11461500", subbasin = 2)
mainProcedure(gageID = "11462080", subbasin = 4)
mainProcedure(gageID = "11464000", subbasin = 13)


# Clean up
base::remove(list = ls())
