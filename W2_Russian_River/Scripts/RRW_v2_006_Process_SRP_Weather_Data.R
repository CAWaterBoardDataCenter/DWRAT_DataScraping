# Verify that all required PRISM weather data has been downloaded
# Then, reformat the data into a structure suitable for the SRP DAT file


# This script has two required input files:

# The first one is the SRP station input file for PRISM

# This time, in addition to the "STATION_ID" column, the script requires 
# columns that link these stations to specific columns in the SRP DAT input file

# Thus, the required fields are:
#  (1) STATION_ID
#  (2) SRP_PRECIP_NAME
#  (3) SRP_TMIN_NAME
#  (4) SRP_TMAX_NAME

# Every SRP station should be linked to at least one column among the 
# 2 precipitation columns and 2 max/min temperature columns

# In addition to these files, the output of the PRISM web scraping script 
# is required:
#  (1) "WebData/PRISM_SRP_Data_[startDate]_[endDate].csv"


# These files will be combined into a single output file:
#  (1) "ProcessedData/SRP_Meteorological_[startDate]_[endDate].csv"


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
  cat("Starting 'RRW_v2_006_Process_SRP_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Start with a tibble containing every required input file
  inputFiles <- tibble("PRISM_INPUT" = 
                         getFromControl_RR("PRISM_SRP_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE),
                       "PRISM_OUTPUT" = 
                         paste0("WebData/PRISM_SRP_Data_",
                                startDate, "_", endDate, ".csv"))
  
  
  # Check if any required input files are missing
  if (anyFalse(map_lgl(inputFiles, file.exists))) {
    
    # Get the names of the missing files before sending a message
    missingFiles <- inputFiles[!map_lgl(inputFiles, file.exists)]
    
    
    # Output the error
    stop(paste0("Missing Required Input File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "This script requires that the PRISM web scraping script ",
                "was run for the chosen date range (",
                startDate, " to ", endDate, ")\n\n",
                "However, the following file", 
                if_else(length(missingFiles) > 1, "s are", " is"), 
                " missing:\n\n",
                paste0(" (*) ", names(missingFiles), ": \"", 
                       missingFiles, "\"", collapse = "\n\n"), "\n\n",
                "Please prepare any required input files and then run ",
                "the corresponding script", 
                if_else(length(missingFiles) > 1, "s", ""),
                " first") |>
           errWrap())
    
  }
  
  
  # Read in the files next
  prismInput <- inputFiles$PRISM_INPUT[1] |> getFile() |> unique()
  prismDF <- getPRISM(inputFiles$PRISM_OUTPUT[1])
  
  
  # Validate the variables next
  cat("[1/2]\tChecking input files...\n")
  
  
  # Ensure that they have the expected formatting
  validateInputs(prismInput, prismDF, inputFiles)
  
  
  cat("\tDone!\n\n")
  
  
  # After all validation requirements have been cleared, prepare a single
  # properly formatted meteorological dataset using PRISM data
  cat("[2/2]\tPreparing final meteorological dataset...\n")
  
  
  prismProcessed <- reformatClimateData(prismDF, prismInput, "PRISM")
  
  
  cat("\tDone!\n\n")
  
  
  # Once this step is complete, write 'prismProcessed' to a file
  outFile <- paste0("ProcessedData/SRP_Meteorological_", startDate, "_",
                    endDate, ".csv")
  
  
  prismProcessed |>
    writeOutput(outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_006_Process_SRP_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateInputs <- function (prismInput, prismDF, inputFiles) {
  
  # Verify that all input tibbles are formatted as expected
  
  
  # The number of expected SRP precipitation columns is hard-coded as 2
  # Similarly, the number of expected minimum/maximum temperature columns is 2
  numPrecip <- 2
  numTemp <- 2
  
  
  # First, check the input PRISM tibble
  validateStationInputs(prismInput, inputFiles$PRISM_INPUT[1], "SRP", 
                        numPrecip, numTemp)
  
  
  # Validate the weather output tibble next
  
  # Using a general function for all weather sources, check 'prismDF'
  validateWebData(prismDF, "PRISM", inputFiles$PRISM_OUTPUT[1], 
                  prismInput$STATION_ID, siPRISM = FALSE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



reformatClimateData <- function (climateDF, climateInput, dataSource) {
  
  # The 'climateDF' data frames need to be widened 
  # (so that each station's data is in its own separate column)
  
  # The "SRP" column names in 'climateInput' will then be used to switch 
  # from the station IDs to the SRP field names
  fieldNameVec <- validateWebData_expectedColumnNames(dataSource, siPRISM = FALSE)
  
  
  # Start by renaming the columns in 'climateDF' to be consistent 
  # Then, pivot the dataset into a wider format (where each station has 
  # three of its own columns--one for each SRP field)
  widerDF <- climateDF |>
    select(all_of(fieldNameVec)) |>
    pivot_wider(names_from = STATION_ID,
                values_from = c(PRECIP, TMIN, TMAX),
                names_sep = "_")
  
  
  # After that, prepare the SRP-equivalent names using 'climateInput'
  # Appending the station IDs to "PRECIP"/"TMAX"/"TMIN" gives the 
  # column names that appear in 'widerDF'
  # The values in "SRP_PRECIP_NAME", "SRP_TMAX_NAME", and "SRP_TMIN_NAME" 
  # are the intended replacements for these column names
  equivalentNames <- climateInput |>
    mutate(NAME_1 = paste0("PRECIP_", STATION_ID),
           NAME_2 = paste0("TMAX_", STATION_ID),
           NAME_3 = paste0("TMIN_", STATION_ID)) |>
    select(NAME_1, NAME_2, NAME_3, 
           SRP_PRECIP_NAME, SRP_TMAX_NAME, SRP_TMIN_NAME)
  
  
  # Create a vector from 'equivalentNames' that can be used with rename()
  renameVec <- c(equivalentNames$NAME_1, equivalentNames$NAME_2,
                 equivalentNames$NAME_3) |>
    set_names(c(equivalentNames$SRP_PRECIP_NAME, equivalentNames$SRP_TMAX_NAME, 
                equivalentNames$SRP_TMIN_NAME))
  
  
  # Not every station will be used for precipitation and max/min temperature
  # In those cases, the names will be "NA"
  # Remove them from 'renameVec'
  renameVec <- renameVec[!is.na(names(renameVec)) & renameVec != "NA"]
  
  
  # After that, apply 'renameVec' to 'widerDF'
  # Then, keep DATE and the renamed variables only
  processedDF <- widerDF |>
    rename(any_of(renameVec)) |>
    select(DATE, any_of(names(renameVec)))
  
  
  # Return 'processedDF'
  return(processedDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())




