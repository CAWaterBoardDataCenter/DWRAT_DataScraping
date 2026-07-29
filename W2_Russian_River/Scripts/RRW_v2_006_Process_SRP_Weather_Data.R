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
#  (1) "W2_Russian_River/Intermediate/PRISM_SRP_Data_[startDate]_[endDate].csv"


# These files will be combined into a single output file:
#  (1) "W2_Russian_River/Output/SRP_Meteorological_[startDate]_[endDate].csv"


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
  cat("Starting 'RRW_v2_006_Process_SRP_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Start with a tibble containing every required input file
  inputFiles <- tibble("PRISM_INPUT" = 
                         getFromControl_RR("PRISM_SRP_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE),
                       "PRISM_OUTPUT" = 
                         paste0("W2_Russian_River/Intermediate/PRISM_SRP_Data_",
                                startDate, "_", endDate, ".csv"))
  
  
  # Check if any required input files are missing
  if (!all(map_lgl(inputFiles, file.exists))) {
    
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
  
  
  # Next, import functions from "RRW_012_Process_SRP_Weather_Data.R"
  c("validateInputs", "reformatClimateData") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_012_Process_SRP_Weather_Data.R",
                          .))
  
  
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
  outFile <- paste0("W2_Russian_River/Output/SRP_Meteorological_", startDate, "_",
                    endDate, ".csv")
  
  
  prismProcessed |>
    writeOutput(outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_006_Process_SRP_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())




