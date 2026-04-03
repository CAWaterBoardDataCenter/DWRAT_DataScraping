# Run the PRMS model
# Use the copy of the model files in the "ProcessedData" folder 


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Supply_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRS_009_Run_PRMS.R'!\n")
  
  
  # Confirm that the "RR_PRMS" folder was copied to "ProcessedData"
  prmsPath <- validateModelCopy_PRMS()
  
  
  # Get the path to the batch file stored in the "windows" folder
  batPath <- paste0(prmsPath, "/windows/run.bat") |>
    normalizePath(mustWork = TRUE)
  
  
  # Notify the user of the impending model run
  cat("[1/1]\tStarting up model...\n")
  
  
  # Get the current time (for time tracking purposes)
  startTime <- Sys.time()
  
  
  # Run the batch file
  # All output in the Command Prompt window will be saved to 'modelOutput'
  modelOutput <- system(batPath, intern = TRUE)
  
  
  # Get the current time (for time tracking purposes)
  endTime <- Sys.time()
  
  
  # Check for errors
  # There should be several output files
  checkForModelOutputs_PRMS(prmsPath, modelOutput, 
                            includeScriptGeneratedOutput = FALSE)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # Save the model output to a file
  modelOutput |>
    writeOutput(paste0(prmsPath, "/PRMS/output/PRMS_Console_Output.txt"),
                "write_lines", quietly = TRUE)
  
  
  # After that, tell the user how long the model run took
  cat(paste0("\n\nThe model ran in ", 
            difftime(endTime, startTime, units = "mins") |> round(),
            " minutes!\n"))
  
  
  # Output a completion message
  cat(col_green("\n'RRS_009_Run_PRMS.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
