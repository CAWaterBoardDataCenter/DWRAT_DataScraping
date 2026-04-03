# Run the SRP model
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
  cat("Starting 'RRS_014_Run_SRP.R'!\n")
  
  
  # Confirm that the "SRPHM_update_ag" folder was copied to "ProcessedData"
  srpPath <- validateModelCopy_SRP()
  
  
  # Get the path to the batch file stored in the root directory
  batPath <- paste0(srpPath, "/Run_updated_Model.bat") |>
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
  checkForModelOutputs_SRP(srpPath, modelOutput, 
                           includeScriptGeneratedOutput = FALSE)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # After that, tell the user how long the model run took
  cat(paste0("\n\nThe model ran in ", 
             difftime(endTime, startTime, units = "mins") |> round(),
             " minutes!\n"))
  
  
  # Output a completion message
  cat(col_green("\n'RRS_014_Run_SRP.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
