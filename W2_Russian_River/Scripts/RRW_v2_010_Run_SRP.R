# Run the SRP model
# Use the copy of the model files in the "Output" folder 


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_v2_010_Run_SRP.R'!\n")
  
  
  # Confirm that the "SRPHM" folder was copied to "Output"
  srpPath <- validateModelCopy_SRP_2024()
  
  
  # Get the path to the batch file stored in the root directory
  batPath <- paste0(srpPath, "/run_SRPHM_spinup.bat") |>
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
  checkForModelOutputs_SRP_2024(srpPath, modelOutput)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # After that, tell the user how long the model run took
  cat(paste0("\n\nThe model ran in ", 
             difftime(endTime, startTime, units = "mins") |> round(),
             " minutes!\n"))
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_010_Run_SRP.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
