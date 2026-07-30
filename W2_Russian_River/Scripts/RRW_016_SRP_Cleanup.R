# After the SRP run has completed successfully, 
# copy key output files into the hydrology model input/output folder

# Then, delete the "SRPHM_update_ag" folder from the "Output" folder


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
  cat("Starting 'RRW_016_SRP_Cleanup.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "Output" folder too
  cat("[1/3]\tChecking directories...\n")
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the "SRPHM_update_ag" folder was copied to "Output"
  srpPath <- validateModelCopy_SRP()
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tCopying output files...\n")
  
  
  # Copy output files into the hydrology folder
  copyOutputs(srpPath, dirPath, startDate, endDate)
  
  
  cat("\tDone!\n\n")
  
  
  # The final step is to delete the "SRPHM_update_ag" folder that was copied to 
  # the "Output" folder
  cat("[3/3]\tDeleting the model files...\n")
  
  
  # Import the model deletion function from the PRMS script
  functionStealer("W2_Russian_River/Scripts/RRW_011_PRMS_Cleanup.R", "deleteFiles")
  
  
  deleteFiles(srpPath, "SRP")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_016_SRP_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyOutputs <- function (srpPath, dirPath, startDate, endDate) {
  
  # Copy several files from the SRP folder (including the control file)
  # into the hydrology folder 
  
  
  # Confirm that they exist in the model folder first
  checkForModelOutputs_SRP(srpPath, modelOutput = NULL)
  
  
  # Get a vector of paths to all important outputs
  copyFiles <- getModelOutputs_SRP(srpPath)
  
  
  # Prepare vectors that contain the proper filepaths and the planned filepaths
  sourcePaths <- copyFiles |>
    normalizePath(mustWork = TRUE)
  
  
  # Use the same exact filenames in the hydrology directory's SRP output folder
  writePaths <- copyFiles |>
    str_remove("^.+[/\\\\]") |>
    paste0(dirPath, "/SRP/Output/", 
           ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # Add rows to 'sourcePaths' and 'writePaths' for "SRPHM_update.control"
  sourcePaths <- c(sourcePaths,
                   paste0(srpPath, "/SRPHM_update.control") |>
                     checkForPreviousOutput())
  
  
  writePaths <- c(writePaths,
                  paste0(dirPath, "/SRP/Input/SRPHM_update.control"))
  
  
  # Copy the files using the `copyFile` function
  # If any of these actions fail, the function will trigger an error 
  map2(sourcePaths, writePaths, copyFile) 
  
  
  # Return nothing if there were no issues
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
