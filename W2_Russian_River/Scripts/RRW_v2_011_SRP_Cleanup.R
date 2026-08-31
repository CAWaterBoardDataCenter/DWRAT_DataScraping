# After the SRP run has completed successfully, 
# copy key output files into the hydrology model input/output folder

# Then, delete the "SRPHM" folder from the "Output" folder


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
  cat("Starting 'RRW_v2_011_SRP_Cleanup.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "Output" folder too
  cat("[1/3]\tChecking directories...\n")
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the "SRPHM" folder was copied to "Output"
  srpPath <- validate_model_copy("SRPHM")
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tCopying output files...\n")
  
  
  # Import the model copy and deletion functions from the PRMS script
  c("copy_model_outputs", "deleteFiles") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_011_PRMS_Cleanup.R", .))
  
  
  # Copy output files into the hydrology folder
  copy_model_outputs("SRPHM", srpPath, dirPath, startDate, endDate,
                     additionalInputFiles = c("external_files/prms_ic_WY1975_toWY2021.out",
                                              "external_files/restartdata_WY1975_to_WY2021.out"), 
                     archiveModelName = "SRP")
  
  
  cat("\tDone!\n\n")
  
  
  # The final step is to delete the "SRPHM" folder that was copied to 
  # the "Output" folder
  cat("[3/3]\tDeleting the model files...\n")
  
  
  deleteFiles(srpPath, "SRPHM")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_011_SRP_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
