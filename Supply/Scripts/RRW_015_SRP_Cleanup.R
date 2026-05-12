# After the SRP run has completed successfully, 
# copy key output files into the hydrology model input/output folder

# Then, delete the "SRPHM_update_ag" folder from the "ProcessedData" folder


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
  cat("Starting 'RRW_015_SRP_Cleanup.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "ProcessedData" folder too
  cat("[1/3]\tChecking directories...\n")
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the "SRPHM_update_ag" folder was copied to "ProcessedData"
  srpPath <- validateModelCopy_SRP()
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tCopying output files...\n")
  
  
  # Copy output files into the hydrology folder
  copyOutputs(srpPath, dirPath, startDate, endDate)
  
  
  cat("\tDone!\n\n")
  
  
  # The final step is to delete the "SRPHM_update_ag" folder that was copied to 
  # the "ProcessedData" folder
  cat("[3/3]\tDeleting the model files...\n")
  
  
  deleteFiles(srpPath)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_015_SRP_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyOutputs <- function (srpPath, dirPath, startDate, endDate) {
  
  # Copy several files from the PRMS "output" folder
  # into the hydrology folder 
  
  
  # This vector contains the names of the desired output files
  copyFiles <- c("gsflow.log",
                 paste0("SRP_inflow_", 1:6, ".gag"),
                 "SRP_inflow_11465500.gag",
                 "SRP_inflow_11465660.gag",
                 "SRP_inflow_11465680.gag",
                 "SRP_inflow_11465690.gag",
                 "SRP_inflow_11465700.gag",
                 "SRP_inflow_11465750.gag",
                 "SRP_inflow_11466170.gag",
                 "SRP_inflow_11466200.gag",
                 "SRP_inflow_11466320.gag",
                 "SRP_inflow_11466800.gag",
                 "model_output_summary.txt",
                 "basin/basin_.csv",
                 "basin/basin__monthly.csv")
  
  
  # Confirm that they exist in the "output" folder first
  checkForModelOutputs_SRP(srpPath, modelOutput = NULL)
  
  
  # Prepare vectors that contain the proper filepaths and the planned filepaths
  sourcePaths <- paste0(srpPath, "/", copyFiles) |>
    normalizePath(mustWork = TRUE)
  
  
  # Use the same exact filenames in the hydrology directory's SRP output folder
  writePaths <- copyFiles |>
    str_remove("^.+[/\\\\]") |>
    paste0(dirPath, "/SRP/Output/", 
           ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # Copy the files
  # If any of these actions fail, 'copyRes' will contain FALSE in its vector
  copyRes <- file.copy(sourcePaths, writePaths, overwrite = TRUE)
  
  
  # Verify that the files copied successfully
  # If not, output an error message
  if (anyFalse(copyRes) || anyFalse(file.exists(writePaths))) {
    
    missingFiles <- which(!copyRes)
    
    
    stop(paste0("Could Not Copy File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "The script attempted to copy SRP output files to the ",
                "hydrology folder. However, the process was not successful ",
                "for ", vec2QuotedStr(copyFiles[missingFiles]), ".\n\n",
                "Perhaps there was a permission issue? Please investigate.\n\n",
                "The intended new file", 
                if_else(length(missingFiles) > 1, "s were", " was"), ": ",
                vec2QuotedStr(writePaths[missingFiles])) |>
           errWrap())
    
  } 
  
  
  # Return nothing if there were no issues
  return(invisible(NULL))
  
}



deleteFiles <- function (srpPath) {
  
  # Delete the "SRPHM_update_ag" directory in the "ProcessedData" folder
  
  # Start by deleting that folder
  dir_delete(srpPath)
  
  
  # Confirm that it was deleted
  if (dir.exists(srpPath)) {
    
    stop(paste0("Failed to Delete SRP Directory\n\n",
                "The script attempted to delete the SRP model files that ",
                "were located in the \"ProcessedData\" folder. However, it ",
                "was unsuccessful for an unknown reason. Please investigate.\n\n",
                "(This error occurred for \"", srpPath, "\")") |>
           errWrap())
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())
